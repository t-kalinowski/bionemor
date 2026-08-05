recipe_base_image_reference <- function(recipe) {
  if (!S7_inherits(recipe, BioNeMoRecipe)) {
    stop("recipe must be a BioNeMo recipe")
  }
  if (is.null(recipe@base_image_digest)) {
    return(recipe@base_image)
  }
  base <- sub("@sha256:[0-9a-fA-F]{64}$", "", recipe@base_image)
  paste0(base, "@", recipe@base_image_digest)
}

recipe_uv_image_reference <- function(lock) {
  stopifnot(
    "recipe lock must define the uv image" = is.list(lock) &&
      is_scalar_string(lock$uv_image) &&
      is_scalar_string(lock$uv_image_digest)
  )
  paste0(lock$uv_image, "@", lock$uv_image_digest)
}

default_recipe_image <- function(recipe) {
  if (!S7_inherits(recipe, BioNeMoRecipe)) {
    stop("recipe must be a BioNeMo recipe")
  }
  tag <- substr(recipe@revision, 1L, 12L)
  paste0(
    recipe_install_spec(recipe)$image_repository,
    ":",
    tag
  )
}

sha256_file_digest <- function(path) {
  if (
    !is_scalar_string(path) ||
      !file.exists(path) ||
      dir.exists(path) ||
      file.access(path, 4L) != 0L
  ) {
    stop("path must be one readable file")
  }
  if (!nzchar(Sys.which("sha256sum"))) {
    stop("sha256sum is required to record file provenance")
  }
  result <- processx::run(
    "sha256sum",
    c("--", path),
    error_on_status = FALSE,
    echo = FALSE,
    env = process_environment()
  )
  digest <- substr(trimws(result$stdout), 1L, 64L)
  if (result$status != 0L || !grepl("^[0-9a-fA-F]{64}$", digest)) {
    stop("failed to compute the file SHA-256 digest")
  }
  paste0("sha256:", tolower(digest))
}

slurm_image_digest <- function(compute) {
  if (
    !S7_inherits(compute, BioNeMoCompute) ||
      !identical(compute@backend, "slurm") ||
      !identical(compute@engine, "container")
  ) {
    stop("compute must describe a Slurm container runtime")
  }
  if (!is_scalar_string(compute@image)) {
    stop("Slurm container execution requires an image")
  }
  if (is_scalar_string(compute@image_digest)) {
    return(compute@image_digest)
  }
  if (!file.exists(compute@image) || dir.exists(compute@image)) {
    stop(
      "Slurm container image must be a readable local SIF file or a digest-qualified URI"
    )
  }
  sha256_file_digest(compute@image)
}

slurm_sif_verification_lines <- function(compute) {
  if (
    !S7_inherits(compute, BioNeMoCompute) ||
      !identical(compute@backend, "slurm") ||
      !identical(compute@engine, "container")
  ) {
    stop("compute must describe a Slurm container runtime")
  }
  if (
    !is_scalar_string(compute@image_digest) ||
      !grepl("^sha256:[0-9a-fA-F]{64}$", compute@image_digest)
  ) {
    stop("Slurm container execution requires a resolved image digest")
  }
  if (grepl("@sha256:[0-9a-fA-F]{64}$", compute@image)) {
    return(character())
  }
  expected <- sub("^sha256:", "", tolower(compute@image_digest))
  c(
    paste0(
      "BIONEMOR_SIF_SHA256=$(",
      shell_join("sha256sum", c("--", compute@image)),
      ")"
    ),
    "BIONEMOR_SIF_SHA256=\"${BIONEMOR_SIF_SHA256%% *}\"",
    paste0(
      "if [[ \"$BIONEMOR_SIF_SHA256\" != ",
      shQuote(expected),
      " ]]; then"
    ),
    "  printf 'Slurm SIF SHA-256 does not match installed digest\\n' >&2",
    "  exit 66",
    "fi",
    "unset BIONEMOR_SIF_SHA256"
  )
}

#' Describe where BioNeMo operations run
#'
#' A compute descriptor records where commands run, how the BioNeMo recipe
#' runtime is supplied, which workspace is shared with that runtime, and which
#' GPU or scheduler resources to request. Constructing the descriptor creates
#' the workspace if needed, but does not install software, start a container,
#' probe a GPU, or submit a Slurm job.
#'
#' @section Models and compute:
#'
#' A model and compute have separate roles. A family constructor such as
#' [evo2()] or [esm2()] describes model identity,
#' checkpoint or source, and model-level settings. `bionemo_compute()` describes
#' the execution environment and its recipe. An operation uses the supplied
#' compute descriptor or one bound to a model by [evo2_model()] or
#' [esm2_model()].
#' Keeping them separate lets a runtime be installed and diagnosed before a
#' model is prepared and lets an offline model descriptor run in more than one
#' compatible environment.
#'
#' @section Backends and engines:
#'
#' `backend` selects where bionemor launches commands:
#'
#' - `"local"` launches them from the current machine.
#' - `"slurm"` writes a job script, submits it with `sbatch`, reads its state
#'   with `sacct`, and cancels it with `scancel`. Slurm support is experimental;
#'   please report problems at the package's issue tracker.
#'
#' `engine` selects how the recipe runtime is provided:
#'
#' - `"container"` runs commands in an image. Local execution uses Docker by
#'   default; set `config = list(container_engine = "podman")` to use another
#'   Docker-compatible command. Slurm execution uses Apptainer and requires an
#'   existing SIF path or digest-qualified image URI.
#'   Apptainer support is experimental; please report problems through the
#'   package issue tracker.
#' - `"external"` runs the recipe commands and package helper directly in an
#'   environment managed outside bionemor. Those commands must be on the
#'   execution environment's `PATH`. `image` must be `NULL`.
#'
#' bionemor can build the verified recipe image only for the local/container
#' combination. For either external combination, and for Slurm/container,
#' [bionemo_install()] verifies an existing environment instead of creating it.
#'
#' @section Workspace and image:
#'
#' `workspace` is normalized, created if absent, and required to be writable.
#' It is the working directory for external commands and is mounted at the same
#' absolute path inside containers. Relative checkpoint, dataset, and artifact
#' destinations resolve below this directory, and bionemor stores durable run
#' metadata in its `.bionemor` subdirectory. For container and Slurm execution,
#' these inputs and outputs must remain visible at their recorded paths. The
#' submitting process and Slurm compute nodes must therefore resolve the
#' workspace to the same shared storage.
#'
#' With the verified recipe and local/container execution, `image = NULL` or
#' the recipe's locked base-image tag or digest-qualified reference selects the
#' deterministic derived image that [bionemo_install()] builds. Any other image
#' reference is treated as a prebuilt recipe image and inspected rather than
#' rebuilt.
#' Slurm/container requires `image`: a local SIF must be readable on shared
#' storage, while a digest-qualified URI records its supplied digest. External
#' runtimes do not use an image property.
#'
#' @section Resources and site settings:
#'
#' `gpus` is the required GPU count used for compatibility checks and Slurm
#' requests. Local containers expose all available GPUs. Execution uses one
#' node. `queue`, `account`, and `walltime` become Slurm
#' `--partition`, `--account`, and `--time` directives and are ignored by the
#' local backend.
#'
#' `config` is for site-specific runtime metadata. The supported user-facing
#' setting is `container_engine` for local containers. [bionemo_install()]
#' stores a validated capability report in `config$capabilities` on the returned
#' descriptor; users normally should not populate that entry themselves.
#'
#' @param recipe Recipe descriptor such as [evo2_recipe()] or [esm2_recipe()].
#' @param backend Where commands are launched: `"local"` or `"slurm"`.
#' @param engine How the recipe runtime is supplied: `"container"` or
#'   `"external"`.
#' @param workspace Writable workspace used by R and the runtime. It must not be
#'   the filesystem root. Slurm requires the same absolute path on the
#'   submitting host and compute nodes.
#' @param image Container image. For local/container execution, `NULL` or the
#'   verified recipe's base-image tag or digest-qualified reference selects the
#'   deterministic derived recipe image; another reference selects a prebuilt
#'   recipe image. Slurm/container requires a readable SIF on shared storage or
#'   a digest-qualified image URI. Unverified recipes require an explicit image
#'   unless `engine = "external"`.
#' @param gpus Positive integer GPU count.
#' @param queue,account,walltime Optional Slurm partition, account, and time
#'   limit. These values are passed to `sbatch` without interpretation.
#' @param config Named list of site-specific settings. Use `container_engine`
#'   to replace the default `"docker"` command for local containers.
#'
#' @return An S7 `BioNeMoCompute` descriptor. The returned object describes
#'   execution but is not installed or verified until [bionemo_install()] runs.
#'
#' @examples
#' # Descriptor construction is offline and does not require a GPU.
#' workspace <- file.path(tempdir(), "bionemor-compute-example")
#' compute <- bionemo_compute(
#'   recipe = evo2_recipe(),
#'   engine = "external",
#'   workspace = workspace
#' )
#' compute
#'
#' \dontrun{
#' # A site-managed runtime on a Slurm cluster.
#' slurm_compute <- bionemo_compute(
#'   recipe = evo2_recipe(),
#'   backend = "slurm",
#'   engine = "external",
#'   workspace = "/shared/projects/evo2",
#'   gpus = 4L,
#'   queue = "gpu",
#'   account = "biology",
#'   walltime = "01:00:00"
#' )
#' }
#'
#' @seealso [evo2()], [evo2_model()], [esm2()], [esm2_model()],
#'   [bionemo_install()], [bionemo_doctor()]
#' @export
bionemo_compute <- function(
  recipe,
  backend = c("local", "slurm"),
  engine = c("container", "external"),
  workspace = getwd(),
  image = NULL,
  gpus = 1L,
  queue = NULL,
  account = NULL,
  walltime = NULL,
  config = list()
) {
  backend <- match.arg(backend)
  engine <- match.arg(engine)
  if (!is_scalar_string(workspace)) {
    stop("workspace must be one non-empty string")
  }
  if (!S7_inherits(recipe, BioNeMoRecipe)) {
    stop("recipe must be a BioNeMo recipe")
  }
  if (!is.null(image) && !is_scalar_string(image)) {
    stop("image must be NULL or one non-empty string")
  }
  if (!is_scalar_integerish(gpus, min = 1)) {
    stop("gpus must be a positive integer")
  }
  if (!is.null(queue) && !is_scalar_string(queue)) {
    stop("queue must be NULL or one non-empty string")
  }
  if (!is.null(account) && !is_scalar_string(account)) {
    stop("account must be NULL or one non-empty string")
  }
  if (!is.null(walltime) && !is_scalar_string(walltime)) {
    stop("walltime must be NULL or one non-empty string")
  }
  if (
    !is.list(config) ||
      length(config) != 0L &&
        (is.null(names(config)) ||
          !all(nzchar(names(config))) ||
          anyDuplicated(names(config)))
  ) {
    stop("config must be a named list")
  }
  if (engine == "external" && !is.null(image)) {
    stop("image must be NULL when engine is 'external'")
  }
  if (backend == "slurm" && engine == "container" && is.null(image)) {
    stop("Slurm container execution requires an image path or URI")
  }
  if (!recipe@verified && engine != "external" && is.null(image)) {
    stop(
      "unverified recipes require an external runtime or an explicit container image"
    )
  }

  workspace <- normalize_path(workspace)
  if (identical(dirname(workspace), workspace)) {
    stop("workspace must not be the filesystem root")
  }
  dir.create(workspace, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(workspace)) {
    stop("workspace could not be created")
  }
  if (file.access(workspace, 2L) != 0L) {
    stop("workspace must be writable")
  }

  managed_recipe_image <- FALSE
  if (backend == "local" && engine == "container") {
    container_engine <- config$container_engine %||% "docker"
    if (
      !is_scalar_string(container_engine) ||
        !grepl("^[A-Za-z0-9_.-]+$", container_engine)
    ) {
      stop("config$container_engine must be one command name")
    }
    if (
      recipe@verified &&
        (is.null(image) ||
          identical(image, recipe@base_image) ||
          identical(image, recipe_base_image_reference(recipe)))
    ) {
      image <- default_recipe_image(recipe)
      managed_recipe_image <- TRUE
    }
  }
  config$.bionemor_managed_recipe_image <- managed_recipe_image

  image_digest <- NULL
  if (!is.null(image) && grepl("@sha256:[0-9a-fA-F]{64}$", image)) {
    image_digest <- sub("^.*@(sha256:[0-9a-fA-F]{64})$", "\\1", image)
  }

  BioNeMoCompute(
    backend = backend,
    engine = engine,
    workspace = workspace,
    recipe = recipe,
    image = image,
    image_digest = image_digest,
    gpus = as.integer(gpus),
    nodes = 1L,
    queue = queue,
    account = account,
    walltime = walltime,
    config = config
  )
}

method(print, BioNeMoCompute) <- function(x, ...) {
  cat("<BioNeMo compute>\n", sep = "")
  cat("Backend:   ", x@backend, "\n", sep = "")
  cat("Engine:    ", x@engine, "\n", sep = "")
  cat("Workspace: ", x@workspace, "\n", sep = "")
  if (!is.null(x@image)) {
    cat("Image:     ", x@image, "\n", sep = "")
  }
  cat(
    "Recipe:    ",
    x@recipe@recipe_version,
    " @ ",
    substr(x@recipe@revision, 1L, 8L),
    "\n",
    sep = ""
  )
  cat("Resources: ", x@nodes, " node(s), ", x@gpus, " GPU(s)\n", sep = "")
  invisible(x)
}

#' Report capabilities of a BioNeMo recipe runtime
#'
#' `bionemo_capabilities()` runs the package helper in the environment described
#' by `compute` and parses its JSON report. The probe verifies that the helper's
#' protocol, recipe version, and recipe revision match the compute descriptor.
#' It also reports whether the selected recipe's commands for its implemented
#' inference, training, or checkpoint-conversion operations are available.
#'
#' This is a live GPU-backed probe when no cached report is used. A local
#' container is started with GPU access, an external runtime runs the helper
#' directly, and a Slurm backend submits a synchronous probe allocation. The
#' probe does not prepare a model or inspect model weights.
#'
#' @section Report contents:
#'
#' The returned named list contains helper, protocol, recipe, command, feature,
#' and runtime information, plus adapter-specific entries. `runtime` includes
#' software versions, CUDA availability, the GPU count, driver information, and
#' per-GPU properties. bionemor adds `image`, `image_digest`, and the UTC
#' `probed_at` time so the report records where it came from.
#'
#' [bionemo_install()] saves the validated report in
#' `compute@config$capabilities` on the compute descriptor it returns. With
#' `refresh = FALSE`, this function returns that cached list when present;
#' otherwise it probes the runtime. `refresh = TRUE` always probes again. Calling
#' this function does not modify the supplied compute descriptor.
#'
#' @param compute A BioNeMo compute descriptor. Its runtime must expose the
#'   package helper. Use [bionemo_install()] to verify the helper and every
#'   required recipe command.
#' @param refresh Whether to ignore `compute@config$capabilities` and run a new
#'   runtime probe.
#'
#' @return A named list parsed from the helper capability report, with `image`,
#'   `image_digest`, and `probed_at` runtime provenance added by bionemor.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(),
#'   engine = "external",
#'   workspace = "/shared/projects/evo2"
#' )
#' compute <- bionemo_install(compute)
#'
#' capabilities <- bionemo_capabilities(compute)
#' capabilities[c("protocol_version", "recipe_version", "probed_at")]
#' capabilities$commands
#' capabilities$runtime$gpus
#'
#' # Probe again after the runtime or GPU allocation changes.
#' capabilities <- bionemo_capabilities(compute, refresh = TRUE)
#' }
#'
#' @seealso [bionemo_install()], [bionemo_doctor()]
#' @export
bionemo_capabilities <- function(compute, refresh = FALSE) {
  if (!S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be a BioNeMo compute descriptor")
  }
  if (!is_scalar_logical(refresh)) {
    stop("refresh must be TRUE or FALSE")
  }
  runtime_capabilities(compute, refresh = refresh)
}
