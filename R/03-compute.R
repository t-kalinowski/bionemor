evo2_recipe_lock <- function() {
  path <- system.file(
    "recipes",
    "evo2.json",
    package = "bionemor",
    mustWork = TRUE
  )
  lock <- jsonlite::read_json(path, simplifyVector = TRUE)
  stopifnot(
    "unsupported Evo 2 recipe lock schema" =
      identical(lock$schema_version, 1L)
  )
  lock
}

#' Describe the pinned BioNeMo Evo 2 recipe
#'
#' Automatic local image builds are limited to the verified package lock.
#' Use an external runtime or supply an explicit prebuilt image when `recipe`
#' is unverified.
#'
#' @param revision Exact BioNeMo Recipes commit. `"recommended"` uses the
#'   package lock.
#' @param repository Optional BioNeMo Recipes repository.
#' @param base_image Optional NGC PyTorch base image. A digest-qualified
#'   reference records its digest; the locked image and digest are canonicalized
#'   to the package lock.
#' @param allow_mutable Allow a revision other than a full commit SHA.
#'
#' @return An S7 `BioNeMoRecipe`.
#' @export
evo2_recipe <- function(
  revision = "recommended",
  repository = NULL,
  base_image = NULL,
  allow_mutable = FALSE
) {
  lock <- evo2_recipe_lock()
  stopifnot(
    "revision must be one non-empty string" = is_scalar_string(revision),
    "repository must be NULL or one non-empty string" =
      is.null(repository) || is_scalar_string(repository),
    "base_image must be NULL or one non-empty string" =
      is.null(base_image) || is_scalar_string(base_image),
    "allow_mutable must be TRUE or FALSE" =
      is_scalar_logical(allow_mutable)
  )

  revision <- if (identical(revision, "recommended")) {
    lock$revision
  } else {
    revision
  }
  stopifnot(
    "revision must be a full commit SHA unless allow_mutable is TRUE" =
      allow_mutable || grepl("^[0-9a-fA-F]{40}$", revision)
  )
  if (grepl("^[0-9a-fA-F]{40}$", revision)) {
    revision <- tolower(revision)
  }

  repository <- repository %||% lock$repository
  base_image <- base_image %||% lock$base_image
  if (
    identical(
      base_image,
      paste0(lock$base_image, "@", lock$base_image_digest)
    )
  ) {
    base_image <- lock$base_image
  }
  base_image_digest <- if (identical(base_image, lock$base_image)) {
    lock$base_image_digest
  } else if (grepl("@sha256:[0-9a-fA-F]{64}$", base_image)) {
    tolower(sub("^.*@(sha256:[0-9a-fA-F]{64})$", "\\1", base_image))
  } else {
    NULL
  }
  verified <- identical(repository, lock$repository) &&
    identical(revision, lock$revision) &&
    identical(base_image, lock$base_image)

  BioNeMoRecipe(
    repository = repository,
    revision = revision,
    recipe_version = lock$recipe_version,
    subdirectory = lock$subdirectory,
    base_image = base_image,
    base_image_digest = base_image_digest,
    bridge_protocol = as.integer(lock$bridge_protocol),
    verified = verified
  )
}

recipe_base_image_reference <- function(recipe) {
  stopifnot(
    "recipe must be a BioNeMo recipe" =
      S7_inherits(recipe, BioNeMoRecipe)
  )
  if (is.null(recipe@base_image_digest)) {
    return(recipe@base_image)
  }
  base <- sub("@sha256:[0-9a-fA-F]{64}$", "", recipe@base_image)
  paste0(base, "@", recipe@base_image_digest)
}

default_recipe_image <- function(recipe) {
  stopifnot(
    "recipe must be a BioNeMo recipe" =
      S7_inherits(recipe, BioNeMoRecipe)
  )
  paste0("bionemor/evo2:", substr(recipe@revision, 1L, 12L))
}

sha256_file_digest <- function(path) {
  stopifnot(
    "path must be one readable file" =
      is_scalar_string(path) &&
        file.exists(path) &&
        !dir.exists(path) &&
        file.access(path, 4L) == 0L,
    "sha256sum is required to record file provenance" =
      nzchar(Sys.which("sha256sum"))
  )
  result <- processx::run(
    "sha256sum",
    c("--", path),
    error_on_status = FALSE,
    echo = FALSE,
    env = process_environment()
  )
  digest <- substr(trimws(result$stdout), 1L, 64L)
  stopifnot(
    "failed to compute the file SHA-256 digest" =
      result$status == 0L &&
        grepl("^[0-9a-fA-F]{64}$", digest)
  )
  paste0("sha256:", tolower(digest))
}

slurm_image_digest <- function(compute) {
  stopifnot(
    "compute must describe a Slurm container runtime" =
      S7_inherits(compute, BioNeMoCompute) &&
        identical(compute@backend, "slurm") &&
        identical(compute@engine, "container"),
    "Slurm container execution requires an image" =
      is_scalar_string(compute@image)
  )
  if (is_scalar_string(compute@image_digest)) {
    return(compute@image_digest)
  }
  stopifnot(
    "Slurm container image must be a readable local SIF file or a digest-qualified URI" =
      file.exists(compute@image) &&
        !dir.exists(compute@image)
  )
  sha256_file_digest(compute@image)
}

slurm_sif_verification_lines <- function(compute) {
  stopifnot(
    "compute must describe a Slurm container runtime" =
      S7_inherits(compute, BioNeMoCompute) &&
        identical(compute@backend, "slurm") &&
        identical(compute@engine, "container"),
    "Slurm container execution requires a resolved image digest" =
      is_scalar_string(compute@image_digest) &&
        grepl("^sha256:[0-9a-fA-F]{64}$", compute@image_digest)
  )
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

#' Describe BioNeMo execution resources
#'
#' @param backend Execution backend.
#' @param engine Container execution or an externally managed recipe runtime.
#' @param workspace Writable workspace shared with the runtime.
#' @param recipe A recipe descriptor from [evo2_recipe()].
#' @param image Container image. `NULL` selects the deterministic local image
#'   name derived from `recipe`. Slurm container execution requires a readable
#'   SIF on the shared filesystem or a digest-qualified image URI. Unverified
#'   recipes require an explicit prebuilt image for local container execution.
#' @param gpus,nodes Positive integer resource counts. Version 1 supports one
#'   node.
#' @param queue,account,walltime Optional Slurm fields.
#' @param config Named site-specific configuration.
#'
#' @return An S7 `BioNeMoCompute`.
#' @export
bionemo_compute <- function(
  backend = c("local", "slurm"),
  engine = c("container", "external"),
  workspace = getwd(),
  recipe = evo2_recipe(),
  image = NULL,
  gpus = 1L,
  nodes = 1L,
  queue = NULL,
  account = NULL,
  walltime = NULL,
  config = list()
) {
  backend <- match.arg(backend)
  engine <- match.arg(engine)
  stopifnot(
    "workspace must be one non-empty string" = is_scalar_string(workspace),
    "recipe must be a BioNeMo recipe" =
      S7_inherits(recipe, BioNeMoRecipe),
    "image must be NULL or one non-empty string" =
      is.null(image) || is_scalar_string(image),
    "gpus must be a positive integer" = is_scalar_integerish(gpus, min = 1),
    "nodes must be a positive integer" = is_scalar_integerish(nodes, min = 1),
    "version 1 supports a single node" = nodes == 1,
    "queue must be NULL or one non-empty string" =
      is.null(queue) || is_scalar_string(queue),
    "account must be NULL or one non-empty string" =
      is.null(account) || is_scalar_string(account),
    "walltime must be NULL or one non-empty string" =
      is.null(walltime) || is_scalar_string(walltime),
    "config must be a named list" =
      is.list(config) &&
        (length(config) == 0L ||
          !is.null(names(config)) &&
            all(nzchar(names(config))) &&
            !anyDuplicated(names(config))),
    "image must be NULL when engine is 'external'" =
      engine != "external" || is.null(image),
    "Slurm container execution requires an image path or URI" =
      backend != "slurm" || engine != "container" || !is.null(image),
    "unverified recipes require an external runtime or an explicit container image" =
      recipe@verified || engine == "external" || !is.null(image)
  )

  workspace <- normalize_path(workspace)
  stopifnot(
    "workspace must not be the filesystem root" =
      !identical(dirname(workspace), workspace)
  )
  dir.create(workspace, recursive = TRUE, showWarnings = FALSE)
  stopifnot(
    "workspace could not be created" = dir.exists(workspace),
    "workspace must be writable" = file.access(workspace, 2L) == 0L
  )

  if (backend == "local" && engine == "container") {
    container_engine <- config$container_engine %||% "docker"
    stopifnot(
      "config$container_engine must be one command name" =
        is_scalar_string(container_engine) &&
          grepl("^[A-Za-z0-9_.-]+$", container_engine)
    )
    if (
      recipe@verified &&
        (
          is.null(image) ||
            identical(image, recipe@base_image) ||
            identical(image, recipe_base_image_reference(recipe))
        )
    ) {
      image <- default_recipe_image(recipe)
    }
  }

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
    nodes = as.integer(nodes),
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

#' Report capabilities advertised by the installed recipe runtime
#'
#' @param compute A BioNeMo compute descriptor.
#' @param refresh Ignore a cached helper report and probe again.
#'
#' @return The parsed helper capability report with runtime provenance.
#' @export
bionemo_capabilities <- function(compute, refresh = FALSE) {
  stopifnot(
    "compute must be a BioNeMo compute descriptor" =
      S7_inherits(compute, BioNeMoCompute),
    "refresh must be TRUE or FALSE" = is_scalar_logical(refresh)
  )
  runtime_capabilities(compute, refresh = refresh)
}
