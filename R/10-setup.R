recipe_install_spec <- function(recipe) {
  stopifnot(
    "recipe must be a BioNeMo recipe" = S7_inherits(recipe, BioNeMoRecipe)
  )
  spec <- adapter_function(recipe@adapter, "install_spec")(recipe)
  stopifnot(
    "adapter install spec must be a list" = is.list(spec),
    "adapter install spec is incomplete" = all(
      c(
        "lock",
        "helper",
        "helper_asset",
        "helper_filename",
        "semantic_operations",
        "docker_appendage",
        "image_repository",
        "image_version",
        "probes",
        "command_keys"
      ) %in%
        names(spec)
    ),
    "adapter uv fallback must be NULL or one string" = is.null(
      spec$uv_fallback
    ) ||
      is_scalar_string(spec$uv_fallback)
  )
  spec
}

recipe_install_lock <- function(recipe) {
  lock <- recipe_install_spec(recipe)$lock
  if (recipe@verified && !identical(recipe@revision, lock$revision)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "recipe revision is not supported by the installed recipe lock",
      operation = "install",
      recipe_revision = recipe@revision,
      expected_recipe_revision = lock$revision,
      hint = "Use the package-pinned recipe revision."
    )
  }
  if (!is_scalar_string(lock$dockerfile_blob)) {
    bionemor_abort(
      "BN_RECIPE_MISSING",
      "recipe lock does not contain a Dockerfile blob",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Reinstall bionemor from a complete package build."
    )
  }
  lock
}

package_asset <- function(...) {
  path <- system.file(..., package = "bionemor")
  if (nzchar(path)) {
    return(path)
  }
  development <- file.path(...)
  if (!file.exists(development)) {
    bionemor_abort(
      "BN_RECIPE_MISSING",
      paste("required package asset is missing:", development),
      operation = "install",
      hint = "Reinstall bionemor from a complete package build."
    )
  }
  normalizePath(development, mustWork = TRUE)
}

install_paths <- function(compute) {
  root <- file.path(
    compute@workspace,
    ".bionemor",
    "recipes",
    compute@recipe@revision
  )
  list(
    root = root,
    source = file.path(root, "source"),
    context = file.path(root, "build-context")
  )
}

recipe_image_requires_build <- function(compute) {
  compute@recipe@verified &&
    isTRUE(compute@config$.bionemor_managed_recipe_image)
}

expected_container_image_labels <- function(
  recipe,
  helper_revision = package_helper_revision(recipe)
) {
  spec <- recipe_install_spec(recipe)
  c(
    "org.opencontainers.image.source" = recipe@repository,
    "org.opencontainers.image.revision" = recipe@revision,
    "org.opencontainers.image.version" = spec$image_version,
    "io.bionemor.helper.revision" = helper_revision,
    "io.bionemor.base.image" = recipe@base_image,
    "io.bionemor.base.digest" = recipe@base_image_digest %||% "",
    "io.bionemor.bridge.protocol" = as.character(recipe@bridge_protocol)
  )
}

install_probe_commands <- function(recipe, target = "all") {
  probes <- recipe_install_spec(recipe)$probes
  if (identical(target, "all")) {
    return(unique(unlist(probes, use.names = FALSE)))
  }
  commands <- probes[[target]]
  if (is.null(commands)) {
    stop("unsupported installation probe target", call. = FALSE)
  }
  if (!length(commands)) {
    stop(
      "recipe adapter does not support the '",
      target,
      "' operation group",
      call. = FALSE
    )
  }
  commands
}

runtime_probe_command <- function(
  compute,
  executable,
  args = character(),
  gpus = FALSE,
  immutable = TRUE
) {
  stopifnot(
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "probe executable must be one command name" = is_scalar_string(executable),
    "probe arguments must be a character vector" = is.character(args) &&
      !anyNA(args),
    "gpus must be TRUE or FALSE" = is_scalar_logical(gpus),
    "immutable must be TRUE or FALSE" = is_scalar_logical(immutable)
  )
  if (compute@engine == "external") {
    return(command_spec(
      executable,
      args,
      cwd = compute@workspace
    ))
  }
  if (compute@backend == "local") {
    engine <- compute@config$container_engine %||% "docker"
    image <- if (grepl("@sha256:[0-9a-fA-F]{64}$", compute@image)) {
      compute@image
    } else if (!immutable) {
      compute@image
    } else {
      compute@image_digest
    }
    if (
      !is_scalar_string(image) ||
        (immutable &&
          !grepl("^sha256:[0-9a-fA-F]{64}$", image) &&
          !grepl("@sha256:[0-9a-fA-F]{64}$", image))
    ) {
      bionemor_abort(
        "BN_RUNTIME_MISSING",
        "container runtime probe requires a resolved image digest",
        operation = "runtime-probe",
        recipe_revision = compute@recipe@revision,
        hint = "Run bionemo_install() before probing the container runtime."
      )
    }
    return(command_spec(
      engine,
      c(
        "run",
        "--rm",
        if (gpus) c("--gpus", "all"),
        local_container_user_args(),
        "--entrypoint",
        executable,
        "-v",
        paste0(compute@workspace, ":", compute@workspace),
        "-w",
        compute@workspace,
        image,
        args
      ),
      cwd = compute@workspace
    ))
  }
  command_spec(
    "apptainer",
    c(
      "exec",
      if (gpus) "--nv",
      "--bind",
      paste0(compute@workspace, ":", compute@workspace),
      "--pwd",
      compute@workspace,
      compute@image,
      executable,
      args
    ),
    cwd = compute@workspace
  )
}

run_install_command <- function(
  executable,
  args = character(),
  env = process_environment(),
  error = "BioNeMo installation command failed",
  code = "BN_UPSTREAM",
  operation = "install",
  recipe_revision = NULL,
  hint = NULL
) {
  result <- processx::run(
    executable,
    args,
    error_on_status = FALSE,
    echo = FALSE,
    env = env
  )
  if (result$status != 0L) {
    detail <- redact_credentials(trimws(paste(result$stderr, result$stdout)))
    bionemor_abort(
      code,
      paste0(error, if (nzchar(detail)) paste0(": ", detail) else ""),
      operation = operation,
      recipe_revision = recipe_revision,
      hint = hint,
      command = executable,
      upstream_exit_status = as.integer(result$status)
    )
  }
  result
}

verify_recipe_source <- function(source, recipe, lock) {
  revision <- run_install_command(
    "git",
    c("-C", source, "rev-parse", "HEAD"),
    error = "failed to inspect the BioNeMo Recipes checkout",
    code = "BN_RECIPE_MISSING",
    recipe_revision = recipe@revision,
    hint = "Remove the cached recipe source and run bionemo_install() again."
  )
  actual_revision <- trimws(revision$stdout)
  if (!identical(actual_revision, recipe@revision)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "BioNeMo Recipes checkout is not at the locked revision",
      operation = "install",
      recipe_revision = recipe@revision,
      actual_recipe_revision = actual_revision,
      hint = "Remove the cached recipe source and run bionemo_install() again."
    )
  }
  dockerfile <- file.path(source, recipe@subdirectory, "Dockerfile")
  if (!file.exists(dockerfile)) {
    bionemor_abort(
      "BN_RECIPE_MISSING",
      "locked recipe Dockerfile is missing",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Remove the cached recipe source and run bionemo_install() again."
    )
  }
  blob <- run_install_command(
    "git",
    c("-C", source, "hash-object", dockerfile),
    error = "failed to hash the locked recipe Dockerfile",
    code = "BN_RECIPE_MISMATCH",
    recipe_revision = recipe@revision
  )
  actual_blob <- trimws(blob$stdout)
  if (!identical(actual_blob, lock$dockerfile_blob)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "recipe Dockerfile does not match the package lock",
      operation = "install",
      recipe_revision = recipe@revision,
      expected_dockerfile_blob = lock$dockerfile_blob,
      actual_dockerfile_blob = actual_blob,
      hint = "Remove the cached recipe source and run bionemo_install() again."
    )
  }
  invisible(source)
}

fetch_recipe_source <- function(paths, recipe, lock) {
  if (dir.exists(paths$source)) {
    verify_recipe_source(paths$source, recipe, lock)
    return(paths$source)
  }
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("source-", tmpdir = paths$root)
  dir.create(temporary)
  complete <- FALSE
  on.exit(
    if (!complete) unlink(temporary, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  run_install_command(
    "git",
    c("-C", temporary, "init"),
    error = "failed to initialize the recipe source cache",
    recipe_revision = recipe@revision
  )
  run_install_command(
    "git",
    c(
      "-C",
      temporary,
      "fetch",
      "--depth",
      "1",
      recipe@repository,
      recipe@revision
    ),
    error = "failed to fetch the locked BioNeMo Recipes revision",
    code = "BN_RECIPE_MISSING",
    recipe_revision = recipe@revision,
    hint = "Check repository access and the pinned recipe revision."
  )
  run_install_command(
    "git",
    c("-C", temporary, "checkout", "--detach", "FETCH_HEAD"),
    error = "failed to check out the locked BioNeMo Recipes revision",
    code = "BN_RECIPE_MISSING",
    recipe_revision = recipe@revision
  )
  verify_recipe_source(temporary, recipe, lock)
  if (!file.rename(temporary, paths$source)) {
    bionemor_abort(
      "BN_UPSTREAM",
      "failed to publish the recipe source cache",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Check that the workspace is writable."
    )
  }
  complete <- TRUE
  paths$source
}

prepare_recipe_build_context <- function(paths, recipe, lock) {
  spec <- recipe_install_spec(recipe)
  if (!dir.exists(paths$source)) {
    bionemor_abort(
      "BN_RECIPE_MISSING",
      "recipe source checkout is missing",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Run bionemo_install() to fetch the pinned recipe source."
    )
  }
  if (
    !path_is_within(paths$context, paths$root) ||
      identical(paths$context, paths$root)
  ) {
    stop("build context must be inside the installation root")
  }
  if (!recipe@verified || !is_scalar_string(recipe@base_image_digest)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "automatic builds require the verified package recipe and base-image digest",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Use the package-pinned recipe or provide a prebuilt image."
    )
  }
  if (!nzchar(Sys.which("tar"))) {
    bionemor_abort(
      "BN_RUNTIME_MISSING",
      "tar is required to construct the recipe build context",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Install tar and run bionemo_install() again."
    )
  }
  if (dir.exists(paths$context)) {
    unlink(paths$context, recursive = TRUE, force = TRUE)
  }
  dir.create(paths$context, recursive = TRUE, showWarnings = FALSE)
  archive <- tempfile(
    "recipe-context-",
    tmpdir = paths$root,
    fileext = ".tar"
  )
  on.exit(unlink(archive), add = TRUE)
  run_install_command(
    "git",
    c(
      "-C",
      paths$source,
      "archive",
      "--format=tar",
      paste0("--output=", archive),
      paste0("HEAD:", recipe@subdirectory)
    ),
    error = "failed to archive the locked recipe source",
    code = "BN_IMAGE_BUILD",
    recipe_revision = recipe@revision
  )
  run_install_command(
    "tar",
    c("-xf", archive, "-C", paths$context),
    error = "failed to extract the locked recipe source",
    code = "BN_IMAGE_BUILD",
    recipe_revision = recipe@revision
  )

  helper_dir <- file.path(paths$context, "bionemor-helper")
  dir.create(helper_dir)
  helper <- do.call(package_asset, as.list(spec$helper_asset))
  if (!file.copy(helper, file.path(helper_dir, spec$helper_filename))) {
    bionemor_abort(
      "BN_IMAGE_BUILD",
      "failed to copy the package helper into the build context",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Check that the workspace is writable."
    )
  }
  official <- file.path(paths$context, "Dockerfile")
  lines <- readLines(official, warn = FALSE)
  instructions <- trimws(lines)
  from <- which(startsWith(instructions, "FROM "))
  if (length(from) != 1L) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "locked recipe Dockerfile must contain exactly one FROM instruction",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Remove the cached recipe source and run bionemo_install() again."
    )
  }
  fields <- strsplit(instructions[[from]], "[[:space:]]+")[[1L]]
  if (length(fields) < 2L || !identical(fields[[2L]], recipe@base_image)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "locked recipe Dockerfile uses an unexpected base image",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Remove the cached recipe source and run bionemo_install() again."
    )
  }
  lines[[from]] <- sub(
    recipe@base_image,
    recipe_base_image_reference(recipe),
    lines[[from]],
    fixed = TRUE
  )
  uv_copy <- paste0(
    "COPY --from=",
    recipe_uv_image_reference(lock),
    " /uv /uvx /bin/"
  )
  if (!is.null(spec$uv_fallback)) {
    uv <- which(instructions == spec$uv_fallback)
    if (length(uv) != 1L) {
      bionemor_abort(
        "BN_RECIPE_MISMATCH",
        "locked recipe Dockerfile does not contain the expected uv fallback",
        operation = "install",
        recipe_revision = recipe@revision,
        hint = "Remove the cached recipe source and run bionemo_install() again."
      )
    }
    lines[[uv]] <- uv_copy
  }
  appendage <- do.call(package_asset, as.list(spec$docker_appendage))
  atomic_write_lines(
    c(
      lines,
      readLines(appendage, warn = FALSE)
    ),
    official
  )
  paths$context
}

container_image_id <- function(compute) {
  engine <- compute@config$container_engine %||% "docker"
  result <- run_install_command(
    engine,
    c("image", "inspect", "--format", "{{.Id}}", compute@image),
    error = "failed to inspect the recipe image",
    code = "BN_RUNTIME_MISSING",
    recipe_revision = compute@recipe@revision,
    hint = "Run bionemo_install() to build or select the recipe image."
  )
  id <- trimws(result$stdout)
  if (!grepl("^sha256:[0-9a-fA-F]{64}$", id)) {
    bionemor_abort(
      "BN_PROTOCOL",
      "container engine returned an invalid image ID",
      operation = "install",
      recipe_revision = compute@recipe@revision,
      hint = "Inspect the configured container engine and image."
    )
  }
  tolower(id)
}

container_image_exists <- function(compute) {
  engine <- compute@config$container_engine %||% "docker"
  result <- processx::run(
    engine,
    c("image", "inspect", compute@image),
    error_on_status = FALSE,
    echo = FALSE,
    env = process_environment()
  )
  result$status == 0L
}

package_helper_revision <- function(recipe) {
  spec <- recipe_install_spec(recipe)
  result <- run_install_command(
    "git",
    c(
      "hash-object",
      do.call(package_asset, as.list(spec$helper_asset))
    ),
    error = "failed to hash the package helper"
  )
  revision <- trimws(result$stdout)
  if (!grepl("^[0-9a-f]{40}$", revision)) {
    bionemor_abort(
      "BN_PROTOCOL",
      "git returned an invalid package helper revision",
      operation = "install",
      hint = "Verify the installed git executable."
    )
  }
  revision
}

container_image_labels <- function(compute) {
  engine <- compute@config$container_engine %||% "docker"
  image <- compute@image_digest %||% compute@image
  result <- run_install_command(
    engine,
    c(
      "image",
      "inspect",
      "--format",
      "{{json .Config.Labels}}",
      image
    ),
    error = "failed to inspect recipe image labels",
    code = "BN_RUNTIME_MISSING",
    recipe_revision = compute@recipe@revision,
    hint = "Run bionemo_install() to build or select the recipe image."
  )
  labels <- tryCatch(
    jsonlite::fromJSON(trimws(result$stdout), simplifyVector = TRUE),
    error = function(error) {
      bionemor_abort(
        "BN_PROTOCOL",
        "container engine returned invalid recipe image labels",
        operation = "install",
        recipe_revision = compute@recipe@revision,
        hint = "Inspect the configured container engine and image."
      )
    }
  )
  if (!is.list(labels) || length(labels) == 0L) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "recipe image does not contain OCI labels",
      operation = "install",
      recipe_revision = compute@recipe@revision,
      hint = "Use an image built from the package-pinned recipe."
    )
  }
  labels
}

verify_container_image_labels <- function(compute) {
  labels <- container_image_labels(compute)
  expected <- expected_container_image_labels(compute@recipe)
  actual <- unlist(labels[names(expected)], use.names = TRUE)
  if (!identical(actual, expected)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "recipe image labels do not match the package lock",
      operation = "install",
      recipe_revision = compute@recipe@revision,
      hint = "Use an image built from the package-pinned recipe."
    )
  }
  invisible(labels)
}

verify_base_image_digest <- function(engine, recipe) {
  if (!is_scalar_string(engine)) {
    stop("container engine must be one command name")
  }
  if (!S7_inherits(recipe, BioNeMoRecipe)) {
    stop("recipe must be a BioNeMo recipe")
  }
  if (recipe@verified && !is_scalar_string(recipe@base_image_digest)) {
    stop("verified recipes require a locked base-image digest")
  }
  if (is.null(recipe@base_image_digest)) {
    return(invisible(recipe))
  }
  inspected <- run_install_command(
    engine,
    c(
      "image",
      "inspect",
      "--format",
      "{{json .RepoDigests}}",
      recipe_base_image_reference(recipe)
    ),
    error = "failed to inspect the official NGC PyTorch base image",
    code = "BN_IMAGE_BUILD",
    recipe_revision = recipe@revision
  )
  references <- tryCatch(
    jsonlite::fromJSON(trimws(inspected$stdout)),
    error = function(error) {
      bionemor_abort(
        "BN_PROTOCOL",
        "container engine returned invalid base-image digests",
        operation = "install",
        recipe_revision = recipe@revision,
        hint = "Inspect the configured container engine and base image."
      )
    }
  )
  digests <- sub("^.*@", "", references)
  if (!recipe@base_image_digest %in% digests) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "official NGC PyTorch base image does not match the package lock",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Pull the digest-qualified base image and retry."
    )
  }
  invisible(recipe)
}

#' Install or verify a BioNeMo recipe runtime
#'
#' `bionemo_install()` makes the runtime described by `compute` ready for
#' bionemor operations. For the verified local/container configuration, it can
#' build the package-pinned recipe image. For an explicit local image, an
#' external environment, or either Slurm engine, it verifies the existing
#' runtime instead. Installation is independent of model weights: prepare,
#' select, or attach model weights separately with the family-specific model
#' functions.
#'
#' @section Setup lifecycle:
#'
#' A typical setup has two explicit stages:
#'
#' 1. Create a descriptor with [bionemo_compute()]. This chooses the backend,
#'    engine, workspace, recipe, image, and requested resources without probing
#'    the runtime.
#' 2. Call `bionemo_install()` and retain its returned compute descriptor. The
#'    return value contains the resolved image digest when applicable and a
#'    validated capability report in `compute@config$capabilities`.
#'
#' [bionemo_doctor()] can then check the environment for a particular operation
#' group and, optionally, a particular model checkpoint. Re-run installation
#' after changing the recipe runtime or image. Use
#' [bionemo_capabilities()] with `refresh = TRUE` when only a fresh runtime
#' report is needed.
#'
#' @section What installation does:
#'
#' For the verified local/container path, installation checks for the
#' deterministic derived image. When it must build, it fetches the exact locked
#' BioNeMo Recipes revision, verifies the upstream Dockerfile blob, prepares a
#' synthetic build context containing the package helper, optionally pulls the
#' digest-qualified base image, verifies that digest, and builds the image.
#' Existing explicit images are not rebuilt; their immutable ID and required
#' provenance labels are verified.
#'
#' Every path performs a GPU-backed helper capability probe and verifies the
#' commands advertised by the selected recipe. The helper's protocol, recipe
#' version, and recipe revision must match the compute descriptor. Local
#' containers are probed by the configured Docker-compatible engine. Local
#' external runtimes are probed directly.
#'
#' Slurm installation does not install packages or build an image. It submits
#' synchronous probe jobs so the checks run in allocations rather than on the
#' login host. External Slurm environments must already expose the helper and
#' recipe commands. Slurm/container requires Apptainer and an existing image;
#' a local SIF is hashed before probing and its digest is checked again inside
#' each allocation.
#'
#' @param compute A BioNeMo compute descriptor from [bionemo_compute()]. Assign
#'   the return value of `bionemo_install()` before using it for model
#'   operations.
#' @param rebuild Whether to rebuild an existing deterministic local image.
#'   This is only supported for the package-managed verified recipe image; an
#'   explicit prebuilt image is always verified in place.
#' @param pull Whether to pull the locked digest-qualified base image before a
#'   local build. The base-image digest is still verified when `FALSE`.
#' @param keep_source Whether to keep the synthetic local build context after a
#'   successful build. The content-addressed recipe source checkout remains in
#'   the workspace recipe cache.
#'
#' @return The supplied `BioNeMoCompute` descriptor with resolved runtime
#'   metadata. For containers this includes `image_digest`; all paths include a
#'   validated capability report in `config$capabilities`.
#'
#' @examples
#' \dontrun{
#' # Build and verify the package-pinned local container runtime.
#' compute <- bionemo_compute(recipe = evo2_recipe(),
#'   backend = "local",
#'   engine = "container",
#'   workspace = "~/evo2-work"
#' )
#' compute <- bionemo_install(compute)
#'
#' # The returned descriptor carries immutable image and capability metadata.
#' compute@image_digest
#' compute@config$capabilities$runtime$gpus
#' bionemo_doctor(compute, target = "inference", verbose = FALSE)
#' }
#'
#' @seealso [bionemo_capabilities()], [bionemo_doctor()], [evo2_model()],
#'   [esm2_model()]
#' @export
bionemo_install <- function(
  compute,
  rebuild = FALSE,
  pull = TRUE,
  keep_source = FALSE
) {
  stopifnot(
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "rebuild must be TRUE or FALSE" = is_scalar_logical(rebuild),
    "pull must be TRUE or FALSE" = is_scalar_logical(pull),
    "keep_source must be TRUE or FALSE" = is_scalar_logical(keep_source)
  )
  if (compute@engine == "external" || compute@backend == "slurm") {
    if (compute@backend == "slurm" && compute@engine == "container") {
      compute@image_digest <- slurm_image_digest(compute)
    }
    capabilities <- runtime_capabilities(compute, refresh = TRUE)
    verify_runtime_commands(compute, capabilities, target = "all")
    compute@config$capabilities <- capabilities
    return(compute)
  }

  engine <- compute@config$container_engine %||% "docker"
  if (!nzchar(Sys.which(engine))) {
    bionemor_abort(
      "BN_RUNTIME_MISSING",
      paste("container engine is not available:", engine),
      operation = "install",
      recipe_revision = compute@recipe@revision,
      hint = "Install the configured container engine or use an external runtime."
    )
  }
  paths <- install_paths(compute)
  build <- recipe_image_requires_build(compute)

  if (build && (rebuild || !container_image_exists(compute))) {
    lock <- recipe_install_lock(compute@recipe)
    fetch_recipe_source(paths, compute@recipe, lock)
    prepare_recipe_build_context(paths, compute@recipe, lock)
    if (pull) {
      run_install_command(
        engine,
        c("pull", recipe_base_image_reference(compute@recipe)),
        error = "failed to pull the official NGC PyTorch base image",
        code = "BN_IMAGE_BUILD",
        recipe_revision = compute@recipe@revision,
        hint = "Check registry access and the configured container engine."
      )
    }
    verify_base_image_digest(engine, compute@recipe)
    helper_revision <- package_helper_revision(compute@recipe)
    run_install_command(
      engine,
      c(
        "build",
        "--file",
        file.path(paths$context, "Dockerfile"),
        "--tag",
        compute@image,
        "--build-arg",
        paste0("BIONEMOR_RECIPE_REVISION=", compute@recipe@revision),
        "--build-arg",
        paste0("BIONEMOR_HELPER_REVISION=", helper_revision),
        "--build-arg",
        paste0("BIONEMOR_BASE_IMAGE=", compute@recipe@base_image),
        "--build-arg",
        paste0(
          "BIONEMOR_BASE_IMAGE_DIGEST=",
          compute@recipe@base_image_digest %||% ""
        ),
        paths$context
      ),
      error = "failed to build the pinned BioNeMo recipe image",
      code = "BN_IMAGE_BUILD",
      recipe_revision = compute@recipe@revision,
      hint = "Inspect the container build output and pinned recipe inputs."
    )
    if (!keep_source) {
      unlink(paths$context, recursive = TRUE, force = TRUE)
    }
  } else if (!build && rebuild) {
    bionemor_abort(
      "BN_IMAGE_BUILD",
      "rebuild is only supported for the deterministic package recipe image",
      operation = "install",
      recipe_revision = compute@recipe@revision,
      hint = "Set rebuild = FALSE for an explicit prebuilt image."
    )
  }

  compute@image_digest <- container_image_id(compute)
  verify_container_image_labels(compute)
  capabilities <- runtime_capabilities(compute, refresh = TRUE)
  verify_runtime_commands(compute, capabilities, target = "all")
  compute@config$capabilities <- capabilities
  compute
}

slurm_probe_record <- function(id) {
  result <- processx::run(
    "sacct",
    c("-X", "-n", "-P", "-j", id, "--format=JobIDRaw,State,ExitCode"),
    error_on_status = FALSE,
    echo = FALSE,
    env = process_environment()
  )
  if (result$status != 0L) {
    detail <- redact_credentials(trimws(result$stderr))
    bionemor_abort(
      "BN_UPSTREAM",
      paste0(
        "failed to query Slurm runtime probe",
        if (nzchar(detail)) paste0(": ", detail) else ""
      ),
      operation = "runtime-probe",
      request_id = id,
      command = "sacct",
      upstream_exit_status = as.integer(result$status),
      hint = "Inspect the Slurm accounting service."
    )
  }
  lines <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1L]]
  fields <- lapply(lines[nzchar(lines)], strsplit, split = "|", fixed = TRUE)
  fields <- lapply(fields, `[[`, 1L)
  matching <- vapply(
    fields,
    function(x) length(x) == 3L && identical(x[[1L]], id),
    logical(1)
  )
  if (sum(matching) == 0L) {
    return(NULL)
  }
  if (sum(matching) != 1L) {
    bionemor_abort(
      "BN_PROTOCOL",
      "sacct returned more than one allocation record for the runtime probe",
      operation = "runtime-probe",
      request_id = id,
      hint = "Inspect the Slurm accounting output for this allocation."
    )
  }
  fields[[which(matching)]]
}

slurm_runtime_probe <- function(
  compute,
  executable,
  args = character(),
  gpus = FALSE
) {
  required <- c("sbatch", "sacct", "scancel")
  missing <- required[!vapply(required, command_available, logical(1))]
  if (length(missing) > 0L) {
    bionemor_abort(
      "BN_RUNTIME_MISSING",
      paste(
        "required Slurm command is not available:",
        paste(missing, collapse = ", ")
      ),
      operation = "runtime-probe",
      recipe_revision = compute@recipe@revision,
      hint = "Load the Slurm client commands before probing the runtime."
    )
  }
  command <- runtime_probe_command(
    compute,
    executable,
    args,
    gpus = gpus
  )
  root <- file.path(compute@workspace, ".bionemor", "runtime-probes")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  path <- tempfile(
    paste0("probe-", executable, "-"),
    tmpdir = root
  )
  dir.create(path)
  stdout <- file.path(path, "stdout.log")
  stderr <- file.path(path, "stderr.log")
  script <- file.path(path, "slurm.sh")
  command$stdout <- stdout
  command$stderr <- stderr
  writeLines(
    c(
      "#!/usr/bin/env bash",
      job_directives(
        compute,
        paste0("bionemor-probe-", executable),
        stdout,
        stderr
      ),
      "set -euo pipefail",
      paste("exec >", shQuote(stdout)),
      paste("exec 2>", shQuote(stderr)),
      if (compute@engine == "container") {
        slurm_sif_verification_lines(compute)
      },
      render_shell_command(command)
    ),
    script,
    useBytes = TRUE
  )
  Sys.chmod(script, "0750")
  submitted <- processx::run(
    "sbatch",
    c("--parsable", script),
    error_on_status = FALSE,
    echo = FALSE,
    env = process_environment()
  )
  if (submitted$status != 0L) {
    detail <- redact_credentials(trimws(submitted$stderr))
    bionemor_abort(
      "BN_UPSTREAM",
      paste0(
        "failed to submit Slurm runtime probe",
        if (nzchar(detail)) paste0(": ", detail) else ""
      ),
      operation = "runtime-probe",
      recipe_revision = compute@recipe@revision,
      log_paths = c(stdout = stdout, stderr = stderr),
      command = "sbatch",
      upstream_exit_status = as.integer(submitted$status),
      hint = "Inspect the scheduler response and Slurm submission settings."
    )
  }
  id <- sub(";.*$", "", trimws(submitted$stdout))
  if (!grepl("^[0-9]+$", id)) {
    bionemor_abort(
      "BN_PROTOCOL",
      "sbatch returned an invalid runtime probe job ID",
      operation = "runtime-probe",
      recipe_revision = compute@recipe@revision,
      log_paths = c(stdout = stdout, stderr = stderr),
      hint = "Inspect the configured sbatch wrapper and scheduler output."
    )
  }

  deadline <- Sys.time() + 600
  repeat {
    record <- slurm_probe_record(id)
    if (!is.null(record)) {
      state <- sub("[+ ].*$", "", toupper(trimws(record[[2L]])))
      if (
        state %in%
          c(
            "COMPLETED",
            "CANCELLED",
            "FAILED",
            "TIMEOUT",
            "OUT_OF_MEMORY",
            "NODE_FAIL"
          )
      ) {
        break
      }
    }
    if (Sys.time() >= deadline) {
      processx::run(
        "scancel",
        id,
        error_on_status = FALSE,
        echo = FALSE,
        env = process_environment()
      )
      bionemor_abort(
        "BN_TIMEOUT",
        "timed out waiting for the Slurm runtime probe",
        operation = "runtime-probe",
        recipe_revision = compute@recipe@revision,
        request_id = id,
        log_paths = c(stdout = stdout, stderr = stderr),
        hint = "Inspect the probe logs and Slurm allocation state."
      )
    }
    Sys.sleep(1)
  }

  output <- function(file) {
    if (!file.exists(file)) {
      return("")
    }
    paste(readLines(file, warn = FALSE), collapse = "\n")
  }
  exit_code <- trimws(record[[3L]])
  status <- suppressWarnings(as.integer(sub(":.*$", "", exit_code)))
  if (!identical(state, "COMPLETED") || is.na(status)) {
    status <- 1L
  }
  list(
    status = status,
    stdout = output(stdout),
    stderr = output(stderr),
    request_id = id,
    log_paths = c(stdout = stdout, stderr = stderr)
  )
}

runtime_probe <- function(
  compute,
  executable,
  args = character(),
  gpus = FALSE
) {
  if (compute@backend == "slurm") {
    return(slurm_runtime_probe(
      compute,
      executable,
      args,
      gpus = gpus
    ))
  }
  command <- runtime_probe_command(
    compute,
    executable,
    args,
    gpus = gpus
  )
  processx::run(
    command$executable,
    command$args,
    error_on_status = FALSE,
    echo = FALSE,
    env = process_environment(),
    wd = command$cwd
  )
}

runtime_capabilities <- function(compute, refresh = FALSE) {
  if (!S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be a BioNeMo compute descriptor")
  }
  if (!is_scalar_logical(refresh)) {
    stop("refresh must be TRUE or FALSE")
  }
  cached <- compute@config$capabilities
  if (!refresh && is.list(cached)) {
    return(cached)
  }
  if (compute@backend == "slurm" && compute@engine == "container") {
    compute@image_digest <- slurm_image_digest(compute)
  }
  spec <- recipe_install_spec(compute@recipe)
  result <- runtime_probe(
    compute,
    spec$helper,
    c("describe", "--json"),
    gpus = TRUE
  )
  output <- redact_credentials(trimws(paste(result$stdout, result$stderr)))
  if (result$status != 0L) {
    bionemor_abort(
      "BN_RUNTIME_MISSING",
      paste0(
        "BioNeMo helper capability probe failed",
        if (nzchar(output)) paste0(": ", output) else ""
      ),
      operation = "runtime-capabilities",
      recipe_revision = compute@recipe@revision,
      request_id = result$request_id %||% NULL,
      log_paths = result$log_paths %||% NULL,
      command = spec$helper,
      upstream_exit_status = as.integer(result$status),
      hint = "Install or activate the package-pinned recipe runtime."
    )
  }
  report <- tryCatch(
    jsonlite::fromJSON(result$stdout, simplifyVector = TRUE),
    error = function(error) {
      bionemor_abort(
        "BN_PROTOCOL",
        "BioNeMo helper returned an invalid capability report",
        operation = "runtime-capabilities",
        recipe_revision = compute@recipe@revision,
        request_id = result$request_id %||% NULL,
        log_paths = result$log_paths %||% NULL,
        hint = "Verify that bionemor and the recipe helper are from the same release."
      )
    }
  )
  if (!is.list(report)) {
    bionemor_abort(
      "BN_PROTOCOL",
      "BioNeMo helper returned an invalid capability report",
      operation = "runtime-capabilities",
      recipe_revision = compute@recipe@revision,
      request_id = result$request_id %||% NULL,
      log_paths = result$log_paths %||% NULL,
      hint = "Verify that bionemor and the recipe helper are from the same release."
    )
  }
  if (
    !identical(report$driver, compute@recipe@adapter) ||
      !identical(as.integer(report$execution_schema_version), 1L) ||
      !is.character(report$semantic_operations) ||
      !all(spec$semantic_operations %in% report$semantic_operations)
  ) {
    bionemor_abort(
      "BN_PROTOCOL",
      "BioNeMo helper returned an incompatible driver description",
      operation = "runtime-capabilities",
      recipe_revision = compute@recipe@revision,
      expected_driver = compute@recipe@adapter,
      actual_driver = report$driver %||% NULL,
      hint = "Install the recipe helper version pinned by this bionemor release."
    )
  }
  protocol <- suppressWarnings(as.integer(report$protocol_version))
  expected_protocol <- as.integer(compute@recipe@bridge_protocol)
  if (!identical(protocol, expected_protocol)) {
    bionemor_abort(
      "BN_PROTOCOL",
      "helper did not report a supported protocol version",
      operation = "runtime-capabilities",
      recipe_revision = compute@recipe@revision,
      protocol_version = report$protocol_version %||% NULL,
      expected_protocol_version = expected_protocol,
      hint = "Install the recipe helper version pinned by this bionemor release."
    )
  }
  if (!identical(report$recipe_version, compute@recipe@recipe_version)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "helper recipe version does not match the compute recipe",
      operation = "runtime-capabilities",
      recipe_revision = compute@recipe@revision,
      expected_recipe_version = compute@recipe@recipe_version,
      actual_recipe_version = report$recipe_version %||% NULL,
      hint = "Install the package-pinned recipe runtime."
    )
  }
  if (!identical(report$recipe_revision, compute@recipe@revision)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "helper recipe revision does not match the compute recipe",
      operation = "runtime-capabilities",
      recipe_revision = compute@recipe@revision,
      actual_recipe_revision = report$recipe_revision %||% NULL,
      hint = "Install the package-pinned recipe runtime."
    )
  }
  report$image <- compute@image
  report$image_digest <- compute@image_digest
  report$probed_at <- base::format(Sys.time(), tz = "UTC", usetz = TRUE)
  report
}

runtime_command_key <- function(recipe, command) {
  mapping <- recipe_install_spec(recipe)$command_keys
  key <- unname(mapping[command])
  if (length(key) == 0L || is.na(key)) command else key
}

verify_runtime_commands <- function(compute, report, target = "all") {
  commands <- install_probe_commands(compute@recipe, target)
  advertised <- report$commands %||% list()
  imports <- report$runtime$imports %||% list()
  missing_imports <- names(imports)[
    !vapply(imports, isTRUE, logical(1))
  ]
  if (length(missing_imports)) {
    bionemor_abort(
      "BN_RUNTIME_MISSING",
      paste(
        "recipe runtime failed required import:",
        paste(missing_imports, collapse = ", ")
      ),
      operation = "install",
      recipe_revision = compute@recipe@revision,
      imports = missing_imports,
      hint = "Install the package-pinned recipe runtime."
    )
  }
  keys <- vapply(
    commands,
    function(command) runtime_command_key(compute@recipe, command),
    character(1)
  )
  if (!is.list(advertised)) {
    bionemor_abort(
      "BN_PROTOCOL",
      "recipe runtime returned an invalid command capability report",
      operation = "install",
      recipe_revision = compute@recipe@revision,
      hint = "Install the recipe helper version pinned by this bionemor release."
    )
  }
  available <- vapply(
    keys,
    function(key) isTRUE(advertised[[key]]),
    logical(1)
  )
  if (!all(available)) {
    bionemor_abort(
      "BN_RUNTIME_MISSING",
      paste(
        "recipe runtime does not advertise required command:",
        paste(commands[!available], collapse = ", ")
      ),
      operation = "install",
      recipe_revision = compute@recipe@revision,
      commands = commands[!available],
      hint = "Install the package-pinned recipe runtime."
    )
  }
  invisible(report)
}

doctor_row <- function(check, status, detail, output = "") {
  data.frame(
    check = check,
    status = status,
    detail = detail,
    output = output,
    stringsAsFactors = FALSE
  )
}

doctor_backend <- function(compute) {
  helper <- recipe_install_spec(compute@recipe)$helper
  commands <- if (compute@backend == "slurm") {
    c(
      "sbatch",
      "sacct",
      "scancel",
      if (compute@engine == "container") "apptainer"
    )
  } else if (compute@engine == "container") {
    compute@config$container_engine %||% "docker"
  } else {
    helper
  }
  available <- nzchar(Sys.which(commands))
  detail <- if (all(available)) {
    paste(paste(commands, collapse = ", "), "are available")
  } else {
    paste(
      paste(commands[!available], "is not available"),
      collapse = "; "
    )
  }
  doctor_row(
    "backend",
    if (all(available)) "pass" else "fail",
    detail
  )
}

doctor_host_tools <- function(compute) {
  tools <- c("bash", "awk", "mkfifo")
  script <- paste(
    c(
      "status=0",
      "for tool in bash awk mkfifo; do",
      "  if ! command -v \"$tool\" >/dev/null; then",
      "    printf '%s\\n' \"$tool\"",
      "    status=1",
      "  fi",
      "done",
      "exit \"$status\""
    ),
    collapse = "\n"
  )
  report <- tryCatch(
    {
      if (compute@backend == "slurm") {
        host_compute <- bionemo_compute(
          recipe = compute@recipe,
          backend = "slurm",
          engine = "external",
          workspace = compute@workspace,
          gpus = compute@gpus,
          queue = compute@queue,
          account = compute@account,
          walltime = compute@walltime,
          config = compute@config
        )
        runtime_probe(host_compute, "bash", c("-c", script))
      } else {
        command_probe("bash", c("-c", script))
      }
    },
    error = identity
  )
  if (inherits(report, "error")) {
    return(doctor_row(
      "host tools",
      "fail",
      conditionMessage(report)
    ))
  }
  output <- trimws(paste(report$stdout, report$stderr))
  doctor_row(
    "host tools",
    if (report$status == 0L) "pass" else "fail",
    if (report$status == 0L) {
      paste(paste(tools, collapse = ", "), "are available")
    } else if (nzchar(output)) {
      paste(
        paste(strsplit(output, "\n", fixed = TRUE)[[1L]], collapse = ", "),
        "is not available"
      )
    } else {
      paste(paste(tools, collapse = ", "), "could not be verified")
    }
  )
}

doctor_workspace <- function(compute) {
  writable <- dir.exists(compute@workspace) &&
    file.access(compute@workspace, 2L) == 0L
  doctor_row(
    "workspace",
    if (writable) "pass" else "fail",
    if (writable) compute@workspace else "workspace is not writable"
  )
}

doctor_runtime_rows <- function(report, compute) {
  runtime <- report$runtime %||% list()
  version_row <- function(check, value) {
    available <- is_scalar_string(value) &&
      !identical(tolower(value), "unknown")
    doctor_row(
      check,
      if (available) "pass" else "fail",
      if (available) value else "not reported"
    )
  }
  imports <- runtime$imports %||% list()
  gpu_count <- suppressWarnings(as.integer(runtime$gpu_count %||% 0L))
  gpu_ok <- isTRUE(runtime$cuda_available) &&
    !is.na(gpu_count) &&
    gpu_count >= compute@gpus
  gpu_detail <- paste(gpu_count, "GPU(s)")
  gpus <- runtime$gpus
  if (is.data.frame(gpus) && nrow(gpus) > 0L) {
    details <- vapply(
      seq_len(nrow(gpus)),
      function(index) {
        major <- gpus$compute_capability_major[[index]]
        minor <- gpus$compute_capability_minor[[index]]
        memory <- as.double(gpus$total_memory_bytes[[index]]) / 1024^3
        paste0(
          gpus$name[[index]],
          " compute ",
          major,
          ".",
          minor,
          ", ",
          format(round(memory, 1), nsmall = 1),
          " GiB"
        )
      },
      character(1)
    )
    gpu_detail <- paste(
      paste(details, collapse = "; "),
      "driver",
      runtime$driver %||% "unknown"
    )
  }
  rows <- list(
    version_row("runtime Python", runtime$python),
    version_row("runtime PyTorch", runtime$pytorch),
    version_row("runtime CUDA", runtime$cuda)
  )
  optional_versions <- c(
    "runtime Transformer Engine" = "transformer_engine",
    "runtime Transformers" = "transformers",
    "runtime Megatron Bridge" = "megatron_bridge",
    "runtime vLLM" = "vllm"
  )
  for (check in names(optional_versions)) {
    value <- runtime[[optional_versions[[check]]]]
    if (!is.null(value)) {
      rows[[length(rows) + 1L]] <- version_row(check, value)
    }
  }
  supported_architectures <- runtime$supported_compute_capabilities
  if (!is.null(supported_architectures) && length(supported_architectures)) {
    actual_architectures <- if (
      is.data.frame(gpus) &&
        all(
          c(
            "compute_capability_major",
            "compute_capability_minor"
          ) %in%
            names(gpus)
        )
    ) {
      paste(
        gpus$compute_capability_major,
        gpus$compute_capability_minor,
        sep = "."
      )
    } else {
      character()
    }
    architecture_ok <- is.character(supported_architectures) &&
      length(actual_architectures) >= compute@gpus &&
      all(
        utils::head(actual_architectures, compute@gpus) %in%
          supported_architectures
      )
    rows[[length(rows) + 1L]] <- doctor_row(
      "runtime GPU architecture",
      if (architecture_ok) "pass" else "fail",
      paste0(
        if (length(actual_architectures)) {
          paste(actual_architectures, collapse = ", ")
        } else {
          "not reported"
        },
        "; supported: ",
        paste(supported_architectures, collapse = ", ")
      )
    )
  }
  if (!is.null(imports$bionemo)) {
    rows[[length(rows) + 1L]] <- doctor_row(
      "runtime BioNeMo",
      if (isTRUE(imports$bionemo)) "pass" else "fail",
      if (isTRUE(imports$bionemo)) "import available" else "import unavailable"
    )
  }
  rows <- c(
    rows,
    list(
      doctor_row(
        "GPU",
        if (gpu_ok) "pass" else "fail",
        gpu_detail
      ),
      doctor_row(
        "image",
        if (
          compute@engine == "external" ||
            is_scalar_string(report$image_digest)
        ) {
          "pass"
        } else {
          "fail"
        },
        if (compute@engine == "external") {
          "externally managed runtime"
        } else {
          paste(
            report$image %||% "unknown",
            report$image_digest %||% "no digest"
          )
        }
      ),
      doctor_row(
        "base image",
        if (is_scalar_string(compute@recipe@base_image)) "pass" else "fail",
        paste(
          compute@recipe@base_image %||% "unknown",
          compute@recipe@base_image_digest %||% "digest unavailable"
        )
      )
    )
  )
  if (compute@engine == "container") {
    rows[[length(rows) + 1L]] <- doctor_row(
      "container GPU passthrough",
      if (gpu_ok) "pass" else "fail",
      if (gpu_ok) "CUDA devices are visible" else "CUDA devices are unavailable"
    )
  }
  do.call(rbind, rows)
}

doctor_capabilities <- function(compute, target, model = NULL) {
  report <- tryCatch(
    runtime_capabilities(compute, refresh = TRUE),
    error = identity
  )
  if (inherits(report, "error")) {
    return(doctor_row(
      "helper capabilities",
      "fail",
      conditionMessage(report)
    ))
  }
  rows <- list(
    doctor_row(
      "helper protocol",
      "pass",
      paste("protocol", report$protocol_version)
    ),
    doctor_row(
      "recipe",
      if (identical(report$recipe_version, compute@recipe@recipe_version)) {
        "pass"
      } else {
        "fail"
      },
      paste(
        "recipe",
        report$recipe_version %||% "unknown",
        "at",
        substr(compute@recipe@revision, 1L, 8L)
      )
    ),
    doctor_runtime_rows(report, compute)
  )
  advertised <- report$commands %||% list()
  for (command in install_probe_commands(compute@recipe, target)) {
    key <- runtime_command_key(compute@recipe, command)
    available <- advertised[[key]]
    if (is.null(available) && compute@engine == "external") {
      available <- nzchar(Sys.which(command))
    }
    rows[[length(rows) + 1L]] <- doctor_row(
      command,
      if (isTRUE(available)) "pass" else "fail",
      if (isTRUE(available)) "available" else "not advertised"
    )
  }
  if (!is.null(model)) {
    rows[[length(rows) + 1L]] <- adapter_function(
      compute@recipe@adapter,
      "doctor_model"
    )(
      compute,
      model,
      report
    )
  }
  do.call(rbind, rows)
}

#' Diagnose a BioNeMo Recipes execution environment
#'
#' `bionemo_doctor()` checks whether a compute descriptor is ready for one or
#' more groups of operations supplied by its recipe adapter. It performs live
#' runtime probes; it does not rely on the cached capability report stored by
#' [bionemo_install()]. A model is optional because the runtime can be
#' diagnosed before weights are prepared. Supply one to add the adapter's model
#' compatibility and checkpoint checks. The doctor does not install software,
#' build containers, or prepare checkpoints.
#'
#' @section Targets:
#'
#' `target` selects a command group declared by the selected recipe adapter.
#' `"inference"`, `"training"`, and `"conversion"` check the commands needed
#' for that operation group; `"all"` checks every command implemented by the
#' adapter. An adapter reports an error when a requested group is unsupported.
#'
#' Every target also checks the backend commands, required host tools, writable
#' workspace, helper protocol, recipe identity, runtime software versions, GPU
#' visibility and count, and image metadata and digest availability. When
#' `model` is supplied, the doctor also checks registry compatibility,
#' model source or checkpoint readiness. These checks do not run a workflow on
#' user data.
#'
#' With a Slurm backend, host and runtime checks submit short synchronous jobs
#' so that tools, GPUs, and the runtime are inspected on compute nodes. This can
#' create probe files under `workspace/.bionemor` and consume scheduler time.
#'
#' @section Results:
#'
#' Printing the returned `BioNeMoDoctor` shows its target, overall status, and a
#' table of `check`, `status`, and `detail`. The overall `ok` property is `TRUE`
#' only when no row has `status = "fail"`. Set `verbose = TRUE` to print any
#' complete credential-redacted probe output recorded for failed checks.
#'
#' `as.data.frame()` returns all check data with four columns:
#'
#' - `check`: the component or command inspected.
#' - `status`: `"pass"` or `"fail"`.
#' - `detail`: a concise explanation of the result.
#' - `output`: credential-redacted probe output when one was retained, otherwise
#'   `""`.
#'
#' @param compute A BioNeMo compute descriptor whose runtime has been built or
#'   selected, usually the value returned by [bionemo_install()].
#' @param model Optional BioNeMo model. Supplying a model adds the checks
#'   implemented by the compute recipe's adapter.
#' @param target Operation group to check: `"all"`, `"inference"`,
#'   `"training"`, or `"conversion"`.
#' @param verbose Whether printing the result should include complete retained
#'   probe output in addition to the check table. This does not change the
#'   checks performed or the columns returned by `as.data.frame()`.
#'
#' @return A `BioNeMoDoctor` with `target`, logical `ok`, a `checks` data frame,
#'   and the selected `verbose` print setting. Use `as.data.frame()` to inspect
#'   the `check`, `status`, `detail`, and `output` columns programmatically.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(),
#'   engine = "external",
#'   workspace = "/shared/projects/evo2"
#' )
#' compute <- bionemo_install(compute)
#'
#' doctor <- bionemo_doctor(
#'   compute,
#'   target = "inference",
#'   verbose = FALSE
#' )
#' doctor
#'
#' checks <- as.data.frame(doctor)
#' checks[checks$status == "fail", c("check", "detail")]
#'
#' # Add checkpoint and model compatibility checks.
#' model <- evo2("7b", checkpoint = "/shared/models/evo2-7b-mbridge")
#' bionemo_doctor(compute, model, target = "inference")
#' }
#'
#' @seealso [bionemo_install()], [bionemo_capabilities()], [bionemo_workflows()]
#' @export
bionemo_doctor <- function(
  compute,
  model = NULL,
  target = c("all", "inference", "training", "conversion"),
  verbose = TRUE
) {
  target <- match.arg(target)
  if (!S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be a BioNeMo compute descriptor")
  }
  if (!is.null(model) && !S7_inherits(model, BioNeMoModel)) {
    stop("model must be NULL or a BioNeMo model")
  }
  if (!is_scalar_logical(verbose)) {
    stop("verbose must be TRUE or FALSE")
  }
  checks <- rbind(
    doctor_backend(compute),
    doctor_host_tools(compute),
    doctor_workspace(compute),
    doctor_capabilities(compute, target, model)
  )
  rownames(checks) <- NULL
  BioNeMoDoctor(
    target = target,
    ok = !any(checks$status == "fail"),
    checks = checks,
    verbose = verbose
  )
}

method(print, BioNeMoDoctor) <- function(x, ...) {
  cat("<BioNeMo doctor>\n", sep = "")
  cat("Target: ", x@target, "\n", sep = "")
  cat("Status: ", if (x@ok) "pass" else "fail", "\n", sep = "")
  print(x@checks[c("check", "status", "detail")], row.names = FALSE)
  if (x@verbose) {
    output <- x@checks$output[nzchar(x@checks$output)]
    if (length(output) > 0L) {
      cat(paste(output, collapse = "\n"), "\n", sep = "")
    }
  }
  invisible(x)
}

method(as.data.frame, BioNeMoDoctor) <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  base::as.data.frame(
    x@checks,
    row.names = row.names,
    optional = optional,
    ...
  )
}
