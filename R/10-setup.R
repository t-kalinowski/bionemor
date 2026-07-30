recipe_install_lock <- function(recipe) {
  lock <- evo2_recipe_lock()
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
    (identical(compute@image, default_recipe_image(compute@recipe)) ||
      identical(compute@image, compute@recipe@base_image))
}

install_step <- function(id, purpose, command, expected = list()) {
  list(
    id = id,
    purpose = purpose,
    command = command,
    expected = expected
  )
}

expected_container_image_labels <- function(
  recipe,
  helper_revision = package_helper_revision()
) {
  c(
    "org.opencontainers.image.source" = recipe@repository,
    "org.opencontainers.image.revision" = recipe@revision,
    "org.opencontainers.image.version" = paste0(
      "evo2-recipe-",
      recipe@recipe_version
    ),
    "io.bionemor.helper.revision" = helper_revision,
    "io.bionemor.base.image" = recipe@base_image,
    "io.bionemor.base.digest" = recipe@base_image_digest %||% "",
    "io.bionemor.bridge.protocol" = as.character(recipe@bridge_protocol)
  )
}

install_probe_commands <- function(target = "all") {
  switch(
    target,
    inference = c("infer_evo2", "predict_evo2"),
    training = c("preprocess_evo2", "train_evo2"),
    conversion = c(
      "evo2_convert_savanna_to_mbridge",
      "evo2_convert_nemo2_to_mbridge",
      "evo2_export_mbridge_to_vortex",
      "evo2_remove_optimizer"
    ),
    all = c(
      "infer_evo2",
      "predict_evo2",
      "preprocess_evo2",
      "train_evo2",
      "evo2_convert_savanna_to_mbridge",
      "evo2_convert_nemo2_to_mbridge",
      "evo2_export_mbridge_to_vortex",
      "evo2_remove_optimizer"
    ),
    stop("unsupported installation probe target", call. = FALSE)
  )
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
        "-v",
        paste0(compute@workspace, ":", compute@workspace),
        "-w",
        compute@workspace,
        image,
        executable,
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

runtime_install_steps <- function(compute, target = "all") {
  capabilities <- install_step(
    "runtime-capabilities",
    "verify the helper protocol and current recipe commands",
    runtime_probe_command(
      compute,
      "bionemor-evo2-helper",
      c("capabilities", "--json"),
      gpus = TRUE,
      immutable = FALSE
    ),
    list(protocol_version = compute@recipe@bridge_protocol)
  )
  probes <- lapply(install_probe_commands(target), function(executable) {
    install_step(
      paste0("probe-", executable),
      paste("verify", executable),
      runtime_probe_command(
        compute,
        executable,
        "--help",
        gpus = compute@engine == "container",
        immutable = FALSE
      )
    )
  })
  c(list(capabilities), probes)
}

#' Plan installation of the pinned BioNeMo Evo 2 recipe
#'
#' @param compute A BioNeMo compute descriptor.
#'
#' @return A `BioNeMoSetupPlan` containing structured commands.
#' @export
bionemo_install_plan <- function(compute) {
  stopifnot(
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    )
  )
  paths <- install_paths(compute)
  recipe <- compute@recipe

  if (compute@engine == "external") {
    steps <- runtime_install_steps(compute)
    return(BioNeMoSetupPlan(
      target = "install",
      compute = compute,
      model = NULL,
      path = paths$root,
      steps = steps,
      executed = FALSE
    ))
  }

  if (compute@backend == "slurm") {
    steps <- runtime_install_steps(compute)
    return(BioNeMoSetupPlan(
      target = "install",
      compute = compute,
      model = NULL,
      path = paths$root,
      steps = steps,
      executed = FALSE
    ))
  }

  container_engine <- compute@config$container_engine %||% "docker"
  helper_revision <- package_helper_revision()
  if (!recipe_image_requires_build(compute)) {
    steps <- list(
      install_step(
        "image-inspect",
        "resolve the prebuilt recipe image ID",
        command_spec(
          container_engine,
          c("image", "inspect", "--format", "{{.Id}}", compute@image)
        )
      ),
      install_step(
        "image-labels",
        "verify the prebuilt recipe image labels",
        command_spec(
          container_engine,
          c(
            "image",
            "inspect",
            "--format",
            "{{json .Config.Labels}}",
            compute@image
          )
        ),
        as.list(expected_container_image_labels(recipe, helper_revision))
      )
    )
    steps <- c(steps, runtime_install_steps(compute))
    return(BioNeMoSetupPlan(
      target = "install",
      compute = compute,
      model = NULL,
      path = paths$root,
      steps = steps,
      executed = FALSE
    ))
  }

  lock <- recipe_install_lock(recipe)
  dockerfile <- file.path(paths$source, recipe@subdirectory, "Dockerfile")
  steps <- list(
    install_step(
      "source-init",
      "create a content-addressed BioNeMo Recipes checkout",
      command_spec("git", c("-C", paths$source, "init")),
      list(repository = recipe@repository)
    ),
    install_step(
      "source-fetch",
      "fetch the exact locked recipe revision",
      command_spec(
        "git",
        c(
          "-C",
          paths$source,
          "fetch",
          "--depth",
          "1",
          recipe@repository,
          recipe@revision
        )
      ),
      list(revision = recipe@revision)
    ),
    install_step(
      "source-checkout",
      "check out the fetched recipe revision",
      command_spec(
        "git",
        c("-C", paths$source, "checkout", "--detach", "FETCH_HEAD")
      ),
      list(revision = recipe@revision)
    ),
    install_step(
      "dockerfile-verify",
      "verify the official locked recipe Dockerfile",
      command_spec("git", c("-C", paths$source, "hash-object", dockerfile)),
      list(
        path = file.path(recipe@subdirectory, "Dockerfile"),
        blob = lock$dockerfile_blob
      )
    ),
    install_step(
      "base-image-pull",
      "pull the official NGC PyTorch base image",
      command_spec(
        container_engine,
        c("pull", recipe_base_image_reference(recipe))
      ),
      list(
        base_image = recipe@base_image,
        digest = recipe@base_image_digest,
        reference = recipe_base_image_reference(recipe)
      )
    ),
    install_step(
      "base-image-verify",
      "verify the locked base-image digest",
      command_spec(
        container_engine,
        c(
          "image",
          "inspect",
          "--format",
          "{{json .RepoDigests}}",
          recipe_base_image_reference(recipe)
        )
      ),
      list(digest = recipe@base_image_digest)
    ),
    install_step(
      "image-build",
      "build the official recipe with the package helper appended",
      command_spec(
        container_engine,
        c(
          "build",
          "--file",
          file.path(paths$context, "Dockerfile"),
          "--tag",
          default_recipe_image(recipe),
          "--build-arg",
          paste0("BIONEMOR_RECIPE_REVISION=", recipe@revision),
          "--build-arg",
          paste0("BIONEMOR_HELPER_REVISION=", helper_revision),
          "--build-arg",
          paste0("BIONEMOR_BASE_IMAGE=", recipe@base_image),
          "--build-arg",
          paste0(
            "BIONEMOR_BASE_IMAGE_DIGEST=",
            recipe@base_image_digest %||% ""
          ),
          paths$context
        )
      ),
      list(
        official_dockerfile = file.path(recipe@subdirectory, "Dockerfile"),
        dockerfile_blob = lock$dockerfile_blob,
        base_image_reference = recipe_base_image_reference(recipe),
        helper_revision = helper_revision,
        image = default_recipe_image(recipe)
      )
    ),
    install_step(
      "image-inspect",
      "resolve the built image ID",
      command_spec(
        container_engine,
        c("image", "inspect", "--format", "{{.Id}}", compute@image)
      )
    )
  )
  steps <- c(steps, runtime_install_steps(compute))
  BioNeMoSetupPlan(
    target = "install",
    compute = compute,
    model = NULL,
    path = paths$root,
    steps = steps,
    executed = FALSE
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

prepare_recipe_build_context <- function(paths, recipe) {
  if (!dir.exists(paths$source)) {
    bionemor_abort(
      "BN_RECIPE_MISSING",
      "recipe source checkout is missing",
      operation = "install",
      recipe_revision = recipe@revision,
      hint = "Run bionemo_install() to fetch the pinned recipe source."
    )
  }
  stopifnot(
    "build context must be inside the installation root" = path_is_within(
      paths$context,
      paths$root
    ) &&
      !identical(paths$context, paths$root)
  )
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
  helper <- package_asset("scripts", "materialize-evo2.py")
  if (!file.copy(helper, file.path(helper_dir, "materialize-evo2.py"))) {
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
  appendage <- package_asset(
    "docker",
    "evo2-recipes",
    "Dockerfile.append"
  )
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

package_helper_revision <- function() {
  result <- run_install_command(
    "git",
    c("hash-object", package_asset("scripts", "materialize-evo2.py")),
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
  stopifnot(
    "container engine must be one command name" = is_scalar_string(engine),
    "recipe must be a BioNeMo recipe" = S7_inherits(recipe, BioNeMoRecipe),
    "verified recipes require a locked base-image digest" = !recipe@verified ||
      is_scalar_string(recipe@base_image_digest)
  )
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

#' Install or verify the pinned BioNeMo Evo 2 recipe runtime
#'
#' @param compute A BioNeMo compute descriptor.
#' @param rebuild Rebuild an existing deterministic local image.
#' @param pull Pull the locked digest-qualified base image before building.
#' @param keep_source Keep the synthetic build context for inspection.
#'
#' @return The compute descriptor with resolved runtime metadata.
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
  plan <- bionemo_install_plan(compute)
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
    prepare_recipe_build_context(paths, compute@recipe)
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
    helper_revision <- run_install_command(
      "git",
      c(
        "hash-object",
        package_asset("scripts", "materialize-evo2.py")
      ),
      error = "failed to hash the package helper",
      code = "BN_IMAGE_BUILD",
      recipe_revision = compute@recipe@revision
    )
    run_install_command(
      engine,
      c(
        "build",
        "--file",
        file.path(paths$context, "Dockerfile"),
        "--tag",
        default_recipe_image(compute@recipe),
        "--build-arg",
        paste0("BIONEMOR_RECIPE_REVISION=", compute@recipe@revision),
        "--build-arg",
        paste0(
          "BIONEMOR_HELPER_REVISION=",
          trimws(helper_revision$stdout)
        ),
        "--build-arg",
        paste0("BIONEMOR_BASE_IMAGE=", compute@recipe@base_image),
        "--build-arg",
        paste0(
          "BIONEMOR_BASE_IMAGE_DIGEST=",
          compute@recipe@base_image_digest %||% ""
        ),
        paths$context
      ),
      error = "failed to build the pinned Evo 2 recipe image",
      code = "BN_IMAGE_BUILD",
      recipe_revision = compute@recipe@revision,
      hint = "Inspect the container build output and pinned recipe inputs."
    )
    compute@image <- default_recipe_image(compute@recipe)
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
  invisible(plan)
  compute
}

#' Deprecated recipe setup alias
#'
#' @param compute A BioNeMo compute descriptor.
#' @param ... Unused.
#'
#' @return A `BioNeMoSetupPlan`.
#' @export
bionemo_setup <- function(compute, ...) {
  .Deprecated("bionemo_install_plan")
  bionemo_install_plan(compute)
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
  stopifnot(
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "refresh must be TRUE or FALSE" = is_scalar_logical(refresh)
  )
  cached <- compute@config$capabilities
  if (!refresh && is.list(cached)) {
    return(cached)
  }
  if (compute@backend == "slurm" && compute@engine == "container") {
    compute@image_digest <- slurm_image_digest(compute)
  }
  result <- runtime_probe(
    compute,
    "bionemor-evo2-helper",
    c("capabilities", "--json"),
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
      command = "bionemor-evo2-helper",
      upstream_exit_status = as.integer(result$status),
      hint = "Install or activate the package-pinned Evo 2 recipe runtime."
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
      hint = "Install the package-pinned Evo 2 recipe runtime."
    )
  }
  if (!identical(report$recipe_revision, compute@recipe@revision)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "helper recipe revision does not match the compute recipe",
      operation = "runtime-capabilities",
      recipe_revision = compute@recipe@revision,
      actual_recipe_revision = report$recipe_revision %||% NULL,
      hint = "Install the package-pinned Evo 2 recipe runtime."
    )
  }
  report$image <- compute@image
  report$image_digest <- compute@image_digest
  report$probed_at <- base::format(Sys.time(), tz = "UTC", usetz = TRUE)
  report
}

runtime_command_key <- function(command) {
  switch(
    command,
    evo2_convert_savanna_to_mbridge = "savanna_to_mbridge",
    evo2_convert_nemo2_to_mbridge = "nemo2_to_mbridge",
    evo2_export_mbridge_to_vortex = "mbridge_to_vortex",
    evo2_remove_optimizer = "remove_optimizer",
    command
  )
}

verify_runtime_commands <- function(compute, report, target = "all") {
  commands <- install_probe_commands(target)
  advertised <- report$commands %||% list()
  keys <- vapply(commands, runtime_command_key, character(1))
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
      hint = "Install the package-pinned Evo 2 recipe runtime."
    )
  }
  for (command in commands) {
    probe <- runtime_probe(
      compute,
      command,
      "--help",
      gpus = compute@engine == "container"
    )
    if (probe$status != 0L) {
      detail <- redact_credentials(trimws(paste(probe$stderr, probe$stdout)))
      bionemor_abort(
        "BN_RUNTIME_MISSING",
        paste0(
          "recipe command probe failed for ",
          command,
          if (nzchar(detail)) paste0(": ", detail) else ""
        ),
        operation = "install",
        recipe_revision = compute@recipe@revision,
        request_id = probe$request_id %||% NULL,
        log_paths = probe$log_paths %||% NULL,
        command = command,
        upstream_exit_status = as.integer(probe$status),
        hint = "Install the package-pinned Evo 2 recipe runtime."
      )
    }
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
    "bionemor-evo2-helper"
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
          backend = "slurm",
          engine = "external",
          workspace = compute@workspace,
          recipe = compute@recipe,
          gpus = compute@gpus,
          nodes = compute@nodes,
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
    version_row("runtime CUDA", runtime$cuda),
    version_row(
      "runtime Transformer Engine",
      runtime$transformer_engine
    ),
    version_row(
      "runtime Megatron Bridge",
      runtime$megatron_bridge
    ),
    doctor_row(
      "runtime BioNeMo",
      if (isTRUE(imports$bionemo)) "pass" else "fail",
      if (isTRUE(imports$bionemo)) "import available" else "import unavailable"
    ),
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
        paste(report$image %||% "unknown", report$image_digest %||% "no digest")
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
  for (command in install_probe_commands(target)) {
    key <- switch(
      command,
      evo2_convert_savanna_to_mbridge = "savanna_to_mbridge",
      evo2_convert_nemo2_to_mbridge = "nemo2_to_mbridge",
      evo2_export_mbridge_to_vortex = "mbridge_to_vortex",
      evo2_remove_optimizer = "remove_optimizer",
      command
    )
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
    config <- compute@config
    config$capabilities <- report
    compatible_compute <- compute
    compatible_compute@config <- config
    models <- evo2_models(compatible_compute)
    selected <- models[models$name == model@size, , drop = FALSE]
    stopifnot(
      "model is missing from the compatibility registry" = nrow(selected) == 1L
    )
    rows[[length(rows) + 1L]] <- doctor_row(
      "model compatibility",
      if (isTRUE(selected$compatible[[1L]])) "pass" else "fail",
      selected$compatibility_note[[1L]]
    )
  }
  do.call(rbind, rows)
}

doctor_checkpoint_storage <- function(compute, model) {
  if (is.null(model)) {
    return(NULL)
  }
  checkpoint <- model_checkpoint_path(model, base = compute@workspace)
  present <- is_scalar_string(checkpoint) && dir.exists(checkpoint)
  record <- evo2_model_record(model@size)
  doctor_row(
    "checkpoint storage",
    if (present) "pass" else "fail",
    if (present) {
      "checkpoint is already present"
    } else {
      paste(
        "checkpoint is unavailable;",
        format(round(record$download_size / 1024^3, 1), nsmall = 1),
        "GiB download required"
      )
    }
  )
}

doctor_checkpoint <- function(compute, model) {
  if (is.null(model)) {
    return(NULL)
  }
  checkpoint <- model_checkpoint_path(model, base = compute@workspace)
  if (is.null(checkpoint)) {
    return(doctor_row(
      "model checkpoint",
      "fail",
      "an explicit MBridge checkpoint is required"
    ))
  }
  if (!dir.exists(checkpoint)) {
    return(doctor_row(
      "model checkpoint",
      "fail",
      paste("checkpoint is not available:", checkpoint)
    ))
  }
  root <- file.path(compute@workspace, ".bionemor", "doctor")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  output <- tempfile("checkpoint-", tmpdir = root, fileext = ".json")
  probe <- runtime_probe(
    compute,
    "bionemor-evo2-helper",
    c("inspect-checkpoint", "--path", checkpoint, "--output", output)
  )
  detail <- redact_credentials(trimws(paste(probe$stdout, probe$stderr)))
  if (probe$status != 0L || !file.exists(output)) {
    return(doctor_row(
      "model checkpoint",
      "fail",
      if (nzchar(detail)) detail else "checkpoint inspection failed",
      detail
    ))
  }
  inspection <- read_json_file(output)
  correct <- identical(inspection$model_size, model@model_size)
  doctor_row(
    "model checkpoint",
    if (correct) "pass" else "fail",
    if (correct) {
      paste(inspection$model_size, inspection$kind %||% "unknown")
    } else {
      "checkpoint model size does not match the model"
    }
  )
}

#' Diagnose a BioNeMo Recipes execution environment
#'
#' @param compute A BioNeMo compute descriptor.
#' @param model Optional Evo 2 model.
#' @param target Operation target.
#' @param verbose Include complete probe output when printing.
#'
#' @return A `BioNeMoDoctor`.
#' @export
bionemo_doctor <- function(
  compute,
  model = NULL,
  target = c("all", "inference", "training", "conversion"),
  verbose = TRUE
) {
  target <- match.arg(target)
  stopifnot(
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "model must be NULL or an Evo 2 model" = is.null(model) ||
      S7_inherits(model, Evo2Model),
    "verbose must be TRUE or FALSE" = is_scalar_logical(verbose)
  )
  checks <- rbind(
    doctor_backend(compute),
    doctor_host_tools(compute),
    doctor_workspace(compute),
    doctor_capabilities(compute, target, model),
    doctor_checkpoint_storage(compute, model),
    doctor_checkpoint(compute, model)
  )
  rownames(checks) <- NULL
  BioNeMoDoctor(
    target = target,
    ok = !any(checks$status == "fail"),
    checks = checks,
    verbose = verbose
  )
}

method(print, BioNeMoSetupPlan) <- function(x, ...) {
  cat("<BioNeMo install plan>\n", sep = "")
  cat("Target: ", x@target, "\n", sep = "")
  cat("Steps:  ", length(x@steps), "\n", sep = "")
  cat("Status: ", if (x@executed) "executed" else "planned", "\n", sep = "")
  invisible(x)
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
