checkpoint_manifest_file <- "bionemor-checkpoint.json"

evo2_profile_size <- function(size) {
  switch(
    size,
    `1b` = "1b",
    `7b` = "7b_arc_longcontext",
    `40b` = "40b_arc_longcontext",
    stop("unsupported Evo 2 size", call. = FALSE)
  )
}

normalize_checkpoint_source <- function(source, base) {
  stopifnot("source must be one non-empty string" = is_scalar_string(source))
  if (startsWith(source, "hf://") || startsWith(source, "ngc://")) {
    return(source)
  }
  if (grepl("^[[:alpha:]][[:alnum:]+.-]*://", source)) {
    stop("source must use hf://, ngc://, or an existing local path", call. = FALSE)
  }
  source <- normalize_path(source, base = base)
  stopifnot("local checkpoint source does not exist" = file.exists(source))
  normalizePath(source, mustWork = TRUE)
}

checkpoint_is_complete <- function(path) {
  all(file.exists(c(
    file.path(path, "context", "model.yaml"),
    file.path(path, "weights", "metadata.json"),
    file.path(path, checkpoint_manifest_file)
  )))
}

read_checkpoint_manifest <- function(path) {
  manifest_path <- file.path(path, checkpoint_manifest_file)
  stopifnot(
    "checkpoint manifest does not exist" = file.exists(manifest_path)
  )
  jsonlite::read_json(manifest_path, simplifyVector = TRUE)
}

checkpoint_from_manifest <- function(path, manifest, command = NULL) {
  BioNeMoCheckpoint(
    path = normalizePath(path, mustWork = TRUE),
    format = manifest$format,
    family = manifest$family,
    variant = manifest$variant,
    source = manifest$source,
    profile = manifest$profile,
    provenance = list(
      command = command %||% manifest$command,
      created_at = manifest$created_at
    )
  )
}

assert_manifest_matches <- function(manifest, expected) {
  fields <- c("family", "variant", "source", "profile")
  for (field in fields) {
    if (!identical(manifest[[field]], expected[[field]])) {
      stop(
        "checkpoint manifest ",
        field,
        " does not match the requested ",
        field,
        call. = FALSE
      )
    }
  }
  invisible(manifest)
}

ngc_checkpoint_key <- function() {
  key <- Sys.getenv("NGC_CLI_API_KEY")
  if (!nzchar(key)) {
    key <- Sys.getenv("NGC_API_KEY")
  }
  key
}

ngc_checkpoint_command <- function(source, path) {
  resource <- sub("^ngc://", "", source)
  stopifnot(
    "ngc:// checkpoint source must name one resource" = nzchar(resource)
  )
  paste(
    "set -euo pipefail",
    paste0(
      "BIONEMOR_SOURCE=$(download_bionemo_data ",
      shQuote(resource),
      ")"
    ),
    "test -d \"$BIONEMOR_SOURCE\"",
    paste("mkdir -p", shQuote(path)),
    paste("cp -a \"$BIONEMOR_SOURCE/.\"", shQuote(path)),
    sep = "\n"
  )
}

checkpoint_command <- function(model, source, path, compute) {
  ngc <- startsWith(source, "ngc://")
  inner <- if (ngc) {
    list(
      executable = "bash",
      args = c("-lc", ngc_checkpoint_command(source, path))
    )
  } else {
    list(
      executable = "evo2_convert_to_nemo2",
      args = c(
        "--model-path", source,
        "--model-size", evo2_profile_size(model@size),
        "--output-dir", path
      )
    )
  }
  if (compute@engine == "python") {
    return(list(
      executable = inner$executable,
      args = inner$args,
      display = shell_join(inner$executable, inner$args),
      ngc = ngc
    ))
  }
  stopifnot(
    "container checkpoint preparation requires compute$image" =
      !is.null(compute@image)
  )
  runtime <- if (compute@backend == "local") "docker" else "apptainer"
  args <- if (compute@backend == "local") {
    c(
      "run", "--rm", "--gpus", "all", "--ipc=host",
      if (ngc) c("-e", "NGC_CLI_API_KEY"),
      "-v", paste0(compute@workspace, ":", compute@workspace),
      "-w", compute@workspace,
      compute@image,
      inner$executable,
      inner$args
    )
  } else {
    c(
      "exec", "--nv",
      "--bind", paste0(compute@workspace, ":", compute@workspace),
      "--pwd", compute@workspace,
      compute@image,
      inner$executable,
      inner$args
    )
  }
  list(
    executable = runtime,
    args = args,
    display = shell_join(runtime, args),
    ngc = ngc
  )
}

run_checkpoint_command <- function(command, compute) {
  environment <- process_environment()
  if (command$ngc) {
    environment[["NGC_CLI_API_KEY"]] <- ngc_checkpoint_key()
  }
  if (compute@backend == "slurm") {
    name <- basename(tempfile("checkpoint-"))
    log <- file.path(
      compute@workspace,
      ".bionemor",
      "logs",
      paste0(name, ".log")
    )
    script <- write_job_script(command$display, compute, name, log)
    stopifnot("sbatch is not available" = command_available("sbatch"))
    result <- processx::run(
      "sbatch",
      c("--parsable", "--wait", script),
      error_on_status = FALSE,
      echo = FALSE,
      env = environment
    )
    redact_text_file(log)
    if (result$status != 0L) {
      detail <- redact_credentials(trimws(paste(result$stderr, result$stdout)))
      stop(
        "checkpoint conversion failed",
        if (nzchar(detail)) paste0(": ", detail) else "",
        call. = FALSE
      )
    }
    return(invisible(result))
  }
  result <- processx::run(
    command$executable,
    command$args,
    error_on_status = FALSE,
    echo = FALSE,
    env = environment
  )
  if (result$status != 0L) {
    detail <- redact_credentials(trimws(paste(result$stderr, result$stdout)))
    stop(
      "checkpoint conversion failed",
      if (nzchar(detail)) paste0(": ", detail) else "",
      call. = FALSE
    )
  }
  invisible(result)
}

#' Prepare an Evo 2 checkpoint
#'
#' @param model An Evo 2 model specification.
#' @param source An `hf://` URI, `ngc://` URI, or existing local checkpoint.
#'   NGC resources are fetched with `download_bionemo_data` and require
#'   `NGC_CLI_API_KEY` or `NGC_API_KEY`; the credential is not stored.
#' @param path Destination path, relative to the compute workspace. It must
#'   not be a filesystem root, the workspace itself, or overlap a local source.
#' @param compute A compute specification.
#' @param overwrite Whether to replace an existing destination.
#'
#' @return An S7 `BioNeMoCheckpoint`.
#' @export
evo2_checkpoint <- function(
  model,
  source,
  path,
  compute,
  overwrite = FALSE
) {
  stopifnot(
    "model must be an Evo 2 model specification" =
      S7_inherits(model, Evo2Model),
    "compute must be a BioNeMo compute specification" =
      S7_inherits(compute, BioNeMoCompute),
    "compute workspace must exist" = dir.exists(compute@workspace),
    "path must be one non-empty string" = is_scalar_string(path),
    "overwrite must be TRUE or FALSE" = is_scalar_logical(overwrite)
  )
  source <- normalize_checkpoint_source(source, base = compute@workspace)
  path <- normalize_path(path, base = compute@workspace)
  stopifnot(
    "checkpoint destination must not be the filesystem root" =
      !identical(dirname(path), path),
    "checkpoint destination must not be the compute workspace itself" =
      !identical(path, normalize_path(compute@workspace))
  )
  remote_source <- startsWith(source, "hf://") ||
    startsWith(source, "ngc://")
  if (
    !remote_source &&
      (path_is_within(source, path) || path_is_within(path, source))
  ) {
    stop(
      "local checkpoint source and destination must not overlap",
      call. = FALSE
    )
  }
  if (compute@engine == "container" || compute@backend == "slurm") {
    stopifnot(
      "checkpoint path must be inside the compute workspace" =
        path_is_within(path, compute@workspace),
      "local source must be inside the compute workspace for remote or container execution" =
        remote_source || path_is_within(source, compute@workspace)
    )
  }
  if (startsWith(source, "ngc://") && !nzchar(ngc_checkpoint_key())) {
    stop(
      "NGC_CLI_API_KEY or NGC_API_KEY is required for an ngc:// checkpoint source",
      call. = FALSE
    )
  }

  expected <- list(
    family = "evo2",
    variant = model@size,
    source = source,
    profile = compute@profile,
    format = "nemo2"
  )
  if (file.exists(path) && !overwrite) {
    if (!checkpoint_is_complete(path)) {
      stop(
        "checkpoint destination exists but is incomplete: ",
        path,
        call. = FALSE
      )
    }
    manifest <- read_checkpoint_manifest(path)
    assert_manifest_matches(manifest, expected)
    return(checkpoint_from_manifest(path, manifest))
  }
  if (file.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  command <- checkpoint_command(model, source, path, compute)
  run_checkpoint_command(command, compute)
  stopifnot(
    "converted checkpoint is missing context/model.yaml" =
      file.exists(file.path(path, "context", "model.yaml")),
    "converted checkpoint is missing weights/metadata.json" =
      file.exists(file.path(path, "weights", "metadata.json"))
  )

  manifest <- c(
    expected,
    list(
      command = command$display,
      created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )
  jsonlite::write_json(
    manifest,
    file.path(path, checkpoint_manifest_file),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  checkpoint_from_manifest(path, manifest, command = command$display)
}

#' Return a checkpoint path
#'
#' @param x A checkpoint, model, or one checkpoint path.
#'
#' @return One normalized path.
#' @export
checkpoint_path <- function(x) {
  path <- if (S7_inherits(x, BioNeMoCheckpoint)) {
    x@path
  } else if (S7_inherits(x, BioNeMoModel)) {
    model_checkpoint_path(x)
  } else {
    x
  }
  stopifnot(
    "x does not contain one checkpoint path" = is_scalar_string(path)
  )
  normalize_path(path)
}

#' Read checkpoint conversion provenance
#'
#' @param x A checkpoint, model, or one checkpoint path.
#'
#' @return A named list.
#' @export
checkpoint_manifest <- function(x) {
  read_checkpoint_manifest(checkpoint_path(x))
}
