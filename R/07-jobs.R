command_available <- function(command) {
  nzchar(Sys.which(command))
}

command_probe <- function(
  command,
  args = character(),
  env = process_environment()
) {
  processx::run(
    command,
    args,
    error_on_status = FALSE,
    echo = FALSE,
    env = env
  )
}

job_directives <- function(compute, name, log) {
  directives <- c(
    paste("#SBATCH --job-name", shQuote(name)),
    paste("#SBATCH --nodes", compute@nodes),
    paste("#SBATCH --gpus", compute@gpus),
    paste("#SBATCH --output", shQuote(log))
  )
  if (!is.null(compute@queue)) {
    directives <- c(
      directives,
      paste("#SBATCH --partition", shQuote(compute@queue))
    )
  }
  if (!is.null(compute@account)) {
    directives <- c(
      directives,
      paste("#SBATCH --account", shQuote(compute@account))
    )
  }
  if (!is.null(compute@walltime)) {
    directives <- c(
      directives,
      paste("#SBATCH --time", shQuote(compute@walltime))
    )
  }
  directives
}

write_job_script <- function(command, compute, name, log) {
  jobs <- file.path(compute@workspace, ".bionemor", "jobs")
  dir.create(jobs, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  path <- file.path(jobs, paste0(name, ".sh"))
  writeLines(
    c(
      "#!/usr/bin/env bash",
      if (compute@backend == "slurm") job_directives(compute, name, log),
      "set -euo pipefail",
      command
    ),
    path,
    useBytes = TRUE
  )
  Sys.chmod(path, "0750")
  path
}

submit_job <- function(
  command,
  compute,
  name,
  kind,
  expected_result,
  timeout = Inf,
  metadata = list()
) {
  stopifnot(
    "compute workspace must exist" = dir.exists(compute@workspace)
  )
  log <- file.path(
    compute@workspace,
    ".bionemor",
    "logs",
    paste0(name, ".log")
  )
  script <- write_job_script(command, compute, name, log)
  metadata <- c(
    metadata,
    list(
      name = name,
      script = script,
      started_at = as.numeric(Sys.time())
    )
  )
  if (compute@backend == "local") {
    process <- processx::process$new(
      "bash",
      script,
      stdout = log,
      stderr = "2>&1",
      cleanup = FALSE,
      env = process_environment()
    )
    return(BioNeMoJob(
      id = as.character(process$get_pid()),
      kind = kind,
      state = "running",
      compute = compute,
      command = command,
      log = log,
      expected_result = expected_result,
      timeout = as.double(timeout),
      process = process,
      metadata = metadata
    ))
  }

  stopifnot("sbatch is not available" = command_available("sbatch"))
  submitted <- command_probe("sbatch", c("--parsable", script))
  if (submitted$status != 0L) {
    stop(trimws(submitted$stderr), call. = FALSE)
  }
  id <- sub(";.*$", "", trimws(submitted$stdout))
  stopifnot("sbatch returned an invalid job ID" = grepl("^[0-9]+$", id))
  BioNeMoJob(
    id = id,
    kind = kind,
    state = "submitted",
    compute = compute,
    command = command,
    log = log,
    expected_result = expected_result,
    timeout = as.double(timeout),
    process = NULL,
    metadata = metadata
  )
}

local_job_status <- function(job) {
  if (job@state %in% c("completed", "failed", "cancelled")) {
    return(job@state)
  }
  if (job@process$is_alive()) {
    return("running")
  }
  status <- job@process$get_exit_status()
  if (identical(status, 0L)) "completed" else "failed"
}

slurm_job_status <- function(job) {
  if (job@state %in% c("completed", "failed", "cancelled")) {
    return(job@state)
  }
  stopifnot("sacct is not available" = command_available("sacct"))
  result <- command_probe(
    "sacct",
    c(
      "-X", "-n", "-P", "-j", job@id,
      "--format=JobIDRaw,State,ExitCode"
    )
  )
  if (result$status != 0L) {
    stop(trimws(result$stderr), call. = FALSE)
  }
  lines <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1L]]
  fields <- lapply(lines[nzchar(lines)], strsplit, split = "|", fixed = TRUE)
  fields <- lapply(fields, `[[`, 1L)
  matching <- vapply(
    fields,
    function(x) length(x) == 3L && x[[1L]] == job@id,
    logical(1)
  )
  stopifnot(
    "sacct did not return one allocation record" = sum(matching) == 1L
  )
  record <- fields[[which(matching)]]
  original <- trimws(record[[2L]])
  state <- sub("[+ ].*$", "", toupper(original))
  exit_code <- trimws(record[[3L]])
  if (state == "COMPLETED" && exit_code != "0:0") {
    state <- "FAILED"
  }
  switch(
    state,
    PENDING = "submitted",
    CONFIGURING = "submitted",
    RUNNING = "running",
    COMPLETING = "running",
    COMPLETED = "completed",
    CANCELLED = "cancelled",
    FAILED = "failed",
    TIMEOUT = "failed",
    OUT_OF_MEMORY = "failed",
    NODE_FAIL = "failed",
    stop("sacct returned an unknown job state: ", original, call. = FALSE)
  )
}

#' Return a BioNeMo job state
#'
#' @param x A BioNeMo job.
#' @param refresh Whether to query the execution backend.
#'
#' @return One state string.
#' @export
job_status <- function(x, refresh = TRUE) {
  stopifnot(
    "x must be a BioNeMo job" = S7_inherits(x, BioNeMoJob),
    "refresh must be TRUE or FALSE" = is_scalar_logical(refresh)
  )
  if (!refresh) {
    return(x@state)
  }
  if (x@compute@backend == "local") {
    local_job_status(x)
  } else {
    slurm_job_status(x)
  }
}

#' Read BioNeMo job logs
#'
#' @param x A BioNeMo job.
#' @param tail Optional number of final lines.
#'
#' @return A character vector.
#' @export
job_logs <- function(x, tail = NULL) {
  stopifnot(
    "x must be a BioNeMo job" = S7_inherits(x, BioNeMoJob),
    "tail must be NULL or a positive integer" =
      is.null(tail) || is_scalar_integerish(tail, min = 1)
  )
  if (is.null(x@log) || !file.exists(x@log)) {
    return(character())
  }
  lines <- readLines(x@log, warn = FALSE)
  lines <- redact_credentials(lines)
  if (is.null(tail)) lines else utils::tail(lines, as.integer(tail))
}

stop_local_job <- function(x) {
  if (!is.null(x@process) && x@process$is_alive()) {
    on.exit({
      if (x@process$is_alive()) {
        # processx terminates the subprocess group; deliberately detached
        # descendants are outside the BioNeMo CLI process contract.
        x@process$kill()
      }
    }, add = TRUE)
  }
  container <- x@metadata$container_name
  if (!is.null(container)) {
    stopifnot(
      "docker is not available to stop the running container" =
        command_available("docker")
    )
    stopped <- command_probe("docker", c("stop", container))
    if (stopped$status != 0L) {
      detail <- redact_credentials(trimws(paste(
        stopped$stderr,
        stopped$stdout
      )))
      stop(
        "failed to stop Docker container",
        if (nzchar(detail)) paste0(": ", detail) else "",
        call. = FALSE
      )
    }
  }
  invisible(x)
}

#' Cancel a BioNeMo job
#'
#' @param x A BioNeMo job.
#'
#' @return The updated job, invisibly.
#' @export
job_cancel <- function(x) {
  stopifnot("x must be a BioNeMo job" = S7_inherits(x, BioNeMoJob))
  if (x@compute@backend == "local") {
    stop_local_job(x)
  } else {
    stopifnot("scancel is not available" = command_available("scancel"))
    cancelled <- command_probe("scancel", x@id)
    if (cancelled$status != 0L) {
      stop(trimws(cancelled$stderr), call. = FALSE)
    }
  }
  x@state <- "cancelled"
  invisible(x)
}

one_prediction_tensor <- function(path) {
  tensors <- list.files(
    path,
    pattern = "[.]pt$",
    full.names = TRUE,
    recursive = TRUE
  )
  stopifnot(
    "BioNeMo did not write exactly one prediction tensor" =
      length(tensors) == 1L
  )
  normalizePath(tensors, mustWork = TRUE)
}

materialize_fit_result <- function(job, result) {
  path <- model_checkpoint_path(result)
  stopifnot(
    "completed fitting job did not materialize its checkpoint" =
      is_scalar_string(path) && dir.exists(path),
    "fitted checkpoint is missing context/model.yaml" =
      file.exists(file.path(path, "context", "model.yaml")),
    "fitted checkpoint is missing weights/metadata.json" =
      file.exists(file.path(path, "weights", "metadata.json"))
  )
  manifest <- list(
    family = "evo2",
    variant = result@size,
    source = paste0("fit://", job@metadata$name),
    profile = result@provenance$profile,
    format = "nemo2",
    command = job@command,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  jsonlite::write_json(
    manifest,
    file.path(path, checkpoint_manifest_file),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  checkpoint <- checkpoint_from_manifest(path, manifest, command = job@command)
  Evo2Model(
    family = "evo2",
    checkpoint = checkpoint,
    pretrained = TRUE,
    task = "causal_lm",
    config = result@config,
    provenance = result@provenance,
    size = result@size
  )
}

materialize_prediction_result <- function(result) {
  type <- result@type
  output <- result@metadata$output
  ids <- result@metadata$ids
  if (type == "raw") {
    artifact <- BioNeMoArtifact(
      path = one_prediction_tensor(output),
      format = "pytorch",
      metadata = list(
        checkpoint = result@metadata$checkpoint,
        profile = result@metadata$profile
      )
    )
    result@data <- artifact
    return(result)
  }
  if (type == "score") {
    path <- file.path(output, "scores.json")
    stopifnot("score materialization file is missing" = file.exists(path))
    values <- jsonlite::read_json(path, simplifyVector = TRUE)
    stopifnot(
      "score output has unexpected sequence indices" =
        identical(as.integer(values$sequence_indices), seq_along(ids) - 1L),
      "score output has an unexpected number of values" =
        length(values$scores) == length(ids)
    )
    result@data <- data.frame(
      id = ids,
      score = as.double(values$scores),
      checkpoint = rep(result@metadata$checkpoint, length(ids)),
      profile = rep(result@metadata$profile, length(ids)),
      reduction = rep(result@metadata$reduction, length(ids)),
      stringsAsFactors = FALSE
    )
    return(result)
  }
  files <- file.path(output, sprintf("%06d.txt", seq_along(ids)))
  stopifnot(
    "generation output files are missing" = all(file.exists(files))
  )
  values <- vapply(files, function(path) {
    value <- paste(readLines(path, warn = FALSE), collapse = "\n")
    if (grepl("^\\['[^']*'\\]$", value)) {
      value <- substring(value, 3L, nchar(value) - 2L)
    }
    value
  }, character(1))
  result@data <- stats::setNames(values, ids)
  result
}

materialize_job_result <- function(x) {
  result <- x@expected_result
  if (S7_inherits(result, BioNeMoModel)) {
    materialize_fit_result(x, result)
  } else if (S7_inherits(result, BioNeMoPrediction)) {
    materialize_prediction_result(result)
  } else {
    stop("job has an unsupported result contract", call. = FALSE)
  }
}

#' Return a completed BioNeMo job result
#'
#' @param x A BioNeMo job.
#'
#' @return The operation's typed result.
#' @export
job_result <- function(x) {
  stopifnot("x must be a BioNeMo job" = S7_inherits(x, BioNeMoJob))
  state <- job_status(x)
  if (state != "completed") {
    stop("job is ", state, ", not completed", call. = FALSE)
  }
  materialize_job_result(x)
}

#' Wait for a BioNeMo job
#'
#' @param x A BioNeMo job.
#' @param poll Polling interval in seconds.
#' @param timeout Maximum time spent waiting. A wait timeout does not cancel
#'   the job.
#'
#' @return The operation's typed result.
#' @export
job_wait <- function(x, poll = 10, timeout = Inf) {
  stopifnot(
    "x must be a BioNeMo job" = S7_inherits(x, BioNeMoJob),
    "poll must be positive" = is_scalar_number(poll) && poll > 0,
    "timeout must be positive" =
      is_scalar_number(timeout) && timeout > 0 || identical(timeout, Inf)
  )
  wait_started <- Sys.time()
  repeat {
    operation_elapsed <- as.numeric(Sys.time()) - x@metadata$started_at
    if (
      x@compute@backend == "local" &&
        is.finite(x@timeout) &&
        operation_elapsed >= x@timeout &&
        x@process$is_alive()
    ) {
      stop_local_job(x)
      stop("BioNeMo operation timed out", call. = FALSE)
    }
    if (
      x@compute@backend == "local" &&
        is.finite(x@timeout) &&
        !x@process$is_alive() &&
        x@process$get_exit_status() %in% c(124L, 137L)
    ) {
      stop("BioNeMo operation timed out", call. = FALSE)
    }
    state <- job_status(x)
    if (state == "completed") {
      return(materialize_job_result(x))
    }
    if (state %in% c("failed", "cancelled")) {
      detail <- paste(job_logs(x, tail = 50L), collapse = "\n")
      stop(
        "job ",
        state,
        if (nzchar(detail)) paste0(":\n", detail) else "",
        call. = FALSE
      )
    }
    wait_elapsed <- as.numeric(difftime(
      Sys.time(),
      wait_started,
      units = "secs"
    ))
    if (wait_elapsed >= timeout) {
      stop("timed out waiting for job", call. = FALSE)
    }
    Sys.sleep(min(poll, timeout - wait_elapsed))
  }
}

method(print, BioNeMoJob) <- function(x, ...) {
  cat("<bionemor_job>\n", sep = "")
  cat("ID: ", x@id, "\n", sep = "")
  cat("Kind: ", x@kind, "\n", sep = "")
  cat("State: ", x@state, "\n", sep = "")
  invisible(x)
}
