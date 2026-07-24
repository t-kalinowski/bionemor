probe_commands <- function(target) {
  c(
    paste(
      "nvidia-smi",
      "--query-gpu=name,memory.total,driver_version",
      "--format=csv,noheader"
    ),
    "python --version",
    if (target == "training") {
      c("preprocess_evo2 --help", "train_evo2 --help")
    } else {
      c("predict_evo2 --help", "infer_evo2 --help")
    }
  )
}

setup_probe_command <- function(compute, target) {
  commands <- probe_commands(target)
  if (compute@engine == "python") {
    return(commands)
  }
  stopifnot(
    "container setup requires compute$image" = !is.null(compute@image)
  )
  inner <- paste(commands, collapse = " && ")
  if (compute@backend == "local") {
    paste(
      "docker run --rm --gpus all",
      "--entrypoint bash",
      shQuote(compute@image),
      "-lc",
      shQuote(inner)
    )
  } else {
    paste(
      "apptainer exec --nv",
      shQuote(compute@image),
      "bash -lc",
      shQuote(inner)
    )
  }
}

#' Generate a BioNeMo environment-check plan
#'
#' @param compute A BioNeMo compute specification.
#' @param model Optional Evo 2 model.
#' @param target Operation target.
#' @param path Directory for generated files.
#' @param execute Whether to execute the generated probe.
#'
#' @return An S7 `BioNeMoSetupPlan`.
#' @export
bionemo_setup <- function(
  compute,
  model = NULL,
  target = c("training", "inference"),
  path = ".bionemo",
  execute = FALSE
) {
  target <- match.arg(target)
  stopifnot(
    "compute must be a BioNeMo compute specification" =
      S7_inherits(compute, BioNeMoCompute),
    "compute workspace must exist" = dir.exists(compute@workspace),
    "model must be NULL or an Evo 2 model" =
      is.null(model) || S7_inherits(model, Evo2Model),
    "path must be one non-empty string" = is_scalar_string(path),
    "execute must be TRUE or FALSE" = is_scalar_logical(execute)
  )
  path <- normalize_path(path, base = compute@workspace)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  script <- file.path(path, paste0("check-", target, ".sh"))
  commands <- setup_probe_command(compute, target)
  lines <- c(
    "#!/usr/bin/env bash",
    if (compute@backend == "slurm") {
      job_directives(
        compute,
        paste0("bionemor-", target, "-check"),
        file.path(path, paste0(target, ".log"))
      )
    },
    "set -euo pipefail",
    commands
  )
  writeLines(lines, script, useBytes = TRUE)
  Sys.chmod(script, "0750")

  if (execute) {
    result <- if (compute@backend == "local") {
      processx::run(
        "bash",
        script,
        error_on_status = FALSE,
        echo = FALSE,
        env = process_environment()
      )
    } else {
      processx::run(
        "sbatch",
        c("--parsable", "--wait", script),
        error_on_status = FALSE,
        echo = FALSE,
        env = process_environment()
      )
    }
    if (result$status != 0L) {
      detail <- redact_credentials(trimws(paste(result$stderr, result$stdout)))
      stop(
        "BioNeMo setup probe failed",
        if (nzchar(detail)) paste0(": ", detail) else "",
        call. = FALSE
      )
    }
  }

  BioNeMoSetupPlan(
    target = target,
    compute = compute,
    model = model,
    path = normalizePath(path, mustWork = TRUE),
    files = normalizePath(script, mustWork = TRUE),
    commands = if (compute@backend == "slurm") {
      paste("sbatch --parsable --wait", shQuote(script))
    } else {
      commands
    },
    executed = execute
  )
}

doctor_row <- function(
  check,
  status,
  detail,
  output = "",
  artifact = NA_character_
) {
  data.frame(
    check = check,
    status = status,
    detail = detail,
    output = output,
    artifact = artifact,
    stringsAsFactors = FALSE
  )
}

doctor_probe <- function(command, args = character()) {
  tryCatch(
    processx::run(
      command,
      args,
      error_on_status = FALSE,
      echo = FALSE,
      env = process_environment()
    ),
    error = function(error) {
      list(
        status = 1L,
        stdout = "",
        stderr = conditionMessage(error)
      )
    }
  )
}

doctor_command <- function(check, command, args = character()) {
  if (!command_available(command)) {
    return(doctor_row(
      check,
      "fail",
      paste(command, "is not available")
    ))
  }
  result <- doctor_probe(command, args)
  output <- redact_credentials(trimws(paste(result$stdout, result$stderr)))
  if (result$status == 0L) {
    doctor_row(check, "pass", "available", output)
  } else {
    doctor_row(
      check,
      "fail",
      if (nzchar(output)) output else paste(command, "failed"),
      output
    )
  }
}

doctor_model <- function(model, target, workspace) {
  if (is.null(model)) {
    return(NULL)
  }
  checkpoint <- model_checkpoint_path(model, base = workspace)
  required <- target == "inference" || model@pretrained
  if (!required && is.null(checkpoint)) {
    return(doctor_row(
      "model checkpoint",
      "pass",
      "training from scratch"
    ))
  }
  if (is.null(checkpoint)) {
    return(doctor_row(
      "model checkpoint",
      "fail",
      "an explicit checkpoint is required"
    ))
  }
  if (!dir.exists(checkpoint)) {
    return(doctor_row(
      "model checkpoint",
      "fail",
      paste("checkpoint is not available:", checkpoint)
    ))
  }
  doctor_row("model checkpoint", "pass", checkpoint)
}

doctor_local_python <- function(target) {
  specifications <- if (target == "training") {
    list(
      list(
        "nvidia-smi",
        "nvidia-smi",
        c(
          "--query-gpu=name,memory.total,driver_version",
          "--format=csv,noheader"
        )
      ),
      list("python", "python", "--version"),
      list("preprocess_evo2", "preprocess_evo2", "--help"),
      list("train_evo2", "train_evo2", "--help")
    )
  } else {
    list(
      list(
        "nvidia-smi",
        "nvidia-smi",
        c(
          "--query-gpu=name,memory.total,driver_version",
          "--format=csv,noheader"
        )
      ),
      list("python", "python", "--version"),
      list("predict_evo2", "predict_evo2", "--help"),
      list("infer_evo2", "infer_evo2", "--help")
    )
  }
  do.call(
    rbind,
    lapply(specifications, function(specification) {
      doctor_command(
        specification[[1L]],
        specification[[2L]],
        specification[[3L]]
      )
    })
  )
}

doctor_local_container <- function(compute, target) {
  if (!command_available("docker")) {
    return(doctor_row(
      paste("container", target, "probe"),
      "fail",
      "docker is not available"
    ))
  }
  if (is.null(compute@image)) {
    return(doctor_row(
      paste("container", target, "probe"),
      "fail",
      "compute image is required"
    ))
  }
  command <- paste(probe_commands(target), collapse = " && ")
  result <- doctor_probe(
    "docker",
    c(
      "run", "--rm", "--gpus", "all",
      "--entrypoint", "bash",
      compute@image,
      "-lc", command
    )
  )
  output <- redact_credentials(trimws(paste(result$stdout, result$stderr)))
  doctor_row(
    paste("container", target, "probe"),
    if (result$status == 0L) "pass" else "fail",
    if (result$status == 0L) {
      "required commands are available"
    } else if (nzchar(output)) {
      output
    } else {
      "container probe failed"
    },
    output
  )
}

doctor_slurm <- function(compute, target) {
  check <- paste("Slurm", target, "probe")
  if (!command_available("sbatch")) {
    return(doctor_row(check, "fail", "sbatch is not available"))
  }
  if (compute@engine == "container" && is.null(compute@image)) {
    return(doctor_row(check, "fail", "compute image is required"))
  }
  root <- file.path(compute@workspace, ".bionemor", "doctor")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  name <- basename(tempfile(paste0(target, "-"), tmpdir = root))
  log <- file.path(root, paste0(name, ".log"))
  command <- if (compute@engine == "python") {
    paste(probe_commands(target), collapse = "\n")
  } else {
    paste(
      "apptainer exec --nv",
      shQuote(compute@image),
      "bash -lc",
      shQuote(paste(probe_commands(target), collapse = " && "))
    )
  }
  script <- write_job_script(command, compute, basename(name), log)
  result <- doctor_probe("sbatch", c("--parsable", "--wait", script))
  redact_text_file(log)
  probe_output <- if (file.exists(log)) {
    paste(readLines(log, warn = FALSE), collapse = "\n")
  } else {
    ""
  }
  output <- redact_credentials(trimws(paste(
    probe_output,
    result$stdout,
    result$stderr
  )))
  doctor_row(
    check,
    if (result$status == 0L) "pass" else "fail",
    if (result$status == 0L) {
      "allocation probe completed"
    } else if (nzchar(output)) {
      output
    } else {
      "allocation probe failed"
    },
    output,
    normalizePath(script, mustWork = TRUE)
  )
}

#' Diagnose a BioNeMo execution environment
#'
#' @param compute A BioNeMo compute specification.
#' @param model Optional Evo 2 model.
#' @param target Operation target.
#' @param verbose Whether printing includes complete command output.
#'
#' @return An S7 `BioNeMoDoctor`.
#' @export
bionemo_doctor <- function(
  compute,
  model = NULL,
  target = c("training", "inference"),
  verbose = TRUE
) {
  target <- match.arg(target)
  stopifnot(
    "compute must be a BioNeMo compute specification" =
      S7_inherits(compute, BioNeMoCompute),
    "model must be NULL or an Evo 2 model" =
      is.null(model) || S7_inherits(model, Evo2Model),
    "verbose must be TRUE or FALSE" = is_scalar_logical(verbose)
  )
  model_check <- doctor_model(model, target, compute@workspace)
  runtime_checks <- if (compute@backend == "slurm") {
    doctor_slurm(compute, target)
  } else if (compute@engine == "container") {
    doctor_local_container(compute, target)
  } else {
    doctor_local_python(target)
  }
  checks <- rbind(model_check, runtime_checks)
  rownames(checks) <- NULL
  BioNeMoDoctor(
    target = target,
    ok = all(checks$status == "pass"),
    checks = checks,
    verbose = verbose
  )
}

method(print, BioNeMoSetupPlan) <- function(x, ...) {
  cat("<bionemor_setup_plan>\n", sep = "")
  cat("Target: ", x@target, "\n", sep = "")
  cat("Path: ", x@path, "\n", sep = "")
  cat("Status: ", if (x@executed) "executed" else "planned", "\n", sep = "")
  invisible(x)
}

method(print, BioNeMoDoctor) <- function(x, ...) {
  cat("<bionemor_doctor>\n", sep = "")
  cat("Target: ", x@target, "\n", sep = "")
  cat("Status: ", if (x@ok) "pass" else "fail", "\n", sep = "")
  displayed <- x@checks[c("check", "status", "detail")]
  print(displayed, row.names = FALSE)
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
