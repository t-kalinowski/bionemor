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

local_container_user <- function() {
  ids <- vapply(
    c("-u", "-g"),
    function(argument) {
      result <- command_probe("id", argument)
      value <- trimws(result$stdout)
      if (result$status != 0L || !grepl("^[0-9]+$", value)) {
        stop("local container execution requires numeric user and group IDs")
      }
      value
    },
    character(1)
  )
  paste(ids, collapse = ":")
}

local_container_user_args <- function() {
  c("--user", local_container_user(), "-e", "HOME=/tmp/bionemor")
}

command_spec <- function(
  executable,
  args = character(),
  env = character(),
  cwd = NULL,
  redactions = character(),
  stdin = NULL,
  stdout = NULL,
  stderr = NULL,
  timeout = Inf
) {
  stopifnot(
    "command executable must be one non-empty string" = is_scalar_string(
      executable
    ),
    "command args must be a character vector without missing values" = is.character(
      args
    ) &&
      !anyNA(args),
    "command env must be a named character vector without missing values" = is.character(
      env
    ) &&
      !anyNA(env) &&
      (length(env) == 0L ||
        !is.null(names(env)) &&
          all(nzchar(names(env))) &&
          !anyDuplicated(names(env))),
    "command cwd must be NULL or one non-empty string" = is.null(cwd) ||
      is_scalar_string(cwd),
    "command redactions must be a character vector without missing values" = is.character(
      redactions
    ) &&
      !anyNA(redactions),
    "command stdin must be NULL or one non-empty string" = is.null(stdin) ||
      is_scalar_string(stdin),
    "command stdout must be NULL or one non-empty string" = is.null(stdout) ||
      is_scalar_string(stdout),
    "command stderr must be NULL or one non-empty string" = is.null(stderr) ||
      is_scalar_string(stderr),
    "command timeout must be positive or infinite" = identical(timeout, Inf) ||
      is_scalar_number(timeout) && timeout > 0
  )
  structure(
    list(
      executable = executable,
      args = unname(args),
      env = env,
      cwd = cwd,
      redactions = unique(redactions),
      stdin = stdin,
      stdout = stdout,
      stderr = stderr,
      timeout = as.double(timeout)
    ),
    class = c("bionemor_command", "list")
  )
}

command_plan <- function(steps, metadata = list()) {
  if (!is.list(steps) || length(steps) <= 0L) {
    stop("command plan steps must be a non-empty list")
  }
  if (!all(vapply(steps, inherits, logical(1), "bionemor_command"))) {
    stop("every command plan step must be a command specification")
  }
  if (!is.list(metadata)) {
    stop("command plan metadata must be a list")
  }
  structure(
    list(
      schema_version = 1L,
      steps = unname(steps),
      metadata = metadata
    ),
    class = c("bionemor_command_plan", "list")
  )
}

redact_command_value <- function(value, redactions) {
  value <- redact_credentials(value)
  for (secret in redactions[nzchar(redactions)]) {
    value <- gsub(secret, "[REDACTED]", value, fixed = TRUE)
  }
  value
}

redact_persisted_value <- function(value, redactions = character()) {
  if (is.character(value)) {
    return(redact_command_value(value, redactions))
  }
  if (is.list(value)) {
    result <- lapply(value, redact_persisted_value, redactions = redactions)
    names(result) <- names(value)
    return(result)
  }
  value
}

serializable_command <- function(command) {
  redactions <- unique(c(
    command$redactions,
    unname(command$env[
      names(command$env) %in% credential_environment_variables
    ])
  ))
  env <- redact_command_value(command$env, redactions)
  env[names(env) %in% credential_environment_variables] <- "[REDACTED]"
  list(
    executable = redact_command_value(command$executable, redactions),
    args = unname(redact_command_value(command$args, redactions)),
    env = as.list(env),
    cwd = command$cwd,
    redactions = if (length(redactions)) "[REDACTED]" else character(),
    stdin = command$stdin,
    stdout = command$stdout,
    stderr = command$stderr,
    timeout = if (is.finite(command$timeout)) command$timeout else NULL
  )
}

serializable_plan <- function(plan) {
  redactions <- unique(unlist(lapply(
    plan$steps,
    function(command) {
      c(
        command$redactions,
        unname(command$env[
          names(command$env) %in% credential_environment_variables
        ])
      )
    }
  )))
  list(
    schema_version = 1L,
    steps = lapply(plan$steps, serializable_command),
    metadata = redact_persisted_value(plan$metadata, redactions)
  )
}

read_command_plan <- function(path) {
  value <- read_json_file(path, simplify = FALSE)
  steps <- lapply(value$steps, function(step) {
    env <- unlist(step$env, use.names = TRUE)
    if (is.null(env)) {
      env <- character()
    }
    command_spec(
      executable = step$executable,
      args = unlist(step$args, use.names = FALSE),
      env = env,
      cwd = step$cwd,
      redactions = character(),
      stdin = step$stdin,
      stdout = step$stdout,
      stderr = step$stderr,
      timeout = step$timeout %||% Inf
    )
  })
  command_plan(steps, metadata = value$metadata %||% list())
}

compute_record <- function(compute) {
  recipe <- compute@recipe
  list(
    backend = compute@backend,
    engine = compute@engine,
    workspace = compute@workspace,
    image = compute@image,
    image_digest = compute@image_digest,
    gpus = compute@gpus,
    nodes = compute@nodes,
    queue = compute@queue,
    account = compute@account,
    walltime = compute@walltime,
    config = redact_persisted_value(compute@config),
    recipe = list(
      repository = recipe@repository,
      revision = recipe@revision,
      recipe_version = recipe@recipe_version,
      subdirectory = recipe@subdirectory,
      base_image = recipe@base_image,
      base_image_digest = recipe@base_image_digest,
      bridge_protocol = recipe@bridge_protocol,
      verified = recipe@verified
    )
  )
}

compute_from_record <- function(value) {
  if (
    !is.list(value$recipe) ||
      !all(
        c(
          "repository",
          "revision",
          "recipe_version",
          "subdirectory",
          "base_image",
          "base_image_digest",
          "bridge_protocol",
          "verified"
        ) %in%
          names(value$recipe)
      )
  ) {
    stop("persisted run does not contain a complete recipe record")
  }
  if (
    !is.null(value$image_digest) &&
      (!is_scalar_string(value$image_digest) ||
        !grepl("^sha256:[0-9a-fA-F]{64}$", value$image_digest))
  ) {
    stop("persisted run contains an invalid image digest")
  }
  recipe <- BioNeMoRecipe(
    repository = value$recipe$repository,
    revision = value$recipe$revision,
    recipe_version = value$recipe$recipe_version,
    subdirectory = value$recipe$subdirectory,
    base_image = value$recipe$base_image,
    base_image_digest = value$recipe$base_image_digest,
    bridge_protocol = as.integer(value$recipe$bridge_protocol),
    verified = value$recipe$verified
  )
  compute <- bionemo_compute(
    backend = value$backend,
    engine = value$engine,
    workspace = value$workspace,
    recipe = recipe,
    image = value$image,
    gpus = as.integer(value$gpus),
    nodes = as.integer(value$nodes),
    queue = value$queue,
    account = value$account,
    walltime = value$walltime,
    config = value$config %||% list()
  )
  compute@image_digest <- value$image_digest
  compute
}

utc_timestamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

write_job_state <- function(
  run_path,
  state,
  backend_id = NULL,
  exit_status = NULL,
  failure_reason = NULL
) {
  allowed <- c(
    "created",
    "submitted",
    "starting",
    "running",
    "succeeded",
    "failed",
    "cancelled",
    "unknown"
  )
  if (!state %in% allowed) {
    stop("job state is unsupported")
  }
  path <- file.path(run_path, "state.json")
  current <- if (file.exists(path)) {
    read_json_file(path, simplify = TRUE)
  } else {
    list(
      id = basename(run_path),
      kind = "unknown",
      started_at = utc_timestamp()
    )
  }
  if (current$state %in% terminal_job_states) {
    return(invisible(current))
  }
  value <- list(
    schema_version = 1L,
    id = current$id,
    kind = current$kind,
    state = state,
    backend_id = backend_id %||% current$backend_id,
    started_at = current$started_at %||% utc_timestamp(),
    updated_at = utc_timestamp(),
    exit_status = exit_status,
    failure_reason = failure_reason %||% current$failure_reason
  )
  atomic_write_json(value, path)
  event <- jsonlite::toJSON(
    list(
      time = value$updated_at,
      state = state,
      exit_status = exit_status
    ),
    auto_unbox = TRUE,
    null = "null"
  )
  cat(
    paste0(event, "\n"),
    file = file.path(run_path, "events.jsonl"),
    append = TRUE
  )
  invisible(value)
}

slurm_backend_id_path <- function(run_path) {
  file.path(run_path, "slurm-job.id")
}

persist_slurm_backend_id <- function(run_path, id) {
  if (!is_scalar_string(id) || !grepl("^[0-9]+$", id)) {
    stop("Slurm job ID is invalid")
  }
  atomic_write_lines(id, slurm_backend_id_path(run_path))
  state_path <- file.path(run_path, "state.json")
  state <- read_json_file(state_path, simplify = FALSE)
  if (
    state$state %in%
      terminal_job_states &&
      !identical(as.character(state$backend_id), id)
  ) {
    state$backend_id <- id
    atomic_write_json(state, state_path)
  }
  invisible(id)
}

persisted_slurm_backend_id <- function(run_path, fallback = NULL) {
  path <- slurm_backend_id_path(run_path)
  if (!file.exists(path)) {
    return(fallback)
  }
  value <- trimws(readLines(path, n = 1L, warn = FALSE))
  if (length(value) != 1L || !grepl("^[0-9]+$", value)) {
    stop("persisted Slurm job ID is invalid")
  }
  value
}

create_run <- function(
  compute,
  kind,
  name = NULL,
  request = list(),
  request_origins = list()
) {
  stopifnot(
    "compute must be a BioNeMo compute specification" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "compute workspace must exist" = dir.exists(compute@workspace),
    "kind must be one safe name" = is_scalar_string(kind) &&
      grepl("^[A-Za-z0-9_.-]+$", kind),
    "request must be a list" = is.list(request),
    "request origins must be a list" = is.list(request_origins)
  )
  name <- safe_name(name, paste0("evo2-", kind))
  path <- file.path(compute@workspace, ".bionemor", "runs", name)
  if (file.exists(path)) {
    stop("run directory already exists")
  }
  for (directory in c("", "inputs", "upstream", "outputs")) {
    dir.create(
      file.path(path, directory),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }
  file.create(
    file.path(path, "stdout.log"),
    file.path(path, "stderr.log"),
    file.path(path, "events.jsonl")
  )
  atomic_write_json(
    list(
      schema_version = 1L,
      id = name,
      kind = kind,
      compute = compute_record(compute),
      request = redact_persisted_value(request),
      request_origins = request_origins,
      expected_result = NULL,
      timeout = NULL
    ),
    file.path(path, "request.json")
  )
  atomic_write_json(
    list(schema_version = 1L, steps = list(), metadata = list()),
    file.path(path, "plan.json")
  )
  atomic_write_json(
    list(
      schema_version = 1L,
      id = name,
      kind = kind,
      state = "created",
      backend_id = NULL,
      started_at = utc_timestamp(),
      updated_at = utc_timestamp(),
      exit_status = NULL,
      failure_reason = NULL
    ),
    file.path(path, "state.json")
  )
  normalize_path(path)
}

job_directives <- function(compute, name, stdout, stderr = stdout) {
  directives <- c(
    paste("#SBATCH --job-name", shQuote(name)),
    paste("#SBATCH --nodes", compute@nodes),
    paste("#SBATCH --gpus", compute@gpus),
    paste("#SBATCH --output", shQuote(stdout)),
    paste("#SBATCH --error", shQuote(stderr))
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

wrap_backend_command <- function(command, compute, run_id) {
  if (compute@engine == "external") {
    return(command)
  }
  if (is.null(compute@image)) {
    stop("container execution requires an image")
  }
  environment <- command$env
  if (compute@backend == "local") {
    image <- if (grepl("@sha256:[0-9a-fA-F]{64}$", compute@image)) {
      compute@image
    } else {
      compute@image_digest
    }
    if (!is_scalar_string(image)) {
      stop("container execution requires a resolved image digest")
    }
    container_name <- paste0("bionemor-", run_id)
    env_args <- unlist(
      Map(
        function(name, value) {
          if (name %in% credential_environment_variables) {
            c("-e", name)
          } else {
            c("-e", paste0(name, "=", value))
          }
        },
        names(environment),
        unname(environment)
      ),
      use.names = FALSE
    )
    return(command_spec(
      compute@config$container_engine %||% "docker",
      c(
        "run",
        "--rm",
        "--gpus",
        "all",
        "--ipc=host",
        local_container_user_args(),
        "--name",
        container_name,
        "-v",
        paste0(compute@workspace, ":", compute@workspace),
        "-w",
        command$cwd %||% compute@workspace,
        env_args,
        image,
        command$executable,
        command$args
      ),
      cwd = compute@workspace,
      redactions = command$redactions,
      stdin = command$stdin,
      stdout = command$stdout,
      stderr = command$stderr,
      timeout = command$timeout
    ))
  }
  command_spec(
    "apptainer",
    c(
      "exec",
      "--nv",
      "--bind",
      paste0(compute@workspace, ":", compute@workspace),
      "--pwd",
      command$cwd %||% compute@workspace,
      compute@image,
      command$executable,
      command$args
    ),
    env = environment,
    cwd = compute@workspace,
    redactions = command$redactions,
    stdin = command$stdin,
    stdout = command$stdout,
    stderr = command$stderr,
    timeout = command$timeout
  )
}

render_shell_command <- function(command) {
  visible_env <- command$env[
    !names(command$env) %in% credential_environment_variables
  ]
  env <- if (length(visible_env)) {
    c("env", paste0(names(visible_env), "=", unname(visible_env)))
  } else {
    character()
  }
  value <- shell_join(
    c(env, command$executable)[[1L]],
    c(env, command$executable, command$args)[-1L]
  )
  if (!is.null(command$stdin)) {
    value <- paste(value, "<", shQuote(command$stdin))
  }
  if (!is.null(command$stdout)) {
    value <- paste(value, ">", shQuote(command$stdout))
  }
  if (!is.null(command$stderr)) {
    value <- paste(value, "2>", shQuote(command$stderr))
  }
  if (!is.null(command$cwd)) {
    value <- paste0("(cd ", shQuote(command$cwd), " && ", value, ")")
  }
  value
}

write_manifest_finalizer <- function(run_path) {
  path <- file.path(run_path, "finalize-manifest.R")
  script <- r"(
args <- commandArgs(TRUE)
run_path <- normalizePath(args[[1L]], mustWork = TRUE)

read_json <- function(path) {
  jsonlite::read_json(path, simplifyVector = FALSE)
}

inventory <- function(directory) {
  root <- file.path(run_path, directory)
  paths <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  paths <- sort(paths[!dir.exists(paths)])
  lapply(paths, function(path) {
    list(
      path = substring(path, nchar(run_path) + 2L),
      bytes = unname(file.info(path)$size),
      digest = list(
        algorithm = "md5",
        value = unname(tools::md5sum(path))
      )
    )
  })
}

path_digest <- function(path) {
  if (!dir.exists(path)) {
    return(unname(tools::md5sum(path)))
  }
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[!dir.exists(files)]
  relative <- substring(files, nchar(path) + 2L)
  records <- paste(relative, as.character(tools::md5sum(files)), sep = ":")
  temporary <- tempfile("checkpoint-digest-")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(sort(records), temporary, useBytes = TRUE)
  unname(tools::md5sum(temporary))
}

state <- read_json(file.path(run_path, "state.json"))
manifest <- read_json(file.path(run_path, "manifest-template.json"))
started <- as.POSIXct(
  state$started_at,
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)
ended <- as.POSIXct(
  state$updated_at,
  format = "%Y-%m-%dT%H:%M:%SZ",
  tz = "UTC"
)
duration <- as.numeric(difftime(ended, started, units = "secs"))
manifest$state <- state$state
manifest$backend_id <- state$backend_id
manifest$exit_status <- state$exit_status
manifest$failure_reason <- state$failure_reason
manifest$timing <- list(
  started_at = state$started_at,
  ended_at = state$updated_at,
  duration_seconds = if (is.finite(duration)) max(duration, 0) else NULL
)
manifest$inputs <- inventory("inputs")
manifest$upstream <- inventory("upstream")
manifest$outputs <- inventory("outputs")
if (
  identical(manifest$state, "succeeded") &&
    manifest$kind %in% c(
      "score",
      "profile",
      "embedding",
      "embedding-pooled",
      "embedding-unpooled"
    )
) {
  tensors <- list.files(
    file.path(run_path, "upstream"),
    pattern = "[.]pt$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(tensors)) {
    if (unlink(tensors) != 0L) {
      stop("failed to remove prediction tensors")
    }
  }
}
checkpoint_path <- manifest$checkpoint$path
if (
  is.character(checkpoint_path) &&
    length(checkpoint_path) == 1L &&
    file.exists(checkpoint_path)
) {
  manifest$checkpoint$digest <- list(
    algorithm = "md5",
    value = path_digest(checkpoint_path)
  )
}
base_path <- manifest$checkpoint$base_checkpoint$path
if (
  is.character(base_path) &&
    length(base_path) == 1L &&
    file.exists(base_path)
) {
  manifest$checkpoint$base_checkpoint$digest <- path_digest(base_path)
}

temporary <- tempfile(".manifest-", tmpdir = run_path)
jsonlite::write_json(
  manifest,
  temporary,
  auto_unbox = TRUE,
  null = "null",
  pretty = TRUE
)
if (!file.rename(temporary, file.path(run_path, "manifest.json"))) {
  stop("failed to write run manifest")
}
unlink(file.path(run_path, "manifest-template.json"))
)"
  writeLines(script, path, useBytes = TRUE)
  path
}

process_identity_value <- function(pid) {
  pid <- as.integer(pid)
  inaccessible_identity <- function() {
    list(
      schema_version = 1L,
      pid = pid,
      create_time = NA_character_,
      cmdline = character()
    )
  }
  tryCatch(
    {
      handle <- ps::ps_handle(pid)
      if (!isTRUE(ps::ps_is_running(handle))) {
        return(NULL)
      }
      list(
        schema_version = 1L,
        pid = pid,
        create_time = sprintf(
          "%.17g",
          as.numeric(ps::ps_create_time(handle))
        ),
        cmdline = unname(ps::ps_cmdline(handle))
      )
    },
    no_such_process = function(error) NULL,
    zombie_process = function(error) NULL,
    os_error = function(error) {
      if (identical(error$errno, 2L)) {
        return(NULL)
      }
      if (isTRUE(error$errno %in% c(1L, 13L))) {
        return(inaccessible_identity())
      }
      stop(error)
    },
    access_denied = function(error) {
      inaccessible_identity()
    }
  )
}

write_process_identity <- function(pid, path) {
  identity <- process_identity_value(pid)
  if (is.null(identity)) {
    stop("process exited before its identity could be persisted")
  }
  atomic_write_json(identity, path)
  invisible(identity)
}

write_process_identity_writer <- function(run_path) {
  path <- file.path(run_path, "write-process-identity.R")
  script <- r"(
args <- commandArgs(TRUE)
if (identical(args[[1L]], "--kill-tree")) {
  identity <- jsonlite::read_json(args[[2L]], simplifyVector = FALSE)
  handle <- tryCatch(
    ps::ps_handle(as.integer(identity$pid)),
    no_such_process = function(error) NULL,
    zombie_process = function(error) NULL
  )
  if (is.null(handle) || !ps::ps_is_running(handle)) {
    quit(save = "no", status = 75L)
  }
  observed_create_time <- sprintf(
    "%.17g",
    as.numeric(ps::ps_create_time(handle))
  )
  if (!identical(observed_create_time, identity$create_time)) {
    quit(save = "no", status = 75L)
  }
  group <- paste0("-", as.integer(identity$pid))
  system2(
    "/bin/kill",
    c("-TERM", group),
    stdout = FALSE,
    stderr = FALSE
  )
  Sys.sleep(0.1)
  system2(
    "/bin/kill",
    c("-KILL", group),
    stdout = FALSE,
    stderr = FALSE
  )
  quit(save = "no", status = 0L)
}
pid <- as.integer(args[[1L]])
path <- args[[2L]]
handle <- ps::ps_handle(pid)
if (!ps::ps_is_running(handle)) {
  stop("process exited before its identity could be persisted")
}
identity <- list(
  schema_version = 1L,
  pid = pid,
  create_time = sprintf(
    "%.17g",
    as.numeric(ps::ps_create_time(handle))
  ),
  cmdline = unname(ps::ps_cmdline(handle))
)
temporary <- tempfile(".identity-", tmpdir = dirname(path))
jsonlite::write_json(
  identity,
  temporary,
  auto_unbox = TRUE,
  null = "null",
  pretty = TRUE
)
if (!file.rename(temporary, path)) {
  stop("failed to persist process identity")
}
)"
  writeLines(script, path, useBytes = TRUE)
  path
}

write_log_redactor <- function(run_path) {
  path <- file.path(run_path, "redact-log.awk")
  credentials <- paste(
    vapply(
      seq_along(credential_environment_variables),
      function(index) {
        paste0(
          "  credentials[",
          index,
          '] = "',
          credential_environment_variables[[index]],
          '"'
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
  writeLines(
    c(
      "function redact_fixed(value, secret, position) {",
      "  while (length(secret) && (position = index(value, secret))) {",
      "    value = substr(value, 1, position - 1) \"[REDACTED]\" substr(value, position + length(secret))",
      "  }",
      "  return value",
      "}",
      "BEGIN {",
      credentials,
      "}",
      "{",
      "  value = $0",
      "  for (index_ in credentials) {",
      "    value = redact_fixed(value, ENVIRON[credentials[index_]])",
      "  }",
      "  print value",
      "  fflush()",
      "}"
    ),
    path,
    useBytes = TRUE
  )
  path
}

logging_script_lines <- function(run_path, redactor) {
  stdout_pipe <- file.path(run_path, "stdout.pipe")
  stderr_pipe <- file.path(run_path, "stderr.pipe")
  stdout_pid <- file.path(run_path, "stdout-redactor.pid")
  stderr_pid <- file.path(run_path, "stderr-redactor.pid")
  stdout_identity <- file.path(
    run_path,
    "stdout-redactor.identity.json"
  )
  stderr_identity <- file.path(
    run_path,
    "stderr-redactor.identity.json"
  )
  stdout <- file.path(run_path, "stdout.log")
  stderr <- file.path(run_path, "stderr.log")
  c(
    paste("BIONEMOR_STDOUT_PIPE=", shQuote(stdout_pipe), sep = ""),
    paste("BIONEMOR_STDERR_PIPE=", shQuote(stderr_pipe), sep = ""),
    "bionemor_close_logs() {",
    "  local attempt",
    "  local redactors_alive",
    "  exec 1>&- 2>&- 3>&- 4>&-",
    "  for ((attempt = 0; attempt < 30; attempt++)); do",
    "    redactors_alive=0",
    "    if [[ -n \"${BIONEMOR_STDOUT_REDACTOR_PID:-}\" ]] &&",
    "        kill -0 -- \"-$BIONEMOR_STDOUT_REDACTOR_PID\" 2>/dev/null; then",
    "      redactors_alive=1",
    "    fi",
    "    if [[ -n \"${BIONEMOR_STDERR_REDACTOR_PID:-}\" ]] &&",
    "        kill -0 -- \"-$BIONEMOR_STDERR_REDACTOR_PID\" 2>/dev/null; then",
    "      redactors_alive=1",
    "    fi",
    "    if [[ \"$redactors_alive\" -eq 0 ]]; then",
    "      break",
    "    fi",
    "    sleep 0.1",
    "  done",
    paste(
      "  \"$BIONEMOR_RSCRIPT\" \"$BIONEMOR_IDENTITY_WRITER\"",
      "--kill-tree",
      shQuote(stdout_identity),
      "|| [[ \"$?\" -eq 75 ]]"
    ),
    paste(
      "  \"$BIONEMOR_RSCRIPT\" \"$BIONEMOR_IDENTITY_WRITER\"",
      "--kill-tree",
      shQuote(stderr_identity),
      "|| [[ \"$?\" -eq 75 ]]"
    ),
    "  wait \"$BIONEMOR_STDOUT_REDACTOR_PID\" 2>/dev/null || true",
    "  wait \"$BIONEMOR_STDERR_REDACTOR_PID\" 2>/dev/null || true",
    paste(
      "  rm -f",
      "\"$BIONEMOR_STDOUT_PIPE\"",
      "\"$BIONEMOR_STDERR_PIPE\"",
      shQuote(stdout_pid),
      shQuote(stderr_pid)
    ),
    "}",
    "rm -f \"$BIONEMOR_STDOUT_PIPE\" \"$BIONEMOR_STDERR_PIPE\"",
    "mkfifo \"$BIONEMOR_STDOUT_PIPE\" \"$BIONEMOR_STDERR_PIPE\"",
    "set -m",
    paste0(
      "awk -f ",
      shQuote(redactor),
      " < \"$BIONEMOR_STDOUT_PIPE\" >> ",
      shQuote(stdout),
      " &"
    ),
    "BIONEMOR_STDOUT_REDACTOR_PID=$!",
    paste(
      "bionemor_record_identity",
      "\"$BIONEMOR_STDOUT_REDACTOR_PID\"",
      shQuote(stdout_identity)
    ),
    paste(
      "printf '%s\\n' \"$BIONEMOR_STDOUT_REDACTOR_PID\" >",
      shQuote(stdout_pid)
    ),
    paste0(
      "awk -f ",
      shQuote(redactor),
      " < \"$BIONEMOR_STDERR_PIPE\" >> ",
      shQuote(stderr),
      " &"
    ),
    "BIONEMOR_STDERR_REDACTOR_PID=$!",
    paste(
      "bionemor_record_identity",
      "\"$BIONEMOR_STDERR_REDACTOR_PID\"",
      shQuote(stderr_identity)
    ),
    paste(
      "printf '%s\\n' \"$BIONEMOR_STDERR_REDACTOR_PID\" >",
      shQuote(stderr_pid)
    ),
    "exec 3>\"$BIONEMOR_STDOUT_PIPE\" 4>\"$BIONEMOR_STDERR_PIPE\"",
    "exec 1>&3 2>&4",
    "set +m"
  )
}

state_script_lines <- function(
  run_path,
  id,
  kind,
  finalizer,
  identity_writer
) {
  c(
    paste("BIONEMOR_RUN_PATH=", shQuote(run_path), sep = ""),
    paste("BIONEMOR_ID=", shQuote(id), sep = ""),
    paste("BIONEMOR_KIND=", shQuote(kind), sep = ""),
    paste("BIONEMOR_FINALIZER=", shQuote(finalizer), sep = ""),
    paste(
      "BIONEMOR_IDENTITY_WRITER=",
      shQuote(identity_writer),
      sep = ""
    ),
    paste(
      "BIONEMOR_RSCRIPT=",
      shQuote(file.path(R.home("bin"), "Rscript")),
      sep = ""
    ),
    "bionemor_record_identity() {",
    "  \"$BIONEMOR_RSCRIPT\" \"$BIONEMOR_IDENTITY_WRITER\" \"$1\" \"$2\"",
    "}",
    paste(
      "BIONEMOR_STARTED_AT=",
      shQuote(read_json_file(file.path(run_path, "state.json"))$started_at),
      sep = ""
    ),
    "bionemor_write_state() {",
    "  local next_state=\"$1\"",
    "  local exit_status=\"${2:-null}\"",
    "  local backend_id=\"${SLURM_JOB_ID:-${BASHPID:-$$}}\"",
    "  local now",
    "  now=$(date -u '+%Y-%m-%dT%H:%M:%SZ')",
    "  local temporary=\"${BIONEMOR_RUN_PATH}/state.json.tmp.${BASHPID:-$$}\"",
    paste0(
      "  printf ",
      shQuote(paste0(
        '{"schema_version":1,"id":"%s","kind":"%s",',
        '"state":"%s","backend_id":"%s","started_at":"%s",',
        '"updated_at":"%s","exit_status":%s}\\n'
      )),
      " \"$BIONEMOR_ID\" \"$BIONEMOR_KIND\" \"$next_state\"",
      " \"$backend_id\" \"$BIONEMOR_STARTED_AT\" \"$now\"",
      " \"$exit_status\" > \"$temporary\""
    ),
    "  mv \"$temporary\" \"${BIONEMOR_RUN_PATH}/state.json\"",
    paste0(
      "  printf ",
      shQuote(
        '{"time":"%s","state":"%s","exit_status":%s}\\n'
      ),
      " \"$now\" \"$next_state\" \"$exit_status\"",
      " >> \"${BIONEMOR_RUN_PATH}/events.jsonl\""
    ),
    "}",
    "bionemor_finish() {",
    "  local exit_status=\"$?\"",
    "  trap - EXIT",
    "  trap - TERM",
    "  if [[ -n \"${BIONEMOR_TIMEOUT_PID:-}\" ]]; then",
    "    if [[ -f \"${BIONEMOR_RUN_PATH}/timeout.request\" ]]; then",
    "      wait \"$BIONEMOR_TIMEOUT_PID\" || true",
    "    else",
    "      kill -TERM -- \"-$BIONEMOR_TIMEOUT_PID\" 2>/dev/null || true",
    "      wait \"$BIONEMOR_TIMEOUT_PID\" 2>/dev/null || true",
    "    fi",
    "  fi",
    "  : > \"${BIONEMOR_RUN_PATH}/finalizing\"",
    "  if [[ -f \"${BIONEMOR_RUN_PATH}/timeout.request\" ]]; then",
    "    exit_status=124",
    "    bionemor_write_state failed 124",
    "  elif [[ -f \"${BIONEMOR_RUN_PATH}/cancel.request\" ]]; then",
    "    bionemor_write_state cancelled \"$exit_status\"",
    "  elif [[ \"$exit_status\" -eq 0 ]]; then",
    "    bionemor_write_state succeeded 0",
    "  else",
    "    bionemor_write_state failed \"$exit_status\"",
    "  fi",
    "  if ! \"$BIONEMOR_RSCRIPT\" \"$BIONEMOR_FINALIZER\" \"$BIONEMOR_RUN_PATH\"; then",
    "    exit_status=70",
    "    bionemor_write_state failed 70",
    "    if ! \"$BIONEMOR_RSCRIPT\" \"$BIONEMOR_FINALIZER\" \"$BIONEMOR_RUN_PATH\"; then",
    "      if declare -F bionemor_close_logs >/dev/null; then",
    "        bionemor_close_logs",
    "      fi",
    "      rm -f \"${BIONEMOR_RUN_PATH}/finalizing\"",
    "      exit 70",
    "    fi",
    "  fi",
    "  if declare -F bionemor_close_logs >/dev/null; then",
    "    bionemor_close_logs",
    "  fi",
    "  rm -f \"${BIONEMOR_RUN_PATH}/finalizing\"",
    "  exit \"$exit_status\"",
    "}",
    "trap bionemor_finish EXIT",
    "bionemor_write_state running null"
  )
}

timeout_script_lines <- function(timeout, compute, run_id) {
  if (!is.finite(timeout)) {
    return(character())
  }
  kill <- Sys.which("kill")
  if (!nzchar(kill)) {
    stop("operation timeout requires the kill command")
  }
  container_stop <- if (
    compute@backend == "local" &&
      compute@engine == "container"
  ) {
    shell_join(
      compute@config$container_engine %||% "docker",
      c("kill", paste0("bionemor-", run_id))
    )
  } else {
    "true"
  }
  c(
    "bionemor_operation_timeout() {",
    "  local plan_pid=\"$1\"",
    paste("  sleep", shQuote(format(timeout, scientific = FALSE))),
    "  : > \"${BIONEMOR_RUN_PATH}/timeout.request\"",
    paste0("  ", container_stop, " >/dev/null 2>&1 || true"),
    paste0("  ", shQuote(kill), " -TERM -- \"-$plan_pid\" 2>/dev/null || true"),
    "  sleep 1",
    paste0("  if ", shQuote(kill), " -0 -- \"-$plan_pid\" 2>/dev/null; then"),
    paste0(
      "    ",
      shQuote(kill),
      " -KILL -- \"-$plan_pid\" 2>/dev/null || true"
    ),
    "  fi",
    "}"
  )
}

write_plan_script <- function(plan, compute, run_path, kind, timeout = Inf) {
  id <- basename(run_path)
  stdout <- file.path(run_path, "stdout.log")
  stderr <- file.path(run_path, "stderr.log")
  finalizer <- write_manifest_finalizer(run_path)
  identity_writer <- write_process_identity_writer(run_path)
  redactor <- write_log_redactor(run_path)
  steps <- vapply(
    plan$steps,
    function(step) {
      render_shell_command(wrap_backend_command(step, compute, id))
    },
    character(1)
  )
  step_roles <- vapply(
    plan$steps,
    function(step) {
      if (
        identical(plan$metadata$operation, "generation") &&
          identical(basename(step$executable), "bionemor-evo2-helper") &&
          length(step$args) > 0L &&
          identical(step$args[[1L]], "validate-generation")
      ) {
        "generation-validation"
      } else {
        "upstream"
      }
    },
    character(1)
  )
  step_lines <- unlist(
    Map(
      function(step, role) {
        c(
          paste0(
            "  printf '%s\\n' ",
            shQuote(role),
            " > \"${BIONEMOR_RUN_PATH}/active-step\""
          ),
          paste0("  ", step),
          "  rm -f \"${BIONEMOR_RUN_PATH}/active-step\""
        )
      },
      steps,
      step_roles
    ),
    use.names = FALSE
  )
  operation <- c(
    timeout_script_lines(timeout, compute, id),
    paste(
      "BIONEMOR_PLAN_START=",
      shQuote(file.path(run_path, "plan.start")),
      sep = ""
    ),
    "bionemor_run_plan() {",
    "  while [[ ! -f \"$BIONEMOR_PLAN_START\" ]]; do",
    "    sleep 0.01",
    "  done",
    step_lines,
    "}",
    "rm -f \"$BIONEMOR_PLAN_START\"",
    "set -m",
    "bionemor_run_plan &",
    "BIONEMOR_PLAN_PID=$!",
    paste(
      "bionemor_record_identity",
      "\"$BIONEMOR_PLAN_PID\"",
      shQuote(file.path(run_path, "plan.identity.json"))
    ),
    "printf '%s\\n' \"$BIONEMOR_PLAN_PID\" > \"${BIONEMOR_RUN_PATH}/plan.pid\"",
    ": > \"$BIONEMOR_PLAN_START\"",
    if (is.finite(timeout)) {
      c(
        "bionemor_operation_timeout \"$BIONEMOR_PLAN_PID\" &",
        "BIONEMOR_TIMEOUT_PID=$!"
      )
    },
    "set +m",
    "wait \"$BIONEMOR_PLAN_PID\""
  )
  path <- file.path(
    run_path,
    if (compute@backend == "slurm") "slurm.sh" else "run.sh"
  )
  writeLines(
    c(
      "#!/usr/bin/env bash",
      if (compute@backend == "slurm") {
        job_directives(compute, id, stdout, stderr)
      },
      "set -euo pipefail",
      state_script_lines(
        run_path,
        id,
        kind,
        finalizer,
        identity_writer
      ),
      logging_script_lines(run_path, redactor),
      if (
        compute@backend == "slurm" &&
          compute@engine == "container"
      ) {
        slurm_sif_verification_lines(compute)
      },
      operation
    ),
    path,
    useBytes = TRUE
  )
  Sys.chmod(path, "0750")
  path
}

new_job_object <- function(
  run_path,
  compute,
  plan,
  expected_result,
  timeout,
  process = NULL,
  metadata = list()
) {
  state <- read_json_file(file.path(run_path, "state.json"))
  BioNeMoJob(
    path = run_path,
    id = state$id,
    kind = state$kind,
    state = state$state,
    compute = compute,
    command_plan = plan,
    log = file.path(run_path, "stdout.log"),
    expected_result = expected_result,
    timeout = as.double(timeout),
    process = process,
    metadata = metadata
  )
}

submit_plan <- function(
  plan,
  compute,
  run_path,
  kind,
  expected_result = NULL,
  timeout = Inf,
  async = TRUE
) {
  stopifnot(
    "plan must be a command plan" = inherits(plan, "bionemor_command_plan"),
    "compute must be a BioNeMo compute specification" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "run path must be an existing run directory" = is_scalar_string(run_path) &&
      dir.exists(run_path),
    "kind must match the persisted run kind" = identical(
      read_json_file(file.path(run_path, "request.json"))$kind,
      kind
    ),
    "expected result must be a list, S7 object, or NULL" = is.null(
      expected_result
    ) ||
      is.list(expected_result) ||
      inherits(expected_result, "S7_object"),
    "timeout must be positive or infinite" = identical(timeout, Inf) ||
      is_scalar_number(timeout) && timeout > 0,
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  request <- read_json_file(
    file.path(run_path, "request.json"),
    simplify = FALSE
  )
  persisted_result <- expected_result
  if (inherits(expected_result, "S7_object")) {
    expected_path <- file.path(run_path, "expected-result.rds")
    saveRDS(expected_result, expected_path, version = 3L)
    persisted_result <- list(
      type = "rds",
      path = basename(expected_path),
      kind = kind
    )
  }
  request$expected_result <- redact_persisted_value(persisted_result)
  request$timeout <- if (is.finite(timeout)) as.double(timeout) else NULL
  atomic_write_json(request, file.path(run_path, "request.json"))
  atomic_write_json(
    serializable_plan(plan),
    file.path(run_path, "plan.json")
  )
  template_job <- new_job_object(
    run_path,
    compute,
    plan,
    expected_result,
    timeout
  )
  atomic_write_json(
    run_manifest_value(template_job),
    file.path(run_path, "manifest-template.json")
  )
  script <- write_plan_script(plan, compute, run_path, kind, timeout)
  allowed_credentials <- intersect(
    unique(unlist(lapply(plan$steps, function(step) names(step$env)))),
    credential_environment_variables
  )

  if (compute@backend == "local") {
    write_job_state(run_path, "starting")
    process <- processx::process$new(
      "bash",
      script,
      stdout = file.path(run_path, "stdout.log"),
      stderr = file.path(run_path, "stderr.log"),
      cleanup = FALSE,
      env = process_environment(allow = allowed_credentials)
    )
    runner_pid <- as.character(process$get_pid())
    write_process_identity(
      runner_pid,
      file.path(run_path, "runner.identity.json")
    )
    atomic_write_lines(
      runner_pid,
      file.path(run_path, "runner.pid")
    )
    job <- new_job_object(
      run_path,
      compute,
      plan,
      expected_result,
      timeout,
      process = process,
      metadata = list(
        pid = runner_pid,
        script = script,
        container_name = if (compute@engine == "container") {
          paste0("bionemor-", basename(run_path))
        }
      )
    )
  } else {
    if (!command_available("sbatch")) {
      stop("sbatch is not available")
    }
    write_job_state(run_path, "submitted")
    submitted <- command_probe(
      "sbatch",
      c("--parsable", script),
      env = process_environment(allow = allowed_credentials)
    )
    if (submitted$status != 0L) {
      write_job_state(
        run_path,
        "failed",
        exit_status = as.integer(submitted$status),
        failure_reason = "SBATCH_FAILED"
      )
      bionemor_abort(
        "BN_UPSTREAM",
        redact_credentials(trimws(submitted$stderr)),
        run_path = run_path,
        request_id = basename(run_path),
        operation = kind,
        log_paths = file.path(
          run_path,
          c("stdout.log", "stderr.log")
        ),
        upstream_exit_status = submitted$status
      )
    }
    id <- sub(";.*$", "", trimws(submitted$stdout))
    if (!grepl("^[0-9]+$", id)) {
      write_job_state(
        run_path,
        "failed",
        exit_status = 1L,
        failure_reason = "SBATCH_INVALID_OUTPUT"
      )
      bionemor_abort(
        "BN_PROTOCOL",
        "sbatch returned an invalid job ID",
        run_path = run_path,
        request_id = basename(run_path),
        operation = kind,
        log_paths = file.path(
          run_path,
          c("stdout.log", "stderr.log")
        ),
        upstream_exit_status = submitted$status
      )
    }
    persist_slurm_backend_id(run_path, id)
    job <- new_job_object(
      run_path,
      compute,
      plan,
      expected_result,
      timeout,
      metadata = list(backend_id = id, script = script)
    )
  }
  if (async) job else job_wait(job, poll = 0.05)
}

#' Re-open a persisted BioNeMo job
#'
#' @param path A persisted run directory.
#'
#' @return A `BioNeMoJob`.
#' @export
bionemo_job <- function(path) {
  path <- normalize_path(path)
  if (
    !dir.exists(path) ||
      !all(file.exists(file.path(
        path,
        c("request.json", "plan.json", "state.json")
      )))
  ) {
    stop("path must contain a persisted BioNeMo run")
  }
  request <- read_json_file(
    file.path(path, "request.json"),
    simplify = FALSE
  )
  compute <- compute_from_record(request$compute)
  plan <- read_command_plan(file.path(path, "plan.json"))
  expected_result <- request$expected_result
  if (identical(expected_result$type, "rds")) {
    expected_result <- readRDS(file.path(path, expected_result$path))
  }
  new_job_object(
    path,
    compute,
    plan,
    expected_result,
    request$timeout %||% Inf,
    metadata = list(
      backend_id = persisted_slurm_backend_id(
        path,
        read_json_file(file.path(path, "state.json"))$backend_id
      ),
      pid = if (file.exists(file.path(path, "runner.pid"))) {
        trimws(readLines(
          file.path(path, "runner.pid"),
          n = 1L,
          warn = FALSE
        ))
      },
      script = file.path(
        path,
        if (compute@backend == "slurm") "slurm.sh" else "run.sh"
      ),
      container_name = if (compute@engine == "container") {
        paste0("bionemor-", basename(path))
      }
    )
  )
}

#' Return the run directory for a BioNeMo job
#'
#' @param x A BioNeMo job.
#'
#' @return One normalized path.
#' @export
job_path <- function(x) {
  if (!S7_inherits(x, BioNeMoJob)) {
    stop("x must be a BioNeMo job")
  }
  normalize_path(x@path)
}

terminal_job_states <- c("succeeded", "failed", "cancelled")

process_id_is_alive <- function(pid) {
  isTRUE(tools::pskill(as.integer(pid), signal = 0L))
}

process_group_is_alive <- function(pid) {
  if (is.na(pid)) {
    return(FALSE)
  }
  if (process_id_is_alive(pid)) {
    return(TRUE)
  }
  kill <- if (file.exists("/bin/kill")) "/bin/kill" else Sys.which("kill")
  if (!nzchar(kill)) {
    return(FALSE)
  }
  command_probe(kill, c("-0", paste0("-", pid)))$status == 0L
}

wait_for_process_group <- function(pid, timeout) {
  deadline <- Sys.time() + timeout
  while (process_group_is_alive(pid) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  !process_group_is_alive(pid)
}

slurm_exit_status <- function(exit_code, state) {
  fields <- strsplit(exit_code, ":", fixed = TRUE)[[1L]]
  parsed <- suppressWarnings(as.integer(fields))
  if (length(parsed) != 2L || anyNA(parsed)) {
    return(if (state == "succeeded") 0L else 1L)
  }
  if (parsed[[1L]] > 0L) {
    return(parsed[[1L]])
  }
  if (parsed[[2L]] > 0L) {
    return(128L + parsed[[2L]])
  }
  if (state == "succeeded") 0L else 1L
}

slurm_nonterminal_state <- function(...) {
  states <- unlist(list(...), use.names = FALSE)
  states[states == "created"] <- "submitted"
  progress <- c(
    submitted = 1L,
    starting = 2L,
    running = 3L
  )
  known <- states[states %in% names(progress)]
  if (!length(known)) {
    return("unknown")
  }
  known[[which.max(unname(progress[known]))]]
}

slurm_job_status <- function(job) {
  state <- read_json_file(file.path(job@path, "state.json"))
  if (state$state %in% terminal_job_states) {
    return(state$state)
  }
  persisted_state <- slurm_nonterminal_state(state$state, job@state)
  id <- persisted_slurm_backend_id(
    job@path,
    state$backend_id %||% job@metadata$backend_id
  )
  if (is.null(id)) {
    return(persisted_state)
  }
  if (!command_available("sacct")) {
    stop("sacct is not available")
  }
  result <- command_probe(
    "sacct",
    c("-X", "-n", "-P", "-j", id, "--format=JobIDRaw,State,ExitCode")
  )
  if (result$status != 0L) {
    bionemor_abort(
      "BN_UPSTREAM",
      trimws(result$stderr),
      run_path = job@path,
      request_id = job@id,
      operation = job@kind,
      log_paths = file.path(
        job@path,
        c("stdout.log", "stderr.log")
      ),
      upstream_exit_status = result$status
    )
  }
  lines <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1L]]
  fields <- lapply(lines[nzchar(lines)], strsplit, split = "|", fixed = TRUE)
  fields <- lapply(fields, `[[`, 1L)
  matching <- vapply(
    fields,
    function(x) length(x) == 3L && x[[1L]] == id,
    logical(1)
  )
  if (sum(matching) == 0L) {
    latest <- read_json_file(file.path(job@path, "state.json"))
    return(
      if (latest$state %in% terminal_job_states) {
        latest$state
      } else {
        slurm_nonterminal_state(persisted_state, latest$state)
      }
    )
  }
  if (sum(matching) != 1L) {
    latest <- read_json_file(file.path(job@path, "state.json"))
    return(
      if (latest$state %in% terminal_job_states) {
        latest$state
      } else {
        slurm_nonterminal_state(persisted_state, latest$state)
      }
    )
  }
  record <- fields[[which(matching)]]
  original <- trimws(record[[2L]])
  scheduler <- sub("[+ ].*$", "", toupper(original))
  exit_code <- trimws(record[[3L]])
  mapped <- switch(
    scheduler,
    PENDING = "submitted",
    CONFIGURING = "starting",
    RUNNING = "running",
    COMPLETING = "running",
    COMPLETED = if (exit_code == "0:0") "succeeded" else "failed",
    CANCELLED = "cancelled",
    FAILED = "failed",
    TIMEOUT = "failed",
    OUT_OF_MEMORY = "failed",
    NODE_FAIL = "failed",
    PREEMPTED = "failed",
    BOOT_FAIL = "failed",
    DEADLINE = "failed",
    "unknown"
  )
  latest <- read_json_file(file.path(job@path, "state.json"))
  if (latest$state %in% terminal_job_states) {
    return(latest$state)
  }
  if (!mapped %in% terminal_job_states) {
    resolved <- slurm_nonterminal_state(
      persisted_state,
      latest$state,
      mapped
    )
    if (!identical(latest$state, resolved)) {
      updated <- write_job_state(
        job@path,
        resolved,
        backend_id = id
      )
      return(updated$state)
    }
    return(resolved)
  }
  updated <- write_job_state(
    job@path,
    mapped,
    backend_id = id,
    exit_status = slurm_exit_status(exit_code, mapped),
    failure_reason = if (mapped == "failed") {
      if (scheduler == "COMPLETED") "NONZERO_EXIT" else scheduler
    }
  )
  updated$state
}

local_job_status <- function(job) {
  state <- read_json_file(file.path(job@path, "state.json"))
  if (state$state %in% terminal_job_states) {
    return(state$state)
  }
  if (!is.null(job@process)) {
    if (job@process$is_alive()) {
      return(if (state$state == "starting") "running" else state$state)
    }
    status <- job@process$get_exit_status()
    state <- read_json_file(file.path(job@path, "state.json"))
    if (!state$state %in% terminal_job_states) {
      if (is.null(status) || length(status) != 1L || is.na(status)) {
        status <- 137L
      }
      return(finalize_abandoned_local_run(
        job,
        exit_status = as.integer(status),
        terminal_state = "failed"
      ))
    }
    return(state$state)
  }
  runner_pid <- local_runner_pid(job)
  runner_alive <- !is.na(runner_pid) &&
    identical(
      persisted_process_group_status(job@path, "runner", runner_pid),
      "alive"
    )
  if (!runner_alive) {
    return(finalize_abandoned_local_run(
      job,
      exit_status = 137L,
      terminal_state = "failed"
    ))
  }
  state$state
}

#' Return a BioNeMo job state
#'
#' @param x A BioNeMo job.
#' @param refresh Whether to query the execution backend.
#'
#' @return One state string.
#' @export
job_status <- function(x, refresh = TRUE) {
  if (!S7_inherits(x, BioNeMoJob)) {
    stop("x must be a BioNeMo job")
  }
  if (!is_scalar_logical(refresh)) {
    stop("refresh must be TRUE or FALSE")
  }
  state <- if (file.exists(file.path(x@path, "finalizing"))) {
    if (x@compute@backend == "slurm") {
      scheduler_state <- slurm_job_status(x)
      if (scheduler_state %in% terminal_job_states) {
        unlink(file.path(x@path, "finalizing"))
        scheduler_state
      } else {
        "running"
      }
    } else if (local_runner_is_alive(x)) {
      "running"
    } else {
      persisted <- read_json_file(file.path(x@path, "state.json"))
      finalize_abandoned_local_run(
        x,
        exit_status = persisted$exit_status %||% 137L,
        terminal_state = if (persisted$state %in% terminal_job_states) {
          persisted$state
        } else {
          "failed"
        }
      )
    }
  } else if (!refresh) {
    x@state
  } else if (x@compute@backend == "local") {
    local_job_status(x)
  } else {
    slurm_job_status(x)
  }
  x@state <- state
  if (state %in% terminal_job_states) {
    write_run_manifest(x)
  }
  state
}

#' Read BioNeMo job logs
#'
#' @param x A BioNeMo job.
#' @param tail Optional number of final lines.
#' @param stream Log stream to read.
#'
#' @return A character vector.
#' @export
job_logs <- function(
  x,
  tail = NULL,
  stream = c("both", "stdout", "stderr")
) {
  if (!S7_inherits(x, BioNeMoJob)) {
    stop("x must be a BioNeMo job")
  }
  if (!is.null(tail) && !is_scalar_integerish(tail, min = 1)) {
    stop("tail must be NULL or a positive integer")
  }
  stream <- match.arg(stream)
  paths <- switch(
    stream,
    stdout = file.path(x@path, "stdout.log"),
    stderr = file.path(x@path, "stderr.log"),
    both = file.path(x@path, c("stdout.log", "stderr.log"))
  )
  lines <- unlist(lapply(paths[file.exists(paths)], readLines, warn = FALSE))
  lines <- redact_credentials(lines)
  if (is.null(tail)) lines else utils::tail(lines, as.integer(tail))
}

persisted_process_id <- function(run_path, filename) {
  path <- file.path(run_path, filename)
  if (!file.exists(path)) {
    return(NA_integer_)
  }
  value <- trimws(readLines(path, n = 1L, warn = FALSE))
  if (length(value) != 1L || !grepl("^[1-9][0-9]*$", value)) {
    stop("persisted process ID is invalid")
  }
  as.integer(value)
}

persisted_process_identity <- function(run_path, stem, pid) {
  path <- file.path(run_path, paste0(stem, ".identity.json"))
  if (!file.exists(path)) {
    stop("persisted process identity is missing")
  }
  identity <- read_json_file(path, simplify = FALSE)
  cmdline <- unlist(identity$cmdline, use.names = FALSE)
  if (
    !identical(identity$schema_version, 1L) ||
      !identical(as.integer(identity$pid), as.integer(pid)) ||
      !is_scalar_string(identity$create_time) ||
      !is.character(cmdline) ||
      anyNA(cmdline)
  ) {
    stop("persisted process identity is invalid")
  }
  identity$pid <- as.integer(identity$pid)
  identity$cmdline <- unname(cmdline)
  identity
}

process_identity_matches <- function(expected, observed) {
  identical(expected$pid, observed$pid) &&
    identical(expected$create_time, observed$create_time)
}

persisted_process_group_status <- function(run_path, stem, pid) {
  if (is.na(pid)) {
    return("dead")
  }
  expected <- persisted_process_identity(run_path, stem, pid)
  observed <- process_identity_value(pid)
  if (!is.null(observed)) {
    if (process_identity_matches(expected, observed)) {
      return("alive")
    }
    return("mismatched")
  }
  if (process_group_is_alive(pid)) "unverifiable" else "dead"
}

local_runner_pid <- function(x) {
  persisted <- persisted_process_id(x@path, "runner.pid")
  if (!is.na(persisted)) {
    return(persisted)
  }
  state <- read_json_file(file.path(x@path, "state.json"))
  candidates <- c(
    if (!is.null(x@process)) x@process$get_pid(),
    x@metadata$pid,
    state$backend_id,
    x@metadata$backend_id
  )
  candidates <- as.character(candidates[!is.na(candidates)])
  if (!length(candidates)) {
    return(NA_integer_)
  }
  if (!grepl("^[1-9][0-9]*$", candidates[[1L]])) {
    stop("local runner process ID is invalid")
  }
  as.integer(candidates[[1L]])
}

local_plan_pid <- function(x) {
  persisted_process_id(x@path, "plan.pid")
}

local_process_target <- function(x) {
  plan_pid <- local_plan_pid(x)
  plan_status <- persisted_process_group_status(
    x@path,
    "plan",
    plan_pid
  )
  if (plan_status == "alive") {
    return(list(scope = "plan", pid = plan_pid))
  }
  runner_pid <- local_runner_pid(x)
  runner_status <- persisted_process_group_status(
    x@path,
    "runner",
    runner_pid
  )
  if (runner_status == "alive") {
    return(list(scope = "runner", pid = runner_pid))
  }
  list(scope = "none", pid = NA_integer_)
}

local_runner_is_alive <- function(x) {
  pid <- local_runner_pid(x)
  if (is.na(pid)) {
    return(FALSE)
  }
  persisted_process_group_status(x@path, "runner", pid) == "alive"
}

stop_local_job <- function(x, force = FALSE) {
  container_stopped <- FALSE
  container_error <- character()
  if (x@compute@engine == "container") {
    runtime <- x@compute@config$container_engine %||% "docker"
    action <- if (force) "kill" else "stop"
    stopped <- command_probe(
      runtime,
      c(action, x@metadata$container_name)
    )
    container_stopped <- stopped$status == 0L
    if (!container_stopped) {
      container_error <- redact_credentials(
        trimws(c(stopped$stderr, stopped$stdout))
      )
    }
  }
  target <- local_process_target(x)
  pid <- target$pid
  process_stopped <- target$scope == "none"
  process_error <- character()
  if (!process_stopped) {
    if (!nzchar(Sys.which("kill"))) {
      process_error <- "local job cancellation requires the kill command"
    } else {
      stopped <- command_probe(
        "kill",
        c(if (force) "-KILL" else "-TERM", paste0("-", pid))
      )
      if (stopped$status != 0L && process_group_is_alive(pid)) {
        process_error <- trimws(stopped$stderr)
      } else {
        process_stopped <- wait_for_process_group(pid, if (force) 2 else 1)
        if (!process_stopped && !force) {
          current_status <- persisted_process_group_status(
            x@path,
            target$scope,
            pid
          )
          if (current_status == "mismatched") {
            process_error <- paste(
              "persisted",
              target$scope,
              "identity no longer matches the live process"
            )
          } else if (current_status == "dead") {
            process_stopped <- TRUE
          } else {
            killed <- command_probe("kill", c("-KILL", paste0("-", pid)))
            if (killed$status != 0L && process_group_is_alive(pid)) {
              process_error <- trimws(killed$stderr)
            } else {
              process_stopped <- wait_for_process_group(pid, 2)
            }
          }
        }
        if (!process_stopped && !length(process_error)) {
          process_error <- "process group remained alive after cancellation"
        }
      }
    }
  }
  if (x@compute@engine == "container") {
    if (
      !container_stopped &&
        !process_stopped &&
        job_status(x) == "running"
    ) {
      errors <- c(container_error, process_error)
      bionemor_abort(
        "BN_UPSTREAM",
        paste0(
          "failed to stop local container or process group: ",
          paste(errors[nzchar(errors)], collapse = "; ")
        ),
        run_path = x@path,
        request_id = x@id,
        operation = x@kind,
        log_paths = file.path(
          x@path,
          c("stdout.log", "stderr.log")
        )
      )
    }
  } else if (!process_stopped || length(process_error)) {
    bionemor_abort(
      "BN_UPSTREAM",
      paste0(
        "failed to stop local job process group: ",
        paste(process_error, collapse = "; ")
      ),
      run_path = x@path,
      request_id = x@id,
      operation = x@kind,
      log_paths = file.path(
        x@path,
        c("stdout.log", "stderr.log")
      )
    )
  }
  invisible(list(
    scope = target$scope,
    pid = pid,
    stopped = process_stopped
  ))
}

stop_abandoned_local_children <- function(x) {
  stems <- c("plan", "stdout-redactor", "stderr-redactor")
  pid_files <- paste0(stems, ".pid")
  identity_writer <- file.path(x@path, "write-process-identity.R")
  for (index in seq_along(pid_files)) {
    pid_file <- pid_files[[index]]
    pid <- persisted_process_id(x@path, pid_file)
    status <- if (is.na(pid)) {
      "dead"
    } else {
      persisted_process_group_status(
        x@path,
        stems[[index]],
        pid
      )
    }
    if (status == "alive") {
      stopped <- command_probe(
        file.path(R.home("bin"), "Rscript"),
        c(
          identity_writer,
          "--kill-tree",
          file.path(
            x@path,
            paste0(stems[[index]], ".identity.json")
          )
        )
      )
      if (stopped$status == 75L) {
        next
      }
      if (stopped$status != 0L) {
        bionemor_abort(
          "BN_PROTOCOL",
          paste0(
            "persisted ",
            stems[[index]],
            " process identity does not match the live process",
            if (nzchar(trimws(stopped$stderr))) {
              paste0(": ", trimws(stopped$stderr))
            } else {
              ""
            }
          ),
          run_path = x@path,
          request_id = x@id,
          operation = x@kind,
          log_paths = file.path(
            x@path,
            c("stdout.log", "stderr.log")
          ),
          upstream_exit_status = stopped$status
        )
      }
      if (!wait_for_process_group(pid, 2)) {
        stop("failed to stop an abandoned local child process group")
      }
    }
  }
  unlink(file.path(
    x@path,
    c(pid_files, paste0(stems, ".identity.json"))
  ))
  invisible(x)
}

finalize_abandoned_local_run <- function(
  x,
  exit_status,
  terminal_state = "cancelled"
) {
  finalizing <- file.path(x@path, "finalizing")
  if (!file.exists(finalizing)) {
    file.create(finalizing)
  }
  stop_abandoned_local_children(x)
  state <- read_json_file(file.path(x@path, "state.json"))$state
  if (!state %in% terminal_job_states) {
    write_job_state(
      x@path,
      terminal_state,
      exit_status = exit_status
    )
    state <- terminal_state
  }
  if (!file.exists(file.path(x@path, "manifest.json"))) {
    write_run_manifest(x)
  }
  unlink(file.path(x@path, c("stdout.pipe", "stderr.pipe")))
  if (unlink(finalizing) != 0L) {
    stop("failed to complete abandoned local run finalization")
  }
  state
}

wait_for_cancelled_state <- function(x, exit_status, timeout = 10) {
  deadline <- Sys.time() + timeout
  repeat {
    state <- read_json_file(file.path(x@path, "state.json"))$state
    finalizing <- file.exists(file.path(x@path, "finalizing"))
    if (state %in% terminal_job_states && !finalizing) {
      return(state)
    }
    if (!local_runner_is_alive(x)) {
      return(finalize_abandoned_local_run(x, exit_status))
    }
    if (Sys.time() >= deadline) {
      bionemor_abort(
        "BN_TIMEOUT",
        "timed out waiting for cancellation to finalize",
        run_path = x@path,
        request_id = x@id,
        operation = x@kind,
        log_paths = file.path(
          x@path,
          c("stdout.log", "stderr.log")
        )
      )
    }
    Sys.sleep(0.05)
  }
}

wait_for_slurm_cancellation <- function(x, timeout = 10) {
  deadline <- Sys.time() + timeout
  repeat {
    state <- slurm_job_status(x)
    if (state %in% terminal_job_states) {
      return(state)
    }
    if (Sys.time() >= deadline) {
      bionemor_abort(
        "BN_TIMEOUT",
        "Slurm did not confirm job cancellation",
        run_path = x@path,
        request_id = x@id,
        operation = x@kind,
        log_paths = file.path(
          x@path,
          c("stdout.log", "stderr.log")
        )
      )
    }
    Sys.sleep(0.1)
  }
}

#' Cancel a BioNeMo job
#'
#' @param x A BioNeMo job.
#' @param force Whether to use immediate termination.
#'
#' @return The updated job, invisibly.
#' @export
job_cancel <- function(x, force = FALSE) {
  if (!S7_inherits(x, BioNeMoJob)) {
    stop("x must be a BioNeMo job")
  }
  if (!is_scalar_logical(force)) {
    stop("force must be TRUE or FALSE")
  }
  persisted <- read_json_file(file.path(x@path, "state.json"))
  if (!persisted$state %in% terminal_job_states) {
    job_status(x)
    persisted <- read_json_file(file.path(x@path, "state.json"))
  }
  if (persisted$state %in% terminal_job_states) {
    state <- persisted$state
    if (
      force &&
        x@compute@backend == "local" &&
        file.exists(file.path(x@path, "finalizing"))
    ) {
      stop_local_job(x, force = TRUE)
      state <- wait_for_cancelled_state(
        x,
        exit_status = persisted$exit_status %||% 1L
      )
    }
    x@state <- state
    return(invisible(x))
  }
  cancel_request <- file.path(x@path, "cancel.request")
  file.create(cancel_request)
  keep_cancel_request <- FALSE
  on.exit(
    {
      if (!keep_cancel_request) {
        unlink(cancel_request)
      }
    },
    add = TRUE
  )
  if (x@compute@backend == "local") {
    stop_local_job(x, force = force)
    state <- wait_for_cancelled_state(
      x,
      exit_status = if (force) 137L else 143L
    )
  } else {
    if (!command_available("scancel")) {
      stop("scancel is not available")
    }
    id <- persisted_slurm_backend_id(
      x@path,
      read_json_file(file.path(x@path, "state.json"))$backend_id
    )
    if (is.null(id)) {
      stop("persisted Slurm job ID is missing")
    }
    cancelled <- command_probe(
      "scancel",
      c(if (force) "--signal=KILL", id)
    )
    if (cancelled$status != 0L) {
      bionemor_abort(
        "BN_UPSTREAM",
        trimws(cancelled$stderr),
        run_path = x@path,
        request_id = x@id,
        operation = x@kind,
        log_paths = file.path(
          x@path,
          c("stdout.log", "stderr.log")
        ),
        upstream_exit_status = cancelled$status
      )
    }
    keep_cancel_request <- TRUE
    state <- wait_for_slurm_cancellation(x)
  }
  if (!state %in% terminal_job_states) {
    stop("backend did not confirm a terminal state")
  }
  if (state != "cancelled") {
    unlink(cancel_request)
  } else {
    keep_cancel_request <- TRUE
  }
  x@state <- state
  if (!file.exists(file.path(x@path, "manifest.json"))) {
    write_run_manifest(x)
  }
  invisible(x)
}

materializer_for <- function(type) {
  switch(
    type,
    generation = "materialize_generation_job",
    score = "materialize_score_job",
    profile = "materialize_profile_job",
    `embedding-pooled` = "materialize_embedding_job",
    `embedding-unpooled` = "materialize_embedding_job",
    checkpoint = "materialize_checkpoint_job",
    export = "materialize_checkpoint_job",
    prepare = "materialize_prepare_job",
    `fine-tune` = "materialize_finetune_job",
    fit = "materialize_finetune_job",
    NULL
  )
}

materialize_s7_job_result <- function(job, result) {
  if (S7_inherits(result, Evo2Dataset)) {
    if (!is_scalar_string(result@path) || !dir.exists(result@path)) {
      stop("prepared dataset output does not exist")
    }
    return(result)
  }
  if (S7_inherits(result, Evo2Model)) {
    checkpoint <- result@checkpoint
    if (!S7_inherits(checkpoint, BioNeMoCheckpoint)) {
      stop("fine-tune result is missing its checkpoint descriptor")
    }
    root <- checkpoint@path
    latest_file <- file.path(root, "latest_checkpointed_iteration.txt")
    path <- if (file.exists(latest_file)) {
      iteration <- trimws(readLines(latest_file, n = 1L, warn = FALSE))
      if (!grepl("^[0-9]+$", iteration)) {
        stop("latest checkpoint iteration is invalid")
      }
      file.path(root, sprintf("iter_%07d", as.integer(iteration)))
    } else {
      candidates <- list.dirs(root, recursive = FALSE, full.names = TRUE)
      candidates <- candidates[
        grepl("^iter_[0-9]+$", basename(candidates))
      ]
      if (length(candidates) == 0L) {
        stop("fine-tune did not write a checkpoint iteration")
      }
      sort(candidates)[[length(candidates)]]
    }
    if (!dir.exists(path)) {
      stop("fine-tune checkpoint iteration does not exist")
    }
    checkpoint@path <- normalizePath(path, mustWork = TRUE)
    result@checkpoint <- checkpoint
    return(result)
  }
  if (S7_inherits(result, BioNeMoCheckpoint)) {
    if (!file.exists(result@path)) {
      stop("checkpoint result does not exist")
    }
    return(result)
  }
  bionemor_abort(
    "BN_PROTOCOL",
    "job has an unsupported S7 result contract",
    run_path = job@path,
    request_id = job@id,
    operation = job@kind,
    log_paths = file.path(
      job@path,
      c("stdout.log", "stderr.log")
    )
  )
}

run_manifest_files <- function(run_path, directory) {
  root <- file.path(run_path, directory)
  if (!dir.exists(root)) {
    return(list())
  }
  paths <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  paths <- sort(paths[!dir.exists(paths)])
  lapply(paths, function(path) {
    list(
      path = substring(path, nchar(run_path) + 2L),
      bytes = unname(file.info(path)$size),
      digest = list(
        algorithm = "md5",
        value = unname(tools::md5sum(path))
      )
    )
  })
}

merge_run_manifest_files <- function(current, previous) {
  if (!length(previous)) {
    return(current)
  }
  by_path <- c(
    stats::setNames(previous, pluck_chr(previous, "path")),
    stats::setNames(current, pluck_chr(current, "path"))
  )
  unname(by_path[!duplicated(names(by_path), fromLast = TRUE)])
}

run_manifest_warnings <- function(job, result = NULL) {
  values <- list()
  validation <- file.path(job@path, "outputs", "validation.json")
  if (file.exists(validation)) {
    values <- c(
      values,
      list(read_json_file(validation, simplify = FALSE)$warnings)
    )
  }
  if (is.data.frame(result) && "validation_warnings" %in% names(result)) {
    values <- c(values, unclass(result$validation_warnings))
  }
  provenance <- if (is.null(result)) {
    NULL
  } else if (inherits(result, "S7_object")) {
    tryCatch(result@provenance, error = function(error) NULL)
  } else {
    attr(result, "provenance", exact = TRUE)
  }
  if (is.list(provenance) && !is.null(provenance$warnings)) {
    values <- c(values, list(provenance$warnings))
  }
  descriptor <- job@expected_result
  if (is.list(descriptor) && !is.null(descriptor$warnings)) {
    values <- c(values, list(descriptor$warnings))
  }
  manifest_path <- file.path(job@path, "manifest.json")
  if (is.null(result) && file.exists(manifest_path)) {
    previous <- read_json_file(manifest_path, simplify = FALSE)
    values <- c(values, list(previous$warnings))
  }
  warnings <- unlist(values, use.names = FALSE)
  warnings <- as.character(warnings[!is.na(warnings) & nzchar(warnings)])
  unique(warnings)
}

run_manifest_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("bionemor")),
    error = function(error) "0.0.0.9000"
  )
}

run_manifest_timing <- function(state) {
  started <- as.POSIXct(
    state$started_at,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  ended <- as.POSIXct(
    state$updated_at,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  duration <- as.numeric(difftime(ended, started, units = "secs"))
  list(
    started_at = state$started_at,
    ended_at = state$updated_at,
    duration_seconds = if (is.finite(duration)) max(duration, 0) else NULL
  )
}

run_manifest_checkpoint_context <- function(job) {
  descriptor <- job@expected_result
  if (!is.list(descriptor)) {
    return(list(
      checkpoint = list(),
      model = list(),
      tokenizer = list()
    ))
  }
  candidates <- c(
    descriptor$checkpoint %||% character(),
    descriptor$path %||% character(),
    descriptor$checkpoint_root %||% character()
  )
  candidates <- candidates[
    vapply(candidates, is_scalar_string, logical(1))
  ]
  path <- if (length(candidates)) candidates[[1L]] else NULL
  path_exists <- !is.null(path) && file.exists(path)
  if (path_exists) {
    path <- normalizePath(path, mustWork = TRUE)
  } else if (!is.null(path)) {
    path <- normalize_path(path)
  }
  manifest_path <- if (path_exists) checkpoint_manifest_path(path) else NULL
  metadata <- if (!is.null(manifest_path) && file.exists(manifest_path)) {
    read_checkpoint_manifest(path, manifest_path)
  } else {
    list()
  }
  expected <- descriptor$expected %||% list()
  variant <- metadata$variant %||%
    descriptor$variant %||%
    expected$variant %||%
    NULL
  record <- if (is_scalar_string(variant)) {
    tryCatch(evo2_model_record(variant), error = function(error) list())
  } else {
    list()
  }
  checkpoint_digest <- metadata$checkpoint_digest %||%
    if (path_exists) path_digest(path) else NULL
  base_path <- metadata$base_checkpoint_path %||%
    descriptor$base_checkpoint %||%
    NULL
  base_digest <- metadata$base_checkpoint_digest %||%
    if (!is.null(base_path) && file.exists(base_path)) {
      path_digest(base_path)
    } else {
      NULL
    }
  tokenizer_revision <- metadata$tokenizer_revision %||%
    record$tokenizer_revision %||%
    NULL
  list(
    checkpoint = if (is.null(path)) {
      list()
    } else {
      list(
        path = path,
        source = metadata$source %||% expected$source %||% NULL,
        source_trust = metadata$source_trust %||%
          expected$source_trust %||%
          NULL,
        source_verified = metadata$source_verified %||%
          expected$source_verified %||%
          NULL,
        format = metadata$format %||%
          descriptor$format %||%
          expected$format %||%
          "mbridge",
        kind = metadata$kind %||% descriptor$checkpoint_kind %||% NULL,
        revision = metadata$source_revision %||%
          expected$source_revision %||%
          NULL,
        digest = if (is.null(checkpoint_digest)) {
          NULL
        } else {
          list(algorithm = "md5", value = checkpoint_digest)
        },
        base_checkpoint = list(
          path = base_path,
          source = metadata$base_checkpoint_source %||%
            descriptor$base_checkpoint_source %||%
            NULL,
          source_trust = metadata$base_checkpoint_source_trust %||%
            descriptor$base_checkpoint_source_trust %||%
            NULL,
          source_verified = metadata$base_checkpoint_source_verified %||%
            descriptor$base_checkpoint_source_verified %||%
            NULL,
          digest = base_digest
        )
      )
    },
    model = list(
      name = variant,
      model_size = metadata$model_size %||%
        descriptor$model_size %||%
        expected$model_size %||%
        NULL,
      revision = metadata$source_revision %||%
        record$source_revision %||%
        NULL
    ),
    tokenizer = list(
      identity = metadata$tokenizer %||% record$tokenizer %||% NULL,
      revision = tokenizer_revision,
      digest = if (is.null(tokenizer_revision)) {
        NULL
      } else {
        list(algorithm = "git-revision", value = tokenizer_revision)
      }
    )
  )
}

run_manifest_resolved_origins <- function(plan, request, request_origins) {
  resolved <- plan$metadata$resolved_control %||% list()
  control <- request$control %||% list()
  if (!is.list(control)) {
    control <- list()
  }
  control_origins <- request_origins$control %||% list()
  if (!is.list(control_origins)) {
    control_origins <- list()
  }
  origins <- stats::setNames(
    as.list(rep("adapter_default", length(resolved))),
    names(resolved)
  )
  inherited <- intersect(names(origins), names(control_origins))
  origins[inherited] <- control_origins[inherited]
  origins$operation <- "adapter_default"
  automatic <- intersect(
    c(
      "world_size",
      "processes_per_node",
      "data_parallel_size",
      "gradient_accumulation"
    ),
    names(origins)
  )
  origins[automatic] <- as.list(rep("auto_resolved", length(automatic)))
  if (
    "mixed_precision_recipe" %in%
      names(origins) &&
      is.null(control$mixed_precision_recipe)
  ) {
    origins$mixed_precision_recipe <- "auto_resolved"
  }
  if (
    "vortex_style_fp8" %in%
      names(origins) &&
      identical(control$vortex_style_fp8, "auto")
  ) {
    origins$vortex_style_fp8 <- "auto_resolved"
  }
  if (
    "cuda_graphs" %in% names(origins) && identical(control$cuda_graphs, "auto")
  ) {
    origins$cuda_graphs <- "auto_resolved"
  }
  origins
}

run_manifest_precision <- function(
  plan,
  request,
  checkpoint,
  request_origins,
  resolved_origins
) {
  resolved <- plan$metadata$resolved_control %||% list()
  request_precision <- request$precision_request %||%
    request$precision %||%
    NULL
  request_semantic_precision <- if (is.list(request_precision)) {
    request_precision$semantic %||% NULL
  } else {
    request_precision
  }
  request_control <- request$control %||% list()
  if (!is.list(request_control)) {
    request_control <- list()
  }
  semantic <- resolved$semantic_precision %||%
    resolved$precision %||%
    request_control$precision %||%
    request_semantic_precision %||%
    NULL
  resolved_recipe <- resolved$mixed_precision_recipe %||%
    checkpoint$mixed_precision_recipe %||%
    NULL
  control_origins <- request_origins$control %||% list()
  if (!is.list(control_origins)) {
    control_origins <- list()
  }
  semantic_origin <- control_origins$precision %||%
    request_origins$precision_request %||%
    request_origins$precision %||%
    if (is.null(semantic)) NULL else "adapter_default"
  checkpoint_resolved_origin <- if (!is.null(request$precision_request)) {
    request_origins$precision %||% NULL
  } else {
    NULL
  }
  resolved_origin <- resolved_origins$mixed_precision_recipe %||%
    checkpoint_resolved_origin %||%
    if (is.null(resolved_recipe)) NULL else "adapter_default"
  list(
    semantic = semantic,
    resolved_recipe = resolved_recipe,
    semantic_origin = semantic_origin,
    resolved_origin = resolved_origin,
    origin = resolved_origin
  )
}

run_manifest_value <- function(job, result = NULL) {
  if (!S7_inherits(job, BioNeMoJob)) {
    stop("job must be a BioNeMo job")
  }
  state <- read_json_file(file.path(job@path, "state.json"))
  persisted_request <- read_json_file(
    file.path(job@path, "request.json"),
    simplify = FALSE
  )
  persisted_plan <- read_json_file(
    file.path(job@path, "plan.json"),
    simplify = FALSE
  )
  recipe <- job@compute@recipe
  capabilities <- job@compute@config$capabilities %||% list()
  semantic_request <- persisted_request$request %||% list()
  semantic_request$operation <- semantic_request$operation %||% job@kind
  request_origins <- persisted_request$request_origins %||% list()
  request_origins$operation <- request_origins$operation %||%
    "adapter_default"
  lock <- evo2_recipe_lock()
  manifest_path <- file.path(job@path, "manifest.json")
  previous_manifest <- if (file.exists(manifest_path)) {
    read_json_file(manifest_path, simplify = FALSE)
  } else {
    list()
  }
  context <- run_manifest_checkpoint_context(job)
  checkpoint_metadata <- if (length(context$checkpoint)) {
    checkpoint_manifest <- checkpoint_manifest_path(context$checkpoint$path)
    if (file.exists(checkpoint_manifest)) {
      read_checkpoint_manifest(context$checkpoint$path, checkpoint_manifest)
    } else {
      list()
    }
  } else {
    list()
  }
  resolved_origins <- run_manifest_resolved_origins(
    persisted_plan,
    semantic_request,
    request_origins
  )
  manifest <- list(
    schema_version = 1L,
    id = job@id,
    kind = job@kind,
    package = list(
      name = "bionemor",
      version = run_manifest_package_version()
    ),
    request = semantic_request,
    value_origins = list(
      request = request_origins,
      resolved = resolved_origins
    ),
    plan = persisted_plan,
    recipe = list(
      repository = recipe@repository,
      revision = recipe@revision,
      version = recipe@recipe_version,
      subdirectory = recipe@subdirectory,
      base_image = recipe@base_image,
      base_image_digest = recipe@base_image_digest,
      bridge_protocol = recipe@bridge_protocol,
      verified = recipe@verified
    ),
    dockerfile = list(
      path = file.path(recipe@subdirectory, "Dockerfile"),
      git_blob = if (recipe@verified) lock$dockerfile_blob else NULL
    ),
    helper = list(
      version = capabilities$helper_version %||% NULL,
      sha256 = capabilities$helper_sha256 %||% NULL
    ),
    image = list(
      reference = job@compute@image,
      digest = job@compute@image_digest
    ),
    model = context$model,
    checkpoint = context$checkpoint,
    tokenizer = context$tokenizer,
    precision = run_manifest_precision(
      persisted_plan,
      semantic_request,
      checkpoint_metadata,
      request_origins,
      resolved_origins
    ),
    runtime = list(
      backend = job@compute@backend,
      engine = job@compute@engine,
      helper_version = capabilities$helper_version %||% NULL,
      recipe_version = capabilities$recipe_version %||% NULL,
      protocol_version = capabilities$protocol_version %||% NULL,
      details = capabilities$runtime %||% list(),
      advertised = list(
        commands = capabilities$commands %||% list(),
        features = capabilities$features %||% list()
      )
    ),
    compute = list(
      workspace = job@compute@workspace,
      gpus = job@compute@gpus,
      nodes = job@compute@nodes,
      queue = job@compute@queue,
      account = job@compute@account,
      walltime = job@compute@walltime,
      config = redact_persisted_value(job@compute@config)
    ),
    state = state$state,
    backend_id = state$backend_id,
    exit_status = state$exit_status,
    failure_reason = state$failure_reason,
    timing = run_manifest_timing(state),
    inputs = run_manifest_files(job@path, "inputs"),
    upstream = merge_run_manifest_files(
      run_manifest_files(job@path, "upstream"),
      previous_manifest$upstream %||% list()
    ),
    outputs = run_manifest_files(job@path, "outputs"),
    warnings = run_manifest_warnings(job, result)
  )
  redact_persisted_value(manifest)
}

write_run_manifest <- function(job, result = NULL) {
  state <- read_json_file(file.path(job@path, "state.json"))
  if (!state$state %in% terminal_job_states) {
    stop("run manifest requires a terminal job state")
  }
  manifest <- run_manifest_value(job, result)
  if (state$state == "succeeded" && is.list(job@expected_result)) {
    cleanup_prediction_tensors(job, job@expected_result)
  }
  atomic_write_json(
    manifest,
    file.path(job@path, "manifest.json")
  )
  invisible(manifest)
}

cleanup_prediction_tensors <- function(job, descriptor) {
  type <- descriptor$type %||% job@kind
  if (
    !type %in%
      c(
        "score",
        "profile",
        "embedding-pooled",
        "embedding-unpooled"
      )
  ) {
    return(invisible(NULL))
  }
  upstream <- normalizePath(descriptor$upstream, mustWork = TRUE)
  run_upstream <- normalizePath(
    file.path(job@path, "upstream"),
    mustWork = TRUE
  )
  if (
    !identical(upstream, run_upstream) &&
      !startsWith(upstream, paste0(run_upstream, .Platform$file.sep))
  ) {
    stop("prediction tensors must be inside the run upstream directory")
  }
  tensors <- list.files(
    upstream,
    pattern = "[.]pt$",
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(tensors)) {
    if (unlink(tensors) != 0L) {
      stop("failed to remove prediction tensors")
    }
  }
  invisible(NULL)
}

materialize_job_result <- function(x) {
  descriptor <- x@expected_result
  if (inherits(descriptor, "S7_object")) {
    result <- materialize_s7_job_result(x, descriptor)
    write_run_manifest(x, result)
    return(result)
  }
  if (!is.list(descriptor)) {
    stop("job does not contain a persisted result descriptor")
  }
  type <- descriptor$type %||% x@kind
  name <- materializer_for(type)
  if (is.null(name)) {
    bionemor_abort(
      "BN_PROTOCOL",
      paste0("job has an unsupported result contract: ", type),
      run_path = x@path,
      request_id = x@id,
      operation = x@kind,
      log_paths = file.path(
        x@path,
        c("stdout.log", "stderr.log")
      )
    )
  }
  materializer <- get0(name, mode = "function", inherits = TRUE)
  if (is.null(materializer)) {
    bionemor_abort(
      "BN_PROTOCOL",
      paste0("result materializer is unavailable: ", name),
      run_path = x@path,
      request_id = x@id,
      operation = x@kind,
      log_paths = file.path(
        x@path,
        c("stdout.log", "stderr.log")
      )
    )
  }
  result <- materializer(x, descriptor)
  write_run_manifest(x, result)
  cleanup_prediction_tensors(x, descriptor)
  result
}

persist_job_failure_reason <- function(x, reason) {
  if (!is_scalar_string(reason)) {
    stop("failure reason must be one non-empty string")
  }
  path <- file.path(x@path, "state.json")
  state <- read_json_file(path, simplify = FALSE)
  if (!identical(state$failure_reason, reason)) {
    state$failure_reason <- reason
    atomic_write_json(state, path)
  }
  if (file.exists(file.path(x@path, "manifest.json"))) {
    write_run_manifest(x)
  }
  reason
}

slurm_failure_reason <- function(x) {
  if (
    x@compute@backend != "slurm" ||
      !command_available("sacct")
  ) {
    return(NULL)
  }
  state <- read_json_file(file.path(x@path, "state.json"))
  id <- persisted_slurm_backend_id(
    x@path,
    state$backend_id %||% x@metadata$backend_id
  )
  if (is.null(id)) {
    return(NULL)
  }
  result <- command_probe(
    "sacct",
    c("-X", "-n", "-P", "-j", id, "--format=JobIDRaw,State,ExitCode")
  )
  if (result$status != 0L) {
    return(NULL)
  }
  lines <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1L]]
  fields <- lapply(lines[nzchar(lines)], strsplit, split = "|", fixed = TRUE)
  fields <- lapply(fields, `[[`, 1L)
  matching <- vapply(
    fields,
    function(record) length(record) == 3L && record[[1L]] == id,
    logical(1)
  )
  if (sum(matching) != 1L) {
    return(NULL)
  }
  scheduler <- sub(
    "[+ ].*$",
    "",
    toupper(trimws(fields[[which(matching)]][[2L]]))
  )
  if (
    scheduler %in%
      c(
        "FAILED",
        "TIMEOUT",
        "OUT_OF_MEMORY",
        "NODE_FAIL",
        "PREEMPTED",
        "BOOT_FAIL",
        "DEADLINE"
      )
  ) {
    scheduler
  } else {
    NULL
  }
}

job_log_failure_reason <- function(x) {
  detail <- paste(job_logs(x, tail = 100L), collapse = "\n")
  cuda <- grepl("CUDA", detail, ignore.case = TRUE)
  exhausted <- grepl(
    "out of memory|OutOfMemoryError|oom_kill event",
    detail,
    ignore.case = TRUE
  )
  if (cuda && exhausted) "CUDA_OUT_OF_MEMORY" else NULL
}

job_sequence_summary <- function(x, resolved) {
  paths <- file.path(
    x@path,
    "inputs",
    c("prompts.fasta", "sequences.fasta")
  )
  paths <- paths[file.exists(paths)]
  lengths <- if (length(paths)) {
    as.integer(nchar(read_fasta(paths[[1L]]), type = "chars"))
  } else {
    length <- resolved$sequence_length %||% NULL
    if (is.null(length)) integer() else as.integer(length)
  }
  if (!length(lengths)) {
    return(NULL)
  }
  list(
    count = as.integer(length(lengths)),
    min_length = as.integer(min(lengths)),
    max_length = as.integer(max(lengths)),
    total_length = as.integer(sum(lengths))
  )
}

advertised_gpu_memory <- function(compute) {
  gpus <- compute@config$capabilities$runtime$gpus %||% NULL
  memory <- if (
    is.data.frame(gpus) &&
      "total_memory_bytes" %in% names(gpus)
  ) {
    as.double(gpus$total_memory_bytes)
  } else if (
    is.list(gpus) &&
      "total_memory_bytes" %in% names(gpus)
  ) {
    as.double(unlist(gpus$total_memory_bytes, use.names = FALSE))
  } else if (is.list(gpus)) {
    vapply(
      gpus,
      function(gpu) {
        if (is.list(gpu)) {
          as.double(gpu$total_memory_bytes %||% NA_real_)
        } else {
          NA_real_
        }
      },
      numeric(1)
    )
  } else {
    numeric()
  }
  memory <- unname(memory[!is.na(memory)])
  if (length(memory)) memory else NULL
}

gpu_memory_hint <- function(operation, request, resolved) {
  hints <- character()
  max_batch_size <- resolved$max_batch_size %||% NULL
  micro_batch_size <- request$batch_size %||%
    resolved$micro_batch_size %||%
    NULL
  if (!is.null(max_batch_size) && max_batch_size > 1L) {
    hints <- c(
      hints,
      "Reduce max_batch_size while keeping every prompt unchanged."
    )
  }
  if (!is.null(micro_batch_size) && micro_batch_size > 1L) {
    hints <- c(
      hints,
      if (operation == "fine-tune") {
        paste(
          "Reduce micro_batch_size and increase gradient accumulation",
          "to preserve the global batch size."
        )
      } else {
        "Reduce micro_batch_size while keeping every sequence unchanged."
      }
    )
  }
  c(
    hints,
    paste(
      "Otherwise, use GPUs with more memory or increase model parallelism",
      "without changing precision or sequence lengths."
    )
  ) |>
    paste(collapse = " ")
}

gpu_memory_error_context <- function(x, failure_reason) {
  request_record <- read_json_file(
    file.path(x@path, "request.json"),
    simplify = FALSE
  )
  request <- request_record$request %||% list()
  plan <- read_json_file(
    file.path(x@path, "plan.json"),
    simplify = FALSE
  )
  resolved <- plan$metadata$resolved_control %||% list()
  descriptor <- if (is.list(x@expected_result)) {
    x@expected_result
  } else {
    list()
  }
  tensor <- as.integer(resolved$tensor_parallel_size %||% 1L)
  pipeline <- as.integer(resolved$pipeline_parallel_size %||% 1L)
  context <- as.integer(resolved$context_parallel_size %||% 1L)
  world_size <- as.integer(
    resolved$world_size %||% (x@compute@gpus * x@compute@nodes)
  )
  data <- as.integer(
    resolved$data_parallel_size %||%
      (world_size / (tensor * pipeline * context))
  )
  list(
    failure_reason = failure_reason,
    model = request$model %||%
      descriptor$variant %||%
      descriptor$model_size,
    checkpoint = descriptor$checkpoint %||%
      descriptor$checkpoint_root %||%
      descriptor$resolved_checkpoint %||%
      descriptor$base_checkpoint,
    sequence_summary = job_sequence_summary(x, resolved),
    micro_batch_size = as.integer(
      request$batch_size %||%
        resolved$micro_batch_size %||%
        1L
    ),
    tensor_parallel_size = tensor,
    pipeline_parallel_size = pipeline,
    context_parallel_size = context,
    data_parallel_size = data,
    precision = resolved$semantic_precision %||%
      resolved$precision %||%
      request$precision,
    mixed_precision_recipe = resolved$mixed_precision_recipe %||%
      descriptor$precision,
    gpu_count = world_size,
    gpu_memory_bytes = advertised_gpu_memory(x@compute),
    hint = gpu_memory_hint(x@kind, request, resolved)
  )
}

abort_job_state <- function(x, state, message) {
  persisted <- read_json_file(file.path(x@path, "state.json"))
  active_step_path <- file.path(x@path, "active-step")
  active_step <- if (file.exists(active_step_path)) {
    trimws(readLines(active_step_path, n = 1L, warn = FALSE))
  } else {
    character()
  }
  failure_reason <- persisted$failure_reason %||% NULL
  if (state == "failed" && is.null(failure_reason)) {
    scheduler_failure_reason <- slurm_failure_reason(x)
    logged_failure_reason <- if (
      !identical(active_step, "generation-validation")
    ) {
      job_log_failure_reason(x)
    } else {
      NULL
    }
    failure_reason <- scheduler_failure_reason %||% logged_failure_reason
    if (!is.null(failure_reason)) {
      persist_job_failure_reason(x, failure_reason)
      persisted <- read_json_file(file.path(x@path, "state.json"))
    }
  }
  helper_codes <- c(
    `65` = "BN_OUTPUT_SCHEMA",
    `66` = "BN_NONFINITE_OUTPUT",
    `67` = "BN_INVALID_SEQUENCE"
  )
  exit_status <- suppressWarnings(as.integer(persisted$exit_status))
  helper_code <- if (
    length(exit_status) == 1L &&
      !is.na(exit_status) &&
      identical(active_step, "generation-validation") &&
      as.character(exit_status) %in% names(helper_codes)
  ) {
    unname(helper_codes[[as.character(exit_status)]])
  } else {
    NULL
  }
  code <- if (state == "cancelled") {
    "BN_CANCELLED"
  } else if (
    state == "failed" &&
      !is.null(failure_reason) &&
      failure_reason %in% c("CUDA_OUT_OF_MEMORY", "OUT_OF_MEMORY")
  ) {
    "BN_GPU_MEMORY"
  } else if (
    state == "failed" &&
      identical(exit_status, 124L)
  ) {
    "BN_TIMEOUT"
  } else if (
    state == "failed" &&
      x@kind == "generation" &&
      !is.null(helper_code)
  ) {
    helper_code
  } else if (state == "failed") {
    "BN_UPSTREAM"
  } else {
    "BN_PROTOCOL"
  }
  fields <- list(
    run_path = x@path,
    request_id = x@id,
    operation = x@kind,
    log_paths = file.path(
      x@path,
      c("stdout.log", "stderr.log")
    ),
    upstream_exit_status = persisted$exit_status
  )
  if (code == "BN_GPU_MEMORY") {
    fields <- c(fields, gpu_memory_error_context(x, failure_reason))
    message <- paste("GPU memory exhausted.", message)
  }
  do.call(
    bionemor_abort,
    c(list(code = code, message = message), fields)
  )
}

#' Return a completed BioNeMo job result
#'
#' @param x A BioNeMo job.
#'
#' @return The operation's typed result.
#' @export
job_result <- function(x) {
  if (!S7_inherits(x, BioNeMoJob)) {
    stop("x must be a BioNeMo job")
  }
  state <- job_status(x)
  if (state != "succeeded") {
    detail <- paste(job_logs(x, tail = 50L), collapse = "\n")
    abort_job_state(
      x,
      state,
      paste0(
        "job is ",
        state,
        ", not succeeded; run path: ",
        x@path,
        if (nzchar(detail)) paste0("\n", detail) else ""
      )
    )
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
job_wait <- function(x, poll = 2, timeout = Inf) {
  if (!S7_inherits(x, BioNeMoJob)) {
    stop("x must be a BioNeMo job")
  }
  if (!is_scalar_number(poll) || poll <= 0) {
    stop("poll must be positive")
  }
  if (
    !identical(timeout, Inf) && (!is_scalar_number(timeout) || timeout <= 0)
  ) {
    stop("timeout must be positive or infinite")
  }
  wait_started <- Sys.time()
  repeat {
    state <- job_status(x)
    if (state == "succeeded") {
      return(materialize_job_result(x))
    }
    if (state %in% c("failed", "cancelled", "unknown")) {
      detail <- paste(job_logs(x, tail = 50L), collapse = "\n")
      abort_job_state(
        x,
        state,
        paste0(
          "job ",
          state,
          "; run path: ",
          x@path,
          if (nzchar(detail)) paste0("\n", detail) else ""
        )
      )
    }
    elapsed <- as.numeric(difftime(
      Sys.time(),
      wait_started,
      units = "secs"
    ))
    if (elapsed >= timeout) {
      bionemor_abort(
        "BN_TIMEOUT",
        "timed out waiting for job",
        run_path = x@path,
        request_id = x@id,
        operation = x@kind,
        log_paths = file.path(
          x@path,
          c("stdout.log", "stderr.log")
        )
      )
    }
    Sys.sleep(min(poll, timeout - elapsed))
  }
}

method(print, BioNeMoJob) <- function(x, ...) {
  cat("<bionemor_job>\n", sep = "")
  cat("ID: ", x@id, "\n", sep = "")
  cat("Kind: ", x@kind, "\n", sep = "")
  cat("State: ", job_status(x, refresh = FALSE), "\n", sep = "")
  cat("Path: ", x@path, "\n", sep = "")
  invisible(x)
}

# Compatibility for installation code that still emits one backend script.
write_job_script <- function(command, compute, name, log, stderr = log) {
  jobs <- file.path(compute@workspace, ".bionemor", "jobs")
  dir.create(jobs, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  path <- file.path(jobs, paste0(name, ".sh"))
  writeLines(
    c(
      "#!/usr/bin/env bash",
      if (compute@backend == "slurm") {
        job_directives(compute, name, log, stderr)
      },
      "set -euo pipefail",
      command
    ),
    path,
    useBytes = TRUE
  )
  Sys.chmod(path, "0750")
  path
}
