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

resolved_container_image <- function(compute) {
  if (
    compute@backend == "local" &&
      !grepl("@sha256:[0-9a-fA-F]{64}$", compute@image)
  ) {
    compute@image_digest
  } else {
    compute@image
  }
}

containerize_command <- function(
  command,
  compute,
  gpus,
  name = NULL,
  entrypoint = FALSE
) {
  image <- resolved_container_image(compute)
  stopifnot(
    "command must be a command specification" = inherits(
      command,
      "bionemor_command"
    ),
    "container image must be one non-empty string" = is_scalar_string(image)
  )
  executable <- command$executable
  cwd <- command$cwd %||% compute@workspace
  if (compute@backend == "local") {
    env_args <- unlist(
      Map(
        function(name, value) {
          if (name %in% credential_environment_variables) {
            c("-e", name)
          } else {
            c("-e", paste0(name, "=", value))
          }
        },
        names(command$env),
        unname(command$env)
      ),
      use.names = FALSE
    )
    command$executable <- compute@config$container_engine %||% "docker"
    command$args <- c(
      "run",
      "--rm",
      if (gpus) c("--gpus", "all"),
      if (!is.null(name)) c("--ipc=host", "--name", name),
      local_container_user_args(),
      if (entrypoint) c("--entrypoint", executable),
      "-v",
      paste0(compute@workspace, ":", compute@workspace),
      "-w",
      cwd,
      env_args,
      image,
      if (!entrypoint) executable,
      command$args
    )
    command$env <- character()
  } else {
    command$executable <- "apptainer"
    command$args <- c(
      "exec",
      if (gpus) "--nv",
      "--bind",
      paste0(compute@workspace, ":", compute@workspace),
      "--pwd",
      cwd,
      image,
      executable,
      command$args
    )
  }
  command$cwd <- compute@workspace
  command
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
  timeout = Inf,
  role = "upstream",
  failure_codes = character()
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
      is_scalar_number(timeout) && timeout > 0,
    "command role must be one non-empty string" = is_scalar_string(role),
    "command failure codes must be a named character vector" = is.character(
      failure_codes
    ) &&
      !anyNA(failure_codes) &&
      (length(failure_codes) == 0L ||
        !is.null(names(failure_codes)) &&
          all(grepl("^[0-9]+$", names(failure_codes))) &&
          all(nzchar(failure_codes)) &&
          !anyDuplicated(names(failure_codes)))
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
      timeout = as.double(timeout),
      role = role,
      failure_codes = failure_codes
    ),
    class = c("bionemor_command", "list")
  )
}

command_plan <- function(steps) {
  if (!is.list(steps) || length(steps) <= 0L) {
    stop("command plan steps must be a non-empty list")
  }
  if (!all(vapply(steps, inherits, logical(1), "bionemor_command"))) {
    stop("every command plan step must be a command specification")
  }
  structure(
    list(
      schema_version = 3L,
      steps = unname(steps)
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
    timeout = if (is.finite(command$timeout)) command$timeout else NULL,
    role = command$role,
    failure_codes = as.list(command$failure_codes)
  )
}

serializable_plan <- function(plan) {
  list(
    schema_version = plan$schema_version,
    steps = lapply(plan$steps, serializable_command)
  )
}

read_command_plan <- function(path) {
  value <- read_json_file(path, simplify = FALSE)
  valid <- identical(value$schema_version, 3L) &&
    identical(names(value), c("schema_version", "steps")) &&
    is.list(value$steps) &&
    length(value$steps) > 0L &&
    all(vapply(
      value$steps,
      function(step) {
        is.list(step) &&
          identical(
            names(step),
            c(
              "executable",
              "args",
              "env",
              "cwd",
              "redactions",
              "stdin",
              "stdout",
              "stderr",
              "timeout",
              "role",
              "failure_codes"
            )
          ) &&
          is_scalar_string(step$role) &&
          is.list(step$failure_codes)
      },
      logical(1)
    ))
  if (!valid) {
    run_path <- dirname(path)
    bionemor_abort(
      "BN_PROTOCOL",
      "persisted command plan schema is unsupported",
      run_path = run_path,
      request_id = basename(run_path),
      operation = "job-reopen"
    )
  }
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
      timeout = step$timeout %||% Inf,
      role = step$role,
      failure_codes = unlist(step$failure_codes, use.names = TRUE) %||%
        character()
    )
  })
  command_plan(steps)
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
      adapter = recipe@adapter,
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
  if (!identical(as.integer(value$nodes), 1L)) {
    stop("persisted run must use one compute node")
  }
  if (
    !is.list(value$recipe) ||
      !all(
        c(
          "adapter",
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
    adapter = value$recipe$adapter,
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
    recipe = recipe,
    backend = value$backend,
    engine = value$engine,
    workspace = value$workspace,
    image = value$image,
    gpus = as.integer(value$gpus),
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
  name = NULL
) {
  stopifnot(
    "compute must be a BioNeMo compute specification" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "compute workspace must exist" = dir.exists(compute@workspace),
    "kind must be one safe name" = is_scalar_string(kind) &&
      grepl("^[A-Za-z0-9_.-]+$", kind)
  )
  adapter <- adapter_record(compute@recipe@adapter)
  name <- safe_name(name, paste0(adapter$family, "-", kind))
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
      state = "created",
      backend_id = NULL,
      started_at = utc_timestamp(),
      updated_at = utc_timestamp(),
      exit_status = NULL,
      failure_reason = NULL
    ),
    file.path(path, "state.json")
  )
  structure(
    list(
      path = normalize_path(path),
      id = name,
      kind = kind,
      compute = compute
    ),
    class = c("bionemor_run", "list")
  )
}

validate_operation_cleanup <- function(cleanup) {
  if (is.null(cleanup)) {
    return(invisible(cleanup))
  }
  stopifnot(
    "operation cleanup must name a supported directory and suffix" = is.list(
      cleanup
    ) &&
      identical(names(cleanup), c("directory", "suffix")) &&
      cleanup$directory %in% c("inputs", "upstream", "outputs") &&
      is_scalar_string(cleanup$suffix) &&
      !grepl("[/\\\\]", cleanup$suffix)
  )
  invisible(cleanup)
}

operation_context <- function(
  model = list(),
  checkpoint = list(),
  tokenizer = list(),
  precision = list(),
  warnings = list()
) {
  list(
    model = model,
    checkpoint = checkpoint,
    tokenizer = tokenizer,
    precision = precision,
    warnings = warnings
  )
}

operation_spec <- function(
  run,
  request,
  plan,
  result,
  context,
  execution = list(),
  cleanup = NULL,
  timeout = Inf
) {
  stopifnot(
    "run must be a BioNeMo run" = inherits(run, "bionemor_run"),
    "operation request must be a list" = is.list(request),
    "operation plan must be a command plan" = inherits(
      plan,
      "bionemor_command_plan"
    ),
    "operation result must name one result type" = is.list(result) &&
      is_scalar_string(result$type),
    "operation context must contain canonical manifest fields" = is.list(
      context
    ) &&
      identical(
        names(context),
        c("model", "checkpoint", "tokenizer", "precision", "warnings")
      ) &&
      all(vapply(context, is.list, logical(1))),
    "operation execution must be a list" = is.list(execution),
    "operation timeout must be positive or infinite" = identical(
      timeout,
      Inf
    ) ||
      is_scalar_number(timeout) && timeout > 0
  )
  validate_operation_cleanup(cleanup)
  version <- result_versions(
    run$compute@recipe@adapter,
    run$kind
  )[[result$type]]
  if (is.null(version)) {
    stop("result type is unsupported for this operation")
  }
  result$result_version <- as.integer(version)
  structure(
    list(
      run = run,
      request = request,
      execution = execution,
      plan = plan,
      result = result,
      context = context,
      cleanup = cleanup,
      timeout = as.double(timeout)
    ),
    class = c("bionemor_operation", "list")
  )
}

serializable_operation <- function(operation) {
  run <- operation$run
  redact_persisted_value(list(
    schema_version = 4L,
    id = run$id,
    kind = run$kind,
    compute = compute_record(run$compute),
    request = operation$request,
    execution = operation$execution,
    result = operation$result,
    context = operation$context,
    cleanup = operation$cleanup,
    timeout = if (is.finite(operation$timeout)) operation$timeout else NULL
  ))
}

read_operation <- function(run_path) {
  record <- read_operation_record(run_path)
  compute <- compute_from_record(record$compute)
  validate_result_contract(
    compute@recipe@adapter,
    record$kind,
    record$result,
    run_path,
    record$id,
    "job-reopen"
  )
  validate_operation_cleanup(record$cleanup)
  run <- structure(
    list(
      path = run_path,
      id = record$id,
      kind = record$kind,
      compute = compute
    ),
    class = c("bionemor_run", "list")
  )
  structure(
    list(
      run = run,
      request = record$request,
      execution = record$execution,
      plan = read_command_plan(file.path(run_path, "plan.json")),
      result = record$result,
      context = record$context,
      cleanup = record$cleanup,
      timeout = record$timeout %||% Inf
    ),
    class = c("bionemor_operation", "list")
  )
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
  if (!is_scalar_string(resolved_container_image(compute))) {
    stop("container execution requires a resolved image digest")
  }
  containerize_command(
    command,
    compute,
    gpus = TRUE,
    name = paste0("bionemor-", run_id)
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

path_digest <- function(path, format = NULL) {
  if (!dir.exists(path) && !identical(format, "vortex")) {
    return(unname(tools::md5sum(path)))
  }
  if (identical(format, "vortex")) {
    config <- file.path(dirname(path), "config.json")
    stopifnot(file.exists(path), file.exists(config))
    files <- c(path, config)
    relative <- basename(files)
  } else {
    files <- list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    files <- files[!dir.exists(files)]
    relative <- substring(files, nchar(path) + 2L)
    keep <- !relative %in% c(
      "bionemor-checkpoint.json",
      ".bionemor-complete"
    )
    files <- files[keep]
    relative <- relative[keep]
  }
  records <- paste(relative, as.character(tools::md5sum(files)), sep = ":")
  temporary <- tempfile("checkpoint-digest-")
  on.exit(unlink(temporary), add = TRUE)
  writeLines(sort(records, method = "radix"), temporary, useBytes = TRUE)
  unname(tools::md5sum(temporary))
}

state <- read_json(file.path(run_path, "state.json"))
manifest <- read_json(file.path(run_path, "manifest-template.json"))
operation <- read_json(file.path(run_path, "request.json"))
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
cleanup <- operation$cleanup
if (identical(manifest$state, "succeeded") && is.list(cleanup)) {
  stopifnot(
    cleanup$directory %in% c("inputs", "upstream", "outputs"),
    is.character(cleanup$suffix),
    length(cleanup$suffix) == 1L,
    nzchar(cleanup$suffix),
    !grepl("[/\\\\]", cleanup$suffix)
  )
  temporary <- list.files(
    file.path(run_path, cleanup$directory),
    recursive = TRUE,
    full.names = TRUE
  )
  temporary <- temporary[endsWith(temporary, cleanup$suffix)]
  if (length(temporary) && unlink(temporary) != 0L) {
    stop("failed to remove job temporary files")
  }
}
checkpoint_path <- manifest$checkpoint$path
checkpoint_digest <- manifest$checkpoint$digest
checkpoint_digest_resolved <- is.list(checkpoint_digest) &&
  identical(checkpoint_digest$algorithm, "md5") &&
  is.character(checkpoint_digest$value) &&
  length(checkpoint_digest$value) == 1L &&
  grepl("^[0-9a-f]{32}$", checkpoint_digest$value)
if (
  is.character(checkpoint_path) &&
    length(checkpoint_path) == 1L &&
    file.exists(checkpoint_path) &&
    !checkpoint_digest_resolved
) {
  manifest$checkpoint$digest <- list(
    algorithm = "md5",
    value = path_digest(checkpoint_path, manifest$checkpoint$format)
  )
}
base_path <- manifest$checkpoint$base_checkpoint$path
base_digest <- manifest$checkpoint$base_checkpoint$digest
base_digest_resolved <- (
  is.character(base_digest) &&
    length(base_digest) == 1L &&
    grepl("^[0-9a-f]{32}$", base_digest)
) || (
  is.list(base_digest) &&
    identical(base_digest$algorithm, "md5") &&
    is.character(base_digest$value) &&
    length(base_digest$value) == 1L &&
    grepl("^[0-9a-f]{32}$", base_digest$value)
)
if (
  is.character(base_path) &&
    length(base_path) == 1L &&
    file.exists(base_path) &&
    !base_digest_resolved
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
      if (
        !isTRUE(ps::ps_is_running(handle)) ||
          ps::ps_status(handle) %in% c("zombie", "dead")
      ) {
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
  if (
    is.null(handle) ||
      !ps::ps_is_running(handle) ||
      ps::ps_status(handle) %in% c("zombie", "dead")
  ) {
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
  step_roles <- vapply(plan$steps, `[[`, character(1), "role")
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

new_job_object <- function(operation, process = NULL, metadata = list()) {
  run <- operation$run
  state <- read_json_file(file.path(run$path, "state.json"))
  BioNeMoJob(
    path = run$path,
    id = state$id,
    kind = state$kind,
    state = state$state,
    compute = run$compute,
    log = file.path(run$path, "stdout.log"),
    operation = operation,
    timeout = operation$timeout,
    process = process,
    metadata = metadata
  )
}

submit_operation <- function(operation, async = TRUE) {
  stopifnot(
    "operation must be a BioNeMo operation" = inherits(
      operation,
      "bionemor_operation"
    ),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  run <- operation$run
  compute <- run$compute
  plan <- operation$plan
  run_path <- run$path
  kind <- run$kind
  timeout <- operation$timeout
  atomic_write_json(
    serializable_operation(operation),
    file.path(run_path, "request.json")
  )
  atomic_write_json(
    serializable_plan(plan),
    file.path(run_path, "plan.json")
  )
  template_job <- new_job_object(operation)
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
      operation,
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
      operation,
      metadata = list(backend_id = id, script = script)
    )
  }
  if (async) job else job_wait(job, poll = 0.05)
}

#' Reopen a persisted BioNeMo job
#'
#' Each operation dispatched through the job runner creates a run directory
#' under `<workspace>/.bionemor/runs/<name>`. The directory records
#' the request, command plan, state, logs, outputs, and provenance needed to
#' inspect the run after the R session that started it has ended. A
#' `BioNeMoJob` is a handle to those persisted files and the local or Slurm
#' execution backend.
#'
#' Operations that support `async` return their typed result directly when
#' `async = FALSE`. With `async = TRUE`, they return a `BioNeMoJob` immediately.
#' Pass that job to [job_status()], [job_logs()], [job_wait()], [job_cancel()],
#' or [job_result()]. `bionemo_job()` reconstructs the same handle from its run
#' directory, including for a run that is still active or already complete. It
#' does not resubmit or restart the operation. The recorded operation and result
#' format must be supported by the installed package.
#'
#' @param path Path to a run directory created by bionemor. It must contain the
#'   persisted request, command plan, and state files.
#'
#' @return A `BioNeMoJob` for the persisted run.
#' @examples
#' \dontrun{
#' # Save job_path(job), then reopen the run in this or a later R session.
#' job <- bionemo_job(
#'   "/shared/workspace/.bionemor/runs/my-generation"
#' )
#' job_status(job)
#' result <- job_wait(job)
#' }
#' @family BioNeMo job lifecycle
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
  operation <- read_operation(path)
  run <- operation$run
  compute <- run$compute
  state <- read_json_file(file.path(path, "state.json"), simplify = FALSE)
  if (
    !identical(state$kind, run$kind) ||
      !identical(state$id, run$id)
  ) {
    bionemor_abort(
      "BN_PROTOCOL",
      "persisted run contains inconsistent job identity",
      run_path = path,
      request_id = run$id,
      operation = "job-reopen"
    )
  }
  new_job_object(
    operation,
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
#' The run directory identifies a job and stores the files needed to inspect or
#' reopen it. Save this path to reopen the job with [bionemo_job()] in another R
#' session. Keep the directory intact: status updates, logs, result
#' materialization, and provenance all use files stored below it.
#'
#' @param x A `BioNeMoJob` returned by an asynchronous operation or
#'   [bionemo_job()].
#'
#' @return A length-one character vector containing the normalized absolute run
#'   path.
#' @examples
#' \dontrun{
#' job <- bionemo_job(
#'   "/shared/workspace/.bionemor/runs/my-generation"
#' )
#' path <- job_path(job)
#'
#' # Reopen the same run later.
#' same_job <- bionemo_job(path)
#' }
#' @family BioNeMo job lifecycle
#' @export
job_path <- function(x) {
  if (!S7_inherits(x, BioNeMoJob)) {
    stop("x must be a BioNeMo job")
  }
  normalize_path(x@path)
}

terminal_job_states <- c("succeeded", "failed", "cancelled")

process_group_is_alive <- function(pid) {
  if (is.na(pid)) {
    return(FALSE)
  }
  ps <- Sys.which("ps")
  if (!nzchar(ps)) {
    stop("ps is required to inspect local process groups")
  }
  result <- command_probe(ps, c("-eo", "pgid=,stat="))
  if (result$status != 0L) {
    stop("failed to inspect local process groups")
  }
  processes <- utils::read.table(
    text = result$stdout,
    col.names = c("pgid", "status"),
    colClasses = "character",
    strip.white = TRUE
  )
  any(
    processes$pgid == as.character(pid) &
      !grepl("^[ZX]", processes$status)
  )
}

wait_for_process_group <- function(pid, timeout) {
  deadline <- Sys.time() + timeout
  while (process_group_is_alive(pid) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  !process_group_is_alive(pid)
}

slurm_failure_states <- c(
  "FAILED",
  "TIMEOUT",
  "OUT_OF_MEMORY",
  "NODE_FAIL",
  "PREEMPTED",
  "BOOT_FAIL",
  "DEADLINE"
)

slurm_accounting_record <- function(id, operation, request_id = id, ...) {
  stopifnot(
    "Slurm job ID must be one non-empty string" = is_scalar_string(id),
    "Slurm operation must be one non-empty string" = is_scalar_string(
      operation
    )
  )
  result <- command_probe(
    "sacct",
    c("-X", "-n", "-P", "-j", id, "--format=JobIDRaw,State,ExitCode")
  )
  if (result$status != 0L) {
    detail <- redact_credentials(trimws(result$stderr))
    bionemor_abort(
      "BN_UPSTREAM",
      paste0(
        "failed to query Slurm accounting",
        if (nzchar(detail)) paste0(": ", detail) else ""
      ),
      operation = operation,
      request_id = request_id,
      backend_id = id,
      ...,
      command = "sacct",
      upstream_exit_status = as.integer(result$status),
      hint = "Inspect the Slurm accounting service."
    )
  }
  lines <- strsplit(trimws(result$stdout), "\n", fixed = TRUE)[[1L]]
  records <- lapply(
    lines[nzchar(lines)],
    function(line) strsplit(line, "|", fixed = TRUE)[[1L]]
  )
  records <- Filter(
    function(record) length(record) == 3L && identical(record[[1L]], id),
    records
  )
  if (length(records) > 1L) {
    bionemor_abort(
      "BN_PROTOCOL",
      "sacct returned more than one allocation record",
      operation = operation,
      request_id = request_id,
      backend_id = id,
      ...,
      hint = "Inspect the Slurm accounting output for this allocation."
    )
  }
  if (length(records)) records[[1L]] else NULL
}

slurm_scheduler_state <- function(state) {
  sub("[+ ].*$", "", toupper(trimws(state)))
}

slurm_mapped_state <- function(scheduler, exit_code) {
  switch(
    scheduler,
    PENDING = "submitted",
    CONFIGURING = "starting",
    RUNNING = "running",
    COMPLETING = "running",
    COMPLETED = if (exit_code == "0:0") "succeeded" else "failed",
    CANCELLED = "cancelled",
    if (scheduler %in% slurm_failure_states) "failed" else "unknown"
  )
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
  record <- slurm_accounting_record(
    id,
    operation = job@kind,
    request_id = job@id,
    run_path = job@path,
    log_paths = file.path(job@path, c("stdout.log", "stderr.log"))
  )
  if (is.null(record)) {
    latest <- read_json_file(file.path(job@path, "state.json"))
    return(
      if (latest$state %in% terminal_job_states) {
        latest$state
      } else {
        slurm_nonterminal_state(persisted_state, latest$state)
      }
    )
  }
  scheduler <- slurm_scheduler_state(record[[2L]])
  exit_code <- trimws(record[[3L]])
  mapped <- slurm_mapped_state(scheduler, exit_code)
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
#' With `refresh = TRUE`, `job_status()` reconciles the persisted state with the
#' execution backend. Local jobs are checked against their runner process, and
#' Slurm jobs are checked with scheduler accounting. Terminal states are
#' persisted in the run directory. With `refresh = FALSE`, the function returns
#' the state held by the job handle without a routine backend query; that value
#' may be stale. Reopening a job with [bionemo_job()] loads its persisted state.
#'
#' @section States:
#'
#' A job has one of these state strings:
#'
#' - `"created"`: the run directory has been initialized.
#' - `"submitted"`: the backend accepted the job and it is waiting to start.
#' - `"starting"`: the local runner is starting the operation.
#' - `"running"`: the operation is running or finalizing its outputs.
#' - `"succeeded"`: the operation completed and its result can be read.
#' - `"failed"`: the operation ended with an error.
#' - `"cancelled"`: cancellation was confirmed.
#' - `"unknown"`: the backend state could not be mapped to a supported state.
#'
#' The terminal states are `"succeeded"`, `"failed"`, and `"cancelled"`.
#' [job_wait()] and [job_result()] report non-success states as typed bionemor
#' errors.
#'
#' @param x A `BioNeMoJob` returned by an asynchronous operation or
#'   [bionemo_job()].
#' @param refresh Whether to query the execution backend for current state.
#'   Set this to `FALSE` to read the state held by the job handle without a
#'   routine backend query.
#'
#' @return A length-one character vector containing the job state.
#' @examples
#' \dontrun{
#' job <- bionemo_job(
#'   "/shared/workspace/.bionemor/runs/my-generation"
#' )
#' job_status(job)
#' job_status(job, refresh = FALSE)
#' }
#' @family BioNeMo job lifecycle
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
#' `job_logs()` reads a snapshot of the persisted log files, so it can be called
#' while a job is running and again after completion. With `stream = "both"`,
#' stdout is returned first and stderr second; the two files are not merged in
#' chronological order. `tail` is applied after the selected streams are
#' combined. Credential-like values are redacted before lines are returned.
#'
#' @param x A `BioNeMoJob` returned by an asynchronous operation or
#'   [bionemo_job()].
#' @param tail `NULL` to return every available line, or a positive integer
#'   giving the number of final lines to return.
#' @param stream Which persisted log stream to read: `"stdout"`, `"stderr"`, or
#'   `"both"`.
#'
#' @return A character vector with one log line per element. The result is empty
#'   when no selected log file has content.
#' @examples
#' \dontrun{
#' job <- bionemo_job(
#'   "/shared/workspace/.bionemor/runs/my-generation"
#' )
#' job_logs(job, tail = 20L)
#' job_logs(job, tail = 20L, stream = "stderr")
#' }
#' @family BioNeMo job lifecycle
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
#' `job_cancel()` asks the local or Slurm backend to stop an active job and
#' waits for the backend to report a terminal state. The default requests an
#' orderly termination; local execution escalates if the process does not stop.
#' `force = TRUE` requests immediate termination instead. A job that is already
#' terminal is returned unchanged.
#'
#' Cancellation races with normal completion. The final state may therefore be
#' `"succeeded"` or `"failed"` if the operation finishes before cancellation is
#' confirmed. Cancelling does not delete the run directory, logs, or outputs.
#'
#' @param x A `BioNeMoJob` returned by an asynchronous operation or
#'   [bionemo_job()].
#' @param force Whether to request immediate termination. The default first
#'   requests an orderly stop.
#'
#' @return `x`, updated to the backend's terminal state, invisibly.
#' @examples
#' \dontrun{
#' job <- bionemo_job(
#'   "/shared/workspace/.bionemor/runs/my-generation"
#' )
#' job_cancel(job)
#' job_status(job)
#'
#' # To skip orderly termination, use this instead on an active job:
#' # job_cancel(job, force = TRUE)
#' }
#' @family BioNeMo job lifecycle
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

run_manifest_warnings <- function(
  job,
  result = NULL,
  adapter_warnings = list()
) {
  values <- list(adapter_warnings)
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
  descriptor <- job@operation$result
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

checkpoint_result_context <- function(context, result) {
  if (S7_inherits(result, BioNeMoModel)) {
    result <- result@checkpoint
  }
  if (is.null(result) || !S7_inherits(result, BioNeMoCheckpoint)) {
    return(context)
  }
  manifest <- checkpoint_manifest(result)
  context$model$revision <- manifest$source_revision
  context$checkpoint <- list(
    path = checkpoint_path(result),
    source = manifest$source,
    source_trust = manifest$source_trust,
    source_verified = manifest$source_verified,
    format = manifest$format,
    kind = manifest$kind,
    revision = manifest$source_revision,
    digest = list(
      algorithm = "md5",
      value = manifest$checkpoint_digest
    ),
    base_checkpoint = list(
      path = manifest$base_checkpoint_path,
      source = manifest$base_checkpoint_source,
      source_trust = manifest$base_checkpoint_source_trust,
      source_verified = manifest$base_checkpoint_source_verified,
      digest = manifest$base_checkpoint_digest
    )
  )
  context$tokenizer <- list(
    identity = manifest$tokenizer_identity,
    revision = manifest$tokenizer_revision,
    digest = list(
      algorithm = "git-revision",
      value = manifest$tokenizer_revision
    )
  )
  context$precision$resolved_recipe <- manifest$mixed_precision_recipe
  context
}

run_manifest_value <- function(job, result = NULL) {
  if (!S7_inherits(job, BioNeMoJob)) {
    stop("job must be a BioNeMo job")
  }
  state <- read_json_file(file.path(job@path, "state.json"))
  operation <- job@operation
  adapter <- job@compute@recipe@adapter
  context <- checkpoint_result_context(operation$context, result)
  validation <- operation$result$validation
  if (is_scalar_string(validation) && file.exists(validation)) {
    context$warnings <- read_json_file(validation, simplify = FALSE)$warnings
  }
  provenance <- adapter_function(adapter, "provenance")(job)
  capabilities <- job@compute@config$capabilities %||% list()
  manifest_path <- file.path(job@path, "manifest.json")
  previous_manifest <- if (file.exists(manifest_path)) {
    read_json_file(manifest_path, simplify = FALSE)
  } else {
    list()
  }
  checkpoint <- context$checkpoint
  if (
    is.null(checkpoint$digest) &&
      identical(checkpoint$path, previous_manifest$checkpoint$path) &&
      !is.null(previous_manifest$checkpoint$digest)
  ) {
    checkpoint$digest <- previous_manifest$checkpoint$digest
  }
  if (
    is_scalar_string(checkpoint$path) &&
      file.exists(checkpoint$path) &&
      is.null(checkpoint$digest) &&
      state$state %in% terminal_job_states
  ) {
    checkpoint$digest <- list(
      algorithm = "md5",
      value = checkpoint_payload_digest(
        checkpoint$path,
        checkpoint$format
      )
    )
  }
  manifest <- list(
    schema_version = 1L,
    id = job@id,
    kind = job@kind,
    result = list(
      type = operation$result$type,
      version = operation$result$result_version
    ),
    package = list(
      name = "bionemor",
      version = run_manifest_package_version()
    ),
    request = operation$request,
    execution = operation$execution,
    recipe = provenance$recipe,
    dockerfile = provenance$dockerfile,
    helper = provenance$helper,
    image = list(
      reference = job@compute@image,
      digest = job@compute@image_digest
    ),
    model = context$model,
    checkpoint = checkpoint,
    tokenizer = context$tokenizer,
    precision = context$precision,
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
    warnings = run_manifest_warnings(job, result, context$warnings)
  )
  redact_persisted_value(manifest)
}

write_run_manifest <- function(job, result = NULL) {
  state <- read_json_file(file.path(job@path, "state.json"))
  if (!state$state %in% terminal_job_states) {
    stop("run manifest requires a terminal job state")
  }
  manifest <- run_manifest_value(job, result)
  atomic_write_json(
    manifest,
    file.path(job@path, "manifest.json")
  )
  invisible(manifest)
}

materialize_job_result <- function(x) {
  operation <- x@operation
  adapter <- x@compute@recipe@adapter
  materialize <- adapter_function(adapter, "materialize")
  result <- materialize(x, operation)
  write_run_manifest(x, result)
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
  record <- slurm_accounting_record(
    id,
    operation = x@kind,
    request_id = x@id,
    run_path = x@path,
    log_paths = file.path(x@path, c("stdout.log", "stderr.log"))
  )
  if (is.null(record)) {
    return(NULL)
  }
  scheduler <- slurm_scheduler_state(record[[2L]])
  if (scheduler %in% slurm_failure_states) {
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
  operation <- x@operation
  request <- operation$request
  resolved <- operation$execution$resolved_control %||% list()
  context_record <- operation$context
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
    model = context_record$model$name,
    checkpoint = context_record$checkpoint$path,
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
    precision = context_record$precision$semantic,
    mixed_precision_recipe = context_record$precision$resolved_recipe,
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
  plan <- x@operation$plan
  matching_step <- if (is_scalar_string(active_step)) {
    which(vapply(
      plan$steps,
      function(step) identical(step$role, active_step),
      logical(1)
    ))
  } else {
    integer()
  }
  contract_codes <- if (length(matching_step) == 1L) {
    plan$steps[[matching_step]]$failure_codes
  } else {
    character()
  }
  contract_stage <- length(contract_codes) > 0L
  failure_reason <- persisted$failure_reason %||% NULL
  if (state == "failed" && is.null(failure_reason)) {
    scheduler_failure_reason <- slurm_failure_reason(x)
    logged_failure_reason <- if (!contract_stage) {
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
  exit_status <- suppressWarnings(as.integer(persisted$exit_status))
  helper_code <- if (
    length(exit_status) == 1L &&
      !is.na(exit_status) &&
      contract_stage &&
      as.character(exit_status) %in% names(contract_codes)
  ) {
    unname(contract_codes[[as.character(exit_status)]])
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
#' `job_result()` refreshes job state and materializes the portable outputs of a
#' successful run as the same typed R object returned by the corresponding
#' synchronous operation. It does not wait for an active job; use [job_wait()]
#' when the operation may still be running.
#'
#' A failed, cancelled, active, or unknown job produces a typed bionemor error.
#' The condition includes the run path and available log context so the saved
#' run can be inspected after the error is caught.
#'
#' @param x A `BioNeMoJob` returned by an asynchronous operation or
#'   [bionemo_job()].
#'
#' @return The operation's typed result, such as an `evo2_generation` or
#'   `evo2_scores` data frame, an embedding matrix, an `Evo2Model`, an
#'   `Evo2Dataset`, a `BioNeMoCheckpoint`, or a `BioNeMoArtifact`.
#' @examples
#' \dontrun{
#' job <- bionemo_job(
#'   "/shared/workspace/.bionemor/runs/my-generation"
#' )
#' if (job_status(job) == "succeeded") {
#'   generated <- job_result(job)
#' }
#' }
#' @family BioNeMo job lifecycle
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

#' Wait for a BioNeMo job and return its result
#'
#' `job_wait()` refreshes job state every `poll` seconds until the run succeeds,
#' reaches a non-success state, or the wait reaches `timeout`. A successful run
#' is materialized as the same typed R object returned by the corresponding
#' operation with `async = FALSE`. Failed, cancelled, and unknown states produce
#' typed bionemor errors with the run path and available log context.
#'
#' `timeout` limits only this call to `job_wait()`. It is measured from the time
#' the call starts, does not change the operation's own execution timeout, and
#' does not cancel the job. After a wait timeout, use [job_status()] or
#' [job_logs()] to inspect the still-running job, call `job_wait()` again, or
#' cancel it explicitly with [job_cancel()].
#'
#' @param x A `BioNeMoJob` returned by an asynchronous operation or
#'   [bionemo_job()].
#' @param poll Positive polling interval in seconds.
#' @param timeout Positive maximum number of seconds to spend waiting, or `Inf`
#'   to wait without a limit. A wait timeout does not cancel the job.
#'
#' @return The operation's typed result.
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "/shared/workspace")
#' model <- evo2(
#'   "7b",
#'   checkpoint = "/shared/workspace/checkpoints/evo2-7b"
#' )
#' job <- evo2_generate(
#'   model,
#'   c(example = "ACGT"),
#'   compute,
#'   num_tokens = 32L,
#'   async = TRUE
#' )
#'
#' generated <- job_wait(job, poll = 2, timeout = 600)
#' }
#' @family BioNeMo job lifecycle
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
