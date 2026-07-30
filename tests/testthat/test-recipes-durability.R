test_that("detached local jobs finalize state, events, and provenance", {
  workspace <- tempfile("bionemor-detached-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "detached-provenance",
    async = TRUE
  )
  run_path <- job_path(job)
  state_path <- file.path(run_path, "state.json")
  manifest_path <- file.path(run_path, "manifest.json")
  deadline <- Sys.time() + 3
  repeat {
    state <- jsonlite::read_json(state_path)
    if (
      state$state %in% c("succeeded", "failed", "cancelled") &&
        file.exists(manifest_path)
    ) {
      break
    }
    if (Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }

  expect_equal(state$state, "succeeded")
  expect_true(file.exists(manifest_path))
  events <- lapply(
    readLines(file.path(run_path, "events.jsonl"), warn = FALSE),
    jsonlite::parse_json
  )
  expect_true(all(c("running", "succeeded") %in% vapply(
    events,
    `[[`,
    character(1),
    "state"
  )))
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  expect_equal(manifest$state, "succeeded")
  expect_equal(manifest$exit_status, 0L)
  expect_true(length(manifest$inputs) > 0L)
  expect_true(length(manifest$upstream) > 0L)
})

test_that("credential-bearing public workflows redact raw persisted logs", {
  workspace <- tempfile("bionemor-credential-log-")
  bin <- tempfile("bionemor-bin-")
  source <- tempfile("bionemor-ngc-source-")
  secret <- "ngc-secret-that-must-never-reach-disk"
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_NGC_SOURCE = source,
    BIONEMOR_FAKE_ECHO_CREDENTIAL = "true",
    NGC_CLI_API_KEY = secret
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)

  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    source = "ngc://example/evo2-7b:1.0",
    format = "nemo2",
    path = "checkpoints/evo2-7b",
    compute = compute,
    revision = "1.0",
    trust = TRUE
  )
  run_path <- checkpoint_manifest(checkpoint)$provenance$run_path
  log_paths <- file.path(run_path, c("stdout.log", "stderr.log"))
  raw_logs <- unlist(lapply(log_paths, readLines, warn = FALSE))
  expect_true(any(grepl("[REDACTED]", raw_logs, fixed = TRUE)))
  expect_false(any(grepl(secret, raw_logs, fixed = TRUE)))
  persisted <- list.files(run_path, recursive = TRUE, full.names = TRUE)
  persisted <- persisted[!dir.exists(persisted)]
  contents <- unlist(lapply(persisted, readLines, warn = FALSE))
  expect_false(any(grepl(secret, contents, fixed = TRUE)))

  reopened_logs <- withr::with_envvar(
    c(NGC_CLI_API_KEY = NA),
    job_logs(bionemo_job(run_path))
  )
  expect_false(any(grepl(secret, reopened_logs, fixed = TRUE)))
  script <- readLines(file.path(run_path, "run.sh"), warn = FALSE)
  expect_false(any(grepl("> >(", script, fixed = TRUE)))
  expect_true(any(grepl("mkfifo", script, fixed = TRUE)))
})

test_that("operation timeout is enforced without an active R waiter", {
  workspace <- tempfile("bionemor-operation-timeout-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  pid_file <- tempfile("bionemor-timeout-pid-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  data <- evo2_dataset(
    train = c(first = "ACGT", second = "TGCA"),
    validation = c(validation = "AAAA"),
    test = c(test = "CCCC")
  )
  prepared <- evo2_prepare(
    data,
    model,
    compute,
    path = "datasets/timeout"
  )
  withr::local_envvar(
    BIONEMOR_FAKE_DELAY = "5",
    BIONEMOR_FAKE_PID_FILE = pid_file
  )

  job <- evo2_finetune(
    model,
    prepared,
    compute,
    steps = 1L,
    method = evo2_lora(),
    timeout = 1,
    name = "operation-timeout",
    async = TRUE
  )
  withr::defer({
    state <- jsonlite::read_json(file.path(job_path(job), "state.json"))$state
    if (!state %in% c("succeeded", "failed", "cancelled")) {
      try(job_cancel(job, force = TRUE), silent = TRUE)
    }
  })
  state_path <- file.path(job_path(job), "state.json")
  manifest_path <- file.path(job_path(job), "manifest.json")
  deadline <- Sys.time() + 4
  repeat {
    state <- jsonlite::read_json(state_path)
    if (state$state == "failed" && file.exists(manifest_path)) {
      break
    }
    if (Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }

  expect_equal(state$state, "failed")
  expect_equal(state$exit_status, 124L)
  expect_true(file.exists(manifest_path))
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  expect_equal(manifest$model$name, "7b")
  expect_equal(manifest$checkpoint$kind, "lora")
  expect_equal(
    manifest$checkpoint$base_checkpoint$path,
    checkpoint_path(model)
  )
  expect_true(file.exists(file.path(job_path(job), "plan.pid")))
  expect_true(file.exists(pid_file))
  pid <- as.integer(readLines(pid_file, warn = FALSE))
  expect_false(isTRUE(tools::pskill(pid, signal = 0L)))
})

test_that("operation timeout escalates when the recipe ignores TERM", {
  workspace <- tempfile("bionemor-operation-timeout-term-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  pid_file <- tempfile("bionemor-timeout-term-pid-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  data <- evo2_dataset(
    train = c(first = "ACGT", second = "TGCA"),
    validation = c(validation = "AAAA"),
    test = c(test = "CCCC")
  )
  prepared <- evo2_prepare(
    data,
    model,
    compute,
    path = "datasets/timeout-term"
  )
  write_executable(
    file.path(bin, "torchrun"),
    c(
      "trap '' TERM",
      "printf '%s\\n' \"$$\" > \"$BIONEMOR_FAKE_PID_FILE\"",
      "while true; do sleep 60; done"
    )
  )
  withr::local_envvar(BIONEMOR_FAKE_PID_FILE = pid_file)

  job <- evo2_finetune(
    model,
    prepared,
    compute,
    steps = 1L,
    method = evo2_lora(),
    timeout = 0.1,
    name = "operation-timeout-term",
    async = TRUE
  )
  withr::defer({
    if (file.exists(pid_file)) {
      tools::pskill(
        as.integer(readLines(pid_file, warn = FALSE)),
        signal = 9L
      )
    }
  })
  deadline <- Sys.time() + 5
  repeat {
    state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
    if (state$state == "failed" &&
        file.exists(file.path(job_path(job), "manifest.json"))) {
      break
    }
    if (Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }

  expect_equal(state$state, "failed")
  expect_equal(state$exit_status, 124L)
  expect_true(file.exists(pid_file))
  pid <- as.integer(readLines(pid_file, warn = FALSE))
  expect_false(isTRUE(tools::pskill(pid, signal = 0L)))
})

test_that("detached prediction jobs remove tensors after recording provenance", {
  workspace <- tempfile("bionemor-detached-prediction-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  jobs <- list(
    score = evo2_score(
      model,
      "ACGT",
      compute,
      name = "detached-score",
      async = TRUE
    ),
    embedding = evo2_embed(
      model,
      "ACGT",
      compute,
      name = "detached-embedding",
      async = TRUE
    )
  )

  for (job in jobs) {
    manifest_path <- file.path(job_path(job), "manifest.json")
    deadline <- Sys.time() + 5
    while (!file.exists(manifest_path) && Sys.time() < deadline) {
      Sys.sleep(0.01)
    }

    expect_true(file.exists(manifest_path))
    state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
    expect_equal(state$state, "succeeded")
    tensors <- list.files(
      file.path(job_path(job), "upstream"),
      pattern = "[.]pt$",
      recursive = TRUE,
      full.names = TRUE
    )
    expect_length(tensors, 0L)
    manifest <- jsonlite::read_json(
      manifest_path,
      simplifyVector = FALSE
    )
    expect_true(any(vapply(
      manifest$upstream,
      function(file) endsWith(file$path, ".pt"),
      logical(1)
    )))
  }
})

test_that("prediction tensor cleanup failures fail the detached job", {
  permission_probe <- tempfile("bionemor-unlink-probe-")
  dir.create(permission_probe)
  probe_file <- file.path(permission_probe, "probe.pt")
  writeLines("probe", probe_file)
  Sys.chmod(permission_probe, "0500")
  can_unlink_without_directory_write <- unlink(probe_file) == 0L
  Sys.chmod(permission_probe, "0700")
  unlink(permission_probe, recursive = TRUE)
  skip_if(
    can_unlink_without_directory_write,
    "effective user can unlink files from an unwritable directory"
  )

  workspace <- tempfile("bionemor-prediction-cleanup-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  real_torchrun <- file.path(bin, "real-torchrun")
  file.copy(
    file.path(bin, "torchrun"),
    real_torchrun,
    copy.mode = TRUE
  )
  write_executable(
    file.path(bin, "torchrun"),
    c(
      paste(shQuote(real_torchrun), "\"$@\""),
      "output=",
      "previous=",
      "for argument in \"$@\"; do",
      "  if [[ \"$previous\" == \"--output-dir\" ]]; then",
      "    output=\"$argument\"",
      "    break",
      "  fi",
      "  previous=\"$argument\"",
      "done",
      "chmod a-w \"$output\""
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "prediction-cleanup-failure",
    async = TRUE
  )
  upstream <- file.path(job_path(job), "upstream", "predictions")
  withr::defer(Sys.chmod(upstream, "0700"))
  deadline <- Sys.time() + 5
  repeat {
    state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
    if (state$state == "failed" &&
        !file.exists(file.path(job_path(job), "finalizing"))) {
      break
    }
    if (Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }

  expect_equal(state$state, "failed")
  expect_equal(state$exit_status, 70L)
  manifest_path <- file.path(job_path(job), "manifest.json")
  expect_true(file.exists(manifest_path))
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  expect_equal(manifest$state, "failed")
  expect_equal(manifest$exit_status, 70L)
  expect_true(any(vapply(
    manifest$upstream,
    function(file) endsWith(file$path, ".pt"),
    logical(1)
  )))
  expect_true(length(list.files(
    upstream,
    pattern = "[.]pt$",
    full.names = TRUE
  )) > 0L)
  expect_match(
    paste(job_logs(job), collapse = "\n"),
    "failed to remove prediction tensors",
    fixed = TRUE
  )
})

test_that("terminal state waits for log redactors to exit", {
  workspace <- tempfile("bionemor-finalizing-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  awk <- Sys.which("awk")
  stopifnot(nzchar(awk))
  dir.create(workspace)
  fake_recipes_runtime(bin)
  write_executable(
    file.path(bin, "awk"),
    c(
      paste(shQuote(awk), "\"$@\""),
      "sleep 1"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "wait-for-log-redactors",
    async = TRUE
  )
  manifest <- file.path(job_path(job), "manifest.json")
  deadline <- Sys.time() + 5
  while (!file.exists(manifest) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }

  expect_true(file.exists(manifest))
  expect_true(file.exists(file.path(job_path(job), "finalizing")))
  expect_equal(job_status(job), "running")
  expect_no_error(job_cancel(job))
  expect_false(file.exists(file.path(job_path(job), "cancel.request")))
  job_wait(job, poll = 0.01, timeout = 5)
  expect_equal(job_status(job), "succeeded")
})

test_that("force cancellation recovers a hung terminal log drain", {
  workspace <- tempfile("bionemor-hung-redactor-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  redactor_children <- tempfile("bionemor-redactor-children-")
  awk <- Sys.which("awk")
  kill <- Sys.which("kill")
  stopifnot(nzchar(awk))
  stopifnot(nzchar(kill))
  group_is_alive <- function(pid) {
    processx::run(
      kill,
      c("-0", paste0("-", pid)),
      error_on_status = FALSE
    )$status == 0L
  }
  dir.create(workspace)
  fake_recipes_runtime(bin)
  write_executable(
    file.path(bin, "awk"),
    c(
      paste(shQuote(awk), "\"$@\""),
      "(trap '' TERM; while true; do sleep 60; done) &",
      "printf '%s\\n' \"$!\" >> \"$BIONEMOR_REDACTOR_CHILDREN\"",
      "wait \"$!\""
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_REDACTOR_CHILDREN = redactor_children
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "hung-log-redactors",
    async = TRUE
  )
  manifest_path <- file.path(job_path(job), "manifest.json")
  deadline <- Sys.time() + 5
  while (!file.exists(manifest_path) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
  runner_pid <- as.integer(state$backend_id)
  withr::defer(processx::run(
    kill,
    c("-KILL", paste0("-", runner_pid)),
    error_on_status = FALSE
  ))

  expect_true(file.exists(manifest_path))
  expect_equal(state$state, "succeeded")
  expect_true(file.exists(file.path(job_path(job), "finalizing")))
  expect_no_error(job_cancel(job, force = TRUE))
  expect_false(file.exists(file.path(job_path(job), "cancel.request")))
  expect_false(file.exists(file.path(job_path(job), "finalizing")))
  expect_false(isTRUE(tools::pskill(runner_pid, signal = 0L)))
  expect_false(group_is_alive(runner_pid))
  children <- as.integer(readLines(redactor_children, warn = FALSE))
  expect_length(children, 2L)
  child_is_running <- function(pid) {
    tryCatch(
      ps::ps_is_running(ps::ps_handle(pid)),
      no_such_process = function(error) FALSE,
      zombie_process = function(error) FALSE
    )
  }
  expect_false(any(vapply(
    children,
    child_is_running,
    logical(1)
  )))
  expect_equal(job_status(job), "succeeded")

  bounded <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "bounded-log-redactors",
    async = TRUE
  )
  bounded_runner <- as.integer(readLines(
    file.path(job_path(bounded), "runner.pid"),
    warn = FALSE
  ))
  withr::defer(processx::run(
    kill,
    c("-KILL", paste0("-", bounded_runner)),
    error_on_status = FALSE
  ))
  deadline <- Sys.time() + 10
  repeat {
    bounded_state <- job_status(bounded)
    if (bounded_state %in% c("succeeded", "failed", "cancelled")) {
      break
    }
    if (Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }
  expect_equal(bounded_state, "succeeded")
  expect_false(file.exists(file.path(job_path(bounded), "finalizing")))
  expect_false(isTRUE(tools::pskill(bounded_runner, signal = 0L)))
  expect_false(group_is_alive(bounded_runner))
})

test_that("force cancellation stops a hung foreground manifest finalizer", {
  workspace <- tempfile("bionemor-hung-finalizer-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_DELAY = "1"
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "hung-finalizer",
    async = TRUE
  )
  finalizer_pid_path <- file.path(job_path(job), "hung-finalizer.pid")
  writeLines(
    c(
      "args <- commandArgs(TRUE)",
      paste0(
        "writeLines(as.character(Sys.getpid()), ",
        "file.path(args[[1L]], 'hung-finalizer.pid'))"
      ),
      "repeat Sys.sleep(60)"
    ),
    file.path(job_path(job), "finalize-manifest.R")
  )
  deadline <- Sys.time() + 5
  while (!file.exists(finalizer_pid_path) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(file.exists(finalizer_pid_path))
  finalizer_pid <- as.integer(readLines(finalizer_pid_path, warn = FALSE))
  withr::defer(tools::pskill(finalizer_pid, signal = 9L))

  expect_no_error(job_cancel(bionemo_job(job_path(job)), force = TRUE))
  expect_false(isTRUE(tools::pskill(finalizer_pid, signal = 0L)))
  expect_false(file.exists(file.path(job_path(job), "finalizing")))
  expect_equal(job_status(bionemo_job(job_path(job))), "succeeded")
})

test_that("reopened cancellation never signals a mismatched process identity", {
  workspace <- tempfile("bionemor-stale-process-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_DELAY = "60"
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "stale-process-identity",
    async = TRUE
  )
  lifecycle_paths <- file.path(
    job_path(job),
    c(
      "runner.pid",
      "runner.identity.json",
      "plan.pid",
      "plan.identity.json"
    )
  )
  deadline <- Sys.time() + 5
  while (!all(file.exists(lifecycle_paths)) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(all(file.exists(lifecycle_paths)))
  if (!all(file.exists(lifecycle_paths))) {
    return(invisible())
  }
  identity_paths <- lifecycle_paths[endsWith(
    lifecycle_paths,
    ".identity.json"
  )]
  runner_pid <- as.integer(readLines(
    file.path(job_path(job), "runner.pid"),
    warn = FALSE
  ))
  plan_pid <- as.integer(readLines(
    file.path(job_path(job), "plan.pid"),
    warn = FALSE
  ))
  withr::defer({
    processx::run(
      "/bin/kill",
      c("-KILL", paste0("-", plan_pid)),
      error_on_status = FALSE
    )
    processx::run(
      "/bin/kill",
      c("-KILL", paste0("-", runner_pid)),
      error_on_status = FALSE
    )
  })
  for (path in identity_paths) {
    identity <- jsonlite::read_json(path, simplifyVector = FALSE)
    identity$create_time <- "0.000000"
    jsonlite::write_json(identity, path, auto_unbox = TRUE)
  }

  expect_no_error(job_cancel(bionemo_job(job_path(job)), force = TRUE))
  expect_true(isTRUE(tools::pskill(runner_pid, signal = 0L)))
  expect_true(isTRUE(tools::pskill(plan_pid, signal = 0L)))
  expect_false(file.exists(file.path(job_path(job), "cancel.request")))
  expect_equal(job_status(bionemo_job(job_path(job))), "failed")
})

test_that("reopened status reconciles a killed local process tree", {
  workspace <- tempfile("bionemor-killed-process-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_DELAY = "60"
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "killed-process-tree",
    async = TRUE
  )
  lifecycle_paths <- file.path(
    job_path(job),
    c(
      "runner.pid",
      "runner.identity.json",
      "plan.pid",
      "plan.identity.json"
    )
  )
  deadline <- Sys.time() + 5
  while (!all(file.exists(lifecycle_paths)) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(all(file.exists(lifecycle_paths)))
  if (!all(file.exists(lifecycle_paths))) {
    return(invisible())
  }
  runner_pid <- as.integer(readLines(
    file.path(job_path(job), "runner.pid"),
    warn = FALSE
  ))
  plan_pid <- as.integer(readLines(
    file.path(job_path(job), "plan.pid"),
    warn = FALSE
  ))
  processx::run(
    "/bin/kill",
    c("-KILL", paste0("-", plan_pid)),
    error_on_status = FALSE
  )
  processx::run(
    "/bin/kill",
    c("-KILL", paste0("-", runner_pid)),
    error_on_status = FALSE
  )

  reopened <- bionemo_job(job_path(job))
  expect_equal(job_status(reopened), "failed")
  state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
  expect_equal(state$exit_status, 137L)
  expect_true(file.exists(file.path(job_path(job), "manifest.json")))
  expect_false(file.exists(file.path(job_path(job), "finalizing")))
})

test_that("reopened status cleans children after runner-only death", {
  workspace <- tempfile("bionemor-dead-runner-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_DELAY = "60"
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "dead-runner",
    async = TRUE
  )
  stems <- c(
    "runner",
    "plan",
    "stdout-redactor",
    "stderr-redactor"
  )
  lifecycle_paths <- file.path(
    job_path(job),
    c(
      paste0(stems, ".pid"),
      paste0(stems, ".identity.json")
    )
  )
  deadline <- Sys.time() + 5
  while (!all(file.exists(lifecycle_paths)) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(all(file.exists(lifecycle_paths)))
  if (!all(file.exists(lifecycle_paths))) {
    return(invisible())
  }
  pids <- vapply(
    stems,
    function(stem) {
      as.integer(readLines(
        file.path(job_path(job), paste0(stem, ".pid")),
        warn = FALSE
      ))
    },
    integer(1)
  )
  withr::defer({
    for (pid in pids) {
      processx::run(
        "/bin/kill",
        c("-KILL", paste0("-", pid)),
        error_on_status = FALSE
      )
    }
  })
  processx::run(
    "/bin/kill",
    c("-KILL", as.character(pids[["runner"]])),
    error_on_status = FALSE
  )
  deadline <- Sys.time() + 2
  while (
    isTRUE(tools::pskill(pids[["runner"]], signal = 0L)) &&
      Sys.time() < deadline
  ) {
    Sys.sleep(0.01)
  }

  reopened <- bionemo_job(job_path(job))
  expect_equal(job_status(reopened), "failed")
  for (pid in pids[c("plan", "stdout-redactor", "stderr-redactor")]) {
    expect_false(isTRUE(tools::pskill(pid, signal = 0L)))
    expect_false(
      processx::run(
        "/bin/kill",
        c("-0", paste0("-", pid)),
        error_on_status = FALSE
      )$status == 0L
    )
  }
  state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
  expect_equal(state$exit_status, 137L)
  expect_true(file.exists(file.path(job_path(job), "manifest.json")))
  expect_false(file.exists(file.path(job_path(job), "finalizing")))
})

test_that("reopened status never signals a leaderless process group", {
  workspace <- tempfile("bionemor-leaderless-group-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  child_path <- tempfile("bionemor-plan-child-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_DELAY = "60",
    BIONEMOR_FAKE_PID_FILE = child_path
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "leaderless-group",
    async = TRUE
  )
  lifecycle_paths <- file.path(
    job_path(job),
    c(
      "runner.pid",
      "runner.identity.json",
      "plan.pid",
      "plan.identity.json"
    )
  )
  deadline <- Sys.time() + 5
  while (
    (!all(file.exists(lifecycle_paths)) || !file.exists(child_path)) &&
      Sys.time() < deadline
  ) {
    Sys.sleep(0.01)
  }
  expect_true(all(file.exists(lifecycle_paths)))
  expect_true(file.exists(child_path))
  if (!all(file.exists(lifecycle_paths)) || !file.exists(child_path)) {
    return(invisible())
  }
  runner_pid <- as.integer(readLines(
    file.path(job_path(job), "runner.pid"),
    warn = FALSE
  ))
  plan_pid <- as.integer(readLines(
    file.path(job_path(job), "plan.pid"),
    warn = FALSE
  ))
  child_pid <- as.integer(readLines(child_path, warn = FALSE))
  withr::defer({
    for (pid in c(child_pid, plan_pid, runner_pid)) {
      tools::pskill(pid, signal = 9L)
    }
  })
  processx::run(
    "/bin/kill",
    c("-KILL", as.character(plan_pid)),
    error_on_status = FALSE
  )
  processx::run(
    "/bin/kill",
    c("-KILL", as.character(runner_pid)),
    error_on_status = FALSE
  )
  deadline <- Sys.time() + 2
  while (
    (
      isTRUE(tools::pskill(plan_pid, signal = 0L)) ||
        isTRUE(tools::pskill(runner_pid, signal = 0L))
    ) &&
      Sys.time() < deadline
  ) {
    Sys.sleep(0.01)
  }
  expect_false(isTRUE(tools::pskill(plan_pid, signal = 0L)))
  expect_false(isTRUE(tools::pskill(runner_pid, signal = 0L)))
  expect_true(isTRUE(tools::pskill(child_pid, signal = 0L)))

  reopened <- bionemo_job(job_path(job))
  expect_equal(job_status(reopened), "failed")
  expect_true(isTRUE(tools::pskill(child_pid, signal = 0L)))
  expect_equal(
    processx::run(
      "/bin/kill",
      c("-0", paste0("-", plan_pid)),
      error_on_status = FALSE
    )$status,
    0L
  )
})

test_that("force cancellation before plan startup finalizes the run", {
  workspace <- tempfile("bionemor-cancel-startup-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  mkfifo <- Sys.which("mkfifo")
  stopifnot(nzchar(mkfifo))
  dir.create(workspace)
  fake_recipes_runtime(bin)
  write_executable(
    file.path(bin, "mkfifo"),
    c(
      "sleep 5",
      paste("exec", shQuote(mkfifo), "\"$@\"")
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "cancel-before-plan",
    async = TRUE
  )
  state_path <- file.path(job_path(job), "state.json")
  deadline <- Sys.time() + 2
  repeat {
    state <- jsonlite::read_json(state_path)
    if (state$state == "running" &&
        !file.exists(file.path(job_path(job), "plan.pid"))) {
      break
    }
    if (Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }
  runner_pid <- as.integer(state$backend_id)
  withr::defer(tools::pskill(runner_pid, signal = 9L))

  expect_equal(state$state, "running")
  expect_false(file.exists(file.path(job_path(job), "plan.pid")))
  expect_no_error(job_cancel(bionemo_job(job_path(job)), force = TRUE))
  state <- jsonlite::read_json(state_path)
  expect_equal(state$state, "cancelled")
  expect_true(file.exists(file.path(job_path(job), "manifest.json")))
  expect_false(isTRUE(tools::pskill(runner_pid, signal = 0L)))
})
