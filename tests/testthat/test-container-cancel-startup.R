test_that("container cancellation stops the process group during startup", {
  workspace <- tempfile("bionemor-container-cancel-")
  bin <- tempfile("bionemor-bin-")
  pid_file <- tempfile("bionemor-container-pid-")
  cancel_log <- tempfile("bionemor-container-cancel-log-")
  kill_log <- tempfile("bionemor-process-group-cancel-log-")
  dir.create(workspace)
  dir.create(bin)
  write_executable(
    file.path(bin, "docker"),
    c(
      "if [[ \"${1:-}\" == \"run\" ]]; then",
      "  printf '%s\\n' \"$$\" > \"$BIONEMOR_CONTAINER_PID_FILE\"",
      "  exec sleep 60",
      "fi",
      "printf '%s\\n' \"$@\" > \"$BIONEMOR_CONTAINER_CANCEL_LOG\"",
      "printf 'No such container\\n' >&2",
      "exit 1"
    )
  )
  write_executable(
    file.path(bin, "kill"),
    c(
      "printf '%s\\n' \"$@\" > \"$BIONEMOR_PROCESS_GROUP_CANCEL_LOG\"",
      "exit 1"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_CONTAINER_PID_FILE = pid_file,
    BIONEMOR_CONTAINER_CANCEL_LOG = cancel_log,
    BIONEMOR_PROCESS_GROUP_CANCEL_LOG = kill_log
  )
  compute <- bionemo_compute(
    workspace = workspace,
    image = paste0("example/evo2@sha256:", strrep("a", 64L)),
    config = list(
      capabilities = list(
        runtime = list(
          gpu_count = 1L,
          gpus = data.frame(compute_capability_major = 9L)
        )
      )
    )
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
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
  deadline <- Sys.time() + 2
  while (!file.exists(pid_file) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(file.exists(pid_file))
  container_pid <- as.integer(readLines(pid_file, warn = FALSE))
  alive <- function(pid) {
    isTRUE(tools::pskill(pid, signal = 0L))
  }

  reopened <- bionemo_job(job_path(job))
  expect_error(
    job_cancel(reopened, force = TRUE),
    "failed to stop local container or process group"
  )
  expect_false(file.exists(file.path(job_path(job), "cancel.request")))
  expect_true(alive(container_pid))
  expect_true(file.exists(kill_log))

  write_executable(
    file.path(bin, "kill"),
    "exec /bin/kill \"$@\""
  )
  expect_no_error(job_cancel(reopened, force = TRUE))
  deadline <- Sys.time() + 2
  while (alive(container_pid) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }

  expect_false(alive(container_pid))
  expect_equal(
    readLines(cancel_log, warn = FALSE),
    c("kill", paste0("bionemor-", basename(job_path(job))))
  )
  expect_equal(job_status(reopened), "cancelled")
})
