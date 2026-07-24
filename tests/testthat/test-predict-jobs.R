test_that("batch prediction maps generation, score, and raw commands", {
  workspace <- tempfile("bionemor-predict-command-")
  bin <- tempfile("bionemor-slurm-")
  dir.create(workspace)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "python",
    workspace = workspace,
    gpus = 2L
  )
  model <- evo2("7b", checkpoint = checkpoint)

  response <- withr::with_options(
    list(OutDec = ","),
    predict(
      model,
      "ACGT",
      type = "response",
      compute = compute,
      async = TRUE,
      name = "response-job",
      num_tokens = 8L,
      temperature = 0.5,
      top_k = 2L,
      top_p = 0.8,
      precision = "fp8"
    )
  )
  expect_match(response@command, "infer_evo2")
  expect_match(response@command, "--max-new-tokens 8")
  expect_match(response@command, "--temperature 0.5")
  expect_match(response@command, "--top-k 2")
  expect_match(response@command, "--top-p 0.8")
  expect_match(response@command, "--vortex-style-fp8 True")
  expect_match(response@command, "--flash-decode=")
  expect_false(grepl(",", response@command, fixed = TRUE))

  score <- predict(
    model,
    c(first = "ACGT", second = "TGCA"),
    type = "score",
    compute = compute,
    async = TRUE,
    name = "score-job",
    reduction = "sum"
  )
  expect_match(score@command, "predict_evo2")
  expect_match(score@command, "--output-log-prob-seqs")
  expect_match(score@command, "--log-prob-collapse-option sum")
  expect_match(score@command, "--tensor-parallel-size 2")
  expect_match(score@command, "--pipeline-model-parallel-size 1")
  expect_match(score@command, "--context-parallel-size 1")
  expect_match(score@command, "materialize-evo2.py.*score")

  raw <- predict(
    model,
    "ACGT",
    type = "raw",
    compute = compute,
    async = TRUE,
    name = "raw-job"
  )
  expect_match(raw@command, "predict_evo2")
  expect_false(grepl("--output-log-prob-seqs", raw@command, fixed = TRUE))

  expect_error(
    predict(model, "ACGT", type = "representation", compute = compute),
    "response.*score.*raw"
  )
  expect_error(
    predict(
      evo2("1b"),
      "ACGT",
      type = "raw",
      compute = compute,
      async = TRUE
    ),
    "explicit checkpoint"
  )
  expect_error(
    predict(
      model,
      "ACGT",
      type = "response",
      compute = compute,
      precision = "bf16",
      extra_args = "--fp8"
    ),
    "extra_args.*precision"
  )
  expect_error(
    predict(
      model,
      "ACGT",
      type = "response",
      compute = compute,
      extra_args = "--ckpt-dir /another/checkpoint"
    ),
    "extra_args.*checkpoint"
  )
  expect_error(
    predict(
      model,
      "ACGT",
      type = "score",
      compute = compute,
      extra_args = "--tensor-parallel-size=1"
    ),
    "extra_args.*parallelism"
  )
  expect_error(
    predict(
      model,
      "ACGT",
      type = "raw",
      compute = compute,
      async = TRUE,
      name = "reserved-predict-argument",
      unsupported = TRUE
    ),
    "reserved"
  )
  expect_error(
    predict(
      model,
      "ACGT",
      type = "raw",
      compute = compute,
      async = TRUE,
      name = ".."
    ),
    "safe job name"
  )
})

test_that("prediction materializes scores and generated sequences into R", {
  workspace <- tempfile("bionemor-predict-result-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)
  compute <- bionemo_compute(workspace = workspace)
  model <- evo2("1b", checkpoint = checkpoint)

  score <- predict(
    model,
    c(first = "ACGT", second = "TGCA"),
    type = "score",
    compute = compute,
    name = "score-result",
    reduction = "mean"
  )
  expect_s3_class(score, "bionemor::BioNeMoPrediction")
  expect_equal(score@type, "score")
  expect_equal(
    score@data$id,
    c("first", "second")
  )
  expect_equal(score@data$score, c(-1.25, -2.5))
  expect_equal(score@data$reduction, c("mean", "mean"))
  expect_equal(score@data$checkpoint, rep(normalizePath(checkpoint), 2L))

  response <- predict(
    model,
    c(first = "ACGT", second = "TGCA"),
    type = "response",
    compute = compute,
    name = "response-result",
    num_tokens = 4L
  )
  expect_s3_class(response, "bionemor::BioNeMoPrediction")
  expect_equal(
    response@data,
    c(first = "generated-ACGT", second = "generated-TGCA")
  )
})

test_that("raw tensors remain file-backed", {
  workspace <- tempfile("bionemor-raw-result-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)

  result <- predict(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    type = "raw",
    compute = bionemo_compute(workspace = workspace),
    name = "raw-result"
  )

  expect_s3_class(result@data, "bionemor::BioNeMoArtifact")
  expect_equal(result@data@format, "pytorch")
  expect_true(file.exists(result@data@path))
  expect_match(result@data@path, "[.]pt$")
})

test_that("local job lifecycle supports logs, cancellation, and wait-only timeouts", {
  workspace <- tempfile("bionemor-job-lifecycle-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_INFER_SLEEP = "5"
  )
  checkpoint <- make_checkpoint_dir(workspace)
  compute <- bionemo_compute(workspace = workspace)

  job <- predict(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    type = "response",
    compute = compute,
    async = TRUE,
    name = "wait-timeout"
  )
  expect_equal(job_status(job), "running")
  expect_error(job_wait(job, poll = 0.01, timeout = 0.05), "timed out waiting")
  expect_equal(job_status(job), "running")

  job <- job_cancel(job)
  expect_equal(job_status(job, refresh = FALSE), "cancelled")

  withr::local_envvar(BIONEMOR_INFER_SLEEP = "0")
  completed <- predict(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    type = "response",
    compute = compute,
    async = TRUE,
    name = "logged-job"
  )
  result <- job_wait(completed, poll = 0.01)
  expect_s3_class(result, "bionemor::BioNeMoPrediction")
  expect_match(paste(job_logs(completed), collapse = "\n"), "generated")
  expect_identical(job_result(completed), result)
})

test_that("a fit operation timeout terminates its local process", {
  workspace <- tempfile("bionemor-operation-timeout-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_TRAIN_SLEEP = "5"
  )
  checkpoint <- make_checkpoint_dir(workspace)
  job <- generics::fit(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    compute = bionemo_compute(workspace = workspace),
    steps = 1L,
    name = "operation-timeout",
    timeout = 0.05,
    async = TRUE
  )

  expect_match(
    job@command,
    "timeout --signal=TERM --kill-after=15s 0.05s",
    fixed = TRUE
  )
  deadline <- Sys.time() + 1
  while (job@process$is_alive() && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_false(job@process$is_alive())
  expect_error(job_wait(job, poll = 0.01, timeout = 1), "operation timed out")
})

test_that("Slurm job states and cancellation preserve scheduler semantics", {
  workspace <- tempfile("bionemor-slurm-state-")
  bin <- tempfile("bionemor-slurm-")
  cancel_args <- tempfile("bionemor-cancel-")
  dir.create(workspace)
  fake_slurm_runtime(bin)
  checkpoint <- make_checkpoint_dir(workspace)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE = "RUNNING",
    BIONEMOR_CANCEL_ARGS = cancel_args
  )
  job <- predict(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    type = "raw",
    compute = bionemo_compute(
      backend = "slurm",
      workspace = workspace
    ),
    async = TRUE,
    name = "slurm-state"
  )

  expect_equal(job_status(job), "running")
  job <- job_cancel(job)
  expect_equal(job_status(job, refresh = FALSE), "cancelled")
  expect_equal(readLines(cancel_args), "123")

  withr::local_envvar(BIONEMOR_FAKE_STATE = "FUTURE_STATE")
  unknown <- predict(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    type = "raw",
    compute = bionemo_compute(
      backend = "slurm",
      workspace = workspace
    ),
    async = TRUE,
    name = "slurm-unknown"
  )
  expect_error(job_status(unknown, refresh = TRUE), "FUTURE_STATE")
})

test_that("job construction covers Docker and Slurm Apptainer", {
  workspace <- tempfile("bionemor-engine-commands-")
  slurm_bin <- tempfile("bionemor-slurm-")
  docker_bin <- tempfile("bionemor-docker-")
  dir.create(workspace)
  dir.create(docker_bin)
  fake_slurm_runtime(slurm_bin)
  write_executable(
    file.path(docker_bin, "docker"),
    c(
      "if [[ \"${1:-}\" == \"stop\" ]]; then exit 0; fi",
      "sleep 5"
    )
  )
  checkpoint <- make_checkpoint_dir(workspace)
  model <- evo2("1b", checkpoint = checkpoint)

  withr::local_envvar(
    PATH = paste(
      docker_bin,
      slurm_bin,
      Sys.getenv("PATH"),
      sep = .Platform$path.sep
    )
  )
  docker_job <- predict(
    model,
    "ACGT",
    type = "raw",
    compute = bionemo_compute(
      workspace = workspace,
      engine = "container",
      image = "bionemo:2.6.3"
    ),
    async = TRUE,
    name = "docker-job"
  )
  expect_match(docker_job@command, "docker run --rm --gpus all")
  docker_job <- job_cancel(docker_job)

  apptainer_job <- predict(
    model,
    "ACGT",
    type = "raw",
    compute = bionemo_compute(
      backend = "slurm",
      workspace = workspace,
      engine = "container",
      image = "/images/bionemo.sif"
    ),
    async = TRUE,
    name = "apptainer-job"
  )
  expect_match(apptainer_job@command, "apptainer exec --nv")
  script <- paste(readLines(apptainer_job@metadata$script), collapse = "\n")
  expect_match(script, "#SBATCH --nodes 1")
  expect_match(script, "#SBATCH --gpus 1")
})

test_that("Docker cancellation reports a container stop failure", {
  workspace <- tempfile("bionemor-docker-cancel-")
  bin <- tempfile("bionemor-docker-bin-")
  dir.create(workspace)
  dir.create(bin)
  write_executable(
    file.path(bin, "docker"),
    c(
      "if [[ \"${1:-}\" == \"stop\" ]]; then",
      "  printf 'container remained running\\n' >&2",
      "  exit 17",
      "fi",
      "sleep 5"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)
  job <- predict(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    type = "raw",
    compute = bionemo_compute(
      workspace = workspace,
      engine = "container",
      image = "bionemo:2.6.3"
    ),
    async = TRUE,
    name = "docker-stop-failure"
  )

  expect_error(job_cancel(job), "container remained running")
  expect_false(job@process$is_alive())
  expect_equal(job_status(job, refresh = FALSE), "running")
})

test_that("jobs do not inherit or record NGC credentials", {
  workspace <- tempfile("bionemor-job-credentials-")
  bin <- tempfile("bionemor-runtime-")
  secret <- "bionemor-job-secret"
  cli_secret <- "bionemor-cli-secret"
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    NGC_API_KEY = secret,
    NGC_CLI_API_KEY = cli_secret,
    BIONEMOR_ECHO_NGC = "true"
  )
  checkpoint <- make_checkpoint_dir(workspace)

  job <- predict(
    evo2("1b", checkpoint = checkpoint),
    "ACGT",
    type = "response",
    compute = bionemo_compute(workspace = workspace),
    async = TRUE,
    name = "credential-free-job"
  )
  job_wait(job, poll = 0.01)

  raw_log <- paste(readLines(job@log, warn = FALSE), collapse = "\n")
  public_log <- paste(job_logs(job), collapse = "\n")
  script <- paste(readLines(job@metadata$script), collapse = "\n")
  exposed <- paste(job@command, raw_log, public_log, script, collapse = "\n")
  expect_false(grepl(secret, exposed, fixed = TRUE))
  expect_false(grepl(cli_secret, exposed, fixed = TRUE))
})
