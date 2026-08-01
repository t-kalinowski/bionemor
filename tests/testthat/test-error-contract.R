test_that("domain failures expose stable BioNeMo condition codes", {
  error <- expect_error(
    evo2("not-a-model"),
    class = "BN_MODEL_UNKNOWN"
  )

  expect_s3_class(error, "bionemor_error")
  expect_identical(error$code, "BN_MODEL_UNKNOWN")
  expect_match(error$message, "supported sizes")
})

test_that("checkpoint source failures expose their stable condition code", {
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = tempfile("bionemor-condition-")
  )

  error <- expect_error(
    evo2_checkpoint(
      evo2("7b"),
      source = "ftp://example.invalid/evo2",
      path = "checkpoints/evo2",
      compute = compute
    ),
    class = "BN_CHECKPOINT_SOURCE"
  )

  expect_s3_class(error, "bionemor_error")
  expect_identical(error$code, "BN_CHECKPOINT_SOURCE")
  expect_identical(error$operation, "checkpoint")
  expect_identical(error$model, "7b")
})

test_that("incomplete checkpoints expose their stable condition code", {
  path <- tempfile("bionemor-incomplete-checkpoint-")

  error <- expect_error(
    checkpoint_manifest(path),
    class = "BN_CHECKPOINT_INCOMPLETE"
  )

  expect_s3_class(error, "bionemor_error")
  expect_identical(error$code, "BN_CHECKPOINT_INCOMPLETE")
  expect_identical(
    error$checkpoint,
    file.path(normalizePath(dirname(path)), basename(path))
  )
})

test_that("inference context failures expose BN_CONTEXT_LIMIT before launch", {
  workspace <- tempfile("bionemor-context-limit-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(
    workspace,
    model_size = "evo2_7b_base"
  )
  model <- evo2("7b-base", checkpoint = checkpoint)
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )

  generation_error <- expect_error(
    evo2_generate(
      model,
      c(prompt = strrep("A", 8190L)),
      compute,
      num_tokens = 3L
    ),
    class = "BN_CONTEXT_LIMIT"
  )
  expect_identical(generation_error$operation, "generation")
  expect_identical(generation_error$model, "7b-base")
  expect_identical(generation_error$request_id, "prompt")
  expect_identical(generation_error$context_length, 8192L)
  expect_identical(generation_error$sequence_length, 8190L)
  expect_identical(generation_error$additional_tokens, 3L)
  expect_identical(generation_error$required_length, 8193L)

  too_long <- c(sequence = strrep("A", 8193L))
  operations <- list(
    score = function() evo2_score(model, too_long, compute),
    profile = function() {
      evo2_profile(
        model,
        too_long,
        compute,
        output = "profiles/context-limit.parquet"
      )
    },
    embedding = function() evo2_embed(model, too_long, compute)
  )
  for (operation in names(operations)) {
    error <- expect_error(
      operations[[operation]](),
      class = "BN_CONTEXT_LIMIT"
    )
    expect_identical(error$operation, operation)
    expect_identical(error$request_id, "sequence")
    expect_identical(error$context_length, 8192L)
    expect_identical(error$sequence_length, 8193L)
    expect_identical(error$additional_tokens, 0L)
    expect_identical(error$required_length, 8193L)
  }
  expect_false(file.exists(log))
})

test_that("invalid public sequence input exposes BN_INVALID_SEQUENCE", {
  workspace <- tempfile("bionemor-invalid-sequence-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )

  error <- expect_error(
    evo2_score(model, c(sequence = "AC?T"), compute),
    class = "BN_INVALID_SEQUENCE"
  )
  expect_identical(error$operation, "sequence-input")
  expect_identical(error$request_id, "sequence")
  expect_false(file.exists(log))
})

test_that("malformed generation output exposes BN_OUTPUT_SCHEMA metadata", {
  workspace <- tempfile("bionemor-generation-schema-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )
  job <- evo2_generate(
    model,
    c(prompt = "ACGT"),
    compute,
    num_tokens = 4L,
    async = TRUE
  )

  deadline <- Sys.time() + 10
  while (job_status(job) != "succeeded" && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_identical(job_status(job), "succeeded")
  writeLines(
    '{"id":"prompt::1"}',
    file.path(job_path(job), "outputs", "generation.jsonl")
  )

  error <- expect_error(
    job_result(bionemo_job(job_path(job))),
    class = "BN_OUTPUT_SCHEMA"
  )
  expect_identical(error$operation, "generation")
  expect_identical(error$request_id, "prompt::1")
  expect_identical(error$model, "7b")
  expect_identical(error$run_path, job_path(job))
  expect_identical(
    error$log_paths,
    file.path(job_path(job), c("stdout.log", "stderr.log"))
  )
})
