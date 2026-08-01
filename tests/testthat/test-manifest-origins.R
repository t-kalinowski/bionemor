test_that("run manifests retain exact request and resolved values", {
  workspace <- tempfile("bionemor-manifest-origins-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  default_job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "origin-default",
    async = TRUE
  )
  job_wait(default_job, poll = 0.01, timeout = 10)
  default <- jsonlite::read_json(
    file.path(job_path(default_job), "manifest.json"),
    simplifyVector = FALSE
  )

  explicit_job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    temperature = 0.7,
    control = evo2_inference_control(precision = "auto"),
    name = "origin-explicit",
    async = TRUE
  )
  job_wait(explicit_job, poll = 0.01, timeout = 10)
  explicit <- jsonlite::read_json(
    file.path(job_path(explicit_job), "manifest.json"),
    simplifyVector = FALSE
  )

  recipe_job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    control = evo2_inference_control(
      mixed_precision_recipe = "bf16_mixed"
    ),
    name = "origin-explicit-recipe",
    async = TRUE
  )
  job_wait(recipe_job, poll = 0.01, timeout = 10)
  explicit_recipe <- jsonlite::read_json(
    file.path(job_path(recipe_job), "manifest.json"),
    simplifyVector = FALSE
  )

  expect_identical(default$request$temperature, explicit$request$temperature)
  expect_null(default$value_origins)
  expect_null(explicit$value_origins)
  expect_null(explicit_recipe$value_origins)
  expect_identical(default$request$model, "7b")
  expect_identical(default$request$input_source$source, "memory")
  expect_identical(default$request$control$precision, "auto")
  expect_identical(explicit$request$control$precision, "auto")
  expect_identical(
    explicit_recipe$request$control$mixed_precision_recipe,
    "bf16_mixed"
  )
  expect_identical(
    default$plan$metadata$operation,
    "generation"
  )
  expect_identical(
    default$plan$metadata$resolved_control$mixed_precision_recipe,
    "bf16_mixed"
  )
  expect_identical(default$precision$semantic, "auto")
  expect_identical(default$precision$resolved_recipe, "bf16_mixed")
  expect_named(default$precision, c("semantic", "resolved_recipe"))
})

test_that("checkpoint manifests retain requested and resolved precision", {
  workspace <- tempfile("bionemor-checkpoint-origin-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )
  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    source = make_mbridge_checkpoint(workspace, "source"),
    format = "mbridge",
    path = "checkpoints/default-precision",
    compute = compute
  )
  checkpoint_metadata <- checkpoint_manifest(checkpoint)
  manifest <- jsonlite::read_json(
    file.path(checkpoint_metadata$provenance$run_path, "manifest.json"),
    simplifyVector = FALSE
  )

  expect_identical(manifest$precision$semantic, "auto")
  expect_identical(manifest$precision$resolved_recipe, "bf16_mixed")
  expect_named(manifest$precision, c("semantic", "resolved_recipe"))
  request <- jsonlite::read_json(
    file.path(checkpoint_metadata$provenance$run_path, "request.json"),
    simplifyVector = FALSE
  )
  expect_null(request$request_origins)
  expect_identical(request$request$precision_request, "auto")
  expect_identical(request$request$precision, "bf16_mixed")
})
