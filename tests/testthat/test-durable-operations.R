test_that("Evo 2 preprocessing jobs persist and reopen", {
  workspace <- tempfile("bionemor-preprocess-job-")
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
  data <- evo2_dataset(c(first = "ACGT", second = "TGCA"))

  job <- evo2_preprocess(
    data,
    model,
    compute,
    path = "datasets/preprocessed",
    async = TRUE
  )
  reopened <- bionemo_job(job_path(job))
  prepared <- job_wait(reopened, poll = 0.01, timeout = 10)

  expect_s3_class(prepared, "bionemor::Evo2Dataset")
  expect_true(prepared@prepared)
  expect_identical(prepared@provenance$run_path, job_path(job))

  request <- jsonlite::read_json(
    file.path(job_path(job), "request.json"),
    simplifyVector = FALSE
  )
  plan <- jsonlite::read_json(
    file.path(job_path(job), "plan.json"),
    simplifyVector = FALSE
  )
  manifest <- jsonlite::read_json(
    file.path(job_path(job), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_identical(request$schema_version, 3L)
  expect_identical(request$kind, "preprocess")
  expect_identical(request$expected_result$type, "preprocess")
  expect_identical(request$expected_result$result_version, 1L)
  expect_false("workflow" %in% names(request))
  expect_false("workflow" %in% names(plan$metadata))
  expect_identical(manifest$kind, "preprocess")
  expect_identical(
    manifest$result,
    list(
      type = "preprocess",
      version = 1L
    )
  )
  expect_null(manifest$workflow)
})
