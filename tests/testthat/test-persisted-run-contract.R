test_that("unsupported persisted run schemas fail clearly", {
  workspace <- tempfile("bionemor-old-run-")
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
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_score(
    model,
    c(reference = "ACGT"),
    compute,
    async = TRUE,
    name = "old-schema-score"
  )
  job_wait(job, poll = 0.01, timeout = 10)

  request_path <- file.path(job_path(job), "request.json")
  request <- jsonlite::read_json(request_path, simplifyVector = FALSE)
  request$schema_version <- 2L
  jsonlite::write_json(
    request,
    request_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  error <- expect_error(
    bionemo_job(job_path(job)),
    class = "BN_PROTOCOL"
  )
  expect_identical(error$operation, "job-reopen")

  request$schema_version <- 3L
  jsonlite::write_json(
    request,
    request_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  plan_path <- file.path(job_path(job), "plan.json")
  plan <- jsonlite::read_json(plan_path, simplifyVector = FALSE)
  plan$schema_version <- 1L
  jsonlite::write_json(
    plan,
    plan_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )

  error <- expect_error(
    bionemo_job(job_path(job)),
    class = "BN_PROTOCOL"
  )
  expect_identical(error$operation, "job-reopen")
})
