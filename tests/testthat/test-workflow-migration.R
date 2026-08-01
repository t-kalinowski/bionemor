test_that("unsupported persisted run schemas fail clearly", {
  workspace <- tempfile("bionemor-workflow-v1-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )

  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_score(
    model,
    c(reference = "ACGT"),
    compute,
    async = TRUE,
    name = "schema-one-score"
  )
  job_wait(job, poll = 0.01, timeout = 10)

  request_path <- file.path(job_path(job), "request.json")
  request <- jsonlite::read_json(request_path, simplifyVector = FALSE)
  request$schema_version <- 1L
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
})
