test_that("detached generation completes portable outputs before succeeding", {
  workspace <- tempfile("bionemor-detached-generation-")
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

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "detached-portable-outputs",
    async = TRUE
  )
  run_path <- job_path(job)
  deadline <- Sys.time() + 5
  repeat {
    state <- job_status(bionemo_job(run_path))
    if (state %in% c("succeeded", "failed", "cancelled")) {
      break
    }
    if (Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }

  expect_identical(state, "succeeded")
  expected <- file.path(
    run_path,
    "outputs",
    c("generation.jsonl", "generated.fasta", "validation.json")
  )
  expect_true(all(file.exists(expected)))
  manifest <- jsonlite::read_json(
    file.path(run_path, "manifest.json"),
    simplifyVector = FALSE
  )
  expect_setequal(
    vapply(manifest$outputs, `[[`, character(1), "path"),
    file.path("outputs", basename(expected))
  )

  unlink(file.path(run_path, "upstream", "generation.jsonl"))
  result <- job_result(bionemo_job(run_path))
  expect_s3_class(result, "evo2_generation")
  expect_identical(result$id, "seq_1::1")
  expect_identical(result$completion, "ACGT")
})
