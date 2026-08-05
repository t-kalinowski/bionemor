test_that("terminal jobs persist complete redacted run provenance", {
  workspace <- tempfile("bionemor-manifest-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  secret <- "manifest-credential-must-not-persist"
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_COMPLETION = "ACNT",
    NGC_API_KEY = secret
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace,
    config = list(site_note = secret)
  )
  capabilities <- bionemo_capabilities(compute, refresh = TRUE)
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace,
    config = list(site_note = secret, capabilities = capabilities)
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "manifest-provenance",
    async = TRUE
  )
  state <- job_status(job)
  deadline <- Sys.time() + 10
  while (!state %in% c("succeeded", "failed", "cancelled") &&
    Sys.time() < deadline) {
    Sys.sleep(0.01)
    state <- job_status(job)
  }
  expect_identical(state, "succeeded")
  detached <- jsonlite::read_json(
    file.path(job_path(job), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_true(any(grepl("non-ACGT", unlist(detached$warnings))))

  result <- job_result(job)
  expect_s3_class(result, "evo2_generation")

  run_path <- job_path(job)
  manifest_path <- file.path(run_path, "manifest.json")
  expect_true(file.exists(manifest_path))
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  expect_equal(manifest$schema_version, 1L)
  expect_equal(manifest$id, "manifest-provenance")
  expect_equal(manifest$kind, "generation")
  expect_equal(manifest$state, "succeeded")
  expect_equal(manifest$exit_status, 0L)
  expect_true(is.list(manifest$execution$resolved_control))
  expect_null(manifest$plan)

  expect_equal(manifest$recipe$revision, evo2_recipe()@revision)
  expect_equal(manifest$recipe$version, evo2_recipe()@recipe_version)
  expect_equal(
    manifest$dockerfile$git_blob,
    "93ee109724fb44effb35262c0cd2279707c7c3a6"
  )
  expect_equal(manifest$helper$version, "0.2.0")
  expect_match(manifest$helper$sha256, "^[0-9a-f]{64}$")
  expect_equal(manifest$model$name, "7b")
  expect_equal(manifest$model$model_size, "evo2_7b")
  expect_equal(manifest$checkpoint$format, "mbridge")
  expect_equal(manifest$checkpoint$digest$algorithm, "md5")
  expect_match(manifest$checkpoint$digest$value, "^[0-9a-f]{32}$")
  expect_equal(
    manifest$tokenizer$identity,
    "tokenizers/nucleotide_fast_tokenizer_512"
  )
  expect_equal(
    manifest$tokenizer$revision,
    evo2_recipe()@revision
  )
  expect_equal(manifest$precision$semantic, "auto")
  expect_equal(manifest$precision$resolved_recipe, "bf16_mixed")
  expect_named(manifest$precision, c("semantic", "resolved_recipe"))
  expect_equal(manifest$runtime$backend, "local")
  expect_equal(manifest$runtime$engine, "external")
  expect_equal(manifest$runtime$details$megatron_core, "0.13.0")
  expect_equal(manifest$compute$gpus, 1L)
  expect_equal(manifest$compute$nodes, 1L)
  expect_true(is.character(manifest$timing$started_at))
  expect_true(is.character(manifest$timing$ended_at))
  expect_true(manifest$timing$duration_seconds >= 0)

  expect_true(length(manifest$inputs) >= 1L)
  expect_true(length(manifest$outputs) >= 1L)
  files <- c(manifest$inputs, manifest$outputs)
  expect_true(all(vapply(
    files,
    function(file) {
      identical(file$digest$algorithm, "md5") &&
        is.character(file$digest$value) &&
        nzchar(file$digest$value)
    },
    logical(1)
  )))
  expect_true(any(grepl("non-ACGT", unlist(manifest$warnings))))

  persisted <- list.files(run_path, recursive = TRUE, full.names = TRUE)
  persisted <- persisted[!dir.exists(persisted)]
  contents <- unlist(
    lapply(persisted, readLines, warn = FALSE),
    use.names = FALSE
  )
  expect_false(any(grepl(secret, contents, fixed = TRUE)))

  unlink(manifest_path)
  expect_equal(job_status(bionemo_job(run_path)), "succeeded")
  expect_true(file.exists(manifest_path))
})

test_that("observing a failed job writes its terminal manifest", {
  workspace <- tempfile("bionemor-failed-manifest-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_DELAY = "not-a-number"
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
    name = "failed-manifest",
    async = TRUE
  )
  expect_error(
    job_wait(job, poll = 0.01, timeout = 10),
    "job failed"
  )
  manifest <- jsonlite::read_json(
    file.path(job_path(job), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_equal(manifest$state, "failed")
  expect_true(manifest$exit_status != 0L)
})
