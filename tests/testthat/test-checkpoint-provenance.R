test_that("custom pickle checkpoint manifests record explicit trust", {
  workspace <- tempfile("bionemor-checkpoint-trust-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  revision <- "0123456789abcdef0123456789abcdef01234567"
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

  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    source = "hf://example/custom-evo2",
    format = "savanna",
    revision = revision,
    path = "checkpoints/trusted",
    compute = compute,
    trust = TRUE
  )
  manifest <- checkpoint_manifest(checkpoint)

  expect_false("tokenizer_revision.1" %in% names(manifest))
  expect_identical(manifest$source_trust, "explicit")
  expect_false(manifest$source_verified)

  model <- evo2("7b", checkpoint = checkpoint)
  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "trusted-checkpoint-generation",
    async = TRUE
  )
  job_wait(job, poll = 0.01, timeout = 10)
  run_manifest <- jsonlite::read_json(
    file.path(job_path(job), "manifest.json"),
    simplifyVector = FALSE
  )

  expect_identical(run_manifest$checkpoint$source_trust, "explicit")
  expect_false(run_manifest$checkpoint$source_verified)
})

test_that("copying a BioNeMo checkpoint writes destination metadata", {
  workspace <- tempfile("bionemor-checkpoint-copy-")
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
  source <- evo2_checkpoint(
    evo2("7b"),
    source = make_mbridge_checkpoint(workspace, "source"),
    format = "mbridge",
    path = "checkpoints/original",
    compute = compute
  )

  copied <- evo2_checkpoint(
    evo2("7b"),
    source = source,
    path = "checkpoints/copied",
    compute = compute
  )

  source_manifest <- checkpoint_manifest(source)
  copied_manifest <- checkpoint_manifest(copied)
  expect_equal(copied_manifest$source, checkpoint_path(source))
  expect_false(identical(
    copied_manifest$provenance$run_path,
    source_manifest$provenance$run_path
  ))
})

test_that("detached checkpoint finalization digests completed output", {
  workspace <- tempfile("bionemor-checkpoint-output-digest-")
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
  source <- make_mbridge_checkpoint(workspace, "source")

  job <- evo2_checkpoint(
    evo2("7b"),
    source = source,
    format = "mbridge",
    path = "checkpoints/copied",
    compute = compute,
    async = TRUE
  )
  run_manifest_path <- file.path(job_path(job), "manifest.json")
  deadline <- Sys.time() + 10
  while (!file.exists(run_manifest_path) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(file.exists(run_manifest_path))
  detached <- jsonlite::read_json(
    run_manifest_path,
    simplifyVector = FALSE
  )
  state <- job_status(job)
  deadline <- Sys.time() + 10
  while (!state %in% c("succeeded", "failed", "cancelled") &&
    Sys.time() < deadline) {
    Sys.sleep(0.01)
    state <- job_status(job)
  }
  expect_identical(state, "succeeded")
  observed <- jsonlite::read_json(
    run_manifest_path,
    simplifyVector = FALSE
  )

  checkpoint <- job_wait(job, poll = 0.01, timeout = 10)
  stored <- checkpoint_manifest(checkpoint)$checkpoint_digest
  completed <- jsonlite::read_json(
    run_manifest_path,
    simplifyVector = FALSE
  )

  expect_match(detached$checkpoint$digest$value, "^[0-9a-f]{32}$")
  expect_identical(observed$checkpoint$digest, detached$checkpoint$digest)
  expect_identical(detached$checkpoint$digest$value, stored)
  expect_identical(completed$checkpoint$digest$value, stored)
  expect_identical(
    completed$checkpoint$kind,
    checkpoint_manifest(checkpoint)$kind
  )
  expect_identical(
    completed$checkpoint$revision,
    checkpoint_manifest(checkpoint)$source_revision
  )
})

test_that("detached Vortex finalization preserves file and config identity", {
  workspace <- tempfile("bionemor-vortex-output-digest-")
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
  model <- evo2(
    "7b",
    checkpoint = make_mbridge_checkpoint(workspace, "source")
  )

  job <- evo2_export(
    model,
    path = "exports/model.pt",
    compute = compute,
    async = TRUE
  )
  run_manifest_path <- file.path(job_path(job), "manifest.json")
  deadline <- Sys.time() + 10
  while (!file.exists(run_manifest_path) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(file.exists(run_manifest_path))
  detached <- jsonlite::read_json(
    run_manifest_path,
    simplifyVector = FALSE
  )
  state <- job_status(job)
  deadline <- Sys.time() + 10
  while (!state %in% c("succeeded", "failed", "cancelled") &&
    Sys.time() < deadline) {
    Sys.sleep(0.01)
    state <- job_status(job)
  }
  expect_identical(state, "succeeded")
  observed <- jsonlite::read_json(
    run_manifest_path,
    simplifyVector = FALSE
  )

  checkpoint <- job_wait(job, poll = 0.01, timeout = 10)
  stored <- checkpoint_manifest(checkpoint)$checkpoint_digest
  completed <- jsonlite::read_json(
    run_manifest_path,
    simplifyVector = FALSE
  )

  expect_identical(checkpoint_manifest(checkpoint)$format, "vortex")
  expect_match(detached$checkpoint$digest$value, "^[0-9a-f]{32}$")
  expect_identical(observed$checkpoint$digest, detached$checkpoint$digest)
  expect_identical(detached$checkpoint$digest$value, stored)
  expect_identical(completed$checkpoint$digest$value, stored)
  expect_false(identical(
    stored,
    unname(tools::md5sum(checkpoint_path(checkpoint)))
  ))
})

test_that("checkpoint reuse matches precision and tokenizer identity", {
  workspace <- tempfile("bionemor-checkpoint-identity-")
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
  source <- make_mbridge_checkpoint(workspace, "source")
  tokenizer_a <- file.path(workspace, "tokenizer-a")
  tokenizer_b <- file.path(workspace, "tokenizer-b")
  dir.create(tokenizer_a)
  dir.create(tokenizer_b)
  writeLines("a", file.path(tokenizer_a, "tokenizer.json"))
  writeLines("b", file.path(tokenizer_b, "tokenizer.json"))
  revision_a <- strrep("a", 40L)
  revision_b <- strrep("b", 40L)
  model_a <- evo2("7b", config = list(tokenizer_revision = revision_a))

  checkpoint <- evo2_checkpoint(
    model_a,
    source = source,
    format = "mbridge",
    path = "checkpoints/reused",
    compute = compute,
    tokenizer = tokenizer_a,
    precision = "bf16"
  )
  manifest <- checkpoint_manifest(checkpoint)

  expect_equal(manifest$tokenizer_identity, normalizePath(tokenizer_a))
  expect_equal(manifest$tokenizer_revision, revision_a)
  expect_equal(manifest$mixed_precision_recipe, "bf16_mixed")
  expect_error(
    evo2_checkpoint(
      model_a,
      source = source,
      format = "mbridge",
      path = "checkpoints/reused",
      compute = compute,
      tokenizer = tokenizer_a,
      precision = "fp8"
    ),
    "mixed_precision_recipe"
  )
  expect_error(
    evo2_checkpoint(
      model_a,
      source = source,
      format = "mbridge",
      path = "checkpoints/reused",
      compute = compute,
      tokenizer = tokenizer_b,
      precision = "bf16"
    ),
    "tokenizer_identity"
  )
  expect_error(
    evo2_checkpoint(
      evo2("7b", config = list(tokenizer_revision = revision_b)),
      source = source,
      format = "mbridge",
      path = "checkpoints/reused",
      compute = compute,
      tokenizer = tokenizer_a,
      precision = "bf16"
    ),
    "tokenizer_revision"
  )
})
