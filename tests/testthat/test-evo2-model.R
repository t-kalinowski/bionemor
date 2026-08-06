test_that("evo2_models reports the pinned 7b source size", {
  models <- evo2_models()
  model <- models[models$name == "7b", , drop = FALSE]

  expect_equal(model$download_size, 23428959022)
})

test_that("evo2_model prepares and reuses the canonical checkpoint", {
  workspace <- tempfile("bionemor-evo2-model-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  testthat::local_mocked_bindings(
    bionemo_install = function(...) {
      stop("evo2_model must not install the runtime")
    },
    bionemo_doctor = function(...) {
      stop("evo2_model must not run diagnostics")
    },
    .package = "bionemor"
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )

  model <- evo2_model("7b-1m", compute)
  registry <- evo2_models()
  record <- registry[registry$name == "7b", , drop = FALSE]
  expected <- file.path(
    workspace,
    "checkpoints",
    paste0(
      "evo2-7b-",
      substr(record$source_revision, 1L, 12L),
      "-",
      substr(compute@recipe@revision, 1L, 12L),
      "-mbridge"
    )
  )

  expect_s3_class(model, "bionemor::Evo2Model")
  expect_equal(model@size, "7b")
  expect_s3_class(model@checkpoint, "bionemor::BioNeMoCheckpoint")
  expect_equal(model@checkpoint@format, "mbridge")
  expect_equal(model@checkpoint@kind, "dense")
  expect_equal(checkpoint_path(model@checkpoint), normalizePath(expected))
  expect_equal(
    checkpoint_manifest(model@checkpoint)$source_revision,
    record$source_revision
  )

  write("reuse-sentinel", file = log, append = TRUE)
  reused <- evo2_model("evo2_7b", compute)

  expect_equal(
    checkpoint_path(reused@checkpoint),
    checkpoint_path(model@checkpoint)
  )
  expect_true("reuse-sentinel" %in% readLines(log))
})

test_that("evo2_model accepts an explicit checkpoint destination", {
  workspace <- tempfile("bionemor-evo2-model-path-")
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

  model <- evo2_model(
    compute = compute,
    path = "prepared/evo2-7b"
  )

  expect_equal(
    checkpoint_path(model@checkpoint),
    normalizePath(file.path(workspace, "prepared", "evo2-7b"))
  )
  expect_named(formals(evo2_model), c("size", "compute", "path"))
})

test_that("evo2_model reuses a relocated complete checkpoint", {
  workspace <- tempfile("bionemor-evo2-model-relocated-")
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
  prepared <- evo2_model(compute = compute)
  original <- checkpoint_path(prepared)
  moved <- file.path(workspace, "uploaded", basename(original))
  dir.create(dirname(moved), recursive = TRUE)
  write("relocation-sentinel", file = log, append = TRUE)

  expect_true(file.rename(original, moved))
  expect_false(file.exists(original))
  stale_manifest <- checkpoint_manifest(moved)
  expect_equal(stale_manifest$inspection$path, original)
  expect_equal(stale_manifest$inspection$resolved_path, original)

  reused <- evo2_model(compute = compute, path = moved)

  expect_equal(checkpoint_path(reused), normalizePath(moved))
  expect_equal(reused@checkpoint@format, "mbridge")
  expect_equal(reused@checkpoint@kind, "dense")
  expect_false(file.exists(original))
  expect_true("relocation-sentinel" %in% readLines(log))
})

test_that("evo2_model rejects mismatched and incomplete destinations", {
  workspace <- tempfile("bionemor-evo2-model-invalid-")
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
  shared <- "checkpoints/shared"

  evo2_model("7b", compute, path = shared)

  expect_error(
    evo2_model("7b-base", compute, path = shared),
    "checkpoint manifest",
    class = "BN_CHECKPOINT_SOURCE"
  )

  incomplete <- file.path(workspace, "checkpoints", "incomplete")
  dir.create(incomplete, recursive = TRUE)
  sentinel <- file.path(incomplete, "keep")
  writeLines("keep", sentinel)

  expect_error(
    evo2_model("7b", compute, path = incomplete),
    "checkpoint destination exists but is incomplete",
    class = "BN_CHECKPOINT_INCOMPLETE"
  )
  expect_equal(readLines(sentinel), "keep")
})

test_that("evo2_model binds compute for inference", {
  workspace <- tempfile("bionemor-evo2-model-compute-")
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
  model <- evo2_model("7b", compute)
  sequences <- c(first = "ACGTACGT", second = "TGCATGCA")

  generated <- evo2_generate(
    model,
    sequences,
    num_tokens = 4L,
    seed = 17L
  )
  scores <- evo2_score(model, sequences)
  embeddings <- evo2_embed(model, sequences)

  expect_equal(generated$input_id, names(sequences))
  expect_equal(scores$id, names(sequences))
  expect_equal(rownames(embeddings), names(sequences))

  unbound <- evo2("7b", checkpoint = model@checkpoint)
  expect_error(
    evo2_score(unbound, sequences),
    "compute is required for an unbound model"
  )

  rebound <- evo2(
    "7b",
    checkpoint = model@checkpoint,
    compute = compute
  )
  expect_equal(evo2_score(rebound, sequences)$id, names(sequences))
})

test_that("registered checkpoints reuse their payload digest", {
  workspace <- tempfile("bionemor-checkpoint-digest-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  suppressWarnings(fake_bionemo_runtime(bin))
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )
  path <- make_mbridge_checkpoint(workspace, "registered")

  model <- evo2("7b", checkpoint = path, compute = compute)
  registered <- checkpoint_manifest(model@checkpoint)

  expect_match(registered$checkpoint_digest, "^[0-9a-f]{32}$")
  writeLines("complete", file.path(path, ".bionemor-complete"))

  job <- evo2_generate(
    model,
    "ACGT",
    num_tokens = 4L,
    seed = 17L,
    async = TRUE
  )
  manifest_path <- file.path(job_path(job), "manifest.json")
  deadline <- Sys.time() + 10
  while (!file.exists(manifest_path) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(file.exists(manifest_path))
  run_manifest <- jsonlite::read_json(
    manifest_path,
    simplifyVector = FALSE
  )

  expect_identical(run_manifest$checkpoint$digest$algorithm, "md5")
  expect_identical(
    run_manifest$checkpoint$digest$value,
    registered$checkpoint_digest
  )
})

test_that("legacy checkpoints can be read from read-only storage", {
  skip_on_os("windows")
  workspace <- tempfile("bionemor-read-only-checkpoint-")
  dir.create(workspace)
  path <- make_mbridge_checkpoint(workspace, "shared")
  model <- evo2("7b", checkpoint = path)
  manifest_path <- model@checkpoint@manifest
  manifest <- checkpoint_manifest(model@checkpoint)
  manifest$checkpoint_digest <- NULL
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  Sys.chmod(manifest_path, "0444")
  Sys.chmod(path, "0555")
  on.exit({
    Sys.chmod(path, "0755")
    Sys.chmod(manifest_path, "0644")
  }, add = TRUE)

  expect_no_error(evo2("7b", checkpoint = path))
})
