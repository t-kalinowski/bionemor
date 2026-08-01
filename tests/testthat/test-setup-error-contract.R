test_that("runtime capability failures expose BN_RUNTIME_MISSING", {
  workspace <- tempfile("bionemor-runtime-missing-")
  bin <- tempfile("bionemor-runtime-missing-bin-")
  dir.create(workspace)
  dir.create(bin)
  write_executable(
    file.path(bin, "bionemor-evo2-helper"),
    c(
      "printf 'CUDA runtime unavailable\\n' >&2",
      "exit 17"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )

  error <- expect_error(
    bionemo_capabilities(compute, refresh = TRUE),
    class = "BN_RUNTIME_MISSING"
  )

  expect_s3_class(error, "bionemor_error")
  expect_identical(error$code, "BN_RUNTIME_MISSING")
  expect_identical(error$operation, "runtime-capabilities")
  expect_identical(error$recipe_revision, compute@recipe@revision)
  expect_identical(error$upstream_exit_status, 17L)
})

test_that("runtime recipe mismatches expose BN_RECIPE_MISMATCH", {
  workspace <- tempfile("bionemor-recipe-mismatch-")
  bin <- tempfile("bionemor-recipe-mismatch-bin-")
  actual_revision <- strrep("a", 40L)
  dir.create(workspace)
  dir.create(bin)
  write_r_executable(
    file.path(bin, "bionemor-evo2-helper"),
    c(
      "report <- list(",
      "  protocol_version = 1L,",
      "  driver = 'evo2-megatron',",
      "  execution_schema_version = 1L,",
      "  semantic_operations = list('generate'),",
      "  recipe_version = '2.4',",
      paste0("  recipe_revision = '", actual_revision, "'"),
      ")",
      "cat(jsonlite::toJSON(report, auto_unbox = TRUE))"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )

  error <- expect_error(
    bionemo_capabilities(compute, refresh = TRUE),
    class = "BN_RECIPE_MISMATCH"
  )

  expect_s3_class(error, "bionemor_error")
  expect_identical(error$code, "BN_RECIPE_MISMATCH")
  expect_identical(error$operation, "runtime-capabilities")
  expect_identical(error$recipe_revision, compute@recipe@revision)
  expect_identical(error$actual_recipe_revision, actual_revision)
})

test_that("missing advertised commands expose BN_RUNTIME_MISSING", {
  workspace <- tempfile("bionemor-install-runtime-missing-")
  bin <- tempfile("bionemor-install-runtime-missing-bin-")
  dir.create(workspace)
  dir.create(bin)
  write_r_executable(
    file.path(bin, "bionemor-evo2-helper"),
    c(
      "report <- list(",
      "  protocol_version = 1L,",
      "  driver = 'evo2-megatron',",
      "  execution_schema_version = 1L,",
      "  semantic_operations = list('generate'),",
      "  recipe_version = '2.4',",
      "  recipe_revision = 'e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e',",
      "  commands = list(infer_evo2 = FALSE, predict_evo2 = TRUE, train_evo2 = TRUE, preprocess_evo2 = TRUE, savanna_to_mbridge = TRUE, nemo2_to_mbridge = TRUE, mbridge_to_vortex = TRUE, remove_optimizer = TRUE)",
      ")",
      "cat(jsonlite::toJSON(report, auto_unbox = TRUE))"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )

  error <- expect_error(
    bionemo_install(compute),
    class = "BN_RUNTIME_MISSING"
  )

  expect_s3_class(error, "bionemor_error")
  expect_identical(error$code, "BN_RUNTIME_MISSING")
  expect_identical(error$operation, "install")
  expect_identical(error$recipe_revision, compute@recipe@revision)
  expect_identical(error$commands, "infer_evo2")
})
