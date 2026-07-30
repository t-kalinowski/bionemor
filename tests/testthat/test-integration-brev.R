test_that("Brev acceptance uses the locked image build and requires inference", {
  build_script <- system.file(
    "scripts",
    "brev-evo2-recipes.sh",
    package = "bionemor",
    mustWork = TRUE
  )
  smoke_script <- system.file(
    "scripts",
    "brev-evo2-recipes-smoke.R",
    package = "bionemor",
    mustWork = TRUE
  )
  run_script <- system.file(
    "scripts",
    "brev-evo2-run.sh",
    package = "bionemor",
    mustWork = TRUE
  )
  build <- paste(readLines(build_script, warn = FALSE), collapse = "\n")
  smoke <- paste(readLines(smoke_script, warn = FALSE), collapse = "\n")
  runner <- paste(readLines(run_script, warn = FALSE), collapse = "\n")

  expect_match(build, "inst/recipes/evo2.json", fixed = TRUE)
  expect_match(build, "rev-parse", fixed = TRUE)
  expect_match(build, "hash-object", fixed = TRUE)
  expect_match(build, "RepoDigests", fixed = TRUE)
  expect_match(
    build,
    'base_image_reference="${base_image}@${base_image_digest}"',
    fixed = TRUE
  )
  expect_match(
    build,
    'docker pull "$base_image_reference"',
    fixed = TRUE
  )
  expect_match(
    build,
    'replacement="FROM $base_image_reference"',
    fixed = TRUE
  )
  expect_match(
    build,
    'image_id="$(docker image inspect --format',
    fixed = TRUE
  )
  expect_match(build, '"$image_id"', fixed = TRUE)
  expect_match(
    build,
    "inst/docker/evo2-recipes/Dockerfile.append",
    fixed = TRUE
  )
  expect_true(all(vapply(
    c(
      "BIONEMOR_RECIPE_REVISION",
      "BIONEMOR_HELPER_REVISION",
      "BIONEMOR_BASE_IMAGE",
      "BIONEMOR_BASE_IMAGE_DIGEST"
    ),
    grepl,
    logical(1),
    x = build,
    fixed = TRUE
  )))

  expect_match(smoke, "bionemo_install(", fixed = TRUE)
  expect_match(smoke, "pull = FALSE", fixed = TRUE)
  expect_match(smoke, "evo2_generate(", fixed = TRUE)
  expect_match(smoke, "evo2_score(", fixed = TRUE)
  expect_match(smoke, "evo2_embed(", fixed = TRUE)
  expect_false(grepl("if (nzchar(checkpoint_path))", smoke, fixed = TRUE))
  expect_match(
    runner,
    "BIONEMOR_EVO2_CHECKPOINT_SOURCE",
    fixed = TRUE
  )
  expect_match(runner, "brev copy", fixed = TRUE)
  expect_match(runner, "test -d", fixed = TRUE)

  smoke_expressions <- parse(smoke_script)
  expect_error(
    withr::with_envvar(
      c(
        BIONEMOR_EVO2_CHECKPOINT = "",
        BIONEMOR_EVO2_IMAGE = "unused",
        BIONEMOR_EVO2_WORKSPACE = tempdir()
      ),
      eval(smoke_expressions[-1L], envir = new.env(parent = globalenv()))
    ),
    "BIONEMOR_EVO2_CHECKPOINT is required",
    fixed = TRUE
  )

  runner_result <- withr::with_envvar(
    c(BIONEMOR_EVO2_CHECKPOINT_SOURCE = ""),
    processx::run(
      "bash",
      c(run_script, "--run"),
      error_on_status = FALSE
    )
  )
  expect_equal(runner_result$status, 2L)
  expect_match(
    runner_result$stderr,
    "BIONEMOR_EVO2_CHECKPOINT_SOURCE is required",
    fixed = TRUE
  )
})

test_that("the opt-in Brev acceptance workflow completes and stops its instance", {
  skip_if(
    Sys.getenv("BIONEMOR_RUN_BREV") != "true",
    "Set BIONEMOR_RUN_BREV=true to provision the acceptance instance."
  )
  script <- system.file(
    "scripts",
    "brev-evo2-run.sh",
    package = "bionemor"
  )
  result <- processx::run(
    "bash",
    c(script, "--run"),
    error_on_status = FALSE,
    echo = TRUE,
    timeout = 7200
  )
  expect_equal(result$status, 0L, info = result$stderr)
})
