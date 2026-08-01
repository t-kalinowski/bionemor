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
  acceptance_script <- system.file(
    "scripts",
    "brev-evo2-recipes-acceptance.R",
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
  acceptance <- paste(
    readLines(acceptance_script, warn = FALSE),
    collapse = "\n"
  )
  acceptance_expressions <- parse(acceptance_script)
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
    'uv_image_reference="${uv_image}@${uv_image_digest}"',
    fixed = TRUE
  )
  expect_match(
    build,
    'uv_replacement="COPY --from=$uv_image_reference /uv /uvx /bin/"',
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
  expect_match(
    build,
    'r_library="$HOME/R-library"',
    fixed = TRUE
  )
  expect_match(
    build,
    'mkdir -p "$r_library"',
    fixed = TRUE
  )
  expect_match(
    build,
    'R_LIBS_USER="$r_library${R_LIBS_USER:+:$R_LIBS_USER}"',
    fixed = TRUE
  )
  expect_match(
    build,
    'R CMD INSTALL --library="$r_library" "$repo_dir"',
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
    build,
    "brev-evo2-recipes-acceptance.R",
    fixed = TRUE
  )
  expect_match(
    acceptance,
    'evo2("7b", checkpoint = checkpoint_path)',
    fixed = TRUE
  )
  expect_match(acceptance, "evo2_score(", fixed = TRUE)
  expect_match(acceptance, "evo2_generate(", fixed = TRUE)
  expect_match(acceptance, "evo2_embed(", fixed = TRUE)
  expect_match(acceptance, "evo2_prepare(", fixed = TRUE)
  expect_match(acceptance, "evo2_finetune(", fixed = TRUE)
  expect_match(acceptance, "job_wait(", fixed = TRUE)
  expect_match(acceptance, "job_path(", fixed = TRUE)
  expect_match(acceptance, "sample_length = 128L", fixed = TRUE)
  expect_match(acceptance, "steps = 2L", fixed = TRUE)
  expect_match(acceptance, "rank = 4L", fixed = TRUE)
  expect_match(acceptance, 'precision = "bf16"', fixed = TRUE)
  expect_match(acceptance, "generated_tokens == 8L", fixed = TRUE)
  expect_match(acceptance, "capture_evidence(", fixed = TRUE)
  expect_match(acceptance, "evidence.json", fixed = TRUE)
  expect_match(acceptance, "outputs/dense.json", fixed = TRUE)
  expect_match(acceptance, "outputs/fitted.json", fixed = TRUE)
  expect_match(acceptance, "manifests/dense-score.json", fixed = TRUE)
  expect_match(acceptance, "manifests/dense-generation.json", fixed = TRUE)
  expect_match(acceptance, "manifests/dense-embedding.json", fixed = TRUE)
  expect_match(acceptance, "manifests/fine-tune.json", fixed = TRUE)
  expect_match(acceptance, "manifests/fitted-score.json", fixed = TRUE)
  expect_match(acceptance, "manifests/fitted-generation.json", fixed = TRUE)
  expect_match(acceptance, "lora-inspection.json", fixed = TRUE)
  expect_false(grepl("score improvement", acceptance, fixed = TRUE))
  expect_false(grepl("converted", acceptance, fixed = TRUE))
  expect_false(grepl("bionemor:::", acceptance, fixed = TRUE))
  expect_identical(
    as.character(acceptance_expressions[[length(acceptance_expressions)]][[
      1L
    ]]),
    "capture_evidence"
  )
  expect_match(
    runner,
    "BIONEMOR_EVO2_CHECKPOINT_SOURCE",
    fixed = TRUE
  )
  expect_match(runner, "BIONEMOR_EVO2_EVIDENCE", fixed = TRUE)
  expect_match(runner, "--acceptance", fixed = TRUE)
  expect_match(runner, "brev copy", fixed = TRUE)
  expect_match(runner, "test -d", fixed = TRUE)
  expect_match(runner, "evidence.json", fixed = TRUE)
  expect_match(runner, "validation/brev-evo2", fixed = TRUE)

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

test_that("the Brev acceptance capture fails before starting GPU work", {
  acceptance_script <- system.file(
    "scripts",
    "brev-evo2-recipes-acceptance.R",
    package = "bionemor",
    mustWork = TRUE
  )
  acceptance_expressions <- parse(acceptance_script)
  expect_error(
    withr::with_envvar(
      c(
        BIONEMOR_EVO2_CHECKPOINT = "",
        BIONEMOR_EVO2_IMAGE = "",
        BIONEMOR_EVO2_WORKSPACE = tempdir(),
        BIONEMOR_EVO2_EVIDENCE = "",
        BIONEMOR_EVO2_CAPTURE_DATE = "",
        BIONEMOR_PACKAGE_REVISION = "",
        BIONEMOR_PACKAGE_DIRTY = "",
        BIONEMOR_PACKAGE_SOURCE = ""
      ),
      eval(
        acceptance_expressions[-1L],
        envir = new.env(parent = globalenv())
      )
    ),
    "BIONEMOR_EVO2_IMAGE is required",
    fixed = TRUE
  )
})

test_that("the Brev runner copies evidence before stopping the instance", {
  run_script <- system.file(
    "scripts",
    "brev-evo2-run.sh",
    package = "bionemor",
    mustWork = TRUE
  )
  bin <- tempfile("bionemor-fake-brev-bin-")
  checkpoint <- tempfile("bionemor-brev-checkpoint-")
  dir.create(bin)
  dir.create(checkpoint)
  package_revision <- paste(rep("a", 40L), collapse = "")
  write_executable(
    file.path(bin, "git"),
    c(
      "case \"${3:-}\" in",
      "  rev-parse) printf '%s\\n' \"$BIONEMOR_FAKE_PACKAGE_REVISION\" ;;",
      "  status) exit 0 ;;",
      "  *) exit 2 ;;",
      "esac"
    )
  )
  write_executable(
    file.path(bin, "brev"),
    c(
      "printf '%s\\n' \"$*\" >> \"$BIONEMOR_FAKE_BREV_LOG\"",
      paste(
        "if [[ \"${1:-}\" == \"stop\" ]]; then",
        "exit \"$BIONEMOR_FAKE_BREV_STOP_STATUS\"; fi"
      ),
      "if [[ \"${1:-}\" != \"copy\" ]]; then exit 0; fi",
      "source_path=\"${2:-}\"",
      "destination=\"${3:-}\"",
      "if [[ \"$source_path\" != *:* || \"$destination\" == *:* ]]; then",
      "  exit 0",
      "fi",
      "mkdir -p \"$destination/outputs\" \"$destination/manifests\"",
      "touch \"$destination/README.md\" \"$destination/evidence.json\"",
      "touch \"$destination/outputs/dense.json\"",
      "touch \"$destination/outputs/fitted.json\"",
      "touch \"$destination/manifests/dense-score.json\"",
      "touch \"$destination/manifests/dense-generation.json\"",
      "touch \"$destination/manifests/dense-embedding.json\"",
      "touch \"$destination/manifests/prepare.json\"",
      "touch \"$destination/manifests/fine-tune.json\"",
      "touch \"$destination/manifests/fitted-score.json\"",
      "touch \"$destination/manifests/fitted-generation.json\"",
      "touch \"$destination/lora-inspection.json\""
    )
  )

  run_capture <- function(capture_date, stop_status) {
    log <- tempfile("bionemor-fake-brev-log-")
    capture <- testthat::test_path(
      "..",
      "..",
      "validation",
      "brev-evo2",
      capture_date
    )
    expect_false(file.exists(capture))
    on.exit(unlink(capture, recursive = TRUE, force = TRUE))

    result <- withr::with_envvar(
      c(
        PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
        BIONEMOR_EVO2_CAPTURE_DATE = capture_date,
        BIONEMOR_EVO2_CHECKPOINT_SOURCE = checkpoint,
        BIONEMOR_FAKE_BREV_LOG = log,
        BIONEMOR_FAKE_BREV_STOP_STATUS = as.character(stop_status),
        BIONEMOR_FAKE_PACKAGE_REVISION = package_revision
      ),
      processx::run(
        "bash",
        c(run_script, "--run"),
        error_on_status = FALSE
      )
    )
    expected_status <- if (stop_status == 0L) 0L else 1L
    expect_equal(result$status, expected_status, info = result$stderr)
    expect_true(file.exists(file.path(capture, "evidence.json")))

    calls <- readLines(log, warn = FALSE)
    expect_true(any(grepl(
      paste0("BIONEMOR_PACKAGE_REVISION='", package_revision, "'"),
      calls,
      fixed = TRUE
    )))
    copied <- grep(
      paste0("^copy bionemor-evo2:.*/", capture_date, "/ "),
      calls
    )
    stopped <- grep("^stop bionemor-evo2$", calls)
    expect_length(copied, 1L)
    expect_length(stopped, 1L)
    expect_lt(copied, stopped)
  }

  run_capture("2999-12-31", 0L)
  run_capture("2999-12-30", 19L)
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
