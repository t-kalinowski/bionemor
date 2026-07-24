package_file <- function(...) {
  parts <- list(...)
  installed_parts <- if (identical(parts[[1L]], "vignettes")) {
    c("doc", parts[-1L])
  } else {
    parts
  }
  installed <- do.call(
    system.file,
    c(installed_parts, list(package = "bionemor"))
  )
  if (nzchar(installed)) {
    return(installed)
  }
  root <- getwd()
  while (!file.exists(file.path(root, "DESCRIPTION"))) {
    parent <- dirname(root)
    stopifnot("could not locate package root" = parent != root)
    root <- parent
  }
  if (parts[[1L]] %in% c("scripts", "docker")) {
    parts <- c("inst", parts)
  }
  do.call(file.path, c(list(root), parts))
}

test_that("the public API exports only BioNeMo compute-side names", {
  exports <- getNamespaceExports("bionemor")

  expect_true(all(c(
    "evo2",
    "evo2_checkpoint",
    "checkpoint_path",
    "checkpoint_manifest",
    "bionemo_compute",
    "evo2_fit_control",
    "fit",
    "bionemo_setup",
    "bionemo_doctor",
    "bionemo_capabilities",
    "job_status",
    "job_wait",
    "job_result",
    "job_logs",
    "job_cancel"
  ) %in% exports))
  expect_false(any(startsWith(exports, "nim_")))
  expect_false("deploy" %in% exports)
  expect_false("evaluate" %in% exports)
})

test_that("package metadata points at the public repository", {
  description <- read.dcf(package_file("DESCRIPTION"))[1L, ]

  expect_identical(
    unname(description[["URL"]]),
    "https://github.com/t-kalinowski/bionemor"
  )
  expect_identical(
    unname(description[["BugReports"]]),
    "https://github.com/t-kalinowski/bionemor/issues"
  )
})

test_that("the primary guide documents the complete NGC-free public workflow", {
  guide <- paste(
    readLines(package_file("vignettes", "bionemor.Rmd"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(guide, "does not require an NGC key", fixed = TRUE)
  expect_match(guide, "hf://arcinstitute/savanna_evo2_1b_base", fixed = TRUE)
  expect_match(guide, "evo2_checkpoint", fixed = TRUE)
  expect_match(guide, "generics::fit", fixed = TRUE)
  expect_match(guide, 'type = \"response\"', fixed = TRUE)
  expect_match(guide, 'type = \"score\"', fixed = TRUE)
  expect_match(guide, 'type = \"raw\"', fixed = TRUE)
  expect_match(guide, "R API")
  expect_match(guide, "external Python process")
  expect_match(guide, "bionemo-2.6.3", fixed = TRUE)
  expect_match(guide, "| Evo 2 | 1B |")
  expect_match(guide, "ngc://evo2/7b-1m:1.0", fixed = TRUE)
  expect_match(guide, "NGC_CLI_API_KEY", fixed = TRUE)
  expect_match(guide, "checkpoint_path(fitted_7b)", fixed = TRUE)
  expect_match(guide, "nimr::nim_deploy", fixed = TRUE)
  expect_match(guide, "eval = FALSE", fixed = TRUE)
})

test_that("the Slurm guide covers Python and Apptainer job control", {
  guide <- paste(
    readLines(package_file("vignettes", "slurm.Rmd"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(guide, 'backend = \"slurm\"', fixed = TRUE)
  expect_match(guide, 'engine = \"python\"', fixed = TRUE)
  expect_match(guide, 'engine = \"container\"', fixed = TRUE)
  expect_match(guide, "Apptainer")
  expect_match(guide, "sbatch")
  expect_match(guide, "sacct")
  expect_match(guide, "scancel")
  expect_match(guide, "job_wait")
  expect_match(guide, "does not cancel", fixed = TRUE)
})

test_that("the Brev workflow is explicit, credential-free, and self-stopping", {
  script_dir <- package_file("scripts")
  assets <- list.files(
    script_dir,
    pattern = "^brev-evo2-",
    full.names = TRUE
  )
  expect_setequal(
    basename(assets),
    c(
      "brev-evo2-create.sh",
      "brev-evo2-finetune.R",
      "brev-evo2-finetune.sh",
      "brev-evo2-framework.sh",
      "brev-evo2-framework-smoke.R",
      "brev-evo2-run.sh"
    )
  )

  contents <- unlist(lapply(assets, readLines, warn = FALSE), use.names = FALSE)
  expect_false(any(grepl("NGC_API_KEY", contents, fixed = TRUE)))
  expect_false(any(grepl("NGC_CLI_API_KEY", contents, fixed = TRUE)))
  expect_false(any(grepl("nimr::", contents, fixed = TRUE)))
  expect_true(any(grepl(
    "hf://arcinstitute/savanna_evo2_7b",
    contents,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "hf://arcinstitute/savanna_evo2_1b_base",
    contents,
    fixed = TRUE
  )))

  create <- paste(readLines(file.path(script_dir, "brev-evo2-create.sh")), collapse = "\n")
  expect_match(create, "l40s-48gb.1x", fixed = TRUE)
  expect_match(create, "--dry-run", fixed = TRUE)
  expect_match(create, "--create", fixed = TRUE)

  fine_tune <- paste(readLines(file.path(script_dir, "brev-evo2-finetune.sh")), collapse = "\n")
  expect_match(
    fine_tune,
    "timeout --signal=TERM --kill-after=15s 300s",
    fixed = TRUE
  )
  expect_match(fine_tune, "brev-evo2-finetune-train", fixed = TRUE)

  run <- paste(readLines(file.path(script_dir, "brev-evo2-run.sh")), collapse = "\n")
  expect_match(run, "trap stop_instance EXIT", fixed = TRUE)
  expect_match(run, "brev stop", fixed = TRUE)
  expect_match(run, "brev-evo2-framework.sh", fixed = TRUE)
  expect_match(run, "brev-evo2-finetune.sh", fixed = TRUE)
})

test_that("the Brev runner attempts cleanup when provisioning fails", {
  bin <- tempfile("bionemor-brev-bin-")
  calls <- tempfile("bionemor-brev-calls-")
  dir.create(bin)
  write_executable(
    file.path(bin, "brev"),
    c(
      "printf '%s\\n' \"$*\" >> \"$BIONEMOR_BREV_CALLS\"",
      "if [[ \"${1:-}\" == \"create\" ]]; then exit 42; fi"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_BREV_CALLS = calls
  )

  result <- processx::run(
    "bash",
    c(
      package_file("scripts", "brev-evo2-run.sh"),
      "--run",
      "cleanup-test"
    ),
    error_on_status = FALSE
  )

  expect_equal(result$status, 42L)
  expect_match(
    paste(readLines(calls), collapse = "\n"),
    "stop cleanup-test",
    fixed = TRUE
  )
})

test_that("the Brev fitting script rejects special path run identifiers", {
  workspace <- tempfile("bionemor-brev-workspace-")
  bin <- tempfile("bionemor-docker-bin-")
  calls <- tempfile("bionemor-docker-calls-")
  dir.create(workspace)
  dir.create(bin)
  write_executable(
    file.path(bin, "docker"),
    c(
      "printf '%s\\n' \"$*\" >> \"$BIONEMOR_DOCKER_CALLS\"",
      "exit 77"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_DOCKER_CALLS = calls,
    BIONEMOR_EVO2_WORKSPACE = workspace,
    BIONEMOR_EVO2_RUN_ID = ".."
  )

  result <- processx::run(
    "bash",
    package_file("scripts", "brev-evo2-finetune.sh"),
    error_on_status = FALSE
  )

  expect_equal(result$status, 2L)
  expect_match(result$stderr, "safe name", fixed = TRUE)
  expect_false(file.exists(calls))
})

test_that("the Brev guide states scope, cost boundary, and stop behavior", {
  guide <- paste(
    readLines(package_file("vignettes", "brev-evo2.Rmd"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(guide, "does not require an NGC key", fixed = TRUE)
  expect_match(guide, "L40S")
  expect_match(guide, "dry run")
  expect_match(guide, "three")
  expect_match(guide, "300-second")
  expect_match(guide, "brev-evo2-run.sh", fixed = TRUE)
  expect_match(guide, "stops the instance", fixed = TRUE)
  expect_false(grepl("nim_connect", guide, fixed = TRUE))
  expect_false(grepl("nim_deploy", guide, fixed = TRUE))
})

test_that("the integration image pins BioNeMo and installs bionemor", {
  dockerfile <- paste(
    readLines(
      package_file("docker", "bionemo-framework", "Dockerfile"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    dockerfile,
    "FROM nvcr.io/nvidia/clara/bionemo-framework:2.6.3",
    fixed = TRUE
  )
  expect_match(dockerfile, "R CMD INSTALL /opt/bionemor", fixed = TRUE)
  expect_false(grepl("httr2", dockerfile, fixed = TRUE))
  expect_false(grepl("rlang", dockerfile, fixed = TRUE))
})

test_that("integration scripts parse before remote execution", {
  script_dir <- package_file("scripts")
  shell_scripts <- list.files(
    script_dir,
    pattern = "[.]sh$",
    full.names = TRUE
  )
  r_scripts <- list.files(
    script_dir,
    pattern = "[.]R$",
    full.names = TRUE
  )

  for (script in shell_scripts) {
    result <- processx::run(
      "bash",
      c("-n", script),
      error_on_status = FALSE
    )
    expect_equal(result$status, 0L, info = result$stderr)
  }
  for (script in r_scripts) {
    expect_no_error(parse(file = script))
  }
})
