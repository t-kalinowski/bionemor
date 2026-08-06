test_that("the Brev setup script installs R and bionemor", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  script <- file.path(root, "tools", "brev", "setup.sh")
  expect_true(file.exists(script))

  bin <- tempfile("bionemor-brev-setup-bin-")
  home <- tempfile("bionemor-brev-setup-home-")
  log <- tempfile("bionemor-brev-setup-log-")
  rscript_stdin <- tempfile("bionemor-brev-setup-rscript-")
  dir.create(bin)
  dir.create(home)

  write_executable(
    file.path(bin, "sudo"),
    c(
      "printf 'sudo %s\\n' \"$*\" >> \"$BIONEMOR_TEST_LOG\"",
      "if [[ \"${1:-}\" == \"-n\" ]]; then shift; fi",
      "if [[ \"${1:-}\" == \"tee\" ]]; then cat >/dev/null; fi"
    )
  )
  write_executable(
    file.path(bin, "rig"),
    "printf 'rig %s\\n' \"$*\" >> \"$BIONEMOR_TEST_LOG\""
  )
  write_executable(
    file.path(bin, "Rscript"),
    c(
      "printf 'Rscript %s\\n' \"$*\" >> \"$BIONEMOR_TEST_LOG\"",
      "cat > \"$BIONEMOR_TEST_RSCRIPT_STDIN\""
    )
  )

  result <- withr::with_envvar(
    c(
      PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
      HOME = home,
      BIONEMOR_PACKAGE_SPEC = "example/bionemor@abc123",
      BIONEMOR_TEST_LOG = log,
      BIONEMOR_TEST_RSCRIPT_STDIN = rscript_stdin
    ),
    processx::run("bash", script, error_on_status = FALSE)
  )

  expect_equal(result$status, 0L, info = result$stderr)
  calls <- readLines(log, warn = FALSE)
  expect_true(any(grepl("sudo -n true", calls, fixed = TRUE)))
  expect_true(any(grepl(
    "apt-get -o DPkg::Lock::Timeout=600 update",
    calls,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "apt-get -o DPkg::Lock::Timeout=600 install -y ca-certificates curl git tar",
    calls,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "apt-get -o DPkg::Lock::Timeout=600 install -y r-rig",
    calls,
    fixed = TRUE
  )))
  expect_true(any(grepl("sudo -n rig add release", calls, fixed = TRUE)))
  expect_true(any(grepl("sudo -n rig default release", calls, fixed = TRUE)))
  expect_true("Rscript --vanilla -" %in% calls)
  r_code <- readLines(rscript_stdin, warn = FALSE)
  expect_true(any(grepl("BIONEMOR_PACKAGE_SPEC", r_code, fixed = TRUE)))
  expect_true(dir.exists(file.path(home, "workspace", "bionemor")))
})

test_that("the Brev setup guide uses the setup script and persistent workspace", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  paths <- file.path(
    root,
    c("vignettes-src/bionemor.Rmd", "vignettes/bionemor.Rmd")
  )
  documents <- vapply(
    paths,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )

  expect_true(all(grepl("tools/brev/setup.sh", documents, fixed = TRUE)))
  expect_true(all(grepl(
    "brev exec bionemor-gpu @bionemor-brev-setup.sh",
    documents,
    fixed = TRUE
  )))
  expect_true(all(grepl("~/workspace/bionemor", documents, fixed = TRUE)))
  expect_true(all(grepl("R runs", documents, fixed = TRUE)))

  create_script <- file.path(
    root,
    "validation",
    "brev-evo2",
    "scripts",
    "brev-evo2-create.sh"
  )
  bin <- tempfile("bionemor-brev-create-bin-")
  log <- tempfile("bionemor-brev-create-log-")
  dir.create(bin)
  write_executable(
    file.path(bin, "brev"),
    "printf '%s\\n' \"$*\" >> \"$BIONEMOR_TEST_LOG\""
  )
  result <- withr::with_envvar(
    c(
      PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
      BIONEMOR_TEST_LOG = log
    ),
    processx::run("bash", create_script, error_on_status = FALSE)
  )
  expect_equal(result$status, 0L, info = result$stderr)
  create_call <- readLines(log, warn = FALSE)
  expect_length(create_call, 1L)
  expect_match(create_call[[1L]], "--mode vm", fixed = TRUE)
  expect_match(create_call[[1L]], "--dry-run", fixed = TRUE)
  expect_false(grepl("--startup-script", create_call[[1L]], fixed = TRUE))

  unlink(log)
  result <- withr::with_envvar(
    c(
      PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
      BIONEMOR_TEST_LOG = log
    ),
    processx::run(
      "bash",
      c(create_script, "--create", "bionemor-test"),
      error_on_status = FALSE
    )
  )
  expect_equal(result$status, 0L, info = result$stderr)
  create_call <- readLines(log, warn = FALSE)
  expect_length(create_call, 2L)
  expect_match(create_call[[1L]], "create bionemor-test", fixed = TRUE)
  expect_identical(
    create_call[[2L]],
    paste(
      "exec bionemor-test",
      paste0("@", normalizePath(file.path(root, "tools", "brev", "setup.sh")))
    )
  )

  validation_paths <- file.path(
    root,
    "validation",
    "brev-evo2",
    "scripts",
    c(
      "brev-evo2-recipes.sh",
      "brev-evo2-recipes-smoke.R",
      "brev-evo2-recipes-acceptance.R",
      "brev-evo2-run.sh"
    )
  )
  validation_scripts <- vapply(
    validation_paths,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  expect_true(all(grepl(
    "/home/ubuntu/workspace/bionemor",
    validation_scripts,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "/home/ubuntu/bionemor-workspace",
    validation_scripts,
    fixed = TRUE
  )))

  validation_readme <- paste(
    readLines(
      file.path(root, "validation", "brev-evo2", "README.md"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    validation_readme,
    "/home/ubuntu/workspace/bionemor",
    fixed = TRUE
  )
  expect_match(
    validation_readme,
    "pak::local_install_dev_deps()",
    fixed = TRUE
  )
  expect_match(
    validation_readme,
    'pak::pkg_install("devtools")',
    fixed = TRUE
  )
  expect_lt(
    regexpr("pak::local_install_dev_deps()", validation_readme, fixed = TRUE),
    regexpr("devtools::test()", validation_readme, fixed = TRUE)
  )
})

test_that("the Brev recipe workflow resolves package inputs", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  script <- file.path(
    root,
    "validation",
    "brev-evo2",
    "scripts",
    "brev-evo2-recipes.sh"
  )
  bin <- tempfile("bionemor-brev-recipes-bin-")
  workspace <- tempfile("bionemor-brev-recipes-workspace-")
  checkpoint <- file.path(workspace, "checkpoint")
  log <- tempfile("bionemor-brev-recipes-log-")
  dir.create(bin)
  dir.create(checkpoint, recursive = TRUE)
  for (command in c("docker", "git", "R")) {
    write_executable(file.path(bin, command), "exit 0")
  }
  write_executable(
    file.path(bin, "Rscript"),
    c(
      "printf '%s\\n' \"$*\" > \"$BIONEMOR_TEST_LOG\"",
      "exit 0"
    )
  )

  result <- withr::with_envvar(
    c(
      PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
      BIONEMOR_EVO2_WORKSPACE = workspace,
      BIONEMOR_EVO2_CHECKPOINT = checkpoint,
      BIONEMOR_TEST_LOG = log
    ),
    processx::run("bash", script, error_on_status = FALSE)
  )

  expect_equal(result$status, 1L)
  expect_match(
    readLines(log, warn = FALSE),
    normalizePath(file.path(root, "inst", "recipes", "evo2.json")),
    fixed = TRUE
  )
})

test_that("user documentation exposes the prebuilt image extension point", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  paths <- file.path(
    root,
    c("vignettes-src/bionemor.Rmd", "vignettes/bionemor.Rmd")
  )
  documents <- vapply(
    paths,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )

  expect_true(all(grepl("prebuilt recipe image", documents, fixed = TRUE)))
  expect_true(all(grepl('image = "', documents, fixed = TRUE)))
  expect_true(all(grepl("FROM", documents, fixed = TRUE)))
  expect_true(all(grepl(
    "export BIONEMOR_RECIPE_IMAGE",
    documents,
    fixed = TRUE
  )))

  api_documents <- vapply(
    file.path(root, c("R/03-compute.R", "man/bionemo_compute.Rd")),
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  expect_true(all(grepl(
    'image = "example/bionemor-evo2:site"',
    api_documents,
    fixed = TRUE
  )))
})
