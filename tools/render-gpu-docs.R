#!/usr/bin/env Rscript

stopifnot(
  "set BIONEMOR_DOCS_RENDER=1 to run GPU documentation" = identical(
    Sys.getenv("BIONEMOR_DOCS_RENDER"),
    "1"
  ),
  "knitr is required" = requireNamespace("knitr", quietly = TRUE),
  "bionemor must be installed" = requireNamespace("bionemor", quietly = TRUE)
)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
stopifnot("the renderer must be run with Rscript" = length(script_arg) == 1L)
script <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
root <- normalizePath(file.path(dirname(script), ".."), mustWork = TRUE)
old_working_directory <- setwd(root)
on.exit(setwd(old_working_directory), add = TRUE)

required_environment <- c(
  "BIONEMOR_DOCS_WORKSPACE",
  "BIONEMOR_DOCS_GPU",
  "BIONEMOR_DOCS_RENDER_DATE",
  "BIONEMOR_DOCS_RUN_ID"
)
missing_environment <- required_environment[
  !nzchar(Sys.getenv(required_environment))
]
stopifnot(
  "documentation render environment is incomplete" = length(
    missing_environment
  ) ==
    0L,
  "documentation workspace does not exist" = dir.exists(
    Sys.getenv("BIONEMOR_DOCS_WORKSPACE")
  ),
  "render date must use YYYY-MM-DD" = grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
    Sys.getenv("BIONEMOR_DOCS_RENDER_DATE")
  ),
  "render ID must contain only safe path characters" = grepl(
    "^[A-Za-z0-9._-]+$",
    Sys.getenv("BIONEMOR_DOCS_RUN_ID")
  )
)

revision <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
status <- system2("git", c("status", "--porcelain"), stdout = TRUE)
stopifnot(
  "package revision must be a full commit SHA" = length(revision) == 1L &&
    grepl("^[0-9a-f]{40}$", revision),
  "commit documentation sources before rendering" = length(status) == 0L
)
Sys.setenv(BIONEMOR_DOCS_PACKAGE_REVISION = revision)

targets <- c(
  "README.Rmd" = "README.md",
  "vignettes-src/bionemor.Rmd" = "vignettes/bionemor.Rmd",
  "vignettes-src/evo2-finetune.Rmd" = "vignettes/evo2-finetune.Rmd",
  "vignettes-src/slurm.Rmd" = "vignettes/slurm.Rmd"
)
stopifnot(
  "documentation sources are missing" = all(file.exists(names(targets)))
)

staging <- tempfile("bionemor-docs-", tmpdir = root)
dir.create(staging)
on.exit(unlink(staging, recursive = TRUE), add = TRUE)

staged <- character(length(targets))
for (index in seq_along(targets)) {
  input <- names(targets)[[index]]
  output <- file.path(staging, targets[[index]])
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  knitr::knit(
    input = input,
    output = output,
    quiet = FALSE,
    envir = new.env(parent = globalenv())
  )
  text <- readLines(output, warn = FALSE)
  stopifnot(
    "rendered document still contains executable chunks" = !any(
      grepl("^```\\{", text)
    ),
    "rendered document contains obsolete runtime prose" = !any(
      grepl("NIM", text, fixed = TRUE)
    )
  )
  if (!identical(input, "vignettes-src/slurm.Rmd")) {
    stopifnot(
      "GPU-rendered document does not contain captured output" = any(
        startsWith(text, "#>")
      )
    )
  }
  staged[[index]] <- output
}

for (index in seq_along(targets)) {
  destination <- targets[[index]]
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(staged[[index]], destination, overwrite = TRUE)
  stopifnot("could not publish rendered documentation" = copied)
}

message("Rendered README.md and three static package vignettes")
