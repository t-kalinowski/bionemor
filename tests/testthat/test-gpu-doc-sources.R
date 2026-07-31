test_that("GPU documentation has an explicit manual render workflow", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  sources <- file.path(
    root,
    c(
      "README.Rmd",
      "vignettes-src/bionemor.Rmd",
      "vignettes-src/evo2-finetune.Rmd",
      "vignettes-src/slurm.Rmd",
      "tools/render-gpu-docs.R"
    )
  )

  expect_true(all(file.exists(sources)))

  renderer <- paste(
    readLines(file.path(root, "tools/render-gpu-docs.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(renderer, "BIONEMOR_DOCS_RENDER", fixed = TRUE)
  expect_match(renderer, "knitr::knit", fixed = TRUE)
  expect_match(renderer, "vignettes-src", fixed = TRUE)
  expect_match(renderer, "rendered document contains an error", fixed = TRUE)
  expect_match(renderer, "finally", fixed = TRUE)

  executable_sources <- sources[grepl("\\.Rmd$", sources)]
  source_text <- lapply(executable_sources, readLines, warn = FALSE)
  expect_true(all(vapply(
    source_text,
    function(lines) any(grepl("error = FALSE", lines, fixed = TRUE)),
    logical(1)
  )))

  workflow <- paste(
    readLines(
      file.path(root, "validation", "brev-evo2", "README.md"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(workflow, "%H%M%S", fixed = TRUE)
  expect_match(
    paste(source_text[[2L]], collapse = "\n"),
    "BIONEMOR_DOCS_RUN_ID",
    fixed = TRUE
  )
  expect_match(
    paste(source_text[[3L]], collapse = "\n"),
    'paste0(run_id, "-tiny-evo2-128")',
    fixed = TRUE
  )
})
