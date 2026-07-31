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
})
