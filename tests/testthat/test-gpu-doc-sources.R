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
  gpu_source_text <- vapply(
    source_text[1:3],
    paste,
    collapse = "\n",
    FUN.VALUE = character(1)
  )
  expect_true(all(grepl(
    "stopifnot(doctor@ok)",
    gpu_source_text,
    fixed = TRUE
  )))

  readme_source <- gpu_source_text[[1L]]
  expect_match(readme_source, "without leaving R", fixed = TRUE)
  expect_match(readme_source, "Slurm support is experimental", fixed = TRUE)
  expect_match(readme_source, "bionemo_workflows()", fixed = TRUE)
  expect_match(readme_source, "str(models)", fixed = TRUE)
  expect_match(readme_source, "\n)\nscores\n\nembeddings <-", fixed = TRUE)
  expect_match(readme_source, "str(embeddings)", fixed = TRUE)

  vignette_source <- gpu_source_text[[2L]]
  expect_match(vignette_source, "str(models)", fixed = TRUE)
  expect_match(vignette_source, "\n)\nscores\n", fixed = TRUE)
  expect_match(vignette_source, "str(embeddings)", fixed = TRUE)

  workflow <- paste(
    readLines(
      file.path(root, "validation", "brev-evo2", "README.md"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(workflow, "%H%M%S", fixed = TRUE)
  expect_no_match(
    paste(source_text[[2L]], collapse = "\n"),
    "docs_run_id",
    fixed = TRUE
  )
  expect_match(
    paste(source_text[[3L]], collapse = "\n"),
    'paste0(run_id, "-tiny-evo2-128")',
    fixed = TRUE
  )
  slurm_source <- paste(source_text[[4L]], collapse = "\n")
  expect_match(
    slurm_source,
    "candidate_sequences <- c(",
    fixed = TRUE
  )
  expect_match(
    slurm_source,
    'checkpoint = "/shared/models/evo2-7b-mbridge",\n  compute = compute',
    fixed = TRUE
  )
  expect_match(
    slurm_source,
    "candidate_sequences,\n  async = TRUE",
    fixed = TRUE
  )
})
