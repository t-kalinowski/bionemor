test_that("the pkgdown reference index follows the public API", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  config_path <- file.path(root, "_pkgdown.yml")
  expect_true(file.exists(config_path))

  expected <- list(
    "Start here" = "bionemor-package",
    "Configure compute and runtimes" = c(
      "bionemo_compute",
      "evo2_recipe",
      "esm2_recipe",
      "bionemo_install",
      "bionemo_doctor",
      "bionemo_capabilities"
    ),
    "Evo 2 models and checkpoints" = c(
      "evo2_models",
      "evo2",
      "evo2_model",
      "evo2_checkpoint",
      "evo2_export",
      "checkpoint_metadata"
    ),
    "Evo 2 inference" = c(
      "evo2_generate",
      "evo2_score",
      "evo2_profile",
      "evo2_embed",
      "evo2_phylo_tag",
      "evo2_inference_control"
    ),
    "Evo 2 fine-tuning" = c(
      "evo2_dataset",
      "evo2_preprocess_control",
      "evo2_preprocess",
      "evo2_lora",
      "evo2_full",
      "evo2_fit_control",
      "evo2_finetune"
    ),
    "ESM-2 protein embeddings" = c(
      "esm2_models",
      "esm2",
      "esm2_model",
      "esm2_embed"
    ),
    "Run and monitor jobs" = c(
      "bionemo_job",
      "job_path",
      "job_status",
      "job_logs",
      "job_wait",
      "job_result",
      "job_cancel"
    ),
    "Compatibility generics" = "reexports"
  )

  config <- yaml12::read_yaml(config_path)
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  expect_match(
    description[[1L, "URL"]],
    "https://t-kalinowski.github.io/bionemor/",
    fixed = TRUE
  )
  reference <- config$reference
  expect_type(reference, "list")
  expect_true(all(vapply(
    reference,
    function(group) identical(names(group), c("title", "contents")),
    logical(1)
  )))

  titles <- vapply(reference, `[[`, character(1), "title")
  contents <- lapply(reference, `[[`, "contents")
  names(contents) <- titles
  expect_identical(contents, expected)
  expect_identical(anyDuplicated(titles), 0L)

  topics <- unlist(contents, use.names = FALSE)
  expect_identical(anyDuplicated(topics), 0L)

  exported <- sub(
    "^export\\((.*)\\)$",
    "\\1",
    grep("^export\\(", readLines(file.path(root, "NAMESPACE")), value = TRUE)
  )
  expected_topics <- c(
    "bionemor-package",
    setdiff(
      exported,
      c("fit", "predict", "checkpoint_path", "checkpoint_manifest")
    ),
    "reexports",
    "checkpoint_metadata"
  )
  expect_setequal(topics, expected_topics)
})

test_that("Slurm and Apptainer do not have a dedicated article", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  expect_false(file.exists(file.path(root, "vignettes-src", "slurm.Rmd")))
  expect_false(file.exists(file.path(root, "vignettes", "slurm.Rmd")))

  onboarding <- paste(
    readLines(file.path(root, "vignettes-src", "bionemor.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(onboarding, 'backend = "slurm"', fixed = TRUE)
  expect_match(onboarding, "Apptainer", fixed = TRUE)
  expect_no_match(onboarding, "experimental", ignore.case = TRUE)
  expect_no_match(onboarding, "untested", ignore.case = TRUE)
  expect_no_match(onboarding, "not executed", ignore.case = TRUE)
})

test_that("checkpoint inspection has one help topic", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  topic <- file.path(root, "man", "checkpoint_metadata.Rd")
  expect_true(file.exists(topic))
  text <- paste(readLines(topic, warn = FALSE), collapse = "\n")
  expect_match(text, "\\alias{checkpoint_path}", fixed = TRUE)
  expect_match(text, "\\alias{checkpoint_manifest}", fixed = TRUE)
  expect_false(file.exists(file.path(root, "man", "checkpoint_path.Rd")))
  expect_false(file.exists(file.path(root, "man", "checkpoint_manifest.Rd")))
})

test_that("the first release has a concise changelog and announcement draft", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  news <- readLines(file.path(root, "NEWS.md"), warn = FALSE)
  expect_lte(length(news[nzchar(news)]), 2L)
  expect_match(paste(news, collapse = "\n"), "Initial release", fixed = TRUE)

  post_path <- file.path(root, "blog", "first-release.md")
  expect_true(file.exists(post_path))
  post <- paste(readLines(post_path, warn = FALSE), collapse = "\n")
  expect_match(post, "Bioconductor", fixed = TRUE)
  expect_match(post, "Evo 2", fixed = TRUE)
  expect_match(post, "ESM-2", fixed = TRUE)
  expect_match(post, "fine-tun", ignore.case = TRUE)
  expect_match(post, "CUDA-capable NVIDIA GPU", fixed = TRUE)
})
