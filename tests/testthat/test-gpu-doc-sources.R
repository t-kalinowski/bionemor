test_that("GPU documentation has a guarded manual render workflow", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  paths <- file.path(
    root,
    c(
      "README.Rmd",
      "vignettes-src/bionemor.Rmd",
      "vignettes-src/evo2-finetune.Rmd",
      "vignettes-src/slurm.Rmd",
      "tools/render-gpu-docs.R"
    )
  )
  expect_true(all(file.exists(paths)))

  renderer <- paste(
    readLines(paths[[5L]], warn = FALSE),
    collapse = "\n"
  )
  expect_match(renderer, "BIONEMOR_DOCS_RENDER", fixed = TRUE)
  expect_match(renderer, "knitr::knit", fixed = TRUE)
  expect_match(renderer, "rendered document contains an error", fixed = TRUE)
  expect_match(renderer, "finally", fixed = TRUE)

  sources <- vapply(
    paths[1:4],
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  expect_true(all(grepl("error = FALSE", sources, fixed = TRUE)))

  onboarding <- sources[1:2]
  expect_true(all(grepl("NVIDIA GPU", onboarding, fixed = TRUE)))
  expect_true(all(grepl("no CPU fallback", onboarding, fixed = TRUE)))
  expect_true(all(grepl("BioNeMo Recipes", onboarding, fixed = TRUE)))
  expect_true(all(grepl("Evo 2", onboarding, fixed = TRUE)))
  expect_true(all(grepl("ESM-2", onboarding, fixed = TRUE)))
  expect_true(all(grepl("Brev", onboarding, fixed = TRUE)))
  expect_true(all(grepl("bionemo_workflows()", onboarding, fixed = TRUE)))
  expect_true(all(grepl("recipe = evo2_recipe()", onboarding, fixed = TRUE)))
  expect_true(all(grepl("recipe = esm2_recipe()", onboarding, fixed = TRUE)))
  expect_true(all(grepl(
    'evo2_model("7b", evo2_compute)',
    onboarding,
    fixed = TRUE
  )))
  expect_true(all(grepl(
    'esm2_model("8m", esm2_compute)',
    onboarding,
    fixed = TRUE
  )))
  expect_true(all(grepl("esm2_embed(", onboarding, fixed = TRUE)))
  expect_true(all(grepl("native Transformers", onboarding, fixed = TRUE)))
  expect_true(all(grepl("does not compile vLLM", onboarding, fixed = TRUE)))
  expect_true(all(grepl("gpus = 1", onboarding, fixed = TRUE)))
  expect_true(all(grepl("similarity", onboarding, fixed = TRUE)))
  expect_true(all(grepl("clustering", onboarding, fixed = TRUE)))
  expect_true(all(grepl(
    "downstream[[:space:]]+R[[:space:]]+models",
    onboarding
  )))

  fine_tune <- sources[[3L]]
  expect_match(fine_tune, "evo2_finetune(", fixed = TRUE)
  expect_match(fine_tune, "fitted_score", fixed = TRUE)
  expect_match(fine_tune, "fitted_generation", fixed = TRUE)
  expect_match(fine_tune, "biological", ignore.case = TRUE)

  slurm <- sources[[4L]]
  expect_match(slurm, "not executed", ignore.case = TRUE)
  expect_match(slurm, "Evo 2 example", fixed = TRUE)
  expect_match(slurm, "recipe = evo2_recipe()", fixed = TRUE)
  expect_match(slurm, "async = TRUE", fixed = TRUE)
})
