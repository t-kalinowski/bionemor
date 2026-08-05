test_that("checked-in GPU documentation is a successful static capture", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  paths <- file.path(
    root,
    c(
      "README.md",
      "vignettes/bionemor.Rmd",
      "vignettes/evo2-finetune.Rmd"
    )
  )
  expect_true(all(file.exists(paths)))

  documents <- vapply(
    paths,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  expect_false(any(grepl("```\\{", documents)))
  expect_false(any(grepl("#> Error", documents, fixed = TRUE)))
  expect_false(any(grepl("NGC_CLI_API_KEY", documents, fixed = TRUE)))
  expect_false(any(grepl("HF_TOKEN", documents, fixed = TRUE)))

  onboarding <- documents[1:2]
  expect_true(all(grepl("NVIDIA GPU", onboarding, fixed = TRUE)))
  expect_true(all(grepl("no CPU fallback", onboarding, fixed = TRUE)))
  expect_true(all(grepl("Evo 2", onboarding, fixed = TRUE)))
  expect_true(all(grepl("ESM-2", onboarding, fixed = TRUE)))
  expect_true(all(grepl("esm2/embed", onboarding, fixed = TRUE)))
  expect_true(all(grepl(
    "#> \\[1\\][[:space:]]+2[[:space:]]+320",
    onboarding
  )))
  expect_true(all(grepl("Brev", onboarding, fixed = TRUE)))
  expect_true(all(grepl("NVIDIA L40S", onboarding, fixed = TRUE)))
  expect_true(all(grepl("forward_score", onboarding, fixed = TRUE)))
  expect_true(all(grepl("finish_reason", onboarding, fixed = TRUE)))
  expect_true(all(grepl("Megatron Bridge", onboarding, fixed = TRUE)))
  expect_true(all(grepl("# In a new R session:", onboarding, fixed = TRUE)))

  fine_tune <- documents[[3L]]
  expect_match(fine_tune, "NVIDIA L40S", fixed = TRUE)
  expect_match(fine_tune, '"succeeded"', fixed = TRUE)
  expect_match(fine_tune, "fitted_score", fixed = TRUE)
  expect_match(fine_tune, "fitted_generation", fixed = TRUE)
  expect_match(fine_tune, "callr::r(", fixed = TRUE)
})
