test_that("GPU documentation has a guarded manual render workflow", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  paths <- file.path(
    root,
    c(
      "README.Rmd",
      "vignettes-src/bionemor.Rmd",
      "vignettes-src/evo2-finetune.Rmd",
      "tools/render-gpu-docs.R"
    )
  )
  expect_true(all(file.exists(paths)))

  renderer <- paste(
    readLines(paths[[4L]], warn = FALSE),
    collapse = "\n"
  )
  expect_match(renderer, "BIONEMOR_DOCS_RENDER", fixed = TRUE)
  expect_match(renderer, "knitr::knit", fixed = TRUE)
  expect_match(renderer, "rendered document contains an error", fixed = TRUE)
  expect_match(renderer, "finally", fixed = TRUE)

  sources <- vapply(
    paths[1:3],
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  expect_true(all(grepl("error = FALSE", sources, fixed = TRUE)))

  onboarding <- sources[1:2]
  expect_true(all(grepl("NVIDIA GPU", onboarding, fixed = TRUE)))
  expect_true(all(grepl("no CPU fallback", onboarding, fixed = TRUE)))
  expect_true(all(grepl("BioNeMo Recipes", onboarding, fixed = TRUE)))
  expect_match(
    sources[[1L]],
    "saved, monitored, and reopened",
    fixed = TRUE
  )
  expect_true(all(grepl("Evo 2", onboarding, fixed = TRUE)))
  expect_true(all(grepl("ESM-2", onboarding, fixed = TRUE)))
  expect_true(all(grepl("Brev", onboarding, fixed = TRUE)))
  expect_true(all(grepl(
    "Supported models and operations",
    onboarding,
    fixed = TRUE
  )))
  expect_true(all(grepl("evo2_score()", onboarding, fixed = TRUE)))
  expect_true(all(grepl("esm2_embed()", onboarding, fixed = TRUE)))
  expect_true(all(grepl("recipe = evo2_recipe()", onboarding, fixed = TRUE)))
  expect_true(all(grepl("recipe = esm2_recipe()", onboarding, fixed = TRUE)))
  expect_true(all(grepl("Megatron Bridge", onboarding, fixed = TRUE)))
  expect_true(all(grepl(
    "model descriptor",
    tolower(onboarding),
    fixed = TRUE
  )))
  expect_true(all(grepl("compute descriptor", onboarding, fixed = TRUE)))
  expect_true(all(grepl("container", onboarding, fixed = TRUE)))
  expect_true(all(grepl("pulls it anonymously", onboarding, fixed = TRUE)))
  expect_false(any(grepl("NGC_API_KEY", onboarding, fixed = TRUE)))
  expect_true(all(grepl(
    'evo2_model("7b", evo2_compute)',
    onboarding,
    fixed = TRUE
  )))
  expect_true(all(grepl(
    "BIONEMOR_DOCS_CHECKPOINT",
    onboarding,
    fixed = TRUE
  )))
  expect_true(all(grepl(
    "checkpoint = checkpoint",
    onboarding,
    fixed = TRUE
  )))
  expect_true(all(grepl(
    'esm2_model("8m", esm2_compute)',
    onboarding,
    fixed = TRUE
  )))
  expect_true(all(grepl("esm2_embed(", onboarding, fixed = TRUE)))
  expect_true(all(grepl("Transformers", onboarding, fixed = TRUE)))
  expect_false(any(grepl("vLLM", onboarding, fixed = TRUE)))
  expect_true(all(grepl("gpus = 1", onboarding, fixed = TRUE)))
  expect_true(all(grepl("similarity", onboarding, fixed = TRUE)))
  expect_true(all(grepl("clustering", onboarding, fixed = TRUE)))
  expect_true(all(grepl(
    "downstream[[:space:]]+R[[:space:]]+models",
    onboarding
  )))
  expect_true(all(grepl("# In a new R session:", onboarding, fixed = TRUE)))

  site_guides <- c(
    "https://t-kalinowski.github.io/bionemor/articles/bionemor.html",
    "https://t-kalinowski.github.io/bionemor/articles/evo2-finetune.html"
  )
  expect_true(all(vapply(
    site_guides,
    grepl,
    logical(1),
    x = sources[[1L]],
    fixed = TRUE
  )))

  fine_tune <- sources[[3L]]
  expect_match(fine_tune, "BIONEMOR_DOCS_CHECKPOINT", fixed = TRUE)
  expect_match(fine_tune, "checkpoint = checkpoint", fixed = TRUE)
  expect_match(fine_tune, "evo2_finetune(", fixed = TRUE)
  expect_match(fine_tune, "fitted_score", fixed = TRUE)
  expect_match(fine_tune, "fitted_generation", fixed = TRUE)
  expect_match(fine_tune, "callr::r(", fixed = TRUE)
  expect_match(fine_tune, "bionemo_job(", fixed = TRUE)
  expect_match(fine_tune, "biological", ignore.case = TRUE)

  announcement <- paste(
    readLines(file.path(root, "blog", "first-release.md"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(announcement, "run <- evo2_finetune(", fixed = TRUE)
  expect_match(announcement, "async = TRUE", fixed = TRUE)
  expect_match(announcement, "fitted <- job_wait(run)", fixed = TRUE)
  expect_match(announcement, "Biostrings::DNAStringSet(", fixed = TRUE)
})

test_that("user-facing docs present ESM-2 as an added capability", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))
  paths <- file.path(
    root,
    c(
      "README.Rmd",
      "README.md",
      "vignettes-src/bionemor.Rmd",
      "vignettes/bionemor.Rmd",
      "blog/first-release.md",
      "R/bionemor-package.R",
      "man/bionemor-package.Rd"
    )
  )
  documents <- vapply(
    paths,
    function(path) paste(readLines(path, warn = FALSE), collapse = " "),
    character(1)
  )
  expect_true(all(grepl("supports.*ESM-2", documents)))

  restriction_documents <- c(
    documents,
    vapply(
      file.path(
        root,
        c("R/06-adapter-esm2-recipe.R", "man/esm2_recipe.Rd")
      ),
      function(path) paste(readLines(path, warn = FALSE), collapse = " "),
      character(1)
    )
  )

  restriction_first <- c(
    "has its own adapter",
    "separate model families",
    "has the broadest interface",
    "uses its own recipe",
    "has a separate model and recipe",
    "different model families",
    "same API works with another model family",
    "Why Evo 2 and ESM-2 are in one package",
    "does not compile vLLM"
  )
  for (phrase in restriction_first) {
    expect_false(
      any(grepl(phrase, restriction_documents, fixed = TRUE)),
      info = phrase
    )
  }
})

test_that("user-facing docs describe saved jobs without durable terminology", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))

  prose_paths <- c(
    file.path(
      root,
      c("DESCRIPTION", "README.Rmd", "README.md", "_pkgdown.yml")
    ),
    list.files(file.path(root, "blog"), full.names = TRUE),
    list.files(file.path(root, "vignettes-src"), full.names = TRUE),
    list.files(file.path(root, "vignettes"), full.names = TRUE),
    list.files(file.path(root, "man"), full.names = TRUE)
  )
  prose <- unlist(lapply(prose_paths, readLines, warn = FALSE), use.names = FALSE)

  r_paths <- list.files(file.path(root, "R"), full.names = TRUE)
  roxygen <- unlist(
    lapply(r_paths, function(path) {
      lines <- readLines(path, warn = FALSE)
      lines[startsWith(lines, "#'")]
    }),
    use.names = FALSE
  )

  expect_false(any(grepl("\\bdurable\\b", c(prose, roxygen), ignore.case = TRUE)))
})
