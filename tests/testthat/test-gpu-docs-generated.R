test_that("checked-in GPU documentation is a successful static capture", {
  root <- testthat::test_path("..", "..")
  skip_if_not(file.exists(file.path(root, ".git")))

  paths <- file.path(
    root,
    c(
      "README.md",
      "vignettes/bionemor.Rmd",
      "vignettes/evo2-finetune.Rmd",
      "vignettes/slurm.Rmd"
    )
  )
  obsolete <- file.path(
    root,
    c("vignettes/brev-evo2.Rmd", "vignettes/evo2-inference.Rmd")
  )

  expect_true(all(file.exists(paths)))
  expect_false(any(file.exists(obsolete)))

  documents <- lapply(paths, readLines, warn = FALSE)
  expect_false(any(vapply(
    documents,
    function(lines) any(grepl("^[[:space:]]*```\\{", lines)),
    logical(1)
  )))
  expect_false(any(vapply(
    documents,
    function(lines) any(startsWith(trimws(lines), "#> Error")),
    logical(1)
  )))
  expect_false(any(vapply(
    documents,
    function(lines) any(grepl("NIM", lines, fixed = TRUE)),
    logical(1)
  )))

  all_text <- paste(unlist(documents, use.names = FALSE), collapse = "\n")
  expect_no_match(all_text, "Python remains", fixed = TRUE)
  expect_no_match(all_text, "write Python", fixed = TRUE)
  expect_no_match(all_text, "Python objects", fixed = TRUE)

  for (document in documents[1:3]) {
    text <- paste(document, collapse = "\n")
    expect_match(text, "NVIDIA L40S", fixed = TRUE)
    expect_match(text, "#>", fixed = TRUE)
    expect_match(text, "Status: pass", fixed = TRUE)
    expect_match(text, "forward_score", fixed = TRUE)
    expect_match(text, "finish_reason", fixed = TRUE)
  }
  for (document in documents[c(1L, 3L)]) {
    expect_match(
      paste(document, collapse = "\n"),
      '"succeeded"',
      fixed = TRUE
    )
  }
  expect_match(
    paste(documents[[4L]], collapse = "\n"),
    "not executed on Brev",
    fixed = TRUE
  )
})
