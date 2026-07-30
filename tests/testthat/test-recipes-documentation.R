test_that("installed metadata and exports describe the Recipes runtime", {
  description <- packageDescription("bionemor")
  text <- paste(description[c("Title", "Description")], collapse = "\n")

  expect_match(text, "BioNeMo Recipes", fixed = TRUE)
  expect_match(text, "MBridge", fixed = TRUE)
  expect_true(all(
    c(
      "bionemo_install",
      "evo2_generate",
      "evo2_score",
      "evo2_finetune"
    ) %in%
      getNamespaceExports("bionemor")
  ))
  expect_false(grepl("reticulate", description$Imports, fixed = TRUE))
})

test_that("runtime assets are pinned to BioNeMo Recipes", {
  root <- system.file(package = "bionemor")
  lock <- jsonlite::read_json(
    file.path(root, "recipes", "evo2.json"),
    simplifyVector = TRUE
  )
  expect_equal(
    lock$repository,
    "https://github.com/NVIDIA-BioNeMo/bionemo-recipes"
  )
  expect_match(lock$revision, "^[0-9a-f]{40}$")
  expect_equal(lock$subdirectory, "recipes/evo2_megatron")
  expect_equal(
    basename(list.dirs(
      file.path(root, "docker"),
      recursive = FALSE
    )),
    "evo2-recipes"
  )
  expect_true(file.exists(file.path(
    root,
    "scripts",
    "materialize-evo2.py"
  )))
})

test_that("public documentation states the current runtime and API contracts", {
  read_text <- function(path, topic = NULL) {
    source <- testthat::test_path("..", "..", path)
    if (file.exists(source)) {
      return(paste(readLines(source, warn = FALSE), collapse = "\n"))
    }
    if (is.null(topic)) {
      installed <- system.file(path, package = "bionemor")
      stopifnot(nzchar(installed))
      return(paste(readLines(installed, warn = FALSE), collapse = "\n"))
    }
    paste(
      capture.output(tools::Rd2txt(utils:::.getHelpFile(
        help(topic, package = "bionemor")
      ))),
      collapse = "\n"
    )
  }

  readme <- read_text("README.md")
  expect_false(grepl("tested source revision", readme, fixed = TRUE))
  expect_match(readme, "Authenticate Docker to `nvcr.io`", fixed = TRUE)
  expect_match(readme, "GPU-backed", fixed = TRUE)

  reexports <- read_text(file.path("man", "reexports.Rd"), "reexports")
  expect_match(reexports, "fit(", fixed = TRUE)
  expect_match(reexports, "object", fixed = TRUE)
  expect_match(
    reexports,
    'type = c("score", "generate", "embedding", "response", "raw")',
    fixed = TRUE
  )

  generation <- read_text(
    file.path("man", "evo2_generate.Rd"),
    "evo2_generate"
  )
  expect_match(
    generation,
    "At most one of",
    fixed = TRUE
  )

  embeddings <- read_text(file.path("man", "evo2_embed.Rd"), "evo2_embed")
  expect_match(embeddings, "With", fixed = TRUE)
  expect_match(embeddings, 'pool = "none"', fixed = TRUE)

  specification <- testthat::test_path("..", "..", "SPEC.md")
  if (file.exists(specification)) {
    expect_match(
      read_text("SPEC.md"),
      "  timeout = Inf\n)",
      fixed = TRUE
    )
  }
})
