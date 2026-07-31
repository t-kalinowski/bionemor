test_that("installed metadata and exports describe the Recipes runtime", {
  description <- packageDescription("bionemor")
  text <- paste(description[c("Title", "Description")], collapse = "\n")

  expect_match(text, "BioNeMo Recipes", fixed = TRUE)
  expect_match(text, "MBridge", fixed = TRUE)
  expect_true(all(
    c(
      "bionemo_install",
      "evo2_model",
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
  expect_match(
    readme,
    'pak::pak("t-kalinowski/bionemor")',
    fixed = TRUE
  )
  expect_match(readme, "without leaving R", fixed = TRUE)
  expect_match(readme, "## Run inference", fixed = TRUE)
  expect_match(readme, "## Fine-tune", fixed = TRUE)
  expect_match(readme, "Slurm support is experimental", fixed = TRUE)
  expect_match(readme, "Authenticate Docker to `nvcr.io`", fixed = TRUE)
  expect_match(readme, "GPU-backed", fixed = TRUE)

  package_help <- read_text(
    file.path("man", "bionemor-package.Rd"),
    "bionemor-package"
  )
  expect_false(grepl("NIM", package_help, fixed = TRUE))
  expect_false(grepl("Python objects", package_help, fixed = TRUE))

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
  expect_match(generation, "validation_warnings", fixed = TRUE)
  expect_match(generation, "log_probabilities", fixed = TRUE)

  scores <- read_text(file.path("man", "evo2_score.Rd"), "evo2_score")
  expect_match(scores, "reduced log probabilities", fixed = TRUE)
  expect_match(scores, "forward_score", fixed = TRUE)
  expect_match(scores, "reverse_score", fixed = TRUE)

  embeddings <- read_text(file.path("man", "evo2_embed.Rd"), "evo2_embed")
  expect_match(embeddings, "With", fixed = TRUE)
  expect_match(embeddings, 'pool = "none"', fixed = TRUE)
  expect_match(embeddings, "row names", fixed = TRUE)
  expect_match(embeddings, "averaged", fixed = TRUE)

  lora <- read_text(file.path("man", "evo2_lora.Rd"), "evo2_lora")
  expect_match(lora, "effective scale", fixed = TRUE)
  expect_match(lora, "dense_projection", fixed = TRUE)
  expect_match(lora, "linear_qkv", fixed = TRUE)
  expect_match(lora, "linear_fc1", fixed = TRUE)
  expect_match(lora, "word_embeddings", fixed = TRUE)
  expect_match(lora, "evo2_lora(", fixed = TRUE)

  dataset <- read_text(file.path("man", "evo2_dataset.Rd"), "evo2_dataset")
  expect_match(dataset, "stable hash", fixed = TRUE)
  expect_match(dataset, "named\\s+character\\s+vector")
  expect_match(dataset, "sequences <- c(", fixed = TRUE)

  preprocessing <- read_text(
    file.path("man", "evo2_preprocess_control.Rd"),
    "evo2_preprocess_control"
  )
  expect_match(preprocessing, "end-of-document token", fixed = TRUE)
  expect_match(preprocessing, "domain", fixed = TRUE)

  fitting <- read_text(
    file.path("man", "evo2_fit_control.Rd"),
    "evo2_fit_control"
  )
  expect_match(fitting, "gradient accumulation", ignore.case = TRUE)
  expect_match(fitting, "warm-up", ignore.case = TRUE)

  finetune <- read_text(
    file.path("man", "evo2_finetune.Rd"),
    "evo2_finetune"
  )
  expect_match(finetune, "prepared\\s+automatically")
  expect_match(finetune, "base checkpoint", fixed = TRUE)
  expect_match(finetune, "run <- evo2_finetune(", fixed = TRUE)

  compute <- read_text(
    file.path("man", "bionemo_compute.Rd"),
    "bionemo_compute"
  )
  expect_match(compute, "models\\s+and\\s+compute", ignore.case = TRUE)
  expect_match(compute, "bound\\s+to\\s+a\\s+model")
  expect_match(compute, "compute <- bionemo_compute(", fixed = TRUE)

  checkpoint <- read_text(
    file.path("man", "evo2_checkpoint.Rd"),
    "evo2_checkpoint"
  )
  expect_match(checkpoint, "pickle-based", fixed = TRUE)
  expect_match(checkpoint, "source revision", fixed = TRUE)
  expect_match(checkpoint, "checkpoint <- evo2_checkpoint(", fixed = TRUE)

  wait <- read_text(file.path("man", "job_wait.Rd"), "job_wait")
  expect_match(wait, "does\\s+not\\s+cancel")
  expect_match(wait, "generated <- job_wait(", fixed = TRUE)

  doctor <- read_text(
    file.path("man", "bionemo_doctor.Rd"),
    "bionemo_doctor"
  )
  expect_match(doctor, "does\\s+not\\s+install")
  expect_match(doctor, "doctor <- bionemo_doctor(", fixed = TRUE)

  specification <- testthat::test_path("..", "..", "SPEC.md")
  if (file.exists(specification)) {
    expect_match(
      read_text("SPEC.md"),
      "  timeout = Inf\n)",
      fixed = TRUE
    )
  }
})
