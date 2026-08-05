test_that("the installed Evo 2 runtime exercises every prediction result", {
  skip_if(
    Sys.getenv("BIONEMOR_TEST_GPU") != "true",
    "Set BIONEMOR_TEST_GPU=true to run the real GPU runtime tests."
  )

  required <- c(
    "BIONEMOR_EVO2_IMAGE",
    "BIONEMOR_EVO2_CHECKPOINT",
    "BIONEMOR_EVO2_WORKSPACE"
  )
  missing <- required[!nzchar(Sys.getenv(required))]
  if (length(missing)) {
    stop("missing GPU test settings: ", paste(missing, collapse = ", "))
  }

  workspace <- normalizePath(
    Sys.getenv("BIONEMOR_EVO2_WORKSPACE"),
    mustWork = TRUE
  )
  checkpoint <- normalizePath(
    Sys.getenv("BIONEMOR_EVO2_CHECKPOINT"),
    mustWork = TRUE
  )
  run_id <- paste0(
    "test-real-evo2-",
    Sys.getpid(),
    "-",
    format(as.integer(Sys.time()), scientific = FALSE)
  )
  output <- file.path(workspace, "test-outputs", run_id)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)

  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    backend = "local",
    engine = "container",
    image = Sys.getenv("BIONEMOR_EVO2_IMAGE"),
    workspace = workspace
  )
  compute <- bionemo_install(compute, pull = FALSE)
  model <- evo2(
    Sys.getenv("BIONEMOR_EVO2_MODEL", "7b"),
    checkpoint = checkpoint
  )
  sequence <- c(example = "ACGTACGTACGT")

  generated <- evo2_generate(
    model,
    sequence,
    compute,
    num_tokens = 4L,
    seed = 1L,
    return_probabilities = TRUE,
    name = paste0(run_id, "-generation")
  )
  expect_s3_class(generated, "evo2_generation")
  expect_equal(generated$id, names(sequence))
  expect_true(all(nzchar(generated$completion)))
  expect_true(all(vapply(
    generated$probabilities,
    function(value) all(is.finite(value)),
    logical(1)
  )))

  scores <- evo2_score(
    model,
    sequence,
    compute,
    name = paste0(run_id, "-score")
  )
  expect_s3_class(scores, "evo2_scores")
  expect_equal(scores$id, names(sequence))
  expect_true(all(is.finite(scores$score)))

  profile <- evo2_profile(
    model,
    sequence,
    compute,
    output = file.path(output, "profile.parquet"),
    name = paste0(run_id, "-profile")
  )
  expect_s3_class(profile, "bionemor::BioNeMoArtifact")
  expect_equal(profile@format, "parquet")
  expect_true(file.exists(profile@path))
  expect_gt(file.info(profile@path)$size, 0)

  pooled <- evo2_embed(
    model,
    sequence,
    compute,
    pool = "mean",
    name = paste0(run_id, "-embedding-pooled")
  )
  expect_s3_class(pooled, "evo2_embeddings")
  expect_equal(rownames(pooled), names(sequence))
  expect_true(all(is.finite(pooled)))

  unpooled <- evo2_embed(
    model,
    sequence,
    compute,
    pool = "none",
    output = file.path(output, "embedding-unpooled.parquet"),
    name = paste0(run_id, "-embedding-unpooled")
  )
  expect_s3_class(unpooled, "bionemor::BioNeMoArtifact")
  expect_equal(unpooled@format, "parquet")
  expect_identical(
    unpooled@schema,
    list(
      id = "string",
      position = "int64",
      embedding = "list<double>",
      strand = "string"
    )
  )
  expect_true(file.exists(unpooled@path))
  expect_gt(file.info(unpooled@path)$size, 0)
})
