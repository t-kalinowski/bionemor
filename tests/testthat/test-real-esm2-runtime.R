test_that("the installed ESM-2 runtime writes portable embeddings", {
  skip_if(
    Sys.getenv("BIONEMOR_TEST_GPU") != "true",
    "Set BIONEMOR_TEST_GPU=true to run the real GPU runtime tests."
  )

  required <- c("BIONEMOR_ESM2_IMAGE", "BIONEMOR_ESM2_WORKSPACE")
  missing <- required[!nzchar(Sys.getenv(required))]
  if (length(missing)) {
    stop("missing GPU test settings: ", paste(missing, collapse = ", "))
  }

  workspace <- normalizePath(
    Sys.getenv("BIONEMOR_ESM2_WORKSPACE"),
    mustWork = TRUE
  )
  run_id <- basename(tempfile("test-real-esm2-", tmpdir = workspace))
  output <- file.path(workspace, "test-outputs", run_id, "embedding-pooled")
  compute <- bionemo_compute(
    recipe = esm2_recipe(),
    backend = "local",
    engine = "container",
    image = Sys.getenv("BIONEMOR_ESM2_IMAGE"),
    workspace = workspace
  )
  compute <- bionemo_install(compute, pull = FALSE)
  model <- esm2_model("8m", compute)
  proteins <- c(reference = "MKT", variant = "MNT")

  embeddings <- esm2_embed(
    model,
    proteins,
    output = output,
    name = paste0(run_id, "-embedding")
  )

  expect_s3_class(embeddings, "esm2_embeddings")
  expect_identical(dim(embeddings), c(2L, 320L))
  expect_identical(rownames(embeddings), names(proteins))
  expect_true(all(is.finite(embeddings)))
  expect_equal(unname(sqrt(rowSums(embeddings^2))), c(1, 1), tolerance = 1e-5)
  expect_pooled_embedding_output(
    output,
    unname(dim(embeddings)),
    rownames(embeddings),
    "float32"
  )
})
