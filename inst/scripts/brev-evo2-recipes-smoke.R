library(bionemor)

workspace <- Sys.getenv(
  "BIONEMOR_EVO2_WORKSPACE",
  "/home/ubuntu/bionemor-workspace"
)
image <- Sys.getenv("BIONEMOR_EVO2_IMAGE")
checkpoint_path <- Sys.getenv("BIONEMOR_EVO2_CHECKPOINT")
if (!nzchar(image)) {
  stop("BIONEMOR_EVO2_IMAGE is required")
}
if (!nzchar(checkpoint_path)) {
  stop("BIONEMOR_EVO2_CHECKPOINT is required")
}
if (!dir.exists(workspace)) {
  stop("BIONEMOR_EVO2_WORKSPACE must exist")
}
if (!dir.exists(checkpoint_path)) {
  stop("BIONEMOR_EVO2_CHECKPOINT must be an existing directory")
}

workspace <- normalizePath(workspace, mustWork = TRUE)
checkpoint_path <- normalizePath(checkpoint_path, mustWork = TRUE)
if (
  !identical(checkpoint_path, workspace) &&
    !startsWith(checkpoint_path, paste0(workspace, .Platform$file.sep))
) {
  stop("BIONEMOR_EVO2_CHECKPOINT must be inside BIONEMOR_EVO2_WORKSPACE")
}

compute <- bionemo_compute(
  backend = "local",
  engine = "container",
  image = image,
  workspace = workspace
)
compute <- bionemo_install(compute, pull = FALSE)
model <- evo2("7b", checkpoint = checkpoint_path)

doctor <- bionemo_doctor(
  compute,
  model = model,
  target = "inference",
  verbose = TRUE
)
if (!doctor@ok) {
  stop("BioNeMo doctor failed")
}

sequences <- c(first = "ACGTACGT", second = "GCTAGCTA")
generated <- evo2_generate(
  model,
  sequences,
  compute,
  num_tokens = 8L,
  seed = 1L
)
scores <- evo2_score(model, sequences, compute)
embeddings <- evo2_embed(model, sequences, compute, pool = "mean")
if (nrow(generated) != length(sequences)) {
  stop("generation did not return one result per input")
}
if (nrow(scores) != length(sequences)) {
  stop("scoring did not return one result per input")
}
if (!all(is.finite(scores$score))) {
  stop("scoring returned non-finite values")
}
if (nrow(embeddings) != length(sequences)) {
  stop("embedding did not return one result per input")
}
if (!all(is.finite(embeddings))) {
  stop("embedding returned non-finite values")
}

cat("BioNeMo Recipes Evo 2 smoke test passed.\n")
