library(bionemor)

workspace <- Sys.getenv(
  "BIONEMOR_EVO2_WORKSPACE",
  "/home/ubuntu/bionemor-workspace"
)
image <- Sys.getenv("BIONEMOR_EVO2_IMAGE")
checkpoint_path <- Sys.getenv("BIONEMOR_EVO2_CHECKPOINT")
stopifnot(
  "BIONEMOR_EVO2_IMAGE is required" = nzchar(image),
  "BIONEMOR_EVO2_CHECKPOINT is required" = nzchar(checkpoint_path),
  "BIONEMOR_EVO2_WORKSPACE must exist" = dir.exists(workspace),
  "BIONEMOR_EVO2_CHECKPOINT must be an existing directory" = dir.exists(
    checkpoint_path
  )
)

workspace <- normalizePath(workspace, mustWork = TRUE)
checkpoint_path <- normalizePath(checkpoint_path, mustWork = TRUE)
stopifnot(
  "BIONEMOR_EVO2_CHECKPOINT must be inside BIONEMOR_EVO2_WORKSPACE" = identical(
    checkpoint_path,
    workspace
  ) ||
    startsWith(checkpoint_path, paste0(workspace, .Platform$file.sep))
)

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
stopifnot("BioNeMo doctor failed" = doctor@ok)

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
stopifnot(
  "generation did not return one result per input" = nrow(generated) ==
    length(sequences),
  "scoring did not return one result per input" = nrow(scores) ==
    length(sequences),
  "scoring returned non-finite values" = all(is.finite(scores$score)),
  "embedding did not return one result per input" = nrow(embeddings) ==
    length(sequences),
  "embedding returned non-finite values" = all(is.finite(embeddings))
)

cat("BioNeMo Recipes Evo 2 smoke test passed.\n")
