#!/usr/bin/env Rscript

wait_for_prediction <- function(job) {
  tryCatch(
    bionemor::job_wait(job, poll = 1, timeout = 900),
    error = function(error) {
      writeLines(bionemor::job_logs(job, tail = 200L))
      stop(error)
    }
  )
}

job_name <- function(type) {
  paste(
    "brev-evo2-framework",
    type,
    format(Sys.time(), "%Y%m%d%H%M%S"),
    Sys.getpid(),
    sep = "-"
  )
}

main <- function() {
  workspace <- Sys.getenv(
    "BIONEMOR_EVO2_WORKSPACE",
    unset = "/workspace"
  )
  stopifnot("BIONEMOR_EVO2_WORKSPACE must exist" = dir.exists(workspace))
  workspace <- normalizePath(workspace, mustWork = TRUE)
  compute <- bionemor::bionemo_compute(
    backend = "local",
    engine = "python",
    workspace = workspace,
    profile = "bionemo-2.6.3",
    gpus = 1L
  )
  checkpoint <- bionemor::evo2_checkpoint(
    bionemor::evo2("7b"),
    source = "hf://arcinstitute/savanna_evo2_7b",
    path = "checkpoints/evo2-7b-1m",
    compute = compute
  )
  model <- bionemor::evo2("7b", checkpoint = checkpoint)
  sequences <- c(
    reference = paste(rep("ACGT", 32L), collapse = ""),
    variant = paste0("T", paste(rep("ACGT", 31L), collapse = ""), "ACG")
  )

  checks <- bionemor::bionemo_doctor(
    compute,
    model = model,
    target = "inference",
    verbose = FALSE
  )
  print(checks)
  stopifnot("inference diagnostics failed" = checks@ok)

  score <- wait_for_prediction(stats::predict(
    model,
    sequences,
    type = "score",
    reduction = "mean",
    compute = compute,
    async = TRUE,
    name = job_name("score")
  ))
  stopifnot(
    "score output is incomplete" =
      identical(score@data$id, names(sequences)) &&
        length(score@data$score) == length(sequences) &&
        all(is.finite(score@data$score))
  )

  raw <- wait_for_prediction(stats::predict(
    model,
    sequences,
    type = "raw",
    compute = compute,
    async = TRUE,
    name = job_name("raw")
  ))
  stopifnot(
    "raw logits are not file-backed" =
      inherits(raw@data, "bionemor::BioNeMoArtifact"),
    "raw logits artifact is missing" = file.exists(raw@data@path)
  )

  response <- wait_for_prediction(stats::predict(
    model,
    sequences[["reference"]],
    type = "response",
    compute = compute,
    num_tokens = 8L,
    temperature = 0.7,
    top_k = 1L,
    top_p = 0,
    async = TRUE,
    name = job_name("response")
  ))
  stopifnot(
    "generated sequence is empty" =
      length(response@data) == 1L && nzchar(response@data[[1L]])
  )

  print(score@data)
  print(raw@data)
  print(response@data)
  cat("Evo 2 BioNeMo Framework smoke test passed.\n")
}

main()
