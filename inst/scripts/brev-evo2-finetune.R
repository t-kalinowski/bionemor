#!/usr/bin/env Rscript

genome_accession <- "GCF_000005845.2_ASM584v2"
genome_md5 <- "c13d459b5caa702ff7e1f26fe44b8ad7"
genome_url <- paste0(
  "https://ftp.ncbi.nlm.nih.gov/genomes/all/GCF/000/005/845/",
  genome_accession,
  "/",
  genome_accession,
  "_genomic.fna.gz"
)

write_fasta <- function(sequences, path) {
  lines <- unlist(
    Map(
      function(id, sequence) c(paste0(">", id), sequence),
      names(sequences),
      unname(sequences)
    ),
    use.names = FALSE
  )
  writeLines(lines, path, useBytes = TRUE)
}

read_fasta <- function(path) {
  lines <- readLines(path, warn = FALSE)
  headers <- which(startsWith(lines, ">"))
  stopifnot(
    "FASTA must contain records" = length(headers) > 0L,
    "FASTA must start with a header" = headers[[1L]] == 1L
  )
  ends <- c(headers[-1L] - 1L, length(lines))
  sequences <- Map(
    function(start, end) {
      paste(lines[seq.int(start + 1L, end)], collapse = "")
    },
    headers,
    ends
  )
  names(sequences) <- substring(lines[headers], 2L)
  unlist(sequences, use.names = TRUE)
}

wait_for_job <- function(job) {
  tryCatch(
    bionemor::job_wait(job, poll = 1, timeout = 900),
    error = function(error) {
      writeLines(bionemor::job_logs(job, tail = 200L))
      stop(error)
    }
  )
}

make_compute <- function(workspace) {
  bionemor::bionemo_compute(
    backend = "local",
    engine = "python",
    workspace = workspace,
    profile = "bionemo-2.6.3",
    gpus = 1L
  )
}

make_model <- function(checkpoint) {
  bionemor::evo2("1b", checkpoint = checkpoint)
}

score_checkpoint <- function(checkpoint, sequences, compute, run_dir, label) {
  result <- wait_for_job(stats::predict(
    make_model(checkpoint),
    sequences,
    type = "score",
    reduction = "mean",
    precision = "fp8",
    compute = compute,
    async = TRUE,
    name = paste0(basename(run_dir), "-", label, "-score")
  ))
  stopifnot(
    "score result has the wrong IDs" =
      identical(result@data$id, names(sequences)),
    "score result contains non-finite values" =
      all(is.finite(result@data$score))
  )
  result@data
}

generate_checkpoint <- function(
  checkpoint,
  sequence,
  compute,
  run_dir,
  label
) {
  result <- wait_for_job(stats::predict(
    make_model(checkpoint),
    sequence,
    type = "response",
    precision = "fp8",
    compute = compute,
    async = TRUE,
    name = paste0(basename(run_dir), "-", label, "-response"),
    num_tokens = 16L,
    temperature = 0.7,
    top_k = 1L,
    top_p = 0
  ))
  stopifnot(
    "generated sequence is empty" =
      length(result@data) == 1L && nzchar(result@data[[1L]])
  )
  unname(result@data[[1L]])
}

prepare_data <- function(run_dir) {
  data_dir <- file.path(run_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  compressed <- file.path(
    data_dir,
    paste0(genome_accession, "_genomic.fna.gz")
  )
  if (!file.exists(compressed)) {
    utils::download.file(
      genome_url,
      compressed,
      mode = "wb",
      quiet = FALSE
    )
  }
  stopifnot(
    "downloaded E. coli genome has the wrong MD5" =
      identical(unname(tools::md5sum(compressed)), genome_md5)
  )

  lines <- readLines(gzfile(compressed), warn = FALSE)
  headers <- which(startsWith(lines, ">"))
  stopifnot(
    "downloaded genome has no FASTA records" = length(headers) > 0L,
    "downloaded genome does not start with a FASTA header" =
      headers[[1L]] == 1L
  )
  chromosome_end <- if (length(headers) == 1L) {
    length(lines)
  } else {
    headers[[2L]] - 1L
  }
  chromosome <- paste(lines[seq.int(2L, chromosome_end)], collapse = "")
  stopifnot(
    "chromosome is too short" = nchar(chromosome) >= 20000L,
    "chromosome contains non-ACGT bases" = grepl("^[ACGT]+$", chromosome)
  )

  width <- 1024L
  size <- nchar(chromosome)
  train_starts <- unique(as.integer(floor(seq(
    1,
    floor(size * 0.8) - width + 1L,
    length.out = 384L
  ))))
  heldout_starts <- unique(as.integer(floor(seq(
    ceiling(size * 0.9),
    size - width + 1L,
    length.out = 8L
  ))))
  stopifnot(
    "could not create 384 distinct training windows" =
      length(train_starts) == 384L,
    "could not create eight distinct held-out windows" =
      length(heldout_starts) == 8L
  )

  windows <- function(starts, prefix) {
    values <- substring(chromosome, starts, starts + width - 1L)
    names(values) <- sprintf("%s_%03d", prefix, seq_along(values))
    values
  }
  train <- windows(train_starts, "train")
  heldout <- windows(heldout_starts, "heldout")
  stopifnot(
    "training windows have the wrong length" = all(nchar(train) == width),
    "held-out windows have the wrong length" =
      all(nchar(heldout) == width)
  )
  write_fasta(train, file.path(data_dir, "train.fasta"))
  write_fasta(heldout, file.path(data_dir, "heldout.fasta"))
  jsonlite::write_json(
    list(
      accession = genome_accession,
      source = genome_url,
      md5 = genome_md5,
      training_records = length(train),
      heldout_records = length(heldout),
      sequence_length = width
    ),
    file.path(data_dir, "provenance.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

prepare_checkpoint <- function(compute) {
  bionemor::evo2_checkpoint(
    bionemor::evo2("1b"),
    source = "hf://arcinstitute/savanna_evo2_1b_base",
    path = "checkpoints/evo2-1b-8k",
    compute = compute
  )
}

run_baseline <- function(run_dir, checkpoint, compute) {
  model <- make_model(checkpoint)
  inference_checks <- bionemor::bionemo_doctor(
    compute,
    model = model,
    target = "inference",
    verbose = FALSE
  )
  training_checks <- bionemor::bionemo_doctor(
    compute,
    model = model,
    target = "training",
    verbose = FALSE
  )
  print(inference_checks)
  print(training_checks)
  stopifnot(
    "inference diagnostics failed" = inference_checks@ok,
    "training diagnostics failed" = training_checks@ok
  )

  heldout <- read_fasta(file.path(run_dir, "data", "heldout.fasta"))
  scored <- score_checkpoint(
    checkpoint,
    heldout,
    compute,
    run_dir,
    "baseline"
  )
  generated <- generate_checkpoint(
    checkpoint,
    heldout[[1L]],
    compute,
    run_dir,
    "baseline"
  )
  jsonlite::write_json(
    list(
      checkpoint = bionemor::checkpoint_path(checkpoint),
      scores = scored$score,
      ids = scored$id,
      generated = generated
    ),
    file.path(run_dir, "baseline.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

run_fit <- function(run_dir, checkpoint, compute) {
  control <- bionemor::evo2_fit_control(
    sequence_length = 1024L,
    learning_rate = 1.5e-5,
    minimum_learning_rate = 1.49e-5,
    warmup_steps = 3L,
    micro_batch_size = 1L,
    gradient_accumulation = 1L,
    precision = "fp8",
    clip_gradient = 250,
    weight_decay = 0.001,
    attention_dropout = 0.01,
    hidden_dropout = 0.01,
    validation_interval = 3L,
    validation_batches = 1L,
    activation_checkpoint_layers = 5L,
    workers = 2L,
    seed = 41L,
    asynchronous_checkpoint = TRUE
  )
  job <- generics::fit(
    make_model(checkpoint),
    data = file.path(run_dir, "data", "train.fasta"),
    compute = compute,
    steps = 3L,
    control = control,
    name = paste0(basename(run_dir), "-fit"),
    output = file.path(run_dir, "training"),
    timeout = 300,
    async = TRUE
  )
  fitted <- wait_for_job(job)
  stopifnot(
    "fitted checkpoint is missing" =
      dir.exists(bionemor::checkpoint_path(fitted))
  )
  saveRDS(fitted, file.path(run_dir, "fitted-model.rds"))
}

run_verify <- function(run_dir, compute) {
  baseline_path <- file.path(run_dir, "baseline.json")
  fitted_model_path <- file.path(run_dir, "fitted-model.rds")
  stopifnot(
    "baseline result is missing" = file.exists(baseline_path),
    "fitted model is missing" = file.exists(fitted_model_path)
  )
  baseline <- jsonlite::read_json(
    baseline_path,
    simplifyVector = TRUE
  )
  fitted <- readRDS(fitted_model_path)
  fitted_path <- bionemor::checkpoint_path(fitted)
  heldout <- read_fasta(file.path(run_dir, "data", "heldout.fasta"))
  scored <- score_checkpoint(
    fitted_path,
    heldout,
    compute,
    run_dir,
    "fitted"
  )
  generated <- generate_checkpoint(
    fitted_path,
    heldout[[1L]],
    compute,
    run_dir,
    "fitted"
  )
  changed <- any(abs(as.numeric(scored$score) - baseline$scores) > 1e-12)
  stopifnot("fitting did not change any held-out score" = changed)

  training_seconds <- as.integer(readLines(
    file.path(run_dir, "training-seconds.txt"),
    warn = FALSE,
    n = 1L
  ))
  stopifnot(
    "training duration is invalid" =
      length(training_seconds) == 1L && !is.na(training_seconds),
    "training exceeded 300 seconds" = training_seconds <= 300L
  )
  summary <- list(
    status = "passed",
    training_seconds = training_seconds,
    baseline = baseline,
    fitted = list(
      checkpoint = fitted_path,
      scores = scored$score,
      ids = scored$id,
      generated = generated
    ),
    scores_changed = changed
  )
  jsonlite::write_json(
    summary,
    file.path(run_dir, "summary.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  print(summary)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  stopifnot(
    "supply one phase: prepare, baseline, fit, or verify" =
      length(args) == 1L &&
        args %in% c("prepare", "baseline", "fit", "verify")
  )
  phase <- args[[1L]]
  workspace <- Sys.getenv(
    "BIONEMOR_EVO2_WORKSPACE",
    unset = "/workspace"
  )
  run_id <- Sys.getenv("BIONEMOR_EVO2_RUN_ID")
  stopifnot(
    "BIONEMOR_EVO2_WORKSPACE must exist" = dir.exists(workspace),
    "BIONEMOR_EVO2_RUN_ID must be a safe name" =
      nzchar(run_id) &&
        grepl("^[A-Za-z0-9_.-]+$", run_id) &&
        !(run_id %in% c(".", ".."))
  )
  workspace <- normalizePath(workspace, mustWork = TRUE)
  run_dir <- file.path(workspace, "runs", run_id)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  compute <- make_compute(workspace)

  if (phase == "prepare") {
    prepare_checkpoint(compute)
    prepare_data(run_dir)
    return(invisible(NULL))
  }
  checkpoint_path <- file.path(
    workspace,
    "checkpoints",
    "evo2-1b-8k"
  )
  stopifnot(
    "the Evo 2 1B checkpoint must exist" = dir.exists(checkpoint_path)
  )
  checkpoint <- bionemor::evo2(
    "1b",
    checkpoint = checkpoint_path
  )@checkpoint
  switch(
    phase,
    baseline = run_baseline(run_dir, checkpoint, compute),
    fit = run_fit(run_dir, checkpoint, compute),
    verify = run_verify(run_dir, compute)
  )
}

main()
