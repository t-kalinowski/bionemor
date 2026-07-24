looks_like_path <- function(x) {
  is_scalar_string(x) &&
    (grepl("[/\\\\]", x) ||
      grepl("\\.(fa|fasta|fna|fas)(\\.gz)?$", x, ignore.case = TRUE))
}

as_sequences <- function(x) {
  if (inherits(x, "XStringSet")) {
    x <- as.character(x)
  }
  stopifnot(
    "sequences must be a non-empty character vector or XStringSet" =
      is.character(x) && length(x) > 0L,
    "sequences must not contain missing or empty values" =
      !anyNA(x) && all(nzchar(x))
  )
  ids <- names(x)
  if (is.null(ids)) {
    ids <- as.character(seq_along(x))
  } else {
    empty <- is.na(ids) | !nzchar(ids)
    ids[empty] <- as.character(which(empty))
  }
  stopifnot(
    "sequence IDs must be unique" = !anyDuplicated(ids),
    "FASTA IDs and sequences must not contain line breaks" =
      !any(grepl("[\r\n]", ids)) && !any(grepl("[\r\n]", x))
  )
  names(x) <- ids
  x
}

write_fasta <- function(sequences, path) {
  sequences <- as_sequences(sequences)
  lines <- unlist(
    Map(
      function(id, sequence) c(paste0(">", id), sequence),
      names(sequences),
      sequences
    ),
    use.names = FALSE
  )
  writeLines(lines, path, useBytes = TRUE)
  path
}

read_fasta <- function(path) {
  connection <- if (grepl("\\.gz$", path, ignore.case = TRUE)) {
    gzfile(path, open = "rt")
  } else {
    file(path, open = "rt")
  }
  on.exit(close(connection), add = TRUE)
  lines <- readLines(connection, warn = FALSE)
  headers <- which(startsWith(lines, ">"))
  stopifnot(
    "FASTA data must contain at least one record" = length(headers) > 0L,
    "FASTA data must start with a header" = headers[[1L]] == 1L
  )
  ends <- c(headers[-1L] - 1L, length(lines))
  ids <- substring(lines[headers], 2L)
  stopifnot(
    "FASTA record IDs must not be empty" = all(nzchar(ids))
  )
  sequences <- Map(function(start, end) {
    if (start == end) {
      ""
    } else {
      paste0(lines[seq.int(start + 1L, end)], collapse = "")
    }
  }, headers, ends)
  as_sequences(stats::setNames(unlist(sequences, use.names = FALSE), ids))
}

prepare_sequence_input <- function(data, workspace, name) {
  if (is.character(data) && length(data) == 1L) {
    candidate <- normalize_path(data, base = workspace)
    if (file.exists(candidate)) {
      path <- normalizePath(candidate, mustWork = TRUE)
      sequences <- read_fasta(path)
      return(list(
        path = path,
        ids = names(sequences),
        sequences = sequences,
        source = "fasta"
      ))
    }
    if (looks_like_path(data)) {
      stop("data path does not exist: ", data, call. = FALSE)
    }
  }
  source <- if (is.data.frame(data)) "data.frame" else "memory"
  if (is.data.frame(data)) {
    stopifnot("data must contain a sequence column" = "sequence" %in% names(data))
    sequences <- data$sequence
    if ("id" %in% names(data)) {
      names(sequences) <- as.character(data$id)
    }
  } else {
    sequences <- data
  }
  sequences <- as_sequences(sequences)
  root <- file.path(workspace, ".bionemor", "jobs", name)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  path <- write_fasta(sequences, file.path(root, "sequences.fasta"))
  list(
    path = path,
    ids = names(sequences),
    sequences = sequences,
    source = source
  )
}

format_number <- function(x) {
  format(
    x,
    scientific = FALSE,
    trim = TRUE,
    digits = 15L,
    decimal.mark = "."
  )
}

number_arg <- function(name, value) {
  paste(name, format_number(value))
}

string_arg <- function(name, value) {
  paste(name, shQuote(as.character(value)))
}

evo2_fit_configs <- function(input, compute, name, control) {
  root <- file.path(compute@workspace, ".bionemor", "jobs", name)
  preprocessed <- file.path(root, "preprocessed")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  preprocess <- list(list(
    datapaths = list(input$path),
    output_dir = preprocessed,
    output_prefix = "bionemor",
    train_split = unname(control@split[["train"]]),
    valid_split = unname(control@split[["validation"]]),
    test_split = unname(control@split[["test"]]),
    overwrite = FALSE,
    embed_reverse_complement = FALSE,
    random_reverse_complement = 0,
    random_lineage_dropout = 0,
    transcribe = "back_transcribe",
    force_uppercase = TRUE,
    indexed_dataset_dtype = "uint8",
    tokenizer_type = "Byte-Level",
    append_eod = TRUE,
    workers = control@workers,
    drop_empty_sequences = TRUE,
    nnn_filter = FALSE,
    seed = control@seed
  ))
  preprocess_path <- file.path(root, "preprocess.yaml")
  yaml::write_yaml(preprocess, preprocess_path)

  dataset <- lapply(
    c(train = "train", validation = "val", test = "test"),
    function(suffix) {
      list(
        dataset_prefix = file.path(
          preprocessed,
          paste0("bionemor_byte-level_", suffix)
        ),
        dataset_split = switch(
          suffix,
          train = "train",
          val = "validation",
          test = "test"
        ),
        dataset_weight = 1
      )
    }
  )
  dataset_path <- file.path(root, "dataset.yaml")
  yaml::write_yaml(unname(dataset), dataset_path)
  list(
    preprocess = preprocess_path,
    dataset = dataset_path,
    directory = preprocessed
  )
}

optional_number_arg <- function(name, value) {
  if (is.null(value)) character() else number_arg(name, value)
}

evo2_train_command <- function(
  object,
  configs,
  compute,
  steps,
  control,
  output,
  name
) {
  checkpoint <- model_checkpoint_path(object, base = compute@workspace)
  args <- c(
    string_arg("--dataset-config", configs$dataset),
    string_arg("--dataset-dir", configs$directory),
    number_arg("--num-nodes", compute@nodes),
    number_arg("--devices", compute@gpus),
    number_arg("--seq-length", control@sequence_length),
    string_arg("--model-size", evo2_profile_size(object@size)),
    string_arg("--result-dir", output),
    string_arg("--experiment-name", name),
    number_arg("--max-steps", steps),
    number_arg("--lr", control@learning_rate),
    number_arg("--micro-batch-size", control@micro_batch_size),
    number_arg("--grad-acc-batches", control@gradient_accumulation),
    if (!is.null(checkpoint)) string_arg("--ckpt-dir", checkpoint),
    optional_number_arg("--min-lr", control@minimum_learning_rate),
    optional_number_arg("--warmup-steps", control@warmup_steps),
    if (control@precision == "fp8") "--fp8",
    optional_number_arg("--clip-grad", control@clip_gradient),
    optional_number_arg("--wd", control@weight_decay),
    optional_number_arg("--attention-dropout", control@attention_dropout),
    optional_number_arg("--hidden-dropout", control@hidden_dropout),
    optional_number_arg("--val-check-interval", control@validation_interval),
    optional_number_arg("--limit-val-batches", control@validation_batches),
    optional_number_arg(
      "--activation-checkpoint-recompute-num-layers",
      control@activation_checkpoint_layers
    ),
    number_arg("--workers", control@workers),
    number_arg("--seed", control@seed),
    if (control@asynchronous_checkpoint) "--ckpt-async-save",
    control@extra_args
  )
  checkpoint_root <- file.path(output, name, "checkpoints")
  stable_checkpoint <- file.path(output, "checkpoint")
  paste(
    c(
      paste("preprocess_evo2 --config", shQuote(configs$preprocess)),
      paste(c("train_evo2", args), collapse = " "),
      "shopt -s nullglob",
      paste0("BIONEMOR_CHECKPOINTS=(", shQuote(checkpoint_root), "/*-last)"),
      "test \"${#BIONEMOR_CHECKPOINTS[@]}\" -eq 1",
      paste("ln -s \"${BIONEMOR_CHECKPOINTS[0]}\"", shQuote(stable_checkpoint))
    ),
    collapse = "\n"
  )
}

validate_generation_controls <- function(
  num_tokens,
  temperature,
  top_k,
  top_p
) {
  stopifnot(
    "num_tokens must be a positive integer" =
      is_scalar_integerish(num_tokens, min = 1),
    "temperature must be positive" =
      is_scalar_number(temperature) && temperature > 0,
    "top_k must be a non-negative integer" =
      is_scalar_integerish(top_k, min = 0),
    "top_p must be between zero and one" =
      is_scalar_number(top_p) && top_p >= 0 && top_p <= 1
  )
  invisible(NULL)
}

validate_prediction_extra_args <- function(extra_args, precision) {
  stopifnot(
    "extra_args must be a character vector without missing values" =
      is.character(extra_args) && !anyNA(extra_args)
  )
  argument_flags <- c(
    input = "--fasta",
    checkpoint = "--ckpt-dir",
    output = "--output-dir",
    output = "--output-file",
    model_size = "--model-size",
    parallelism = "--tensor-parallel-size",
    parallelism = "--pipeline-model-parallel-size",
    parallelism = "--context-parallel-size",
    prompt = "--prompt",
    num_tokens = "--max-new-tokens",
    temperature = "--temperature",
    top_k = "--top-k",
    top_p = "--top-p",
    precision = "--fp8",
    precision = "--vortex-style-fp8",
    flash_decode = "--flash-decode",
    score_output = "--output-log-prob-seqs",
    reduction = "--log-prob-collapse-option"
  )
  tokens <- extra_arg_tokens(extra_args)
  duplicated_fields <- unique(names(argument_flags)[
    argument_flags %in% tokens
  ])
  if (length(duplicated_fields) > 0L) {
    stop(
      "extra_args duplicates typed prediction argument ",
      paste(duplicated_fields, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(precision)
  extra_args
}

evo2_predict_command <- function(
  object,
  input,
  type,
  compute,
  output,
  reduction,
  num_tokens,
  temperature,
  top_k,
  top_p,
  precision,
  extra_args,
  helper
) {
  checkpoint <- model_checkpoint_path(object, base = compute@workspace)
  parallel_args <- c(
    number_arg("--tensor-parallel-size", compute@gpus),
    number_arg("--pipeline-model-parallel-size", 1L),
    number_arg("--context-parallel-size", 1L)
  )
  if (type == "response") {
    files <- file.path(output, sprintf("%06d.txt", seq_along(input$sequences)))
    commands <- Map(function(sequence, path) {
      paste(c(
        "infer_evo2",
        string_arg("--prompt", sequence),
        string_arg("--ckpt-dir", checkpoint),
        number_arg("--temperature", temperature),
        number_arg("--top-k", top_k),
        number_arg("--top-p", top_p),
        number_arg("--max-new-tokens", num_tokens),
        parallel_args,
        if (precision == "fp8") c("--vortex-style-fp8", "True"),
        "--flash-decode=",
        string_arg("--output-file", path),
        extra_args
      ), collapse = " ")
    }, unname(input$sequences), files)
    return(paste(
      c(paste("mkdir -p", shQuote(output)), unlist(commands)),
      collapse = "\n"
    ))
  }

  args <- c(
    string_arg("--fasta", input$path),
    string_arg("--ckpt-dir", checkpoint),
    string_arg("--output-dir", output),
    string_arg("--model-size", evo2_profile_size(object@size)),
    parallel_args,
    if (precision == "fp8") "--fp8",
    if (type == "score") {
      c(
        "--output-log-prob-seqs",
        paste("--log-prob-collapse-option", reduction)
      )
    },
    extra_args
  )
  command <- paste(c("predict_evo2", args), collapse = " ")
  if (type == "score") {
    scores <- file.path(output, "scores.json")
    command <- paste(
      command,
      paste(
        "python",
        shQuote(helper),
        "score",
        shQuote(output),
        shQuote(scores)
      ),
      sep = "\n"
    )
  }
  command
}

wrap_compute_command <- function(command, compute, name) {
  if (compute@engine == "python") {
    return(list(command = command, container_name = NULL))
  }
  stopifnot(
    "container execution requires compute$image" = !is.null(compute@image)
  )
  if (compute@backend == "local") {
    container_name <- paste0("bionemor-", name)
    wrapped <- paste(
      "docker run --rm --gpus all --ipc=host",
      "--name", shQuote(container_name),
      "-v", paste0(shQuote(compute@workspace), ":", shQuote(compute@workspace)),
      "-w", shQuote(compute@workspace),
      shQuote(compute@image),
      "bash -lc", shQuote(command)
    )
    return(list(command = wrapped, container_name = container_name))
  }
  list(
    command = paste(
      "apptainer exec --nv",
      "--bind", paste0(shQuote(compute@workspace), ":", shQuote(compute@workspace)),
      "--pwd", shQuote(compute@workspace),
      shQuote(compute@image),
      "bash -lc", shQuote(command)
    ),
    container_name = NULL
  )
}

bound_operation_command <- function(command, compute, timeout) {
  if (!is.finite(timeout)) {
    return(command)
  }
  if (compute@backend == "local") {
    stopifnot(
      "finite operation timeouts require the timeout command" =
        command_available("timeout")
    )
  }
  paste(
    "timeout --signal=TERM --kill-after=15s",
    paste0(format_number(timeout), "s"),
    "bash -c",
    shQuote(command)
  )
}
