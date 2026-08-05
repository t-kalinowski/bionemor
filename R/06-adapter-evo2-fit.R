dataset_records <- function(x, id_col, sequence_col) {
  if (inherits(x, "XStringSet")) {
    x <- as.character(x)
  }
  if (is.data.frame(x)) {
    if (!id_col %in% names(x)) {
      stop("data must contain the configured ID column")
    }
    if (!sequence_col %in% names(x)) {
      stop("data must contain the configured sequence column")
    }
    ids <- as.character(x[[id_col]])
    sequences <- as.character(x[[sequence_col]])
  } else if (
    is.character(x) &&
      length(x) > 1L ||
      is.character(x) && length(x) == 1L && !file.exists(x)
  ) {
    sequences <- as.character(x)
    ids <- names(sequences)
    if (is.null(ids)) {
      ids <- paste0("seq_", seq_along(sequences))
    }
  } else {
    return(NULL)
  }
  stopifnot(
    "dataset must contain at least one sequence" = length(sequences) > 0L,
    "dataset IDs must be non-empty and unique" = !anyNA(ids) &&
      all(nzchar(ids)) &&
      !anyDuplicated(ids),
    "dataset sequences must be non-empty" = !anyNA(sequences) &&
      all(nzchar(sequences)),
    "dataset IDs and sequences must not contain line breaks" = !any(grepl(
      "[\r\n]",
      ids
    )) &&
      !any(grepl("[\r\n]", sequences))
  )
  stats::setNames(sequences, ids)
}

dataset_source <- function(x, id_col, sequence_col) {
  records <- dataset_records(x, id_col, sequence_col)
  if (!is.null(records)) {
    return(records)
  }
  if (!is_scalar_string(x) || !file.exists(x)) {
    stop("dataset input must be in memory or an existing FASTA path")
  }
  normalizePath(x, mustWork = TRUE)
}

#' Describe an Evo 2 dataset
#'
#' `evo2_dataset()` records sequence partitions before the recipe converts them
#' to indexed training data. Each input may be a named character vector, a data
#' frame, an `XStringSet` such as `DNAStringSet`, or an existing FASTA or
#' gzip-compressed FASTA path.
#'
#' Character-vector names become sequence IDs; unnamed vectors receive
#' `seq_1`, `seq_2`, and so on. Data frames use `id_col` and `sequence_col`.
#' IDs must be non-empty and unique, and sequences must be non-empty.
#'
#' @param train,validation,test Sequence inputs. `train` is required.
#'   `validation` and `test` may be `NULL`.
#' @param split Named train, validation, and test proportions used when
#'   both explicit validation and test inputs are absent. Assignment uses a
#'   stable hash of `seed` and sequence ID, so it is unchanged by input order.
#'   The proportions are probabilities and need not produce exact row counts.
#' @param seed Non-negative seed used for stable hash partitioning.
#' @param id_col,sequence_col Data-frame column names containing sequence IDs
#'   and sequence text.
#'
#' @return An S7 `Evo2Dataset`.
#'
#' @examples
#' sequences <- c(
#'   first = "ACGTACGT",
#'   second = "TGCATGCA",
#'   third = "GATTACA"
#' )
#' data <- evo2_dataset(
#'   sequences,
#'   split = c(train = 0.7, validation = 0.2, test = 0.1),
#'   seed = 17L
#' )
#' data@provenance$partition_method
#'
#' @export
evo2_dataset <- function(
  train,
  validation = NULL,
  test = NULL,
  split = c(train = 0.8, validation = 0.1, test = 0.1),
  seed = 1L,
  id_col = "id",
  sequence_col = "sequence"
) {
  stopifnot(
    "split must name train, validation, and test" = is.numeric(split) &&
      setequal(names(split), c("train", "validation", "test")),
    "split values must be finite and non-negative" = length(split) == 3L &&
      !anyNA(split) &&
      all(is.finite(split)) &&
      all(split >= 0),
    "split must sum to one" = abs(sum(split) - 1) < 1e-8,
    "seed must be a non-negative integer" = is_scalar_integerish(seed, min = 0),
    "id_col must be one non-empty string" = is_scalar_string(id_col),
    "sequence_col must be one non-empty string" = is_scalar_string(sequence_col)
  )
  split <- split[c("train", "validation", "test")]
  train <- dataset_source(train, id_col, sequence_col)
  if (!is.null(validation)) {
    validation <- dataset_source(validation, id_col, sequence_col)
  }
  if (!is.null(test)) {
    test <- dataset_source(test, id_col, sequence_col)
  }
  if (
    is_scalar_string(train) &&
      file.exists(train) &&
      is.null(validation) &&
      is.null(test) &&
      any(split[c("validation", "test")] > 0)
  ) {
    train <- read_fasta(train)
  }

  partition_method <- "explicit"
  if (
    is.character(train) &&
      length(train) > 1L &&
      is.null(validation) &&
      is.null(test)
  ) {
    partition_method <- "stable-hash"
    values <- vapply(
      names(train),
      function(id) stable_partition_value(seed, id),
      numeric(1)
    )
    validation_boundary <- split[["train"]] + split[["validation"]]
    assignment <- ifelse(
      values < split[["train"]],
      "train",
      ifelse(values < validation_boundary, "validation", "test")
    )
    partitions <- lapply(
      c("train", "validation", "test"),
      function(partition) train[assignment == partition]
    )
    names(partitions) <- c("train", "validation", "test")
    train <- partitions$train
    validation <- partitions$validation
    test <- partitions$test
  }

  Evo2Dataset(
    train = train,
    validation = validation,
    test = test,
    split = as.double(split) |> stats::setNames(names(split)),
    seed = as.integer(seed),
    id_col = id_col,
    sequence_col = sequence_col,
    prepared = FALSE,
    path = NULL,
    manifest = list(),
    provenance = list(partition_method = partition_method)
  )
}

write_dataset_fasta <- function(x, path) {
  if (is.null(x) || length(x) == 0L) {
    return(NULL)
  }
  if (is_scalar_string(x) && file.exists(x)) {
    if (grepl("\\.gz$", x, ignore.case = TRUE)) {
      return(write_fasta(read_fasta(x), path))
    }
    if (!file.copy(x, path, overwrite = TRUE)) {
      stop("failed to copy FASTA input")
    }
    return(path)
  }
  if (!is.character(x) || is.null(names(x))) {
    stop("in-memory dataset partitions must be named character vectors")
  }
  lines <- unlist(
    Map(
      function(id, sequence) c(paste0(">", id), sequence),
      names(x),
      unname(x)
    ),
    use.names = FALSE
  )
  atomic_write_lines(lines, path)
  path
}

normalize_taxonomy_data <- function(taxonomy) {
  if (is.null(taxonomy)) {
    return(NULL)
  }
  if (is_scalar_string(taxonomy)) {
    if (!file.exists(taxonomy)) {
      stop("taxonomy path does not exist")
    }
    extension <- tolower(tools::file_ext(taxonomy))
    taxonomy <- switch(
      extension,
      json = jsonlite::read_json(taxonomy, simplifyVector = FALSE),
      yaml = yaml12::read_yaml(taxonomy, simplify = FALSE),
      yml = yaml12::read_yaml(taxonomy, simplify = FALSE),
      bionemor_abort(
        "BN_PROTOCOL",
        "taxonomy path must contain JSON or YAML",
        operation = "preprocess"
      )
    )
  }
  if (is.data.frame(taxonomy)) {
    if (!"id" %in% names(taxonomy)) {
      stop("taxonomy data frame must contain an id column")
    }
    if (
      !is.character(taxonomy$id) ||
        anyNA(taxonomy$id) ||
        !all(nzchar(taxonomy$id)) ||
        anyDuplicated(taxonomy$id)
    ) {
      stop("taxonomy IDs must be non-empty and unique")
    }
    ids <- taxonomy$id
    taxonomy$id <- NULL
    taxonomy <- lapply(seq_len(nrow(taxonomy)), function(index) {
      as.list(taxonomy[index, , drop = FALSE])
    })
    names(taxonomy) <- ids
  }
  if (
    !is.list(taxonomy) ||
      length(taxonomy) <= 0L ||
      is.null(names(taxonomy)) ||
      !all(nzchar(names(taxonomy))) ||
      anyDuplicated(names(taxonomy))
  ) {
    stop("taxonomy must be a named mapping of sequence IDs to lineages")
  }
  allowed <- c(
    "domain",
    "phylum",
    "class",
    "clazz",
    "order",
    "family",
    "genus",
    "species"
  )
  lapply(taxonomy, function(lineage) {
    if (
      !is.list(lineage) ||
        is.null(names(lineage)) ||
        !all(nzchar(names(lineage)))
    ) {
      stop("each taxonomy lineage must be a named list")
    }
    if (!all(names(lineage) %in% allowed)) {
      stop("taxonomy lineage contains an unsupported rank")
    }
    if (all(c("class", "clazz") %in% names(lineage))) {
      stop("taxonomy lineage must not contain both class and clazz")
    }
    names(lineage)[names(lineage) == "class"] <- "clazz"
    lineage <- lineage[
      !vapply(
        lineage,
        function(value) {
          is.null(value) ||
            length(value) == 1L && is.atomic(value) && is.na(value)
        },
        logical(1)
      )
    ]
    if (!all(vapply(lineage, is_scalar_string, logical(1)))) {
      stop("taxonomy rank values must be non-empty strings")
    }
    lineage
  })
}

value_digest <- function(value) {
  path <- tempfile("bionemor-value-")
  on.exit(unlink(path), add = TRUE)
  atomic_write_json(value, path)
  path_digest(path)
}

fasta_manifest_record <- function(path) {
  sequences <- read_fasta(path)
  lengths <- nchar(sequences, type = "chars")
  list(
    path = normalizePath(path, mustWork = TRUE),
    digest = path_digest(path),
    records = length(sequences),
    minimum_length = min(lengths),
    maximum_length = max(lengths),
    mean_length = mean(lengths)
  )
}

preprocess_record <- function(
  input,
  output_dir,
  prefix,
  tokenizer,
  control,
  taxonomy
) {
  record <- list(
    datapaths = list(input),
    output_dir = output_dir,
    output_prefix = prefix,
    train_split = if (prefix == "train") 1 else 0,
    valid_split = if (prefix == "validation") 1 else 0,
    test_split = if (prefix == "test") 1 else 0,
    overwrite = FALSE,
    hf_tokenizer_model_path = tokenizer,
    force_uppercase = control@uppercase,
    embed_reverse_complement = control@embed_reverse_complement,
    random_reverse_complement = control@random_reverse_complement,
    random_lineage_dropout = control@random_lineage_dropout,
    transcribe = if (control@transcribe == "none") {
      NULL
    } else {
      control@transcribe
    },
    append_eod = control@append_eod,
    drop_empty_sequences = control@drop_empty_sequences,
    nnn_filter = control@filter_nnn,
    prompt_spacer_length = control@prompt_spacer_length,
    workers = control@workers,
    preproc_concurrency = control@concurrency,
    chunksize = control@chunk_size,
    seed = control@seed
  )
  if (!is.null(control@sample_length)) {
    record$enforce_sample_length <- control@sample_length
  }
  if (!is.null(taxonomy)) {
    record$taxonomy_data <- taxonomy
  }
  record
}

#' Preprocess training data for Evo 2 fine-tuning
#'
#' `evo2_preprocess()` is a training-data preprocessing step. It writes each
#' dataset partition as FASTA, calls the pinned `preprocess_evo2` entry point,
#' and returns an `Evo2Dataset` that points to the resulting indexed files. It
#' does not prepare model weights, fit the model, or run inference. The model
#' argument supplies the model size and compatible tokenizer configuration; its
#' checkpoint weights are not read.
#'
#' Most users can pass raw data directly to [evo2_finetune()].
#' `evo2_finetune()` performs this step automatically with default controls.
#' Call `evo2_preprocess()` first when preprocessing must be customized or when
#' the same indexed data will be reused across fitting runs. Its manifest records
#' input digests, model size, tokenizer and recipe revisions, preprocessing
#' controls, and output digests.
#' Before training, `evo2_finetune()` checks that the prepared path and manifest
#' exist and verifies the recorded model size, tokenizer, tokenizer revision,
#' and recipe revision.
#'
#' @param data An [evo2_dataset()] result or any input accepted by its `train`
#'   argument.
#' @param model An Evo 2 model descriptor.
#' @param compute A BioNeMo compute descriptor. `NULL` uses the descriptor
#'   attached by [evo2_model()] or a previous fine-tuning run.
#' @param path Destination path. Relative paths resolve below the compute
#'   workspace. Container execution requires the destination to remain inside
#'   that workspace.
#' @param control Preprocessing controls from [evo2_preprocess_control()].
#' @param overwrite Whether to replace an existing destination.
#' @param async Whether to return a `BioNeMoJob` before preprocessing completes.
#'
#' @return With `async = FALSE`, a prepared `Evo2Dataset`. With
#'   `async = TRUE`, a `BioNeMoJob`; [job_wait()] materializes the dataset.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
#' compute <- bionemo_install(compute)
#' model <- evo2_model("1b", compute)
#' data <- evo2_dataset(c(first = "ACGT", second = "TGCA"))
#'
#' prepared <- evo2_preprocess(
#'   data,
#'   model,
#'   path = "datasets/example",
#'   control = evo2_preprocess_control(sample_length = 1024L)
#' )
#' }
#'
#' @references
#' [BioNeMo Recipes Evo 2 preprocessing](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#data-preprocessing-preprocess_evo2)
#' @export
evo2_preprocess <- function(
  data,
  model,
  compute = NULL,
  path,
  control = evo2_preprocess_control(),
  overwrite = FALSE,
  async = FALSE
) {
  compute <- resolve_model_compute(model, compute)
  stopifnot(
    "model must be an Evo 2 model" = S7_inherits(model, Evo2Model),
    "path must be one non-empty string" = is_scalar_string(path),
    "control must be an Evo2PreprocessControl" = S7_inherits(
      control,
      Evo2PreprocessControl
    ),
    "overwrite must be TRUE or FALSE" = is_scalar_logical(overwrite),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  if (!S7_inherits(data, Evo2Dataset)) {
    data <- evo2_dataset(data)
  }
  destination <- normalize_path(path, base = compute@workspace)
  if (
    compute@engine == "container" &&
      !path_is_within(destination, compute@workspace)
  ) {
    stop("container dataset output must be inside the compute workspace")
  }
  if (!overwrite && file.exists(destination)) {
    stop("prepared dataset destination exists; use overwrite = TRUE")
  }
  if (file.exists(destination) && overwrite) {
    unlink(destination, recursive = TRUE)
  }
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  name <- safe_name(basename(destination), "evo2-preprocess")
  request <- list(
    model = model@size,
    destination = destination,
    dataset = list(
      split = data@split,
      seed = data@seed,
      id_col = data@id_col,
      sequence_col = data@sequence_col
    ),
    control = S7::props(control)
  )
  run <- create_run(compute, "preprocess", name)
  run_path <- run$path
  inputs <- file.path(run_path, "inputs")
  partitions <- list(
    train = data@train,
    validation = data@validation,
    test = data@test
  )
  fasta <- lapply(names(partitions), function(partition) {
    write_dataset_fasta(
      partitions[[partition]],
      file.path(inputs, paste0(partition, ".fasta"))
    )
  })
  names(fasta) <- names(partitions)
  fasta <- fasta[!vapply(fasta, is.null, logical(1))]
  record <- evo2_model_record(model@size)
  tokenizer_request <- model@config$tokenizer %||% "recommended"
  tokenizer <- evo2_checkpoint_tokenizer(
    tokenizer_request,
    record,
    compute
  )
  tokenizer_provenance <- evo2_checkpoint_tokenizer_provenance(
    model,
    tokenizer_request,
    tokenizer,
    record
  )
  taxonomy <- normalize_taxonomy_data(control@taxonomy)
  records <- Map(
    function(input, partition) {
      preprocess_record(
        input,
        destination,
        partition,
        tokenizer,
        control,
        taxonomy
      )
    },
    fasta,
    names(fasta)
  )
  preprocess_path <- file.path(inputs, "preprocess.yaml")
  yaml12::write_yaml(unname(records), preprocess_path)
  tokenizer_desc <- tolower(gsub(
    " ",
    "",
    basename(tokenizer),
    fixed = TRUE
  ))
  dataset_config <- lapply(names(fasta), function(partition) {
    suffix <- if (partition == "validation") "val" else partition
    list(
      dataset_prefix = file.path(
        destination,
        paste(partition, tokenizer_desc, suffix, sep = "_")
      ),
      dataset_split = partition,
      dataset_weight = 1
    )
  })
  dataset_path <- file.path(destination, "dataset.yaml")
  yaml12::write_yaml(dataset_config, dataset_path)
  input_manifest <- lapply(fasta, fasta_manifest_record)
  control_manifest <- list(
    uppercase = control@uppercase,
    embed_reverse_complement = control@embed_reverse_complement,
    random_reverse_complement = control@random_reverse_complement,
    random_lineage_dropout = control@random_lineage_dropout,
    transcribe = control@transcribe,
    append_eod = control@append_eod,
    sample_length = control@sample_length,
    drop_empty_sequences = control@drop_empty_sequences,
    filter_nnn = control@filter_nnn,
    prompt_spacer_length = control@prompt_spacer_length,
    workers = control@workers,
    concurrency = control@concurrency,
    chunk_size = control@chunk_size,
    seed = control@seed,
    taxonomy_records = if (is.null(taxonomy)) 0L else length(taxonomy),
    taxonomy_digest = if (is.null(taxonomy)) NULL else value_digest(taxonomy)
  )
  output_prefixes <- stats::setNames(
    lapply(dataset_config, `[[`, "dataset_prefix"),
    names(fasta)
  )

  prepared_manifest <- list(
    preprocess_config = preprocess_path,
    preprocess_config_digest = path_digest(preprocess_path),
    dataset_config = dataset_path,
    dataset_config_digest = path_digest(dataset_path),
    tokenizer = tokenizer,
    tokenizer_revision = tokenizer_provenance$revision,
    model_size = model@model_size,
    partitions = names(fasta),
    partition_method = data@provenance$partition_method %||% "explicit",
    partition_seed = data@seed,
    inputs = input_manifest,
    preprocessing = control_manifest,
    output_prefixes = output_prefixes,
    outputs = list(),
    recipe_revision = compute@recipe@revision,
    manifest_path = file.path(destination, "bionemor-prepared.json")
  )
  plan <- command_plan(
    list(command_spec(
      "preprocess_evo2",
      c("--config", preprocess_path),
      cwd = compute@workspace
    ))
  )
  operation <- operation_spec(
    run = run,
    request = request,
    plan = plan,
    result = list(
      type = "preprocess",
      path = destination,
      inputs = fasta,
      manifest = prepared_manifest
    ),
    context = operation_context(
      model = list(
        name = model@size,
        model_size = model@model_size,
        revision = record$source_revision
      ),
      tokenizer = list(
        identity = tokenizer_provenance$identity,
        revision = tokenizer_provenance$revision,
        digest = list(
          algorithm = "git-revision",
          value = tokenizer_provenance$revision
        )
      ),
      precision = list(semantic = NULL, resolved_recipe = NULL)
    )
  )
  submit_operation(operation, async = async)
}

materialize_preprocess_job <- function(job, operation) {
  descriptor <- operation$result
  dataset <- operation$request$dataset
  if (!is_scalar_string(descriptor$path) || !dir.exists(descriptor$path)) {
    stop("prepared-data path is missing")
  }
  config <- yaml12::read_yaml(
    descriptor$manifest$dataset_config,
    simplify = FALSE
  )
  prefixes <- vapply(
    config,
    function(record) record$dataset_prefix,
    character(1)
  )
  if (
    !all(file.exists(c(paste0(prefixes, ".bin"), paste0(prefixes, ".idx"))))
  ) {
    stop("preprocessing did not create every indexed dataset")
  }
  output_paths <- c(
    paste0(prefixes, ".bin"),
    paste0(prefixes, ".idx")
  )
  outputs <- lapply(output_paths, function(path) {
    list(
      path = normalizePath(path, mustWork = TRUE),
      digest = path_digest(path),
      size = unname(file.info(path)$size)
    )
  })
  names(outputs) <- basename(output_paths)
  manifest <- descriptor$manifest
  manifest$outputs <- outputs
  manifest$prepared_at <- base::format(
    Sys.time(),
    tz = "UTC",
    usetz = TRUE
  )
  atomic_write_json(manifest, manifest$manifest_path)
  Evo2Dataset(
    train = descriptor$inputs$train,
    validation = descriptor$inputs$validation,
    test = descriptor$inputs$test,
    split = unlist(dataset$split, use.names = TRUE),
    seed = as.integer(dataset$seed),
    id_col = dataset$id_col,
    sequence_col = dataset$sequence_col,
    prepared = TRUE,
    path = descriptor$path,
    manifest = manifest,
    provenance = list(run_path = job@path)
  )
}

evo2_finetune_preflight <- function(compute, record) {
  if (!is.list(record) || !is_scalar_string(record$training_precision_policy)) {
    stop("fine-tuning model record is invalid")
  }
  policy <- record$training_precision_policy
  if (identical(policy, "unverified")) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      paste0(
        "fine-tuning is unavailable for model '",
        record$name,
        "': no verified precision and hardware combination is registered"
      ),
      operation = "fine-tune",
      model = record$name
    )
  }
  if (identical(policy, "unsupported-vortex")) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      paste0(
        "fine-tuning is unavailable for model '",
        record$name,
        "': train_evo2 has no Vortex-style FP8 training path"
      ),
      operation = "fine-tune",
      model = record$name
    )
  }
  if (!policy %in% c("bf16", "bf16-or-fp8")) {
    stop("model registry contains an unsupported training precision policy")
  }
  compute <- evo2_compute_with_gpu_capabilities(compute)
  advertisement <- evo2_gpu_advertisement(compute)
  requested <- compute@gpus
  advertised <- !is.na(advertisement$count) &&
    advertisement$count >= requested &&
    length(advertisement$capability_majors) >= requested &&
    !anyNA(advertisement$capability_majors[seq_len(requested)])
  if (!advertised) {
    code <- if (identical(advertisement$count, 0L)) {
      "BN_NO_GPU"
    } else {
      "BN_GPU_INCOMPATIBLE"
    }
    bionemor_abort(
      code,
      "fine-tuning runtime does not advertise every requested GPU",
      operation = "fine-tune",
      model = record$name,
      recipe_revision = compute@recipe@revision
    )
  }
  if (!all(advertisement$capability_majors[seq_len(requested)] >= 8L)) {
    bionemor_abort(
      "BN_GPU_INCOMPATIBLE",
      "fine-tuning requires GPUs with compute capability 8.0 or newer",
      operation = "fine-tune",
      model = record$name,
      recipe_revision = compute@recipe@revision
    )
  }
  list(record = record, compute = compute)
}

resolve_fit_precision <- function(control, model_record) {
  if (
    !is.list(model_record) ||
      !model_record$training_precision_policy %in% c("bf16", "bf16-or-fp8")
  ) {
    stop("model record must contain a verified training precision policy")
  }
  precision <- if (!is.null(control@mixed_precision_recipe)) {
    control@mixed_precision_recipe
  } else {
    switch(
      control@precision,
      auto = "bf16_mixed",
      bf16 = "bf16_mixed",
      `fp8-current` = "bf16_with_fp8_current_scaling_mixed"
    )
  }
  if (identical(precision, "bf16_with_fp8_delayed_scaling_mixed")) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      "FP8 delayed scaling is not working in the pinned Evo 2 recipe",
      operation = "fine-tune",
      model = model_record$name
    )
  }
  if (
    identical(model_record$training_precision_policy, "bf16") &&
      !identical(precision, "bf16_mixed")
  ) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      "the documented 1B fine-tuning path requires BF16",
      operation = "fine-tune",
      model = model_record$name
    )
  }
  precision
}

lora_module_names <- function(method) {
  unname(unique(unlist(
    evo2_lora_target_modules[method@targets],
    use.names = FALSE
  )))
}

fit_control_args <- function(control, model_record) {
  args <- c(
    "--seq-length",
    as.character(control@sequence_length),
    "--global-batch-size",
    as.character(control@global_batch_size),
    "--micro-batch-size",
    as.character(control@micro_batch_size),
    "--tensor-model-parallel-size",
    as.character(control@tensor_parallel_size),
    "--pipeline-model-parallel-size",
    as.character(control@pipeline_parallel_size),
    "--context-parallel-size",
    as.character(control@context_parallel_size),
    "--mixed-precision-recipe",
    resolve_fit_precision(control, model_record),
    "--lr",
    format_number(control@learning_rate),
    "--min-lr",
    format_number(control@minimum_learning_rate),
    "--warmup-steps",
    as.character(control@warmup_steps),
    "--constant-steps",
    as.character(control@constant_steps),
    "--wd",
    format_number(control@weight_decay),
    "--eval-interval",
    as.character(control@eval_interval),
    "--eval-iters",
    as.character(control@eval_iters),
    "--log-interval",
    as.character(control@log_interval),
    "--clip-grad",
    format_number(control@clip_gradient),
    "--hidden-dropout",
    format_number(control@hidden_dropout),
    "--attention-dropout",
    format_number(control@attention_dropout),
    "--seed",
    as.character(control@seed),
    "--workers",
    as.character(control@workers),
    "--most-recent-k",
    as.character(control@keep_checkpoints)
  )
  if (!is.null(control@decay_steps)) {
    args <- c(args, "--decay-steps", as.character(control@decay_steps))
  }
  if (!is.null(control@dataset_seed)) {
    args <- c(args, "--dataset-seed", as.character(control@dataset_seed))
  }
  if (control@precision_aware_optimizer) {
    args <- c(args, "--use-precision-aware-optimizer")
  }
  if (control@bf16_main_gradients) {
    args <- c(args, "--bf16-main-grads")
  }
  if (control@gradient_reduce_fp32) {
    args <- c(args, "--grad-reduce-in-fp32")
  }
  if (control@overlap_parameter_gather) {
    args <- c(args, "--overlap-param-gather")
  }
  if (control@overlap_gradient_reduce) {
    args <- c(args, "--overlap-grad-reduce")
  }
  if (control@subquadratic_ops) {
    args <- c(args, "--use-subquadratic-ops")
  }
  if (control@checkpoint_async) {
    args <- c(args, "--ckpt-async-save")
  }
  if (control@activation_checkpointing == "none") {
    args <- c(args, "--no-activation-checkpointing")
  } else if (control@activation_checkpointing == "selective") {
    args <- c(args, "--selective-activation-checkpointing")
  }
  if (!is.null(control@activation_checkpoint_layers)) {
    args <- c(
      args,
      "--activation-checkpoint-recompute-num-layers",
      as.character(control@activation_checkpoint_layers)
    )
  }
  extra_mapping <- c(
    sequence_parallel = "--sequence-parallel",
    no_fp8_wgrad = "--no-fp8-wgrad",
    no_fp8_param_gather = "--no-fp8-param-gather",
    average_in_collective = "--average-in-collective",
    eod_pad_in_loss_mask = "--eod-pad-in-loss-mask",
    cross_entropy_loss_fusion = "--cross-entropy-loss-fusion",
    fp32_residual_connection = "--fp32-residual-connection",
    seq_len_interpolation_factor = "--seq-len-interpolation-factor",
    adam_beta1 = "--adam-beta1",
    adam_beta2 = "--adam-beta2",
    adam_epsilon = "--adam-eps",
    gc_interval = "--gc-interval",
    enable_preemption = "--enable-preemption",
    no_renormalize_loss = "--no-renormalize-loss"
  )
  extra_args <- unlist(
    Map(
      function(name, value) {
        if (
          identical(name, "fp32_residual_connection") &&
            is.logical(value) &&
            !value
        ) {
          return("--no-fp32-residual-connection")
        }
        flag <- unname(extra_mapping[[name]])
        if (is.logical(value)) {
          if (value) flag else character()
        } else {
          c(flag, format_number(value))
        }
      },
      names(control@extra),
      control@extra
    ),
    use.names = FALSE
  )
  args <- c(args, extra_args)
  args
}

#' Fine-tune an Evo 2 model
#'
#' `evo2_finetune()` runs `train_evo2` from an MBridge checkpoint. Raw
#' sequence inputs and unprepared [evo2_dataset()] objects are preprocessed
#' automatically using default preprocessing controls. Call [evo2_preprocess()]
#' first when preprocessing must be customized or reused.
#'
#' `steps` counts optimizer steps, not epochs. The fitting control determines
#' data parallelism and gradient accumulation from the allocated GPU count and
#' model-parallel settings. Training requires GPUs with compute capability 8.0
#' or newer; model-specific precision restrictions are enforced before launch.
#'
#' A LoRA result contains adapter weights and records the dense base checkpoint;
#' that base checkpoint must remain at the recorded path for later inference.
#' LoRA-on-LoRA fine-tuning and full fine-tuning from a LoRA checkpoint are not
#' supported.
#'
#' @param object An Evo 2 model with an MBridge checkpoint.
#' @param data An [evo2_dataset()] result or any input accepted by its `train`
#'   argument.
#' @param compute A BioNeMo compute descriptor. `NULL` uses the descriptor
#'   attached by [evo2_model()] or a previous fine-tuning run.
#' @param steps Positive number of optimizer steps.
#' @param method Fine-tuning strategy from [evo2_lora()] or [evo2_full()].
#' @param control Training controls from [evo2_fit_control()].
#' @param path Result directory. Relative paths resolve below the compute
#'   workspace, and `NULL` uses `artifacts/<name>`. Container execution requires
#'   the result to remain inside the workspace.
#' @param name Optional run name. When `data` must be preprocessed
#'   automatically, its preprocessing run uses `<name>-data`.
#' @param async Whether to return a `BioNeMoJob` before fine-tuning completes.
#' @param timeout Complete operation timeout in seconds. This limits the
#'   launched operation; [job_wait()] has a separate client-side wait timeout.
#'
#' @return With `async = FALSE`, a fitted `Evo2Model` bound to the same compute
#'   descriptor. With `async = TRUE`, a `BioNeMoJob`; [job_wait()] materializes
#'   the fitted model.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
#' compute <- bionemo_install(compute)
#' model <- evo2_model("1b", compute)
#' data <- evo2_dataset(c(first = "ACGT", second = "TGCA"))
#'
#' run <- evo2_finetune(
#'   model,
#'   data,
#'   steps = 500L,
#'   method = evo2_lora(targets = c("attention", "mlp")),
#'   control = evo2_fit_control(
#'     sequence_length = 1024L,
#'     precision = "bf16"
#'   ),
#'   async = TRUE
#' )
#' fitted <- job_wait(run)
#' }
#'
#' @references
#' [BioNeMo Recipes Evo 2 fine-tuning](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#fine-tuning-from-an-existing-checkpoint)
#' @export
evo2_finetune <- function(
  object,
  data,
  compute = NULL,
  steps,
  method = evo2_lora(),
  control = evo2_fit_control(),
  path = NULL,
  name = NULL,
  async = TRUE,
  timeout = Inf
) {
  compute <- resolve_model_compute(object, compute)
  stopifnot(
    "object must be an Evo 2 model" = S7_inherits(object, Evo2Model),
    "steps must be a positive integer" = is_scalar_integerish(steps, min = 1),
    "method must be an Evo2FineTuneMethod" = S7_inherits(
      method,
      Evo2FineTuneMethod
    ),
    "control must be an Evo2FitControl" = S7_inherits(control, Evo2FitControl),
    "async must be TRUE or FALSE" = is_scalar_logical(async),
    "timeout must be positive" = identical(timeout, Inf) ||
      is_scalar_number(timeout) && timeout > 0
  )
  if (control@sequence_length > object@context_length) {
    bionemor_abort(
      "BN_CONTEXT_LIMIT",
      paste0(
        "fine-tuning sequence length ",
        control@sequence_length,
        " exceeds model context length ",
        object@context_length
      ),
      operation = "fine-tune",
      model = object@size,
      context_length = as.integer(object@context_length),
      sequence_length = as.integer(control@sequence_length),
      additional_tokens = 0L,
      required_length = as.integer(control@sequence_length),
      hint = "reduce sequence_length to the model context length or below"
    )
  }
  checkpoint <- object@checkpoint
  if (!S7_inherits(checkpoint, BioNeMoCheckpoint)) {
    stop("fine-tuning requires an explicit MBridge checkpoint")
  }
  if (checkpoint@format != "mbridge" || !dir.exists(checkpoint@path)) {
    stop("fine-tuning requires an explicit MBridge checkpoint")
  }
  checkpoint_record <- checkpoint_manifest(checkpoint)
  resolved_checkpoint <- checkpoint_manifest_resolved_path(
    checkpoint@path,
    checkpoint_record
  )
  model_record <- evo2_model_record(object@size)
  stopifnot(
    "checkpoint family does not match the model" = identical(
      checkpoint@family,
      "evo2"
    ),
    "checkpoint variant does not match the model" = identical(
      checkpoint@variant,
      object@size
    ),
    "checkpoint model size does not match the model" = identical(
      checkpoint_record$model_size,
      object@model_size
    ),
    "checkpoint recipe revision does not match the compute recipe" = identical(
      checkpoint@recipe_revision,
      compute@recipe@revision
    ),
    "LoRA-on-LoRA fine-tuning is not supported" = !S7_inherits(
      method,
      Evo2LoRA
    ) ||
      !identical(checkpoint@kind, "lora"),
    "full fine-tuning from a LoRA checkpoint is not supported" = !S7_inherits(
      method,
      Evo2FullFineTune
    ) ||
      !identical(checkpoint@kind, "lora"),
    "Vortex-style MBridge checkpoints cannot be fine-tuned by train_evo2" = !isTRUE(
      checkpoint_record$inspection$vortex_style_fp8
    )
  )
  assert_checkpoint_manifest_weights(checkpoint@path, checkpoint_record)
  preflight <- evo2_finetune_preflight(
    compute,
    model_record
  )
  compute <- preflight$compute
  tokenizer_request <- object@config$tokenizer %||% "recommended"
  tokenizer <- evo2_checkpoint_tokenizer(
    tokenizer_request,
    model_record,
    compute
  )
  tokenizer_provenance <- evo2_checkpoint_tokenizer_provenance(
    object,
    tokenizer_request,
    tokenizer,
    model_record
  )
  name <- safe_name(name, "evo2-finetune")
  data <- if (S7_inherits(data, Evo2Dataset)) data else evo2_dataset(data)
  if (!data@prepared) {
    data <- evo2_preprocess(
      data,
      object,
      compute,
      path = file.path(
        compute@workspace,
        "datasets",
        paste0(name, "-data")
      ),
      async = FALSE
    )
  }
  stopifnot(
    "prepared data path does not exist" = is_scalar_string(data@path) &&
      dir.exists(data@path),
    "prepared data model size does not match the model" = identical(
      data@manifest$model_size,
      object@model_size
    ),
    "prepared data tokenizer does not match the model registry" = identical(
      data@manifest$tokenizer,
      tokenizer
    ),
    "prepared data tokenizer revision does not match the model registry" = identical(
      data@manifest$tokenizer_revision,
      tokenizer_provenance$revision
    ),
    "prepared data recipe revision does not match the compute recipe" = identical(
      data@manifest$recipe_revision,
      compute@recipe@revision
    ),
    "prepared data manifest is missing" = is_scalar_string(
      data@manifest$manifest_path
    ) &&
      file.exists(data@manifest$manifest_path)
  )

  output <- normalize_path(
    path %||% file.path(compute@workspace, "artifacts", name),
    base = compute@workspace
  )
  if (
    compute@engine == "container" && !path_is_within(output, compute@workspace)
  ) {
    stop("container fine-tuning output must be inside the compute workspace")
  }
  if (
    compute@engine == "container" &&
      !path_is_within(resolved_checkpoint, compute@workspace)
  ) {
    stop("fine-tuning checkpoint must be inside the compute workspace")
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  world_size <- compute@gpus
  model_parallel <- control@tensor_parallel_size *
    control@pipeline_parallel_size *
    control@context_parallel_size
  if (world_size %% model_parallel != 0) {
    stop("GPU count must be divisible by model parallelism")
  }
  data_parallel <- world_size %/% model_parallel
  denominator <- control@micro_batch_size * data_parallel
  if (control@global_batch_size %% denominator != 0) {
    stop(
      "global batch size must be divisible by micro batch size and data parallelism"
    )
  }
  accumulation <- control@global_batch_size %/% denominator
  precision_recipe <- resolve_fit_precision(control, model_record)

  request <- list(
    model = object@size,
    steps = as.integer(steps),
    method = method@kind,
    method_config = S7::props(method),
    data = data@manifest,
    tokenizer = tokenizer,
    precision = list(
      semantic = control@precision,
      mixed_precision_recipe = precision_recipe
    ),
    control = S7::props(control),
    gradient_accumulation = accumulation
  )
  run <- create_run(compute, "fine-tune", name)
  run_path <- run$path
  args <- c(
    "--nproc-per-node",
    as.character(compute@gpus),
    "--no-python",
    "train_evo2",
    "--dataset-config",
    data@manifest$dataset_config,
    "--dataset-dir",
    data@path,
    "--model-size",
    object@model_size,
    "--hf-tokenizer-model-path",
    tokenizer,
    "--max-steps",
    as.character(as.integer(steps)),
    "--result-dir",
    output,
    "--experiment-name",
    name,
    "--finetune-ckpt-dir",
    resolved_checkpoint,
    fit_control_args(control, model_record)
  )
  if (S7_inherits(method, Evo2LoRA)) {
    args <- c(
      args,
      # The locked Bridge logger reads `main_grad` from frozen LoRA parameters.
      "--disable-tensorboard-logger",
      "--lora-finetune",
      "--lora-dim",
      as.character(method@rank),
      "--lora-alpha",
      format_number(method@alpha),
      "--lora-dropout",
      format_number(method@dropout),
      "--lora-target-modules",
      paste(lora_module_names(method), collapse = ",")
    )
    if (length(method@fully_trainable) > 0L) {
      args <- c(
        args,
        "--lora-skip-freeze-modules",
        paste(method@fully_trainable, collapse = ",")
      )
    }
  }
  checkpoint_root <- file.path(output, name, "checkpoints")
  inspection <- file.path(run_path, "outputs", "checkpoint-inspection.json")
  resolved_control <- list(
    semantic_precision = control@precision,
    mixed_precision_recipe = precision_recipe,
    world_size = world_size,
    tensor_parallel_size = control@tensor_parallel_size,
    pipeline_parallel_size = control@pipeline_parallel_size,
    context_parallel_size = control@context_parallel_size,
    data_parallel_size = data_parallel,
    global_batch_size = control@global_batch_size,
    micro_batch_size = control@micro_batch_size,
    gradient_accumulation = accumulation
  )
  plan <- command_plan(
    list(
      command_spec(
        "torchrun",
        args,
        cwd = compute@workspace,
        timeout = timeout
      ),
      checkpoint_inspection_command(
        checkpoint_root,
        inspection,
        compute
      )
    )
  )
  checkpoint_kind <- if (S7_inherits(method, Evo2LoRA)) {
    "lora"
  } else {
    "training"
  }
  base_checkpoint_digest <- if (identical(
    normalize_path(resolved_checkpoint),
    normalize_path(checkpoint@path)
  )) {
    checkpoint_record$checkpoint_digest
  } else {
    path_digest(resolved_checkpoint)
  }
  operation <- operation_spec(
    run = run,
    request = request,
    plan = plan,
    result = list(
      type = "fine-tune",
      checkpoint_root = checkpoint_root,
      inspection = inspection,
      kind = checkpoint_kind
    ),
    execution = list(
      resolved_control = resolved_control
    ),
    context = operation_context(
      model = list(
        name = object@size,
        model_size = object@model_size,
        revision = object@revision,
        context_length = object@context_length,
        config = object@config
      ),
      checkpoint = list(
        path = checkpoint_root,
        source = paste0("fit://", run$id),
        source_trust = "not-required",
        source_verified = FALSE,
        format = "mbridge",
        kind = checkpoint_kind,
        revision = NULL,
        digest = NULL,
        base_checkpoint = list(
          path = resolved_checkpoint,
          source = checkpoint@source,
          source_trust = checkpoint_record$source_trust,
          source_verified = checkpoint_record$source_verified,
          digest = base_checkpoint_digest
        )
      ),
      tokenizer = list(
        identity = tokenizer_provenance$identity,
        revision = tokenizer_provenance$revision,
        digest = list(
          algorithm = "git-revision",
          value = tokenizer_provenance$revision
        )
      ),
      precision = list(
        semantic = control@precision,
        resolved_recipe = precision_recipe
      )
    ),
    timeout = timeout,
    cleanup = NULL
  )
  submit_operation(operation, async = async)
}

materialize_finetune_job <- function(job, operation) {
  descriptor <- operation$result
  request <- operation$request
  context <- operation$context
  if (
    !is_scalar_string(descriptor$inspection) ||
      !file.exists(descriptor$inspection)
  ) {
    stop("fine-tuning checkpoint inspection is missing")
  }
  inspection <- read_json_file(descriptor$inspection)
  if (!is_scalar_string(inspection$path) || !dir.exists(inspection$path)) {
    stop("fine-tuning inspector did not resolve an MBridge checkpoint")
  }
  if (!identical(descriptor$kind, context$checkpoint$kind)) {
    stop("fine-tuning checkpoint kind does not match the requested method")
  }
  root <- descriptor$checkpoint_root
  manifest_path <- checkpoint_manifest_path(root, "mbridge")
  base_checkpoint <- context$checkpoint$base_checkpoint
  if (!is.null(base_checkpoint$path)) {
    if (!dir.exists(base_checkpoint$path)) {
      stop("fine-tuning base checkpoint is missing")
    }
  }
  manifest <- list(
    schema_version = 1L,
    family = "evo2",
    variant = context$model$name,
    model_size = context$model$model_size,
    format = "mbridge",
    kind = descriptor$kind,
    source = context$checkpoint$source,
    source_format = "mbridge",
    source_revision = path_digest(inspection$path),
    source_trust = context$checkpoint$source_trust,
    source_verified = context$checkpoint$source_verified,
    recipe_revision = job@compute@recipe@revision,
    tokenizer_identity = context$tokenizer$identity,
    tokenizer = request$tokenizer,
    tokenizer_revision = context$tokenizer$revision,
    mixed_precision_recipe = context$precision$resolved_recipe,
    base_checkpoint_path = base_checkpoint$path,
    base_checkpoint_digest = base_checkpoint$digest,
    base_checkpoint_source = base_checkpoint$source,
    base_checkpoint_source_trust = base_checkpoint$source_trust,
    base_checkpoint_source_verified = base_checkpoint$source_verified,
    inspection = inspection,
    provenance = list(
      run_path = job@path,
      steps = request$steps,
      gradient_accumulation = request$gradient_accumulation,
      created_at = base::format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )
  atomic_write_json(manifest, manifest_path)
  atomic_write_lines(
    jsonlite::toJSON(
      list(
        schema_version = 1L,
        manifest = basename(manifest_path)
      ),
      auto_unbox = TRUE
    ),
    checkpoint_completion_path(root, "mbridge")
  )
  checkpoint <- checkpoint_from_manifest(root, manifest, manifest_path)
  Evo2Model(
    family = "evo2",
    checkpoint = checkpoint,
    compute = job@compute,
    config = context$model$config,
    provenance = list(
      run_path = job@path,
      method = request$method,
      precision = context$precision$resolved_recipe,
      gradient_accumulation = request$gradient_accumulation
    ),
    size = context$model$name,
    model_size = context$model$model_size,
    context_length = as.integer(context$model$context_length),
    revision = context$model$revision
  )
}

method(fit, Evo2Model) <- function(
  object,
  data,
  compute = NULL,
  steps,
  control = evo2_fit_control(),
  method = evo2_lora(),
  name = NULL,
  output = NULL,
  timeout = Inf,
  async = FALSE,
  ...
) {
  dots <- list(...)
  if (length(dots) != 0L) {
    stop("`...` is reserved and must be empty")
  }
  evo2_finetune(
    object,
    data,
    compute,
    steps,
    method = method,
    control = control,
    path = output,
    name = name,
    async = async,
    timeout = timeout
  )
}
