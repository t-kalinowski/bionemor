validate_inference_context <- function(object, compute, control) {
  compute <- resolve_model_compute(object, compute)
  stopifnot(
    "object must be an Evo 2 model" = S7_inherits(object, Evo2Model),
    "control must be an Evo 2 inference control" = S7_inherits(
      control,
      Evo2InferenceControl
    ),
    "compute workspace must exist" = dir.exists(compute@workspace)
  )
  model_checkpoint <- object@checkpoint
  if (!S7_inherits(model_checkpoint, BioNeMoCheckpoint)) {
    stop("inference requires an explicit checkpoint")
  }
  checkpoint_root <- checkpoint_path(model_checkpoint)
  if (!dir.exists(checkpoint_root)) {
    stop("checkpoint does not exist")
  }
  manifest <- checkpoint_manifest(model_checkpoint)
  checkpoint <- checkpoint_manifest_resolved_path(
    model_checkpoint@path,
    manifest
  )
  if (!identical(model_checkpoint@recipe_revision, compute@recipe@revision)) {
    bionemor_abort(
      "BN_RECIPE_MISMATCH",
      "checkpoint recipe revision does not match the compute recipe",
      operation = "inference",
      model = object@size,
      checkpoint = checkpoint_root,
      recipe_revision = compute@recipe@revision,
      expected_recipe_revision = compute@recipe@revision,
      actual_recipe_revision = model_checkpoint@recipe_revision,
      hint = "use a checkpoint prepared with the active Evo 2 recipe"
    )
  }
  if (identical(model_checkpoint@kind, "lora")) {
    base <- model_checkpoint@base_checkpoint
    if (!is_scalar_string(base) || !dir.exists(base)) {
      bionemor_abort(
        "BN_BASE_CHECKPOINT_MISSING",
        "LoRA base checkpoint is missing",
        operation = "inference",
        model = object@size,
        checkpoint = checkpoint_root,
        base_checkpoint = base,
        recipe_revision = compute@recipe@revision,
        hint = "restore the recorded base checkpoint or prepare the LoRA again"
      )
    }
    base_inspection <- inspect_model_checkpoint(base, object@model_size)
    if (
      !identical(
        normalizePath(base, mustWork = TRUE),
        base_inspection$resolved_path
      )
    ) {
      bionemor_abort(
        "BN_BASE_CHECKPOINT_MISSING",
        paste0(
          "external LoRA base checkpoint must be an explicit checkpoint directory"
        ),
        operation = "inference",
        model = object@size,
        checkpoint = checkpoint_root,
        base_checkpoint = normalizePath(base, mustWork = TRUE),
        resolved_base_checkpoint = base_inspection$resolved_path,
        recipe_revision = compute@recipe@revision,
        hint = "set the LoRA base checkpoint to its resolved iteration directory"
      )
    }
    expected_digest <- manifest$base_checkpoint_digest
    if (!is.null(expected_digest)) {
      actual_digest <- path_digest(base)
      if (!identical(actual_digest, expected_digest)) {
        bionemor_abort(
          "BN_BASE_CHECKPOINT_MISSING",
          "LoRA base checkpoint digest does not match its manifest",
          operation = "inference",
          model = object@size,
          checkpoint = checkpoint_root,
          base_checkpoint = normalizePath(base, mustWork = TRUE),
          recipe_revision = compute@recipe@revision,
          expected_digest = expected_digest,
          actual_digest = actual_digest,
          hint = "restore the exact base checkpoint recorded by the LoRA manifest"
        )
      }
    }
  }
  checkpoint <- normalizePath(checkpoint, mustWork = TRUE)
  checkpoint_root <- normalizePath(checkpoint_root, mustWork = TRUE)
  if (compute@engine == "container") {
    if (!path_is_within(checkpoint, compute@workspace)) {
      stop("container checkpoint must be inside the compute workspace")
    }
  }
  preflight <- evo2_model_preflight(object, compute, "inference")
  vortex <- control@vortex_style_fp8
  if (
    identical(vortex, "yes") &&
      !evo2_verified_hopper(preflight$compute)
  ) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      "Vortex-style FP8 requires a verified Hopper GPU capability report",
      operation = "inference",
      model = preflight$record$name,
      checkpoint = checkpoint_root,
      recipe_revision = preflight$compute@recipe@revision
    )
  }
  if (
    identical(preflight$record$precision_policy, "vortex-fp8-on-hopper") &&
      identical(vortex, "no")
  ) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      paste0(
        "model '",
        preflight$record$name,
        "' requires the Vortex-style FP8 policy"
      ),
      operation = "inference",
      model = preflight$record$name,
      checkpoint = checkpoint_root,
      recipe_revision = preflight$compute@recipe@revision
    )
  }
  list(
    checkpoint = checkpoint,
    checkpoint_root = checkpoint_root,
    checkpoint_manifest = manifest,
    compute = preflight$compute
  )
}

validate_output_path <- function(output, compute, suffixes = character()) {
  stopifnot(
    "output suffixes must be character values" = is.character(suffixes) &&
      !anyNA(suffixes)
  )
  if (!is.null(output) && !is_scalar_string(output)) {
    stop("output must be NULL or one non-empty string")
  }
  if (is.null(output)) {
    return(NULL)
  }
  output <- normalize_path(output, base = compute@workspace)
  if (any(file.exists(paste0(output, c("", suffixes))))) {
    stop("output path already exists")
  }
  if (compute@engine == "container") {
    if (!path_is_within(output, compute@workspace)) {
      stop("container output must be inside the compute workspace")
    }
  }
  output
}

copy_output_directory <- function(run_path, output) {
  if (is.null(output)) {
    return(invisible(NULL))
  }
  if (!file.exists(output)) {
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output)) {
    stop("output must be a directory")
  }
  files <- list.files(
    file.path(run_path, "outputs"),
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  copied <- file.copy(files, output, recursive = TRUE, overwrite = TRUE)
  if (!all(copied)) {
    stop("failed to copy portable outputs")
  }
  invisible(output)
}

copy_output_files <- function(path, output, suffixes = "") {
  if (is.null(output)) {
    return(path)
  }
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  for (suffix in suffixes) {
    if (!file.copy(
      paste0(path, suffix),
      paste0(output, suffix),
      overwrite = TRUE
    )) {
      stop("failed to copy portable output")
    }
  }
  normalize_path(output)
}

generation_prompt_rows <- function(input) {
  lapply(seq_along(input$sequences), function(index) {
    input_id <- input$ids[[index]]
    list(
      id = paste0(input_id, "::1"),
      input_id = input_id,
      sample = 1L,
      prompt = unname(input$sequences[[index]])
    )
  })
}

#' Generate DNA continuations with Evo 2
#'
#' All prompts are written to one request and processed while the recipe keeps
#' the model loaded. Prompt tokens plus `num_tokens` must fit the model context
#' and any smaller `max_sequence_length` in [evo2_inference_control()].
#'
#' @param object An Evo 2 model with an explicit checkpoint.
#' @param prompt Prompts as a character vector, data frame, FASTA path, or
#'   `DNAStringSet`. Character-vector names or an `id` data-frame column become
#'   output IDs.
#' @param compute A BioNeMo compute descriptor. `NULL` uses the descriptor
#'   attached by [evo2_model()] or a previous fine-tuning run.
#' @param num_tokens Maximum number of new tokens per prompt.
#' @param temperature,top_k,top_p Sampling temperature and top-k or nucleus
#'   filtering. At most one of `top_k` and `top_p` may be positive. Set
#'   `top_k = 0` when using top-p sampling.
#' @param seed Optional positive random seed. The pinned recipe maps zero to its
#'   default seed, so zero is rejected rather than recorded as if it were used.
#' @param return_probabilities Whether to retain generated-token log
#'   probabilities and probabilities in list columns.
#' @param normalize Sequence normalization mode. `"evo2"` accepts a leading
#'   [evo2_phylo_tag()], validates the DNA portion, and uppercases the complete
#'   prompt, including the tag. `"dna"` accepts IUPAC DNA only, and `"none"`
#'   sends text unchanged.
#' @param validate Mechanical output validation level. `"basic"` records
#'   warnings for non-ACGT symbols, extreme GC fraction, long homopolymers, low
#'   complexity, and duplicate completions. `"strict"` also rejects non-ACGT
#'   output. `"none"` skips these checks.
#' @param control Controls from [evo2_inference_control()].
#' @param output Optional directory for a copy of portable outputs.
#' @param name Optional run name.
#' @param async Whether to return before completion.
#'
#' @return With `async = FALSE`, an `evo2_generation` data frame with one row
#'   per prompt and these 17 columns:
#'
#'   - `id`, `input_id`, and `sample` identify the request and sample.
#'   - `prompt`, `completion`, and `sequence` contain the input, generated
#'     suffix, and their concatenation.
#'   - `finish_reason` records why generation stopped.
#'   - `prompt_tokens`, `generated_tokens`, and `total_tokens` report token
#'     counts.
#'   - `log_probabilities` and `probabilities` are list columns containing
#'     per-generated-token values when `return_probabilities = TRUE`, and
#'     `NULL` otherwise.
#'   - `generated_bases`, `gc_fraction`, `ambiguous_fraction`, and
#'     `longest_homopolymer` contain mechanical sequence summaries.
#'   - `validation_warnings` is a list column of zero or more mechanical
#'     validation messages.
#'
#'   With `async = TRUE`, a `BioNeMoJob`.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
#' compute <- bionemo_install(compute)
#' model <- evo2_model("7b", compute)
#'
#' generated <- evo2_generate(
#'   model,
#'   c(reference = "ACGTACGT"),
#'   num_tokens = 32L,
#'   seed = 17L
#' )
#' generated[c("input_id", "prompt", "completion", "finish_reason")]
#' }
#'
#' @references
#' [BioNeMo Recipes autoregressive generation](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#autoregressive-generation-infer_evo2)
#' @export
evo2_generate <- function(
  object,
  prompt,
  compute = NULL,
  num_tokens = 100L,
  temperature = 0.7,
  top_k = 3L,
  top_p = 0,
  seed = NULL,
  return_probabilities = FALSE,
  normalize = c("evo2", "dna", "none"),
  validate = c("basic", "strict", "none"),
  control = evo2_inference_control(),
  output = NULL,
  name = NULL,
  async = FALSE
) {
  normalize <- match.arg(normalize)
  validate <- match.arg(validate)
  stopifnot(
    "num_tokens must be a positive integer" = is_scalar_integerish(
      num_tokens,
      min = 1
    ),
    "temperature must be positive" = is_scalar_number(temperature) &&
      temperature > 0,
    "top_k must be a non-negative integer" = is_scalar_integerish(
      top_k,
      min = 0
    ),
    "top_p must be between zero and one" = is_scalar_number(top_p) &&
      top_p >= 0 &&
      top_p <= 1,
    "at most one of top_k and top_p may be positive" = !(top_k > 0 &&
      top_p > 0),
    "seed must be NULL or a positive integer" = is.null(seed) ||
      is_scalar_integerish(seed, min = 1),
    "return_probabilities must be TRUE or FALSE" = is_scalar_logical(
      return_probabilities
    ),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  validate_generation_control(control)
  context <- validate_inference_context(object, compute, control)
  checkpoint <- context$checkpoint
  checkpoint_root <- context$checkpoint_root
  checkpoint_manifest <- context$checkpoint_manifest
  compute <- context$compute
  output <- validate_output_path(output, compute)
  request <- list(
    model = object@size,
    num_tokens = as.integer(num_tokens),
    temperature = as.double(temperature),
    top_k = as.integer(top_k),
    top_p = as.double(top_p),
    seed = as_nullable_integer(seed),
    return_probabilities = return_probabilities,
    normalize = normalize,
    validate = validate,
    control = S7::props(control),
    output = output
  )
  run <- create_run(compute, "generation", name)
  run_path <- run$path
  input <- prepare_sequence_input(
    prompt,
    run_path,
    normalize = normalize,
    column = "prompt",
    filename = "prompts.fasta"
  )
  request$input_source <- input$input_source
  validate_sequence_context(
    input,
    object,
    compute,
    operation = "generation",
    run_path = run_path,
    checkpoint = checkpoint_root,
    additional_tokens = as.integer(num_tokens),
    max_sequence_length = control@max_sequence_length
  )
  prompts <- generation_prompt_rows(input)
  prompt_path <- file.path(run_path, "inputs", "prompts.jsonl")
  write_jsonl_rows(prompts, prompt_path)
  upstream <- file.path(run_path, "upstream", "generation.jsonl")
  portable <- file.path(run_path, "outputs", "generation.jsonl")
  fasta <- file.path(run_path, "outputs", "generated.fasta")
  validation_path <- file.path(run_path, "outputs", "validation.json")
  resolved <- resolved_inference_control(
    control,
    compute,
    "generation",
    checkpoint_manifest
  )
  plan <- evo2_generation_plan(
    run_path = run_path,
    checkpoint = checkpoint,
    prompts = prompt_path,
    upstream = upstream,
    portable = portable,
    fasta = fasta,
    validation = validation_path,
    compute = compute,
    resolved = resolved,
    num_tokens = as.integer(num_tokens),
    temperature = as.double(temperature),
    top_k = as.integer(top_k),
    top_p = as.double(top_p),
    seed = as_nullable_integer(seed),
    return_probabilities = return_probabilities,
    validate = validate
  )
  operation <- operation_spec(
    run = run,
    request = request,
    plan = plan,
    result = list(
      type = "generation",
      portable = portable,
      validation = validation_path
    ),
    execution = list(resolved_control = resolved),
    context = evo2_inference_operation_context(
      object,
      checkpoint_root,
      checkpoint_manifest,
      resolved
    )
  )
  submit_operation(operation, async = async)
}

generation_result_fields <- c(
  "id",
  "input_id",
  "sample",
  "prompt",
  "completion",
  "sequence",
  "finish_reason",
  "prompt_tokens",
  "generated_tokens",
  "total_tokens",
  "log_probabilities",
  "probabilities",
  "generated_bases",
  "gc_fraction",
  "ambiguous_fraction",
  "longest_homopolymer",
  "validation_warnings"
)

generation_string_fields <- c(
  "id",
  "input_id",
  "prompt",
  "completion",
  "sequence",
  "finish_reason"
)

generation_integer_fields <- c(
  sample = 1L,
  prompt_tokens = 0L,
  generated_tokens = 0L,
  total_tokens = 0L,
  generated_bases = 0L,
  longest_homopolymer = 0L
)

generation_list_fields <- c(
  "log_probabilities",
  "probabilities",
  "validation_warnings"
)

is_generation_fraction <- function(x) {
  is_scalar_number(x) && x >= 0 && x <= 1
}

is_generation_number_vector <- function(x, min = -Inf, max = Inf) {
  is.null(x) ||
    length(x) == 0L ||
    is.numeric(x) && all(is.finite(x)) && all(x >= min & x <= max)
}

is_generation_row <- function(row) {
  if (
    !is.list(row) ||
      !identical(sort(names(row)), sort(generation_result_fields))
  ) {
    return(FALSE)
  }
  warnings <- row$validation_warnings
  all(vapply(
    row[generation_string_fields],
    is_scalar_string,
    logical(1)
  )) &&
    all(vapply(
      names(generation_integer_fields),
      function(field) {
        is_scalar_integerish(row[[field]], generation_integer_fields[[field]])
      },
      logical(1)
    )) &&
    (is.null(row$gc_fraction) || is_generation_fraction(row$gc_fraction)) &&
    is_generation_fraction(row$ambiguous_fraction) &&
    is_generation_number_vector(row$log_probabilities, max = 0) &&
    is_generation_number_vector(row$probabilities, min = 0, max = 1) &&
    (is.character(warnings) || is.list(warnings) && length(warnings) == 0L) &&
    !anyNA(warnings) &&
    all(nzchar(warnings))
}

as_generation_number_vector <- function(x) {
  if (is.null(x)) NULL else as.double(unlist(x, use.names = FALSE))
}

generation_rows_data_frame <- function(records) {
  scalar_fields <- setdiff(generation_result_fields, generation_list_fields)
  data <- as.data.frame(
    lapply(scalar_fields, function(name) {
      pluck_vec(records, name, records[[1L]][[name]])
    }),
    stringsAsFactors = FALSE
  )
  names(data) <- scalar_fields
  for (field in generation_list_fields) {
    data[[field]] <- I(lapply(records, `[[`, field))
  }
  data[generation_result_fields]
}

abort_generation_schema <- function(
  job,
  operation,
  message,
  request_id = NULL
) {
  bionemor_abort(
    "BN_OUTPUT_SCHEMA",
    message,
    run_path = job@path,
    request_id = request_id,
    operation = "generation",
    model = operation$context$model$name,
    checkpoint = operation$context$checkpoint$path,
    recipe_revision = job@compute@recipe@revision,
    log_paths = file.path(job@path, c("stdout.log", "stderr.log"))
  )
}

read_generation_jsonl <- function(job, operation, path, label) {
  tryCatch(
    read_jsonl_rows(path),
    error = function(error) {
      abort_generation_schema(
        job,
        operation,
        paste0("generation ", label, " is not valid JSONL")
      )
    }
  )
}

materialize_generation_job <- function(job, operation) {
  descriptor <- operation$result
  request <- operation$request
  rows <- read_generation_jsonl(
    job,
    operation,
    descriptor$portable,
    "portable output"
  )
  if (length(rows) == 0L) {
    abort_generation_schema(
      job,
      operation,
      "generation portable output must contain at least one row"
    )
  }
  records <- lapply(rows, function(row) {
    request_id <- if (is.list(row) && is_scalar_string(row$id)) {
      row$id
    } else {
      NULL
    }
    if (!is_generation_row(row)) {
      abort_generation_schema(
        job,
        operation,
        "generation portable output row has an invalid schema",
        request_id = request_id
      )
    }
    integer_fields <- names(generation_integer_fields)
    row[integer_fields] <- lapply(row[integer_fields], as.integer)
    row$gc_fraction <- as.double(row$gc_fraction %||% NA_real_)
    row$ambiguous_fraction <- as.double(row$ambiguous_fraction)
    vector_fields <- c("log_probabilities", "probabilities")
    row[vector_fields] <- lapply(
      row[vector_fields],
      as_generation_number_vector
    )
    row$validation_warnings <- as.character(unlist(
      row$validation_warnings,
      use.names = FALSE
    ))
    row[generation_result_fields]
  })
  ids <- pluck_chr(records, "id")
  if (anyDuplicated(ids)) {
    abort_generation_schema(
      job,
      operation,
      "generation portable output IDs must be unique",
      request_id = ids[[which(duplicated(ids))[[1L]]]]
    )
  }
  data <- generation_rows_data_frame(records)
  class(data) <- c("evo2_generation", "data.frame")
  attr(data, "provenance") <- list(
    run_path = job@path,
    checkpoint = operation$context$checkpoint$path,
    recipe_revision = job@compute@recipe@revision,
    input_source = request$input_source
  )
  copy_output_directory(job@path, request$output)
  data
}

#' Score DNA sequences with Evo 2
#'
#' Scores are sums or means of token log probabilities selected by the
#' recipe's loss mask. Higher values indicate that the model assigns greater
#' likelihood to the sequence under the requested reduction.
#'
#' @param object An Evo 2 model with an explicit checkpoint.
#' @param newdata Sequences or a FASTA path.
#' @param compute A BioNeMo compute descriptor. `NULL` uses the descriptor
#'   attached by [evo2_model()] or a previous fine-tuning run.
#' @param reduction `"mean"` divides each strand's log-probability sum by its
#'   scored token count; `"sum"` retains the total.
#' @param strand Score the supplied sequence, its reverse complement, or both.
#' @param batch_size Prediction micro-batch size.
#' @param prepend_bos Whether the prediction entry point should prepend the
#'   beginning-of-sequence token before scoring.
#' @param normalize Sequence normalization mode. With `normalize = "none"`,
#'   reverse-strand operations require uppercase IUPAC DNA.
#' @param control Controls from [evo2_inference_control()].
#' @param output Optional directory for a copy of portable outputs.
#' @param name Optional run name.
#' @param async Whether to return before completion.
#'
#' @details The recipe first computes reduced log probabilities separately for
#'   each requested strand. `forward_score` and `reverse_score` retain those
#'   strand-specific values and are `NA` for strands that were not requested.
#'   `score` equals the available strand score, or the arithmetic mean of both
#'   values when `strand = "both"`. The `reduction` column records whether each
#'   strand was reduced by its token mean or sum.
#'
#' @return With `async = FALSE`, an `evo2_scores` data frame with columns
#'   `id`, `sequence_length`, `tokens_scored`, `score`, `forward_score`,
#'   `reverse_score`, `reduction`, and `strand`. With `async = TRUE`, a
#'   `BioNeMoJob`.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
#' compute <- bionemo_install(compute)
#' model <- evo2_model("7b", compute)
#'
#' scores <- evo2_score(
#'   model,
#'   c(reference = "ACGTACGT", variant = "ACGTTCGT"),
#'   reduction = "mean",
#'   strand = "both"
#' )
#' scores
#' }
#'
#' @references
#' [BioNeMo Recipes batch sequence scoring](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#batch-sequence-scoring-predict_evo2)
#' @export
evo2_score <- function(
  object,
  newdata,
  compute = NULL,
  reduction = c("mean", "sum"),
  strand = c("forward", "reverse", "both"),
  batch_size = 1L,
  prepend_bos = FALSE,
  normalize = c("dna", "none"),
  control = evo2_inference_control(),
  output = NULL,
  name = NULL,
  async = FALSE
) {
  reduction <- match.arg(reduction)
  strand <- match.arg(strand)
  normalize <- match.arg(normalize)
  stopifnot(
    "batch_size must be a positive integer" = is_scalar_integerish(
      batch_size,
      min = 1
    ),
    "prepend_bos must be TRUE or FALSE" = is_scalar_logical(prepend_bos),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  context <- validate_inference_context(object, compute, control)
  checkpoint <- context$checkpoint
  checkpoint_root <- context$checkpoint_root
  checkpoint_manifest <- context$checkpoint_manifest
  compute <- context$compute
  output <- validate_output_path(output, compute)
  request <- list(
    model = object@size,
    reduction = reduction,
    strand = strand,
    batch_size = as.integer(batch_size),
    prepend_bos = prepend_bos,
    normalize = normalize,
    control = S7::props(control),
    output = output
  )
  run <- create_run(compute, "score", name)
  run_path <- run$path
  input <- prepare_sequence_input(
    newdata,
    run_path,
    normalize = normalize
  )
  request$input_source <- input$input_source
  validate_sequence_context(
    input,
    object,
    compute,
    operation = "score",
    run_path = run_path,
    checkpoint = checkpoint_root
  )
  input <- prepare_stranded_input(input, run_path, strand)
  upstream <- file.path(run_path, "upstream", "predictions")
  portable <- file.path(run_path, "outputs", "scores.jsonl")
  resolved <- resolved_inference_control(
    control,
    compute,
    "score",
    checkpoint_manifest
  )
  plan <- evo2_prediction_plan(
    mode = "score",
    checkpoint = checkpoint,
    input = input,
    upstream = upstream,
    portable = portable,
    compute = compute,
    resolved = resolved,
    control = control,
    batch_size = as.integer(batch_size),
    reduction = reduction,
    prepend_bos = prepend_bos
  )
  operation <- operation_spec(
    run = run,
    request = request,
    plan = plan,
    result = list(
      type = "score",
      portable = portable,
      sequence_map = file.path(run_path, "inputs", "sequence-map.json")
    ),
    execution = list(resolved_control = resolved),
    context = evo2_inference_operation_context(
      object,
      checkpoint_root,
      checkpoint_manifest,
      resolved
    ),
    cleanup = list(directory = "upstream", suffix = ".pt")
  )
  submit_operation(operation, async = async)
}

materialize_score_job <- function(job, operation) {
  descriptor <- operation$result
  request <- operation$request
  rows <- read_jsonl_rows(descriptor$portable)
  map <- read_json_file(descriptor$sequence_map, simplify = TRUE)
  if (!is.data.frame(map)) {
    map <- as.data.frame(map, stringsAsFactors = FALSE)
  }
  input <- map[!duplicated(map$id), , drop = FALSE]
  derived_ids <- pluck_chr(rows, "derived_id")
  if (!setequal(derived_ids, map$derived_id)) {
    stop("score output contains unexpected derived IDs")
  }
  score <- stats::setNames(
    vapply(rows, function(row) as.double(row$score), numeric(1)),
    derived_ids
  )
  tokens <- stats::setNames(
    vapply(rows, function(row) as.integer(row$tokens_scored), integer(1)),
    derived_ids
  )
  records <- lapply(seq_len(nrow(input)), function(index) {
    id <- input$id[[index]]
    forward_id <- if (request$strand == "forward") {
      id
    } else {
      paste0(id, "::forward")
    }
    reverse_id <- paste0(id, "::reverse")
    forward <- if (forward_id %in% names(score)) {
      unname(score[[forward_id]])
    } else {
      NA_real_
    }
    reverse <- if (reverse_id %in% names(score)) {
      unname(score[[reverse_id]])
    } else {
      NA_real_
    }
    selected <- c(forward, reverse)
    selected <- selected[!is.na(selected)]
    token_values <- tokens[names(tokens) %in% c(forward_id, reverse_id)]
    list(
      id = id,
      sequence_length = as.integer(input$sequence_length[[index]]),
      tokens_scored = as.integer(min(token_values)),
      score = mean(selected),
      forward_score = forward,
      reverse_score = reverse,
      reduction = request$reduction,
      strand = request$strand
    )
  })
  data <- data.frame(
    id = pluck_chr(records, "id"),
    sequence_length = pluck_int(records, "sequence_length"),
    tokens_scored = pluck_int(records, "tokens_scored"),
    score = pluck_dbl(records, "score"),
    forward_score = pluck_dbl(records, "forward_score"),
    reverse_score = pluck_dbl(records, "reverse_score"),
    reduction = pluck_chr(records, "reduction"),
    strand = pluck_chr(records, "strand"),
    stringsAsFactors = FALSE
  )
  class(data) <- c("evo2_scores", "data.frame")
  attr(data, "provenance") <- list(
    run_path = job@path,
    checkpoint = operation$context$checkpoint$path,
    recipe_revision = job@compute@recipe@revision
  )
  copy_output_directory(job@path, request$output)
  data
}

#' Write positional Evo 2 log-probability profiles
#'
#' Profiles retain one log probability per scored nucleotide. Positions are
#' one-based coordinates in the supplied sequence. Reverse-strand positions
#' are mapped back to those coordinates; `strand = "both"` averages forward
#' and reverse values at each aligned position. Context parallelism must be
#' one.
#'
#' @inheritParams evo2_score
#' @param output Required Parquet output path inside the compute workspace for
#'   container execution.
#'
#' @return With `async = FALSE`, a `BioNeMoArtifact` for a Parquet file with
#'   columns `id` (string), `position` (int64), `base` (string),
#'   `log_probability` (double), and `strand` (string). With `async = TRUE`, a
#'   `BioNeMoJob`.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
#' compute <- bionemo_install(compute)
#' model <- evo2_model("7b", compute)
#'
#' profile <- evo2_profile(
#'   model,
#'   c(reference = "ACGTACGT"),
#'   strand = "both",
#'   output = "outputs/reference-profile.parquet"
#' )
#' profile@path
#' }
#' @export
evo2_profile <- function(
  object,
  newdata,
  compute = NULL,
  strand = c("forward", "reverse", "both"),
  batch_size = 1L,
  normalize = c("dna", "none"),
  control = evo2_inference_control(),
  output = NULL,
  name = NULL,
  async = FALSE
) {
  strand <- match.arg(strand)
  normalize <- match.arg(normalize)
  stopifnot(
    "control must be an Evo 2 inference control" = S7_inherits(
      control,
      Evo2InferenceControl
    ),
    "batch_size must be a positive integer" = is_scalar_integerish(
      batch_size,
      min = 1
    ),
    "profile output is required" = is_scalar_string(output),
    "context parallelism is not supported for positional profiles" = identical(
      control@context_parallel_size,
      1L
    ),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  context <- validate_inference_context(object, compute, control)
  checkpoint <- context$checkpoint
  checkpoint_root <- context$checkpoint_root
  checkpoint_manifest <- context$checkpoint_manifest
  compute <- context$compute
  output <- validate_output_path(output, compute)
  request <- list(
    model = object@size,
    strand = strand,
    batch_size = as.integer(batch_size),
    normalize = normalize,
    control = S7::props(control),
    output = output
  )
  run <- create_run(compute, "profile", name)
  run_path <- run$path
  input <- prepare_sequence_input(newdata, run_path, normalize = normalize)
  request$input_source <- input$input_source
  validate_sequence_context(
    input,
    object,
    compute,
    operation = "profile",
    run_path = run_path,
    checkpoint = checkpoint_root
  )
  input <- prepare_stranded_input(input, run_path, strand)
  upstream <- file.path(run_path, "upstream", "predictions")
  portable <- file.path(run_path, "outputs", "profile.parquet")
  resolved <- resolved_inference_control(
    control,
    compute,
    "profile",
    checkpoint_manifest
  )
  plan <- evo2_prediction_plan(
    "profile",
    checkpoint,
    input,
    upstream,
    portable,
    compute,
    resolved,
    control,
    as.integer(batch_size),
    prepend_bos = TRUE
  )
  operation <- operation_spec(
    run = run,
    request = request,
    plan = plan,
    result = list(
      type = "profile",
      portable = portable
    ),
    execution = list(resolved_control = resolved),
    context = evo2_inference_operation_context(
      object,
      checkpoint_root,
      checkpoint_manifest,
      resolved
    ),
    cleanup = list(directory = "upstream", suffix = ".pt")
  )
  submit_operation(operation, async = async)
}

materialize_profile_job <- function(job, operation) {
  descriptor <- operation$result
  request <- operation$request
  if (!file.exists(descriptor$portable)) {
    stop("profile helper did not write its portable output")
  }
  path <- copy_output_files(descriptor$portable, request$output)
  BioNeMoArtifact(
    path = path,
    format = "parquet",
    kind = "evo2_profile",
    shape = list(),
    schema = list(
      id = "string",
      position = "int64",
      base = "string",
      log_probability = "double",
      strand = "string"
    ),
    metadata = list(strand = request$strand),
    provenance = list(
      run_path = job@path,
      checkpoint = operation$context$checkpoint$path,
      recipe_revision = job@compute@recipe@revision
    )
  )
}

#' Extract Evo 2 sequence embeddings
#'
#' @inheritParams evo2_score
#' @param layer `"last"` or a one-based decoder layer. R layer `1` maps to
#'   upstream layer `0`; `"last"` maps to upstream `-1`.
#' @param pool Pooling across non-padding sequence positions. `"mean"` and
#'   `"max"` aggregate values; `"first"` and `"last"` select an endpoint. With
#'   `pool = "none"`, `output` is required and `strand = "both"` is not
#'   supported.
#' @param output For pooled embeddings, an optional prefix for the compressed
#'   float32 data (`.f32.gz`) and JSON metadata (`.json`). For unpooled
#'   embeddings, a required Parquet output path.
#'
#' @details Embeddings currently require `context_parallel_size = 1`. With a
#'   pooling rule, token embeddings are pooled independently for each strand.
#'   With `strand = "both"`, the forward and reverse pooled vectors are then
#'   averaged. The returned matrix preserves input IDs as row names and names
#'   its columns `dim_1`, `dim_2`, and so on.
#'
#'   Pooled embedding files store little-endian, row-major float32 values. R
#'   reads these values into its native double-precision numeric matrix.
#'
#'   With `pool = "none"`, the result is a Parquet artifact with columns `id`
#'   (string), `position` (int64), `embedding` (list of doubles), and `strand`
#'   (string). Unpooled output requires `output` and does not support
#'   `strand = "both"`.
#'
#' @return With `async = FALSE`, an `evo2_embeddings` numeric matrix for
#'   pooled output or a `BioNeMoArtifact` for unpooled output. With
#'   `async = TRUE`, a `BioNeMoJob`.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
#' compute <- bionemo_install(compute)
#' model <- evo2_model("7b", compute)
#'
#' embeddings <- evo2_embed(
#'   model,
#'   c(reference = "ACGTACGT", variant = "ACGTTCGT"),
#'   pool = "mean",
#'   strand = "both"
#' )
#' dim(embeddings)
#' embeddings[, 1:4, drop = FALSE]
#' }
#'
#' @references
#' [BioNeMo Recipes embedding extraction](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#batch-sequence-scoring-predict_evo2)
#' @export
evo2_embed <- function(
  object,
  newdata,
  compute = NULL,
  layer = "last",
  pool = c("mean", "max", "first", "last", "none"),
  strand = c("forward", "reverse", "both"),
  batch_size = 1L,
  normalize = c("dna", "none"),
  control = evo2_inference_control(),
  output = NULL,
  name = NULL,
  async = FALSE
) {
  pool <- match.arg(pool)
  strand <- match.arg(strand)
  normalize <- match.arg(normalize)
  stopifnot(
    "control must be an Evo 2 inference control" = S7_inherits(
      control,
      Evo2InferenceControl
    ),
    "layer must be 'last' or a positive integer" = identical(layer, "last") ||
      is_scalar_integerish(layer, min = 1),
    "batch_size must be a positive integer" = is_scalar_integerish(
      batch_size,
      min = 1
    ),
    "unpooled embeddings require output" = pool != "none" ||
      is_scalar_string(output),
    "unpooled bidirectional embeddings are not supported" = pool != "none" ||
      strand != "both",
    "context parallelism is not supported for embeddings" = identical(
      control@context_parallel_size,
      1L
    ),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  context <- validate_inference_context(object, compute, control)
  checkpoint <- context$checkpoint
  checkpoint_root <- context$checkpoint_root
  checkpoint_manifest <- context$checkpoint_manifest
  compute <- context$compute
  output <- validate_output_path(
    output,
    compute,
    if (pool == "none") character() else c(".f32.gz", ".json")
  )
  request <- list(
    model = object@size,
    layer = layer,
    pool = pool,
    strand = strand,
    batch_size = as.integer(batch_size),
    normalize = normalize,
    control = S7::props(control),
    output = output
  )
  run <- create_run(compute, "embedding", name)
  run_path <- run$path
  input <- prepare_sequence_input(newdata, run_path, normalize = normalize)
  request$input_source <- input$input_source
  validate_sequence_context(
    input,
    object,
    compute,
    operation = "embedding",
    run_path = run_path,
    checkpoint = checkpoint_root
  )
  input <- prepare_stranded_input(input, run_path, strand)
  upstream <- file.path(run_path, "upstream", "predictions")
  mode <- if (pool == "none") "embedding-unpooled" else "embedding-pooled"
  portable <- file.path(
    run_path,
    "outputs",
    if (pool == "none") "embeddings.parquet" else "embeddings"
  )
  upstream_layer <- if (identical(layer, "last")) {
    -1L
  } else {
    as.integer(layer) - 1L
  }
  resolved <- resolved_inference_control(
    control,
    compute,
    mode,
    checkpoint_manifest
  )
  plan <- evo2_prediction_plan(
    mode,
    checkpoint,
    input,
    upstream,
    portable,
    compute,
    resolved,
    control,
    as.integer(batch_size),
    layer = upstream_layer,
    pool = if (pool == "none") NULL else pool
  )
  operation <- operation_spec(
    run = run,
    request = request,
    plan = plan,
    result = list(
      type = mode,
      portable = portable
    ),
    execution = list(
      input_ids = input$ids,
      resolved_control = resolved
    ),
    context = evo2_inference_operation_context(
      object,
      checkpoint_root,
      checkpoint_manifest,
      resolved
    ),
    cleanup = list(directory = "upstream", suffix = ".pt")
  )
  submit_operation(operation, async = async)
}

materialize_embedding_job <- function(job, operation) {
  descriptor <- operation$result
  request <- operation$request
  execution <- operation$execution
  if (descriptor$type == "embedding-unpooled") {
    if (!file.exists(descriptor$portable)) {
      stop("embedding helper did not write its portable output")
    }
    summary_path <- paste0(descriptor$portable, ".summary.json")
    if (!file.exists(summary_path)) {
      stop("embedding helper did not write its summary")
    }
    summary <- read_json_file(summary_path)
    shape <- as.integer(unlist(summary$shape, use.names = FALSE))
    schema <- as.list(summary$schema)
    expected_schema <- list(
      id = "string",
      position = "int64",
      embedding = "list<double>",
      strand = "string"
    )
    if (!identical(summary$mode, "embedding-unpooled")) {
      stop("embedding helper summary has the wrong mode")
    }
    if (
      length(shape) != 2L ||
        !identical(shape[[1L]], as.integer(summary$rows)) ||
        shape[[2L]] != length(expected_schema)
    ) {
      stop("embedding helper summary has an invalid shape")
    }
    if (
      length(schema) != length(expected_schema) ||
        !identical(schema[names(expected_schema)], expected_schema)
    ) {
      stop("embedding helper summary has an invalid schema")
    }
    schema <- expected_schema
    path <- copy_output_files(descriptor$portable, request$output)
    return(BioNeMoArtifact(
      path = path,
      format = "parquet",
      kind = "evo2_embeddings",
      shape = shape,
      schema = schema,
      metadata = list(
        layer = request$layer,
        pool = request$pool,
        strand = request$strand
      ),
      provenance = list(
        run_path = job@path,
        checkpoint = operation$context$checkpoint$path,
        recipe_revision = job@compute@recipe@revision
      )
    ))
  }
  matrix <- read_pooled_embedding_matrix(
    descriptor$portable,
    unlist(execution$input_ids, use.names = FALSE)
  )
  class(matrix) <- c("evo2_embeddings", "matrix", "array")
  attr(matrix, "provenance") <- list(
    run_path = job@path,
    checkpoint = operation$context$checkpoint$path,
    layer = request$layer,
    pool = request$pool,
    strand = request$strand,
    recipe_revision = job@compute@recipe@revision
  )
  if (!is.null(request$output)) {
    copy_output_files(
      descriptor$portable,
      request$output,
      c(".f32.gz", ".json")
    )
  }
  matrix
}

#' Run Evo 2 inference through the compatibility generic
#'
#' @param object An Evo 2 model.
#' @param newdata Sequences or prompts.
#' @param type Inference operation.
#' @param compute A BioNeMo compute descriptor. `NULL` uses the descriptor
#'   attached by [evo2_model()] or a previous fine-tuning run.
#' @param ... Arguments passed to the task-specific function.
#'
#' @return The task-specific result.
#' @noRd
method(predict, Evo2Model) <- function(
  object,
  newdata,
  type = c("score", "generate", "embedding"),
  compute = NULL,
  ...
) {
  type <- match.arg(type)
  arguments <- c(list(object = object, compute = compute), list(...))
  switch(
    type,
    score = do.call(
      evo2_score,
      c(arguments, list(newdata = newdata))
    ),
    generate = do.call(
      evo2_generate,
      c(arguments, list(prompt = newdata))
    ),
    embedding = do.call(
      evo2_embed,
      c(arguments, list(newdata = newdata))
    )
  )
}
