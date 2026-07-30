validate_inference_context <- function(object, compute, control) {
  stopifnot(
    "object must be an Evo 2 model" = S7_inherits(object, Evo2Model),
    "compute must be a BioNeMo compute specification" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "control must be an Evo 2 inference control" = S7_inherits(
      control,
      Evo2InferenceControl
    ),
    "compute workspace must exist" = dir.exists(compute@workspace)
  )
  checkpoint <- model_checkpoint_path(object, base = compute@workspace)
  if (!is_scalar_string(checkpoint)) {
    stop("inference requires an explicit checkpoint")
  }
  if (!dir.exists(checkpoint)) {
    stop("checkpoint does not exist")
  }
  checkpoint_root <- checkpoint
  model_checkpoint <- object@checkpoint
  manifest <- NULL
  if (S7_inherits(model_checkpoint, BioNeMoCheckpoint)) {
    manifest <- checkpoint_manifest(model_checkpoint)
    checkpoint <- checkpoint_manifest_resolved_path(
      model_checkpoint@path,
      manifest
    )
    if (
      !identical(
        model_checkpoint@recipe_revision,
        compute@recipe@revision
      )
    ) {
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
  }
  if (
    S7_inherits(model_checkpoint, BioNeMoCheckpoint) &&
      identical(model_checkpoint@kind, "lora")
  ) {
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
  vortex <- control_property(control, "vortex_style_fp8", "auto")
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

persist_inference_input_source <- function(run_path, input) {
  if (!is_scalar_string(run_path) || !dir.exists(run_path)) {
    stop("run path must exist")
  }
  if (!is.list(input$input_source)) {
    stop("input source metadata must be a list")
  }
  path <- file.path(run_path, "request.json")
  request <- read_json_file(path, simplify = FALSE)
  request$request$input_source <- input$input_source
  request$request_origins$input_source <- "auto_resolved"
  atomic_write_json(request, path)
  input$input_source
}

validate_output_path <- function(output, compute) {
  if (!is.null(output) && !is_scalar_string(output)) {
    stop("output must be NULL or one non-empty string")
  }
  if (is.null(output)) {
    return(NULL)
  }
  output <- normalize_path(output, base = compute@workspace)
  if (file.exists(output)) {
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

copy_output_file <- function(path, output) {
  if (is.null(output)) {
    return(path)
  }
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(path, output, overwrite = TRUE)) {
    stop("failed to copy portable output")
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
#' @param object An Evo 2 model with an explicit checkpoint.
#' @param prompt Prompts as a character vector, data frame, FASTA path, or
#'   `DNAStringSet`.
#' @param compute A BioNeMo compute specification.
#' @param num_tokens Generation length.
#' @param n Samples per prompt. The pinned recipe currently supports only one.
#' @param temperature,top_k,top_p Sampling controls. At most one of `top_k`
#'   and `top_p` may be positive. Set `top_k = 0` when using top-p sampling.
#' @param seed Optional positive random seed. The pinned recipe maps zero to its
#'   default seed, so zero is rejected rather than recorded as if it were used.
#' @param return_probabilities Whether to return per-token probabilities.
#' @param normalize Sequence normalization mode.
#' @param validate Mechanical output validation level.
#' @param control Evo 2 inference controls.
#' @param output Optional directory for a copy of portable outputs.
#' @param name Optional run name.
#' @param async Whether to return before completion.
#'
#' @return An `evo2_generation` data frame or a `BioNeMoJob`.
#' @export
evo2_generate <- function(
  object,
  prompt,
  compute,
  num_tokens = 100L,
  n = 1L,
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
  invocation <- match.call(expand.dots = FALSE)
  normalize <- match.arg(normalize)
  validate <- match.arg(validate)
  stopifnot(
    "num_tokens must be a positive integer" = is_scalar_integerish(
      num_tokens,
      min = 1
    ),
    "n must equal 1 with the pinned recipe" = is_scalar_integerish(
      n,
      min = 1
    ) &&
      n == 1,
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
    n = as.integer(n),
    temperature = as.double(temperature),
    top_k = as.integer(top_k),
    top_p = as.double(top_p),
    seed = as_nullable_integer(seed),
    return_probabilities = return_probabilities,
    normalize = normalize,
    validate = validate,
    control = S7::props(control)
  )
  request_origins <- argument_origin_map(
    request,
    invocation,
    argument_map = c(
      model = "object",
      num_tokens = "num_tokens",
      n = "n",
      temperature = "temperature",
      top_k = "top_k",
      top_p = "top_p",
      seed = "seed",
      return_probabilities = "return_probabilities",
      normalize = "normalize",
      validate = "validate",
      control = "control"
    )
  )
  request_origins$control <- object_value_origins(
    control,
    fallback = request_origins$control
  )
  run_path <- create_run(
    compute,
    "generation",
    name,
    request = request,
    request_origins = request_origins
  )
  input <- prepare_sequence_input(
    prompt,
    run_path,
    normalize = normalize,
    column = "prompt",
    filename = "prompts.fasta"
  )
  input_source <- persist_inference_input_source(run_path, input)
  validate_sequence_context(
    input,
    object,
    compute,
    operation = "generation",
    run_path = run_path,
    checkpoint = checkpoint_root,
    additional_tokens = as.integer(num_tokens),
    max_sequence_length = control_property(
      control,
      "max_sequence_length",
      NULL
    )
  )
  prompts <- generation_prompt_rows(input)
  prompt_path <- file.path(run_path, "inputs", "prompts.jsonl")
  write_jsonl_rows(prompts, prompt_path)
  upstream <- file.path(run_path, "upstream", "generation.jsonl")
  portable <- file.path(run_path, "outputs", "generation.jsonl")
  fasta <- file.path(run_path, "outputs", "generated.fasta")
  validation_path <- file.path(run_path, "outputs", "validation.json")
  plan <- evo2_generation_plan(
    checkpoint = checkpoint,
    prompts = prompt_path,
    upstream = upstream,
    portable = portable,
    fasta = fasta,
    validation = validation_path,
    compute = compute,
    control = control,
    num_tokens = as.integer(num_tokens),
    temperature = as.double(temperature),
    top_k = as.integer(top_k),
    top_p = as.double(top_p),
    seed = as_nullable_integer(seed),
    return_probabilities = return_probabilities,
    validate = validate,
    checkpoint_manifest = checkpoint_manifest
  )
  descriptor <- list(
    type = "generation",
    checkpoint = checkpoint_root,
    resolved_checkpoint = checkpoint,
    variant = object@size,
    model_size = object@model_size,
    prompts = prompt_path,
    upstream = upstream,
    portable = portable,
    fasta = fasta,
    validation_path = validation_path,
    num_tokens = as.integer(num_tokens),
    return_probabilities = return_probabilities,
    validate = validate,
    normalize = normalize,
    input_source = input_source,
    output = output
  )
  submit_plan(
    plan,
    compute,
    run_path,
    "generation",
    descriptor,
    async = async
  )
}

generation_rows_data_frame <- function(records) {
  scalar_names <- c(
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
    "generated_bases",
    "gc_fraction",
    "ambiguous_fraction",
    "longest_homopolymer"
  )
  data <- as.data.frame(
    lapply(scalar_names, function(name) {
      pluck_vec(records, name, records[[1L]][[name]])
    }),
    stringsAsFactors = FALSE
  )
  names(data) <- scalar_names
  data$log_probabilities <- I(lapply(records, `[[`, "log_probabilities"))
  data$probabilities <- I(lapply(records, `[[`, "probabilities"))
  data$validation_warnings <- I(lapply(records, `[[`, "validation_warnings"))
  data[c(
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
  )]
}

abort_generation_schema <- function(
  job,
  descriptor,
  message,
  request_id = NULL
) {
  bionemor_abort(
    "BN_OUTPUT_SCHEMA",
    message,
    run_path = job@path,
    request_id = request_id,
    operation = "generation",
    model = descriptor$variant,
    checkpoint = descriptor$checkpoint,
    recipe_revision = job@compute@recipe@revision,
    log_paths = file.path(job@path, c("stdout.log", "stderr.log"))
  )
}

read_generation_jsonl <- function(job, descriptor, path, label) {
  tryCatch(
    read_jsonl_rows(path),
    error = function(error) {
      abort_generation_schema(
        job,
        descriptor,
        paste0("generation ", label, " is not valid JSONL")
      )
    }
  )
}

materialize_generation_job <- function(job, descriptor) {
  rows <- read_generation_jsonl(
    job,
    descriptor,
    descriptor$portable,
    "portable output"
  )
  if (length(rows) == 0L) {
    abort_generation_schema(
      job,
      descriptor,
      "generation portable output must contain at least one row"
    )
  }
  records <- lapply(rows, function(row) {
    request_id <- if (is.list(row) && is_scalar_string(row$id)) {
      row$id
    } else {
      NULL
    }
    if (!is.list(row)) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output row must be an object"
      )
    }
    string_fields <- c(
      "id",
      "input_id",
      "prompt",
      "completion",
      "sequence",
      "finish_reason"
    )
    invalid_string <- string_fields[
      !vapply(
        row[string_fields],
        is_scalar_string,
        logical(1)
      )
    ]
    if (length(invalid_string)) {
      abort_generation_schema(
        job,
        descriptor,
        paste0(
          "generation portable output has an invalid ",
          invalid_string[[1L]]
        ),
        request_id = request_id
      )
    }
    integer_fields <- c(
      sample = 1L,
      prompt_tokens = 0L,
      generated_tokens = 0L,
      total_tokens = 0L,
      generated_bases = 0L,
      longest_homopolymer = 0L
    )
    invalid_integer <- names(integer_fields)[
      !vapply(
        names(integer_fields),
        function(field) {
          is_scalar_integerish(row[[field]], min = integer_fields[[field]])
        },
        logical(1)
      )
    ]
    if (length(invalid_integer)) {
      abort_generation_schema(
        job,
        descriptor,
        paste0(
          "generation portable output has an invalid ",
          invalid_integer[[1L]]
        ),
        request_id = request_id
      )
    }
    gc_fraction <- row$gc_fraction
    if (is.null(gc_fraction)) {
      gc_fraction <- NA_real_
    } else if (
      !is_scalar_number(gc_fraction) ||
        gc_fraction < 0 ||
        gc_fraction > 1
    ) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output has an invalid gc_fraction",
        request_id = request_id
      )
    }
    ambiguous_fraction <- row$ambiguous_fraction
    if (
      !is_scalar_number(ambiguous_fraction) ||
        ambiguous_fraction < 0 ||
        ambiguous_fraction > 1
    ) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output has an invalid ambiguous_fraction",
        request_id = request_id
      )
    }
    numeric_vector <- function(field) {
      value <- row[[field]]
      if (is.null(value)) {
        return(NULL)
      }
      if (length(value) == 0L) {
        return(numeric())
      }
      value <- unlist(value, use.names = FALSE)
      if (!is.numeric(value) || any(!is.finite(value))) {
        abort_generation_schema(
          job,
          descriptor,
          paste0(
            "generation portable output has invalid ",
            field
          ),
          request_id = request_id
        )
      }
      as.double(value)
    }
    log_probabilities <- numeric_vector("log_probabilities")
    probabilities <- numeric_vector("probabilities")
    if (
      !is.null(log_probabilities) &&
        any(log_probabilities > 0)
    ) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output has invalid log_probabilities",
        request_id = request_id
      )
    }
    if (
      !is.null(probabilities) &&
        any(probabilities < 0 | probabilities > 1)
    ) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output has invalid probabilities",
        request_id = request_id
      )
    }
    validation_warnings <- row$validation_warnings
    if (
      !is.character(validation_warnings) &&
        !is.list(validation_warnings)
    ) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output has invalid validation_warnings",
        request_id = request_id
      )
    }
    validation_warnings <- as.character(unlist(
      validation_warnings,
      use.names = FALSE
    ))
    if (
      anyNA(validation_warnings) ||
        any(!nzchar(validation_warnings))
    ) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output has invalid validation_warnings",
        request_id = request_id
      )
    }
    if (
      !identical(row$sequence, paste0(row$prompt, row$completion)) ||
        row$total_tokens != row$prompt_tokens + row$generated_tokens ||
        row$generated_bases > nchar(row$completion, type = "chars") ||
        descriptor$validate != "none" &&
          row$generated_tokens > descriptor$num_tokens ||
        descriptor$validate != "none" &&
          identical(row$finish_reason, "length") &&
          row$generated_tokens != descriptor$num_tokens ||
        !is.null(log_probabilities) &&
          length(log_probabilities) != row$generated_tokens ||
        !is.null(probabilities) &&
          length(probabilities) != row$generated_tokens
    ) {
      abort_generation_schema(
        job,
        descriptor,
        "generation portable output fields are inconsistent",
        request_id = request_id
      )
    }
    list(
      id = row$id,
      input_id = row$input_id,
      sample = as.integer(row$sample),
      prompt = row$prompt,
      completion = row$completion,
      sequence = row$sequence,
      finish_reason = row$finish_reason,
      prompt_tokens = as.integer(row$prompt_tokens),
      generated_tokens = as.integer(row$generated_tokens),
      total_tokens = as.integer(row$total_tokens),
      log_probabilities = log_probabilities,
      probabilities = probabilities,
      generated_bases = as.integer(row$generated_bases),
      gc_fraction = as.double(gc_fraction),
      ambiguous_fraction = as.double(ambiguous_fraction),
      longest_homopolymer = as.integer(row$longest_homopolymer),
      validation_warnings = validation_warnings
    )
  })
  ids <- pluck_chr(records, "id")
  if (anyDuplicated(ids)) {
    abort_generation_schema(
      job,
      descriptor,
      "generation portable output IDs must be unique",
      request_id = ids[[which(duplicated(ids))[[1L]]]]
    )
  }
  data <- generation_rows_data_frame(records)
  class(data) <- c("evo2_generation", "data.frame")
  attr(data, "provenance") <- list(
    run_path = job@path,
    checkpoint = descriptor$checkpoint,
    recipe_revision = job@compute@recipe@revision,
    input_source = descriptor$input_source
  )
  copy_output_directory(job@path, descriptor$output)
  data
}

#' Score DNA sequences with Evo 2
#'
#' @param object An Evo 2 model with an explicit checkpoint.
#' @param newdata Sequences or a FASTA path.
#' @param compute A BioNeMo compute specification.
#' @param reduction Aggregate score reduction.
#' @param strand Strand rule.
#' @param batch_size Prediction micro-batch size. The inference control's
#'   `micro_batch_size` must remain one.
#' @param prepend_bos Whether to prepend the beginning-of-sequence token.
#' @param mask_phylogenetic_tags Reserved for upstream support. It must be
#'   `FALSE` because the pinned prediction entry point does not apply it.
#' @param normalize Sequence normalization mode. With `normalize = "none"`,
#'   reverse-strand operations require uppercase IUPAC DNA.
#' @param control Evo 2 inference controls.
#' @param output Optional directory for a copy of portable outputs.
#' @param name Optional run name.
#' @param async Whether to return before completion.
#'
#' @return An `evo2_scores` data frame or a `BioNeMoJob`.
#' @export
evo2_score <- function(
  object,
  newdata,
  compute,
  reduction = c("mean", "sum"),
  strand = c("forward", "reverse", "both"),
  batch_size = 1L,
  prepend_bos = FALSE,
  mask_phylogenetic_tags = FALSE,
  normalize = c("dna", "none"),
  control = evo2_inference_control(),
  output = NULL,
  name = NULL,
  async = FALSE
) {
  invocation <- match.call(expand.dots = FALSE)
  reduction <- match.arg(reduction)
  strand <- match.arg(strand)
  normalize <- match.arg(normalize)
  stopifnot(
    "batch_size must be a positive integer" = is_scalar_integerish(
      batch_size,
      min = 1
    ),
    "prepend_bos must be TRUE or FALSE" = is_scalar_logical(prepend_bos),
    "mask_phylogenetic_tags must be TRUE or FALSE" = is_scalar_logical(
      mask_phylogenetic_tags
    ),
    "mask_phylogenetic_tags must be FALSE with the pinned recipe" = identical(
      mask_phylogenetic_tags,
      FALSE
    ),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  validate_prediction_control(control)
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
    mask_phylogenetic_tags = mask_phylogenetic_tags,
    normalize = normalize,
    control = S7::props(control)
  )
  request_origins <- argument_origin_map(
    request,
    invocation,
    argument_map = c(
      model = "object",
      reduction = "reduction",
      strand = "strand",
      batch_size = "batch_size",
      prepend_bos = "prepend_bos",
      mask_phylogenetic_tags = "mask_phylogenetic_tags",
      normalize = "normalize",
      control = "control"
    )
  )
  request_origins$control <- object_value_origins(
    control,
    fallback = request_origins$control
  )
  run_path <- create_run(
    compute,
    "score",
    name,
    request = request,
    request_origins = request_origins
  )
  input <- prepare_sequence_input(
    newdata,
    run_path,
    normalize = normalize
  )
  input_source <- persist_inference_input_source(run_path, input)
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
  plan <- evo2_prediction_plan(
    mode = "score",
    checkpoint = checkpoint,
    input = input,
    upstream = upstream,
    portable = portable,
    compute = compute,
    control = control,
    batch_size = as.integer(batch_size),
    reduction = reduction,
    prepend_bos = prepend_bos,
    checkpoint_manifest = checkpoint_manifest
  )
  descriptor <- list(
    type = "score",
    checkpoint = checkpoint_root,
    resolved_checkpoint = checkpoint,
    variant = object@size,
    model_size = object@model_size,
    input_ids = input$ids,
    sequences = unname(as.list(input$sequences)),
    sequence_map = file.path(run_path, "inputs", "sequence-map.json"),
    portable = portable,
    upstream = upstream,
    reduction = reduction,
    strand = strand,
    input_source = input_source,
    output = output
  )
  submit_plan(
    plan,
    compute,
    run_path,
    "score",
    descriptor,
    async = async
  )
}

materialize_score_job <- function(job, descriptor) {
  rows <- read_jsonl_rows(descriptor$portable)
  map <- read_json_file(descriptor$sequence_map, simplify = TRUE)
  if (!is.data.frame(map)) {
    map <- as.data.frame(map, stringsAsFactors = FALSE)
  }
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
  records <- lapply(seq_along(descriptor$input_ids), function(index) {
    id <- descriptor$input_ids[[index]]
    forward_id <- if (descriptor$strand == "forward") {
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
      sequence_length = as.integer(nchar(descriptor$sequences[[index]])),
      tokens_scored = as.integer(min(token_values)),
      score = mean(selected),
      forward_score = forward,
      reverse_score = reverse,
      reduction = descriptor$reduction,
      strand = descriptor$strand
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
    checkpoint = descriptor$checkpoint,
    recipe_revision = job@compute@recipe@revision
  )
  copy_output_directory(job@path, descriptor$output)
  data
}

#' Write positional Evo 2 log-probability profiles
#'
#' @inheritParams evo2_score
#' @param metric Positional metric.
#' @param output Required Parquet output path.
#'
#' @return A `BioNeMoArtifact` or `BioNeMoJob`.
#' @export
evo2_profile <- function(
  object,
  newdata,
  compute,
  metric = c("log_probability"),
  strand = c("forward", "reverse", "both"),
  batch_size = 1L,
  normalize = c("dna", "none"),
  control = evo2_inference_control(),
  output = NULL,
  name = NULL,
  async = FALSE
) {
  invocation <- match.call(expand.dots = FALSE)
  metric <- match.arg(metric)
  strand <- match.arg(strand)
  normalize <- match.arg(normalize)
  stopifnot(
    "batch_size must be a positive integer" = is_scalar_integerish(
      batch_size,
      min = 1
    ),
    "profile output is required" = is_scalar_string(output),
    "context parallelism is not supported for positional profiles" = identical(
      as.integer(control_property(control, "context_parallel_size", 1L)),
      1L
    ),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  validate_prediction_control(control)
  context <- validate_inference_context(object, compute, control)
  checkpoint <- context$checkpoint
  checkpoint_root <- context$checkpoint_root
  checkpoint_manifest <- context$checkpoint_manifest
  compute <- context$compute
  output <- validate_output_path(output, compute)
  request <- list(
    model = object@size,
    metric = metric,
    strand = strand,
    batch_size = as.integer(batch_size),
    normalize = normalize,
    control = S7::props(control)
  )
  request_origins <- argument_origin_map(
    request,
    invocation,
    argument_map = c(
      model = "object",
      metric = "metric",
      strand = "strand",
      batch_size = "batch_size",
      normalize = "normalize",
      control = "control"
    )
  )
  request_origins$control <- object_value_origins(
    control,
    fallback = request_origins$control
  )
  run_path <- create_run(
    compute,
    "profile",
    name,
    request = request,
    request_origins = request_origins
  )
  input <- prepare_sequence_input(newdata, run_path, normalize = normalize)
  input_source <- persist_inference_input_source(run_path, input)
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
  plan <- evo2_prediction_plan(
    "profile",
    checkpoint,
    input,
    upstream,
    portable,
    compute,
    control,
    as.integer(batch_size),
    prepend_bos = TRUE,
    checkpoint_manifest = checkpoint_manifest
  )
  submit_plan(
    plan,
    compute,
    run_path,
    "profile",
    list(
      type = "profile",
      checkpoint = checkpoint_root,
      resolved_checkpoint = checkpoint,
      variant = object@size,
      model_size = object@model_size,
      portable = portable,
      upstream = upstream,
      output = output,
      strand = strand,
      input_source = input_source
    ),
    async = async
  )
}

materialize_profile_job <- function(job, descriptor) {
  if (!file.exists(descriptor$portable)) {
    stop("profile helper did not write its portable output")
  }
  path <- copy_output_file(descriptor$portable, descriptor$output)
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
    metadata = list(strand = descriptor$strand),
    provenance = list(
      run_path = job@path,
      checkpoint = descriptor$checkpoint,
      recipe_revision = job@compute@recipe@revision
    )
  )
}

#' Extract Evo 2 sequence embeddings
#'
#' @inheritParams evo2_score
#' @param layer Final or one-based decoder layer.
#' @param pool Pooling rule. With `pool = "none"`, `output` is required and
#'   `strand = "both"` is not supported.
#' @param output Optional output file. Required when `pool = "none"`.
#'
#' @details Embeddings currently require `context_parallel_size = 1`. Unpooled
#'   embeddings use a Parquet artifact with columns `id` (string), `position`
#'   (int64), `embedding` (list of doubles), and `strand` (string).
#'
#' @return An `evo2_embeddings` matrix, `BioNeMoArtifact`, or `BioNeMoJob`.
#' @export
evo2_embed <- function(
  object,
  newdata,
  compute,
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
  invocation <- match.call(expand.dots = FALSE)
  pool <- match.arg(pool)
  strand <- match.arg(strand)
  normalize <- match.arg(normalize)
  stopifnot(
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
      as.integer(control_property(control, "context_parallel_size", 1L)),
      1L
    ),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  validate_prediction_control(control)
  context <- validate_inference_context(object, compute, control)
  checkpoint <- context$checkpoint
  checkpoint_root <- context$checkpoint_root
  checkpoint_manifest <- context$checkpoint_manifest
  compute <- context$compute
  output <- validate_output_path(output, compute)
  request <- list(
    model = object@size,
    layer = layer,
    pool = pool,
    strand = strand,
    batch_size = as.integer(batch_size),
    normalize = normalize,
    control = S7::props(control)
  )
  request_origins <- argument_origin_map(
    request,
    invocation,
    argument_map = c(
      model = "object",
      layer = "layer",
      pool = "pool",
      strand = "strand",
      batch_size = "batch_size",
      normalize = "normalize",
      control = "control"
    )
  )
  request_origins$control <- object_value_origins(
    control,
    fallback = request_origins$control
  )
  run_path <- create_run(
    compute,
    "embedding",
    name,
    request = request,
    request_origins = request_origins
  )
  input <- prepare_sequence_input(newdata, run_path, normalize = normalize)
  input_source <- persist_inference_input_source(run_path, input)
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
    if (pool == "none") "embeddings.parquet" else "embeddings.jsonl"
  )
  upstream_layer <- if (identical(layer, "last")) {
    -1L
  } else {
    as.integer(layer) - 1L
  }
  plan <- evo2_prediction_plan(
    mode,
    checkpoint,
    input,
    upstream,
    portable,
    compute,
    control,
    as.integer(batch_size),
    layer = upstream_layer,
    pool = if (pool == "none") NULL else pool,
    checkpoint_manifest = checkpoint_manifest
  )
  submit_plan(
    plan,
    compute,
    run_path,
    "embedding",
    list(
      type = mode,
      checkpoint = checkpoint_root,
      resolved_checkpoint = checkpoint,
      variant = object@size,
      model_size = object@model_size,
      portable = portable,
      upstream = upstream,
      input_ids = input$ids,
      layer = layer,
      pool = pool,
      strand = strand,
      input_source = input_source,
      output = output
    ),
    async = async
  )
}

materialize_embedding_job <- function(job, descriptor) {
  if (!file.exists(descriptor$portable)) {
    stop("embedding helper did not write its portable output")
  }
  if (descriptor$type == "embedding-unpooled") {
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
    if (!identical(schema, expected_schema)) {
      stop("embedding helper summary has an invalid schema")
    }
    path <- copy_output_file(descriptor$portable, descriptor$output)
    return(BioNeMoArtifact(
      path = path,
      format = "parquet",
      kind = "evo2_embeddings",
      shape = shape,
      schema = schema,
      metadata = list(
        layer = descriptor$layer,
        pool = descriptor$pool,
        strand = descriptor$strand
      ),
      provenance = list(
        run_path = job@path,
        checkpoint = descriptor$checkpoint,
        recipe_revision = job@compute@recipe@revision
      )
    ))
  }
  rows <- read_jsonl_rows(descriptor$portable)
  ids <- pluck_chr(rows, "id")
  if (!identical(ids, unlist(descriptor$input_ids, use.names = FALSE))) {
    stop("embedding output IDs do not match input order")
  }
  values <- lapply(rows, function(row) {
    as.double(unlist(row$embedding, use.names = FALSE))
  })
  if (length(unique(lengths(values))) != 1L) {
    stop("embedding rows must have one common width")
  }
  matrix <- do.call(rbind, values)
  rownames(matrix) <- ids
  colnames(matrix) <- paste0("dim_", seq_len(ncol(matrix)))
  class(matrix) <- c("evo2_embeddings", "matrix", "array")
  attr(matrix, "provenance") <- list(
    run_path = job@path,
    checkpoint = descriptor$checkpoint,
    layer = descriptor$layer,
    pool = descriptor$pool,
    strand = descriptor$strand,
    recipe_revision = job@compute@recipe@revision
  )
  if (!is.null(descriptor$output)) {
    copy_output_file(descriptor$portable, descriptor$output)
  }
  matrix
}

#' Run Evo 2 inference through the compatibility generic
#'
#' @param object An Evo 2 model.
#' @param newdata Sequences or prompts.
#' @param type Inference operation.
#' @param compute A BioNeMo compute specification.
#' @param ... Arguments passed to the task-specific function.
#'
#' @return The task-specific result.
#' @noRd
method(predict, Evo2Model) <- function(
  object,
  newdata,
  type = c("score", "generate", "embedding", "response", "raw"),
  compute,
  ...
) {
  type <- match.arg(type)
  if (type == "raw") {
    bionemor_abort(
      "BN_PROTOCOL",
      "portable raw-forward output is not yet supported",
      operation = "raw-forward",
      model = object@size
    )
  }
  if (type == "response") {
    warning(
      "type = \"response\" is deprecated; use type = \"generate\"",
      call. = FALSE
    )
    type <- "generate"
  }
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
