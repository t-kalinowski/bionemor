validate_control_extra <- function(extra, allowed) {
  stopifnot(
    "extra must be a named list" = is.list(extra) &&
      (length(extra) == 0L ||
        !is.null(names(extra)) &&
          all(nzchar(names(extra))) &&
          !anyDuplicated(names(extra))),
    "extra contains an unsupported setting" = all(names(extra) %in% allowed),
    "extra values must be non-missing scalar values" = all(vapply(
      extra,
      function(value) {
        is.atomic(value) && length(value) == 1L && !is.na(value)
      },
      logical(1)
    ))
  )
  extra
}

inference_extra_fields <- c(
  "no_sequence_parallel",
  "min_length",
  "eden_tokenizer",
  "hybrid_override_pattern",
  "num_layers",
  "seq_len_interpolation_factor"
)

#' Construct typed Evo 2 inference controls
#'
#' @param tensor_parallel_size,pipeline_parallel_size,context_parallel_size
#'   Model-parallel rank counts. Prediction currently requires pipeline
#'   parallelism of one. Positional profiles and embeddings also require
#'   context parallelism of one.
#' @param micro_batch_size Reserved for upstream support and must remain one.
#'   Use the task-specific `batch_size` argument for prediction and
#'   `max_batch_size` for generation.
#' @param max_sequence_length Optional generation context limit.
#' @param max_batch_size Maximum generation batch size.
#' @param precision Semantic inference precision.
#' @param mixed_precision_recipe Optional exact upstream precision recipe.
#' @param vortex_style_fp8 Whether to use the Vortex-style FP8 path.
#' @param cuda_graphs CUDA graph implementation.
#' @param subquadratic_ops Whether to enable subquadratic operators.
#' @param chunked_prefill Whether to enable chunked prefill.
#' @param dynamic_max_tokens Optional dynamic-batching token limit.
#' @param dynamic_block_size Dynamic-batching block size.
#' @param extra Named allowlisted advanced upstream settings. Prediction
#'   supports `no_sequence_parallel` and `min_length`; generation supports no
#'   extra settings.
#'
#' @return An S7 `Evo2InferenceControl`.
#' @export
evo2_inference_control <- function(
  tensor_parallel_size = 1L,
  pipeline_parallel_size = 1L,
  context_parallel_size = 1L,
  micro_batch_size = 1L,
  max_sequence_length = NULL,
  max_batch_size = 1L,
  precision = c("auto", "bf16", "fp8"),
  mixed_precision_recipe = NULL,
  vortex_style_fp8 = c("auto", "yes", "no"),
  cuda_graphs = c("auto", "local", "none"),
  subquadratic_ops = FALSE,
  chunked_prefill = FALSE,
  dynamic_max_tokens = NULL,
  dynamic_block_size = 256L,
  extra = list()
) {
  invocation <- match.call(expand.dots = FALSE)
  precision <- match.arg(precision)
  vortex_style_fp8 <- match.arg(vortex_style_fp8)
  cuda_graphs <- match.arg(cuda_graphs)
  stopifnot(
    "tensor_parallel_size must be a positive integer" = is_scalar_integerish(
      tensor_parallel_size,
      min = 1
    ),
    "pipeline_parallel_size must be 1" = is_scalar_integerish(
      pipeline_parallel_size,
      min = 1
    ) &&
      pipeline_parallel_size == 1,
    "context_parallel_size must be a positive integer" = is_scalar_integerish(
      context_parallel_size,
      min = 1
    ),
    "micro_batch_size must be a positive integer" = is_scalar_integerish(
      micro_batch_size,
      min = 1
    ),
    "max_sequence_length must be NULL or a positive integer" = is.null(
      max_sequence_length
    ) ||
      is_scalar_integerish(max_sequence_length, min = 1),
    "max_batch_size must be a positive integer" = is_scalar_integerish(
      max_batch_size,
      min = 1
    ),
    "mixed_precision_recipe must be NULL or one non-empty string" = is.null(
      mixed_precision_recipe
    ) ||
      is_scalar_string(mixed_precision_recipe),
    "subquadratic_ops must be TRUE or FALSE" = is_scalar_logical(
      subquadratic_ops
    ),
    "chunked_prefill must be TRUE or FALSE" = is_scalar_logical(
      chunked_prefill
    ),
    "dynamic_max_tokens must be NULL or a positive integer" = is.null(
      dynamic_max_tokens
    ) ||
      is_scalar_integerish(dynamic_max_tokens, min = 1),
    "dynamic_block_size must be a positive integer" = is_scalar_integerish(
      dynamic_block_size,
      min = 1
    ),
    "subquadratic_ops and local CUDA graphs are incompatible" = !subquadratic_ops ||
      cuda_graphs != "local"
  )
  extra <- validate_control_extra(extra, inference_extra_fields)

  result <- Evo2InferenceControl(
    tensor_parallel_size = as.integer(tensor_parallel_size),
    pipeline_parallel_size = as.integer(pipeline_parallel_size),
    context_parallel_size = as.integer(context_parallel_size),
    micro_batch_size = as.integer(micro_batch_size),
    max_sequence_length = as_nullable_integer(max_sequence_length),
    max_batch_size = as.integer(max_batch_size),
    precision = precision,
    mixed_precision_recipe = mixed_precision_recipe,
    vortex_style_fp8 = vortex_style_fp8,
    cuda_graphs = cuda_graphs,
    subquadratic_ops = subquadratic_ops,
    chunked_prefill = chunked_prefill,
    dynamic_max_tokens = as_nullable_integer(dynamic_max_tokens),
    dynamic_block_size = as.integer(dynamic_block_size),
    extra = extra
  )
  set_object_value_origins(
    result,
    argument_origin_map(S7::props(result), invocation)
  )
}

#' Construct typed Evo 2 preprocessing controls
#'
#' @param uppercase Whether to uppercase sequences during preprocessing.
#' @param embed_reverse_complement Whether to embed reverse complements.
#' @param random_reverse_complement Probability of applying a random reverse
#'   complement.
#' @param random_lineage_dropout Probability of dropping lineage fields.
#' @param transcribe Optional transcription direction.
#' @param append_eod Whether to append an end-of-document token.
#' @param sample_length Optional fixed sample length.
#' @param drop_empty_sequences Whether to discard empty sequences.
#' @param filter_nnn Whether to filter sequences containing `NNN`.
#' @param taxonomy Optional taxonomy path, data frame, or named list.
#' @param prompt_spacer_length Spacer length used with taxonomy prompts.
#' @param workers Preprocessing worker count.
#' @param concurrency Maximum preprocessing task concurrency.
#' @param chunk_size Number of records per preprocessing chunk.
#' @param seed Non-negative preprocessing seed.
#'
#' @return An S7 `Evo2PreprocessControl`.
#' @export
evo2_preprocess_control <- function(
  uppercase = FALSE,
  embed_reverse_complement = FALSE,
  random_reverse_complement = 0,
  random_lineage_dropout = 0,
  transcribe = c("none", "transcribe", "back_transcribe"),
  append_eod = TRUE,
  sample_length = NULL,
  drop_empty_sequences = TRUE,
  filter_nnn = FALSE,
  taxonomy = NULL,
  prompt_spacer_length = 131072L,
  workers = 1L,
  concurrency = 100000L,
  chunk_size = 1L,
  seed = 1L
) {
  invocation <- match.call(expand.dots = FALSE)
  transcribe <- match.arg(transcribe)
  stopifnot(
    "uppercase must be TRUE or FALSE" = is_scalar_logical(uppercase),
    "embed_reverse_complement must be TRUE or FALSE" = is_scalar_logical(
      embed_reverse_complement
    ),
    "random_reverse_complement must be between zero and one" = is_scalar_number(
      random_reverse_complement
    ) &&
      random_reverse_complement >= 0 &&
      random_reverse_complement <= 1,
    "random_lineage_dropout must be between zero and one" = is_scalar_number(
      random_lineage_dropout
    ) &&
      random_lineage_dropout >= 0 &&
      random_lineage_dropout <= 1,
    "append_eod must be TRUE or FALSE" = is_scalar_logical(append_eod),
    "sample_length must be NULL or a positive integer" = is.null(
      sample_length
    ) ||
      is_scalar_integerish(sample_length, min = 1),
    "drop_empty_sequences must be TRUE or FALSE" = is_scalar_logical(
      drop_empty_sequences
    ),
    "filter_nnn must be TRUE or FALSE" = is_scalar_logical(filter_nnn),
    "taxonomy must be NULL, one path, a data frame, or a list" = is.null(
      taxonomy
    ) ||
      is_scalar_string(taxonomy) ||
      is.data.frame(taxonomy) ||
      is.list(taxonomy),
    "prompt_spacer_length must be a non-negative integer" = is_scalar_integerish(
      prompt_spacer_length,
      min = 0
    ),
    "workers must be a positive integer" = is_scalar_integerish(
      workers,
      min = 1
    ),
    "concurrency must be a positive integer" = is_scalar_integerish(
      concurrency,
      min = 1
    ),
    "chunk_size must be a positive integer" = is_scalar_integerish(
      chunk_size,
      min = 1
    ),
    "seed must be a non-negative integer" = is_scalar_integerish(seed, min = 0)
  )

  result <- Evo2PreprocessControl(
    uppercase = uppercase,
    embed_reverse_complement = embed_reverse_complement,
    random_reverse_complement = as.double(random_reverse_complement),
    random_lineage_dropout = as.double(random_lineage_dropout),
    transcribe = transcribe,
    append_eod = append_eod,
    sample_length = as_nullable_integer(sample_length),
    drop_empty_sequences = drop_empty_sequences,
    filter_nnn = filter_nnn,
    taxonomy = taxonomy,
    prompt_spacer_length = as.integer(prompt_spacer_length),
    workers = as.integer(workers),
    concurrency = as.integer(concurrency),
    chunk_size = as.integer(chunk_size),
    seed = as.integer(seed)
  )
  set_object_value_origins(
    result,
    argument_origin_map(S7::props(result), invocation)
  )
}

evo2_lora_target_modules <- list(
  hyena = c("dense_projection", "dense"),
  attention = c("linear_qkv", "linear_proj"),
  mlp = c("linear_fc1", "linear_fc2")
)

#' Describe Evo 2 LoRA fine-tuning
#'
#' @param rank LoRA rank.
#' @param alpha LoRA scaling factor.
#' @param dropout LoRA dropout probability.
#' @param targets Semantic module groups to adapt.
#' @param fully_trainable Plain module names to train without adapters.
#'
#' @return An S7 `Evo2LoRA`.
#' @export
evo2_lora <- function(
  rank = 16L,
  alpha = 32,
  dropout = 0.1,
  targets = c("hyena", "attention", "mlp"),
  fully_trainable = character()
) {
  invocation <- match.call(expand.dots = FALSE)
  stopifnot(
    "rank must be a positive integer" = is_scalar_integerish(rank, min = 1),
    "alpha must be positive" = is_scalar_number(alpha) && alpha > 0,
    "dropout must be between zero and one" = is_scalar_number(dropout) &&
      dropout >= 0 &&
      dropout < 1,
    "targets must contain supported semantic target names" = is.character(
      targets
    ) &&
      length(targets) > 0L &&
      !anyNA(targets) &&
      !anyDuplicated(targets) &&
      all(targets %in% names(evo2_lora_target_modules)),
    "fully_trainable must contain plain module names" = is.character(
      fully_trainable
    ) &&
      !anyNA(fully_trainable) &&
      !anyDuplicated(fully_trainable) &&
      all(grepl("^[A-Za-z][A-Za-z0-9_]*$", fully_trainable))
  )
  target_modules <- unlist(
    evo2_lora_target_modules[targets],
    use.names = FALSE
  )
  stopifnot(
    "targets and fully_trainable must not overlap" = !any(
      fully_trainable %in% target_modules
    ),
    "tied word_embeddings and output_layer must be fully trainable together" = ("word_embeddings" %in%
      fully_trainable) ==
      ("output_layer" %in% fully_trainable)
  )

  result <- Evo2LoRA(
    kind = "lora",
    rank = as.integer(rank),
    alpha = as.double(alpha),
    dropout = as.double(dropout),
    targets = targets,
    fully_trainable = fully_trainable
  )
  set_object_value_origins(
    result,
    argument_origin_map(
      S7::props(result),
      invocation,
      argument_map = c(
        rank = "rank",
        alpha = "alpha",
        dropout = "dropout",
        targets = "targets",
        fully_trainable = "fully_trainable"
      ),
      adapter_defaults = "kind"
    )
  )
}

#' Describe full Evo 2 fine-tuning
#'
#' @return An S7 `Evo2FullFineTune`.
#' @export
evo2_full <- function() {
  result <- Evo2FullFineTune(kind = "full")
  set_object_value_origins(
    result,
    list(kind = "adapter_default")
  )
}

fit_extra_fields <- c(
  "sequence_parallel",
  "no_fp8_wgrad",
  "no_fp8_param_gather",
  "average_in_collective",
  "eod_pad_in_loss_mask",
  "cross_entropy_loss_fusion",
  "fp32_residual_connection",
  "seq_len_interpolation_factor",
  "adam_beta1",
  "adam_beta2",
  "adam_epsilon",
  "gc_interval",
  "enable_preemption",
  "no_renormalize_loss"
)

evo2_precision_recipe <- function(precision, mixed_precision_recipe = NULL) {
  if (!is.null(mixed_precision_recipe)) {
    return(mixed_precision_recipe)
  }
  switch(
    precision,
    auto = NULL,
    bf16 = "bf16_mixed",
    `fp8-current` = "bf16_with_fp8_current_scaling_mixed"
  )
}

#' Construct typed Evo 2 fitting controls
#'
#' @param sequence_length Training sequence length.
#' @param global_batch_size,micro_batch_size Global and per-rank batch sizes.
#' @param learning_rate,minimum_learning_rate Initial and minimum learning
#'   rates.
#' @param warmup_steps,decay_steps,constant_steps Learning-rate schedule
#'   lengths.
#' @param weight_decay Optimizer weight decay.
#' @param eval_interval,eval_iters Evaluation cadence and iteration count.
#' @param log_interval Logging cadence.
#' @param tensor_parallel_size,pipeline_parallel_size,context_parallel_size
#'   Model-parallel rank counts.
#' @param precision Semantic training precision.
#' @param mixed_precision_recipe Optional exact upstream precision recipe.
#' @param precision_aware_optimizer Whether to use the precision-aware
#'   optimizer.
#' @param bf16_main_gradients Whether the precision-aware optimizer keeps main
#'   gradients in BF16.
#' @param gradient_reduce_fp32 Whether to reduce gradients in FP32.
#' @param activation_checkpointing Activation checkpointing policy.
#' @param activation_checkpoint_layers Optional checkpointing layer interval.
#' @param overlap_parameter_gather,overlap_gradient_reduce Whether to overlap
#'   distributed parameter and gradient communication.
#' @param subquadratic_ops Whether to enable subquadratic operators.
#' @param clip_gradient Gradient clipping threshold.
#' @param hidden_dropout,attention_dropout Training dropout probabilities.
#' @param checkpoint_async Whether to write checkpoints asynchronously.
#' @param keep_checkpoints Number of checkpoints to retain, or `-1` for all.
#' @param workers Data-loading worker count.
#' @param seed Training seed.
#' @param dataset_seed Optional dataset seed. `NULL` uses `seed`.
#' @param extra Named allowlisted advanced upstream settings.
#'
#' @return An S7 `Evo2FitControl`.
#' @export
evo2_fit_control <- function(
  sequence_length = 8192L,
  global_batch_size = 8L,
  micro_batch_size = 1L,
  learning_rate = 3e-4,
  minimum_learning_rate = 3e-5,
  warmup_steps = 2500L,
  decay_steps = NULL,
  constant_steps = 2500L,
  weight_decay = 0.01,
  eval_interval = 100L,
  eval_iters = 32L,
  log_interval = 10L,
  tensor_parallel_size = 1L,
  pipeline_parallel_size = 1L,
  context_parallel_size = 1L,
  precision = c("auto", "bf16", "fp8-current"),
  mixed_precision_recipe = NULL,
  precision_aware_optimizer = FALSE,
  bf16_main_gradients = FALSE,
  gradient_reduce_fp32 = FALSE,
  activation_checkpointing = c("auto", "full", "selective", "none"),
  activation_checkpoint_layers = NULL,
  overlap_parameter_gather = FALSE,
  overlap_gradient_reduce = FALSE,
  subquadratic_ops = FALSE,
  clip_gradient = 1,
  hidden_dropout = 0,
  attention_dropout = 0,
  checkpoint_async = FALSE,
  keep_checkpoints = 5L,
  workers = 8L,
  seed = 1234L,
  dataset_seed = NULL,
  extra = list()
) {
  invocation <- match.call(expand.dots = FALSE)
  precision <- match.arg(precision)
  activation_checkpointing <- match.arg(activation_checkpointing)
  stopifnot(
    "sequence_length must be a positive integer" = is_scalar_integerish(
      sequence_length,
      min = 1
    ),
    "global_batch_size must be a positive integer" = is_scalar_integerish(
      global_batch_size,
      min = 1
    ),
    "micro_batch_size must be a positive integer" = is_scalar_integerish(
      micro_batch_size,
      min = 1
    ),
    "global_batch_size must be divisible by micro_batch_size" = global_batch_size %%
      micro_batch_size ==
      0,
    "learning_rate must be positive" = is_scalar_number(learning_rate) &&
      learning_rate > 0,
    "minimum_learning_rate must be non-negative and no greater than learning_rate" = is_scalar_number(
      minimum_learning_rate
    ) &&
      minimum_learning_rate >= 0 &&
      minimum_learning_rate <= learning_rate,
    "warmup_steps must be a non-negative integer" = is_scalar_integerish(
      warmup_steps,
      min = 0
    ),
    "decay_steps must be NULL or a positive integer" = is.null(decay_steps) ||
      is_scalar_integerish(decay_steps, min = 1),
    "constant_steps must be a non-negative integer" = is_scalar_integerish(
      constant_steps,
      min = 0
    ),
    "weight_decay must be non-negative" = is_scalar_number(weight_decay) &&
      weight_decay >= 0,
    "eval_interval must be a positive integer" = is_scalar_integerish(
      eval_interval,
      min = 1
    ),
    "eval_iters must be a positive integer" = is_scalar_integerish(
      eval_iters,
      min = 1
    ),
    "log_interval must be a positive integer" = is_scalar_integerish(
      log_interval,
      min = 1
    ),
    "tensor_parallel_size must be a positive integer" = is_scalar_integerish(
      tensor_parallel_size,
      min = 1
    ),
    "pipeline_parallel_size must be a positive integer" = is_scalar_integerish(
      pipeline_parallel_size,
      min = 1
    ),
    "context_parallel_size must be a positive integer" = is_scalar_integerish(
      context_parallel_size,
      min = 1
    ),
    "mixed_precision_recipe must be NULL or one non-empty string" = is.null(
      mixed_precision_recipe
    ) ||
      is_scalar_string(mixed_precision_recipe),
    "FP8 delayed scaling is not working in the pinned Evo 2 recipe" = !identical(
      mixed_precision_recipe,
      "bf16_with_fp8_delayed_scaling_mixed"
    ),
    "precision_aware_optimizer must be TRUE or FALSE" = is_scalar_logical(
      precision_aware_optimizer
    ),
    "bf16_main_gradients must be TRUE or FALSE" = is_scalar_logical(
      bf16_main_gradients
    ),
    "bf16_main_gradients requires precision_aware_optimizer" = !bf16_main_gradients ||
      precision_aware_optimizer,
    "gradient_reduce_fp32 must be TRUE or FALSE" = is_scalar_logical(
      gradient_reduce_fp32
    ),
    "activation_checkpoint_layers must be NULL or a positive integer" = is.null(
      activation_checkpoint_layers
    ) ||
      is_scalar_integerish(activation_checkpoint_layers, min = 1),
    "activation_checkpoint_layers cannot be used when checkpointing is none" = activation_checkpointing !=
      "none" ||
      is.null(activation_checkpoint_layers),
    "overlap_parameter_gather must be TRUE or FALSE" = is_scalar_logical(
      overlap_parameter_gather
    ),
    "overlap_gradient_reduce must be TRUE or FALSE" = is_scalar_logical(
      overlap_gradient_reduce
    ),
    "subquadratic_ops must be TRUE or FALSE" = is_scalar_logical(
      subquadratic_ops
    ),
    "clip_gradient must be non-negative" = is_scalar_number(clip_gradient) &&
      clip_gradient >= 0,
    "hidden_dropout must be between zero and one" = is_scalar_number(
      hidden_dropout
    ) &&
      hidden_dropout >= 0 &&
      hidden_dropout < 1,
    "attention_dropout must be between zero and one" = is_scalar_number(
      attention_dropout
    ) &&
      attention_dropout >= 0 &&
      attention_dropout < 1,
    "checkpoint_async must be TRUE or FALSE" = is_scalar_logical(
      checkpoint_async
    ),
    "keep_checkpoints must be -1 or a positive integer" = is_scalar_integerish(
      keep_checkpoints
    ) &&
      (keep_checkpoints == -1 || keep_checkpoints >= 1),
    "workers must be a positive integer" = is_scalar_integerish(
      workers,
      min = 1
    ),
    "seed must be a non-negative integer" = is_scalar_integerish(seed, min = 0),
    "dataset_seed must be NULL or a non-negative integer" = is.null(
      dataset_seed
    ) ||
      is_scalar_integerish(dataset_seed, min = 0)
  )
  extra <- validate_control_extra(extra, fit_extra_fields)

  result <- Evo2FitControl(
    sequence_length = as.integer(sequence_length),
    global_batch_size = as.integer(global_batch_size),
    micro_batch_size = as.integer(micro_batch_size),
    learning_rate = as.double(learning_rate),
    minimum_learning_rate = as.double(minimum_learning_rate),
    warmup_steps = as.integer(warmup_steps),
    decay_steps = as_nullable_integer(decay_steps),
    constant_steps = as.integer(constant_steps),
    weight_decay = as.double(weight_decay),
    eval_interval = as.integer(eval_interval),
    eval_iters = as.integer(eval_iters),
    log_interval = as.integer(log_interval),
    tensor_parallel_size = as.integer(tensor_parallel_size),
    pipeline_parallel_size = as.integer(pipeline_parallel_size),
    context_parallel_size = as.integer(context_parallel_size),
    precision = precision,
    mixed_precision_recipe = mixed_precision_recipe,
    precision_aware_optimizer = precision_aware_optimizer,
    bf16_main_gradients = bf16_main_gradients,
    gradient_reduce_fp32 = gradient_reduce_fp32,
    activation_checkpointing = activation_checkpointing,
    activation_checkpoint_layers = as_nullable_integer(
      activation_checkpoint_layers
    ),
    overlap_parameter_gather = overlap_parameter_gather,
    overlap_gradient_reduce = overlap_gradient_reduce,
    subquadratic_ops = subquadratic_ops,
    clip_gradient = as.double(clip_gradient),
    hidden_dropout = as.double(hidden_dropout),
    attention_dropout = as.double(attention_dropout),
    checkpoint_async = checkpoint_async,
    keep_checkpoints = as.integer(keep_checkpoints),
    workers = as.integer(workers),
    seed = as.integer(seed),
    dataset_seed = as_nullable_integer(dataset_seed),
    extra = extra
  )
  set_object_value_origins(
    result,
    argument_origin_map(S7::props(result), invocation)
  )
}
