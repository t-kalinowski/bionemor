validate_control_extra <- function(extra, allowed) {
  if (
    !is.list(extra) ||
      length(extra) != 0L &&
        (is.null(names(extra)) ||
          !all(nzchar(names(extra))) ||
          anyDuplicated(names(extra)))
  ) {
    stop("extra must be a named list")
  }
  if (!all(names(extra) %in% allowed)) {
    stop("extra contains an unsupported setting")
  }
  if (
    !all(vapply(
      extra,
      function(value) {
        is.atomic(value) && length(value) == 1L && !is.na(value)
      },
      logical(1)
    ))
  ) {
    stop("extra values must be non-missing scalar values")
  }
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
#' `evo2_inference_control()` describes model parallelism, numerical precision,
#' and recipe optimizations shared by generation, scoring, profiles, and
#' embeddings. Task-specific sampling, pooling, strand, and batch arguments
#' stay on the corresponding inference function.
#'
#' @param tensor_parallel_size,pipeline_parallel_size,context_parallel_size
#'   Model-parallel rank counts. Their product cannot exceed the GPUs allocated
#'   by [bionemo_compute()]. Generation requires the product to equal the
#'   allocated GPU count. Pipeline parallelism must be one. Positional profiles
#'   and embeddings also require context parallelism of one.
#' @param micro_batch_size Reserved for upstream support and must remain one.
#'   Use the task-specific `batch_size` argument for prediction and
#'   `max_batch_size` for generation.
#' @param max_sequence_length Optional generation context limit. `NULL` lets
#'   the recipe and model determine the limit.
#' @param max_batch_size Maximum prompt records admitted to one generation call
#'   and used to size upstream buffers.
#' @param precision Semantic inference precision. `"auto"` follows the
#'   checkpoint and model registry; `"bf16"` and `"fp8"` request an explicit
#'   policy.
#' @param mixed_precision_recipe Optional exact upstream precision recipe. When
#'   supplied, it takes precedence over the semantic `precision` mapping.
#' @param vortex_style_fp8 Whether to use the Vortex-compatible FP8 path.
#'   `"auto"` follows checkpoint provenance and the model registry.
#' @param cuda_graphs CUDA graph implementation. `"auto"` selects local graphs
#'   unless `subquadratic_ops = TRUE`.
#' @param subquadratic_ops Whether to enable the fused Hyena kernels used by
#'   batch prediction and generation prefill. They may incur a one-time
#'   compilation cost.
#' @param chunked_prefill Whether generation should prefill long prompts in
#'   chunks.
#' @param dynamic_max_tokens Optional dynamic-batching token limit.
#' @param dynamic_block_size Dynamic-batching block size.
#' @param extra Named advanced prediction settings. The pinned prediction
#'   entry point applies `no_sequence_parallel` and `min_length`. Generation
#'   rejects non-empty `extra`; settings accepted by an upstream parser but not
#'   applied by the pinned entry point are rejected before launch.
#'
#' @return An S7 `Evo2InferenceControl`.
#'
#' @examples
#' control <- evo2_inference_control(
#'   tensor_parallel_size = 2L,
#'   precision = "bf16",
#'   subquadratic_ops = TRUE
#' )
#' control@tensor_parallel_size
#' control@precision
#'
#' @references
#' [Pinned BioNeMo Recipes Evo 2 inference and prediction](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#autoregressive-generation-infer_evo2)
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
#' These controls are written to the pinned `preprocess_evo2` configuration by
#' [evo2_prepare()]. The defaults retain input case, append the tokenizer's
#' end-of-document token, and keep each input sequence once.
#'
#' @param uppercase Whether to convert sequences to uppercase during
#'   preprocessing.
#' @param embed_reverse_complement Whether to include each sequence's reverse
#'   complement in the indexed data.
#' @param random_reverse_complement Probability of applying a random reverse
#'   complement to a training record.
#' @param random_lineage_dropout Probability of dropping taxonomy lineage
#'   fields when taxonomy prompts are used.
#' @param transcribe Sequence conversion: `"none"`, DNA to RNA with
#'   `"transcribe"`, or RNA to DNA with `"back_transcribe"`.
#' @param append_eod Whether to append an end-of-document token.
#' @param sample_length Optional fixed tokenized sample length. Use this to
#'   match `sequence_length` in [evo2_fit_control()] when fixed records are
#'   required.
#' @param drop_empty_sequences Whether to discard empty sequences.
#' @param filter_nnn Whether to discard sequences containing `NNN`.
#' @param taxonomy Optional JSON or YAML path, data frame, or named list. Data
#'   frames require an `id` column. Each key is matched as a substring of the
#'   sequence ID, and the first matching key supplies its lineage. Supported
#'   fields are `domain`, `phylum`, `class`, `order`, `family`, `genus`, and
#'   `species`; `class` is written upstream as `clazz`.
#' @param prompt_spacer_length Character interval between the starts of repeated
#'   taxonomy prompts. It counts each tag plus the intervening sequence bases
#'   before tokenization.
#' @param workers Preprocessing worker processes.
#' @param concurrency Maximum number of preprocessing tasks in flight.
#' @param chunk_size Preprocessing tasks batched per multiprocessing worker
#'   dispatch.
#' @param seed Non-negative preprocessing seed.
#'
#' @return An S7 `Evo2PreprocessControl`.
#'
#' @examples
#' control <- evo2_preprocess_control(
#'   uppercase = TRUE,
#'   sample_length = 1024L,
#'   workers = 4L,
#'   seed = 17L
#' )
#' control@sample_length
#'
#' @references
#' [Pinned BioNeMo Recipes Evo 2 preprocessing](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#data-preprocessing-preprocess_evo2)
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
#' LoRA freezes the dense base model and attaches trainable low-rank matrices
#' to selected linear modules. `targets` names R-level groups rather than
#' prediction outcomes or individual layers:
#'
#' - `"hyena"` expands to `dense_projection` and `dense` in Hyena mixers.
#' - `"attention"` expands to `linear_qkv` and `linear_proj`.
#' - `"mlp"` expands to `linear_fc1` and `linear_fc2`.
#'
#' The default adapts all three groups. A module named in `fully_trainable`
#' remains unfrozen and receives no adapter. Raw upstream wildcard target
#' patterns are intentionally not part of this interface.
#'
#' @param rank LoRA rank `r`, which controls the adapter bottleneck dimension.
#' @param alpha LoRA scaling numerator. The effective scale is `alpha / rank`.
#' @param dropout Dropout probability applied on the LoRA path.
#' @param targets One or more of `"hyena"`, `"attention"`, and `"mlp"`.
#' @param fully_trainable Plain upstream module names to train without
#'   adapters. A name cannot also be selected through `targets`. Evo 2 ties
#'   `word_embeddings` and `output_layer`, so those two names must be supplied
#'   together.
#'
#' @return An S7 `Evo2LoRA`.
#'
#' @examples
#' # Adapt every supported inner projection (the default).
#' evo2_lora(rank = 16L, alpha = 32)
#'
#' # Adapt only the attention and MLP projections.
#' evo2_lora(
#'   rank = 8L,
#'   alpha = 16,
#'   dropout = 0,
#'   targets = c("attention", "mlp")
#' )
#'
#' # Train the tied vocabulary weights directly alongside Hyena adapters.
#' evo2_lora(
#'   targets = "hyena",
#'   fully_trainable = c("word_embeddings", "output_layer")
#' )
#'
#' @references
#' [Pinned BioNeMo Recipes Evo 2 LoRA fine-tuning](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#lora-fine-tuning)
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
  if (any(fully_trainable %in% target_modules)) {
    stop("targets and fully_trainable must not overlap")
  }
  if (
    xor(
      "word_embeddings" %in% fully_trainable,
      "output_layer" %in% fully_trainable
    )
  ) {
    stop(
      "tied word_embeddings and output_layer must be fully trainable together"
    )
  }

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
#' Full fine-tuning updates the supported parameters in the base checkpoint
#' instead of adding LoRA adapters. It requires more accelerator memory than
#' [evo2_lora()].
#'
#' @return An S7 `Evo2FullFineTune`.
#'
#' @examples
#' evo2_full()
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
#' These controls map to the pinned `train_evo2` entry point. The allocated GPU
#' count is divided by tensor, pipeline, and context parallelism to obtain data
#' parallelism. Gradient accumulation is then
#' `global_batch_size / (micro_batch_size * data_parallel_size)` and must be an
#' integer.
#'
#' @param sequence_length Tokens in each training sample. It cannot exceed the
#'   selected model's context length.
#' @param global_batch_size,micro_batch_size Global samples per optimizer step
#'   and samples processed by each data-parallel rank per forward pass.
#' @param learning_rate,minimum_learning_rate Initial and floor learning rates.
#' @param warmup_steps,decay_steps,constant_steps Warm-up, optional decay, and
#'   constant learning-rate schedule lengths passed to the recipe.
#' @param weight_decay Optimizer weight decay.
#' @param eval_interval,eval_iters Optimizer steps between evaluations and
#'   validation iterations per evaluation.
#' @param log_interval Optimizer steps between log records.
#' @param tensor_parallel_size,pipeline_parallel_size,context_parallel_size
#'   Model-parallel rank counts. Their product must divide the allocated GPU
#'   count.
#' @param precision Semantic training precision. `"auto"` and `"bf16"` use
#'   `bf16_mixed`; `"fp8-current"` uses the current-scaling FP8 recipe when the
#'   model registry permits it. The supported 1B fine-tuning path is BF16.
#' @param mixed_precision_recipe Optional exact upstream precision recipe. It
#'   overrides `precision`; bionemor's supported training contract rejects the
#'   delayed-scaling FP8 recipe because it is not working in the pinned source.
#' @param precision_aware_optimizer Whether to use the precision-aware
#'   optimizer.
#' @param bf16_main_gradients Whether the precision-aware optimizer keeps main
#'   gradients in BF16.
#' @param gradient_reduce_fp32 Whether to reduce gradients in FP32.
#' @param activation_checkpointing Activation checkpointing policy. `"full"`
#'   recomputes full layers, `"selective"` uses selective recomputation, and
#'   `"none"` disables it; `"auto"` leaves the recipe default.
#' @param activation_checkpoint_layers Optional number of layers between full
#'   activation checkpoints.
#' @param overlap_parameter_gather,overlap_gradient_reduce Whether to overlap
#'   distributed parameter and gradient communication.
#' @param subquadratic_ops Whether to enable subquadratic operators.
#' @param clip_gradient Gradient clipping threshold.
#' @param hidden_dropout,attention_dropout Training dropout probabilities.
#' @param checkpoint_async Whether the recipe writes checkpoints
#'   asynchronously.
#' @param keep_checkpoints Number of checkpoints to retain, or `-1` for all.
#' @param workers Data-loader worker processes.
#' @param seed Training seed.
#' @param dataset_seed Optional dataset seed. `NULL` uses `seed`.
#' @param extra Named advanced settings passed to the pinned recipe. Supported
#'   names are `sequence_parallel`, `no_fp8_wgrad`, `no_fp8_param_gather`,
#'   `average_in_collective`, `eod_pad_in_loss_mask`,
#'   `cross_entropy_loss_fusion`, `fp32_residual_connection`,
#'   `seq_len_interpolation_factor`, `adam_beta1`, `adam_beta2`,
#'   `adam_epsilon`, `gc_interval`, `enable_preemption`, and
#'   `no_renormalize_loss`.
#'
#' @return An S7 `Evo2FitControl`.
#'
#' @examples
#' control <- evo2_fit_control(
#'   sequence_length = 1024L,
#'   global_batch_size = 8L,
#'   micro_batch_size = 1L,
#'   precision = "bf16",
#'   activation_checkpointing = "full"
#' )
#' control@global_batch_size
#'
#' @references
#' [Pinned BioNeMo Recipes Evo 2 training](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#fine-tuning-from-an-existing-checkpoint)
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
  if (!is_scalar_integerish(sequence_length, min = 1)) {
    stop("sequence_length must be a positive integer")
  }
  if (!is_scalar_integerish(global_batch_size, min = 1)) {
    stop("global_batch_size must be a positive integer")
  }
  if (!is_scalar_integerish(micro_batch_size, min = 1)) {
    stop("micro_batch_size must be a positive integer")
  }
  if (global_batch_size %% micro_batch_size != 0) {
    stop("global_batch_size must be divisible by micro_batch_size")
  }
  if (!is_scalar_number(learning_rate) || learning_rate <= 0) {
    stop("learning_rate must be positive")
  }
  if (
    !is_scalar_number(minimum_learning_rate) ||
      minimum_learning_rate < 0 ||
      minimum_learning_rate > learning_rate
  ) {
    stop(
      "minimum_learning_rate must be non-negative and no greater than learning_rate"
    )
  }
  if (!is_scalar_integerish(warmup_steps, min = 0)) {
    stop("warmup_steps must be a non-negative integer")
  }
  if (!is.null(decay_steps) && !is_scalar_integerish(decay_steps, min = 1)) {
    stop("decay_steps must be NULL or a positive integer")
  }
  if (!is_scalar_integerish(constant_steps, min = 0)) {
    stop("constant_steps must be a non-negative integer")
  }
  if (!is_scalar_number(weight_decay) || weight_decay < 0) {
    stop("weight_decay must be non-negative")
  }
  if (!is_scalar_integerish(eval_interval, min = 1)) {
    stop("eval_interval must be a positive integer")
  }
  if (!is_scalar_integerish(eval_iters, min = 1)) {
    stop("eval_iters must be a positive integer")
  }
  if (!is_scalar_integerish(log_interval, min = 1)) {
    stop("log_interval must be a positive integer")
  }
  if (!is_scalar_integerish(tensor_parallel_size, min = 1)) {
    stop("tensor_parallel_size must be a positive integer")
  }
  if (!is_scalar_integerish(pipeline_parallel_size, min = 1)) {
    stop("pipeline_parallel_size must be a positive integer")
  }
  if (!is_scalar_integerish(context_parallel_size, min = 1)) {
    stop("context_parallel_size must be a positive integer")
  }
  if (
    !is.null(mixed_precision_recipe) &&
      !is_scalar_string(mixed_precision_recipe)
  ) {
    stop("mixed_precision_recipe must be NULL or one non-empty string")
  }
  if (
    identical(mixed_precision_recipe, "bf16_with_fp8_delayed_scaling_mixed")
  ) {
    stop("FP8 delayed scaling is not working in the pinned Evo 2 recipe")
  }
  if (!is_scalar_logical(precision_aware_optimizer)) {
    stop("precision_aware_optimizer must be TRUE or FALSE")
  }
  if (!is_scalar_logical(bf16_main_gradients)) {
    stop("bf16_main_gradients must be TRUE or FALSE")
  }
  if (bf16_main_gradients && !precision_aware_optimizer) {
    stop("bf16_main_gradients requires precision_aware_optimizer")
  }
  if (!is_scalar_logical(gradient_reduce_fp32)) {
    stop("gradient_reduce_fp32 must be TRUE or FALSE")
  }
  if (
    !is.null(activation_checkpoint_layers) &&
      !is_scalar_integerish(activation_checkpoint_layers, min = 1)
  ) {
    stop("activation_checkpoint_layers must be NULL or a positive integer")
  }
  if (
    !is.null(activation_checkpoint_layers) &&
      identical(activation_checkpointing, "none")
  ) {
    stop(
      "activation_checkpoint_layers cannot be used when checkpointing is none"
    )
  }
  if (!is_scalar_logical(overlap_parameter_gather)) {
    stop("overlap_parameter_gather must be TRUE or FALSE")
  }
  if (!is_scalar_logical(overlap_gradient_reduce)) {
    stop("overlap_gradient_reduce must be TRUE or FALSE")
  }
  if (!is_scalar_logical(subquadratic_ops)) {
    stop("subquadratic_ops must be TRUE or FALSE")
  }
  if (!is_scalar_number(clip_gradient) || clip_gradient < 0) {
    stop("clip_gradient must be non-negative")
  }
  if (
    !is_scalar_number(hidden_dropout) ||
      hidden_dropout < 0 ||
      hidden_dropout >= 1
  ) {
    stop("hidden_dropout must be between zero and one")
  }
  if (
    !is_scalar_number(attention_dropout) ||
      attention_dropout < 0 ||
      attention_dropout >= 1
  ) {
    stop("attention_dropout must be between zero and one")
  }
  if (!is_scalar_logical(checkpoint_async)) {
    stop("checkpoint_async must be TRUE or FALSE")
  }
  if (
    !is_scalar_integerish(keep_checkpoints) ||
      keep_checkpoints != -1 && keep_checkpoints < 1
  ) {
    stop("keep_checkpoints must be -1 or a positive integer")
  }
  if (!is_scalar_integerish(workers, min = 1)) {
    stop("workers must be a positive integer")
  }
  if (!is_scalar_integerish(seed, min = 0)) {
    stop("seed must be a non-negative integer")
  }
  if (!is.null(dataset_seed) && !is_scalar_integerish(dataset_seed, min = 0)) {
    stop("dataset_seed must be NULL or a non-negative integer")
  }
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
