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

validate_control_values <- function(values, check, requirement, ...) {
  valid <- vapply(values, check, logical(1), ...)
  if (!all(valid)) {
    stop(names(values)[which(!valid)[1L]], " must be ", requirement)
  }
  invisible(NULL)
}

is_nullable_control_value <- function(x, predicate, ...) {
  is.null(x) || predicate(x, ...)
}

construct_control <- function(
  constructor,
  values,
  integer = character(),
  double = character(),
  nullable_integer = character()
) {
  values[integer] <- lapply(values[integer], as.integer)
  values[double] <- lapply(values[double], as.double)
  values[nullable_integer] <- lapply(
    values[nullable_integer],
    as_nullable_integer
  )
  do.call(constructor, values)
}

inference_extra_fields <- c(
  "no_sequence_parallel",
  "min_length"
)

#' Construct typed Evo 2 inference controls
#'
#' `evo2_inference_control()` describes model parallelism, numerical precision,
#' and recipe optimizations shared by generation, scoring, profiles, and
#' embeddings. Task-specific sampling, pooling, strand, and batch arguments
#' stay on the corresponding inference function.
#'
#' @param tensor_parallel_size,context_parallel_size
#'   Model-parallel rank counts. Their product cannot exceed the GPUs allocated
#'   by [bionemo_compute()]. Generation requires the product to equal the
#'   allocated GPU count. Positional profiles and embeddings require context
#'   parallelism of one. Evo 2 inference uses one pipeline stage.
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
  context_parallel_size = 1L,
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
  precision <- match.arg(precision)
  vortex_style_fp8 <- match.arg(vortex_style_fp8)
  cuda_graphs <- match.arg(cuda_graphs)
  validate_control_values(
    mget(c(
      "tensor_parallel_size",
      "context_parallel_size",
      "max_batch_size",
      "dynamic_block_size"
    )),
    is_scalar_integerish,
    "a positive integer",
    min = 1
  )
  validate_control_values(
    mget(c("max_sequence_length", "dynamic_max_tokens")),
    is_nullable_control_value,
    "NULL or a positive integer",
    predicate = is_scalar_integerish,
    min = 1
  )
  validate_control_values(
    list(mixed_precision_recipe = mixed_precision_recipe),
    is_nullable_control_value,
    "NULL or one non-empty string",
    predicate = is_scalar_string
  )
  validate_control_values(
    mget(c("subquadratic_ops", "chunked_prefill")),
    is_scalar_logical,
    "TRUE or FALSE"
  )
  if (subquadratic_ops && cuda_graphs == "local") {
    stop("subquadratic_ops and local CUDA graphs are incompatible")
  }
  extra <- validate_control_extra(extra, inference_extra_fields)

  construct_control(
    Evo2InferenceControl,
    mget(names(formals(evo2_inference_control))),
    integer = c(
      "tensor_parallel_size",
      "context_parallel_size",
      "max_batch_size",
      "dynamic_block_size"
    ),
    nullable_integer = c("max_sequence_length", "dynamic_max_tokens")
  )
}

#' Construct typed Evo 2 preprocessing controls
#'
#' These controls are written to the pinned `preprocess_evo2` configuration by
#' [evo2_preprocess()]. The defaults retain input case, append the tokenizer's
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
  transcribe <- match.arg(transcribe)
  validate_control_values(
    mget(c(
      "uppercase",
      "embed_reverse_complement",
      "append_eod",
      "drop_empty_sequences",
      "filter_nnn"
    )),
    is_scalar_logical,
    "TRUE or FALSE"
  )
  validate_control_values(
    mget(c("random_reverse_complement", "random_lineage_dropout")),
    function(x) is_scalar_number(x) && x >= 0 && x <= 1,
    "between zero and one"
  )
  validate_control_values(
    list(sample_length = sample_length),
    is_nullable_control_value,
    "NULL or a positive integer",
    predicate = is_scalar_integerish,
    min = 1
  )
  if (
    !is.null(taxonomy) &&
      !is_scalar_string(taxonomy) &&
      !is.data.frame(taxonomy) &&
      !is.list(taxonomy)
  ) {
    stop("taxonomy must be NULL, one path, a data frame, or a list")
  }
  validate_control_values(
    mget(c("prompt_spacer_length", "seed")),
    is_scalar_integerish,
    "a non-negative integer",
    min = 0
  )
  validate_control_values(
    mget(c("workers", "concurrency", "chunk_size")),
    is_scalar_integerish,
    "a positive integer",
    min = 1
  )

  construct_control(
    Evo2PreprocessControl,
    mget(names(formals(evo2_preprocess_control))),
    integer = c(
      "prompt_spacer_length",
      "workers",
      "concurrency",
      "chunk_size",
      "seed"
    ),
    double = c("random_reverse_complement", "random_lineage_dropout"),
    nullable_integer = "sample_length"
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

  construct_control(
    Evo2LoRA,
    c(list(kind = "lora"), mget(names(formals(evo2_lora)))),
    integer = "rank",
    double = c("alpha", "dropout")
  )
}

#' Describe full Evo 2 fine-tuning
#'
#' Full fine-tuning updates all trainable model parameters in the base
#' checkpoint instead of adding LoRA adapters. It requires more accelerator
#' memory than [evo2_lora()].
#'
#' @return An S7 `Evo2FullFineTune`.
#'
#' @examples
#' evo2_full()
#' @export
evo2_full <- function() {
  Evo2FullFineTune(kind = "full")
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
  precision <- match.arg(precision)
  activation_checkpointing <- match.arg(activation_checkpointing)
  validate_control_values(
    mget(c(
      "sequence_length",
      "global_batch_size",
      "micro_batch_size",
      "eval_interval",
      "eval_iters",
      "log_interval",
      "tensor_parallel_size",
      "pipeline_parallel_size",
      "context_parallel_size",
      "workers"
    )),
    is_scalar_integerish,
    "a positive integer",
    min = 1
  )
  if (global_batch_size %% micro_batch_size != 0) {
    stop("global_batch_size must be divisible by micro_batch_size")
  }
  validate_control_values(
    list(learning_rate = learning_rate),
    function(x) is_scalar_number(x) && x > 0,
    "positive"
  )
  if (
    !is_scalar_number(minimum_learning_rate) ||
      minimum_learning_rate < 0 ||
      minimum_learning_rate > learning_rate
  ) {
    stop(
      "minimum_learning_rate must be non-negative and no greater than learning_rate"
    )
  }
  validate_control_values(
    mget(c("warmup_steps", "constant_steps", "seed")),
    is_scalar_integerish,
    "a non-negative integer",
    min = 0
  )
  validate_control_values(
    mget(c("decay_steps", "activation_checkpoint_layers")),
    is_nullable_control_value,
    "NULL or a positive integer",
    predicate = is_scalar_integerish,
    min = 1
  )
  validate_control_values(
    mget(c("weight_decay", "clip_gradient")),
    function(x) is_scalar_number(x) && x >= 0,
    "non-negative"
  )
  validate_control_values(
    mget(c("hidden_dropout", "attention_dropout")),
    function(x) is_scalar_number(x) && x >= 0 && x < 1,
    "between zero and one"
  )
  validate_control_values(
    list(mixed_precision_recipe = mixed_precision_recipe),
    is_nullable_control_value,
    "NULL or one non-empty string",
    predicate = is_scalar_string
  )
  if (
    identical(mixed_precision_recipe, "bf16_with_fp8_delayed_scaling_mixed")
  ) {
    stop("FP8 delayed scaling is not working in the pinned Evo 2 recipe")
  }
  validate_control_values(
    mget(c(
      "precision_aware_optimizer",
      "bf16_main_gradients",
      "gradient_reduce_fp32",
      "overlap_parameter_gather",
      "overlap_gradient_reduce",
      "subquadratic_ops",
      "checkpoint_async"
    )),
    is_scalar_logical,
    "TRUE or FALSE"
  )
  if (bf16_main_gradients && !precision_aware_optimizer) {
    stop("bf16_main_gradients requires precision_aware_optimizer")
  }
  if (
    !is.null(activation_checkpoint_layers) &&
      identical(activation_checkpointing, "none")
  ) {
    stop(
      "activation_checkpoint_layers cannot be used when checkpointing is none"
    )
  }
  if (
    !is_scalar_integerish(keep_checkpoints) ||
      keep_checkpoints != -1 && keep_checkpoints < 1
  ) {
    stop("keep_checkpoints must be -1 or a positive integer")
  }
  validate_control_values(
    list(dataset_seed = dataset_seed),
    is_nullable_control_value,
    "NULL or a non-negative integer",
    predicate = is_scalar_integerish,
    min = 0
  )
  extra <- validate_control_extra(extra, fit_extra_fields)

  construct_control(
    Evo2FitControl,
    mget(names(formals(evo2_fit_control))),
    integer = c(
      "sequence_length",
      "global_batch_size",
      "micro_batch_size",
      "warmup_steps",
      "constant_steps",
      "eval_interval",
      "eval_iters",
      "log_interval",
      "tensor_parallel_size",
      "pipeline_parallel_size",
      "context_parallel_size",
      "keep_checkpoints",
      "workers",
      "seed"
    ),
    double = c(
      "learning_rate",
      "minimum_learning_rate",
      "weight_decay",
      "clip_gradient",
      "hidden_dropout",
      "attention_dropout"
    ),
    nullable_integer = c(
      "decay_steps",
      "activation_checkpoint_layers",
      "dataset_seed"
    )
  )
}
