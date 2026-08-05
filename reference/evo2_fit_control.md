# Construct typed Evo 2 fitting controls

These controls map to the pinned `train_evo2` entry point. The allocated
GPU count is divided by tensor, pipeline, and context parallelism to
obtain data parallelism. Gradient accumulation is then
`global_batch_size / (micro_batch_size * data_parallel_size)` and must
be an integer.

## Usage

``` r
evo2_fit_control(
  sequence_length = 8192L,
  global_batch_size = 8L,
  micro_batch_size = 1L,
  learning_rate = 0.0003,
  minimum_learning_rate = 0.00003,
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
)
```

## Arguments

- sequence_length:

  Tokens in each training sample. It cannot exceed the selected model's
  context length.

- global_batch_size, micro_batch_size:

  Global samples per optimizer step and samples processed by each
  data-parallel rank per forward pass.

- learning_rate, minimum_learning_rate:

  Initial and floor learning rates.

- warmup_steps, decay_steps, constant_steps:

  Warm-up, optional decay, and constant learning-rate schedule lengths
  passed to the recipe.

- weight_decay:

  Optimizer weight decay.

- eval_interval, eval_iters:

  Optimizer steps between evaluations and validation iterations per
  evaluation.

- log_interval:

  Optimizer steps between log records.

- tensor_parallel_size, pipeline_parallel_size, context_parallel_size:

  Model-parallel rank counts. Their product must divide the allocated
  GPU count.

- precision:

  Semantic training precision. `"auto"` and `"bf16"` use `bf16_mixed`;
  `"fp8-current"` uses the current-scaling FP8 recipe when the model
  registry permits it. The supported 1B fine-tuning path is BF16.

- mixed_precision_recipe:

  Optional exact upstream precision recipe. It overrides `precision`;
  bionemor's supported training contract rejects the delayed-scaling FP8
  recipe because it is not working in the pinned source.

- precision_aware_optimizer:

  Whether to use the precision-aware optimizer.

- bf16_main_gradients:

  Whether the precision-aware optimizer keeps main gradients in BF16.

- gradient_reduce_fp32:

  Whether to reduce gradients in FP32.

- activation_checkpointing:

  Activation checkpointing policy. `"full"` recomputes full layers,
  `"selective"` uses selective recomputation, and `"none"` disables it;
  `"auto"` leaves the recipe default.

- activation_checkpoint_layers:

  Optional number of layers between full activation checkpoints.

- overlap_parameter_gather, overlap_gradient_reduce:

  Whether to overlap distributed parameter and gradient communication.

- subquadratic_ops:

  Whether to enable subquadratic operators.

- clip_gradient:

  Gradient clipping threshold.

- hidden_dropout, attention_dropout:

  Training dropout probabilities.

- checkpoint_async:

  Whether the recipe writes checkpoints asynchronously.

- keep_checkpoints:

  Number of checkpoints to retain, or `-1` for all.

- workers:

  Data-loader worker processes.

- seed:

  Training seed.

- dataset_seed:

  Optional dataset seed. `NULL` uses `seed`.

- extra:

  Named advanced settings passed to the pinned recipe. Supported names
  are `sequence_parallel`, `no_fp8_wgrad`, `no_fp8_param_gather`,
  `average_in_collective`, `eod_pad_in_loss_mask`,
  `cross_entropy_loss_fusion`, `fp32_residual_connection`,
  `seq_len_interpolation_factor`, `adam_beta1`, `adam_beta2`,
  `adam_epsilon`, `gc_interval`, `enable_preemption`, and
  `no_renormalize_loss`.

## Value

An S7 `Evo2FitControl`.

## References

[Pinned BioNeMo Recipes Evo 2
training](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#fine-tuning-from-an-existing-checkpoint)

## Examples

``` r
control <- evo2_fit_control(
  sequence_length = 1024L,
  global_batch_size = 8L,
  micro_batch_size = 1L,
  precision = "bf16",
  activation_checkpointing = "full"
)
control@global_batch_size
#> [1] 8
```
