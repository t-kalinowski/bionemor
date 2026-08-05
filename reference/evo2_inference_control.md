# Construct typed Evo 2 inference controls

`evo2_inference_control()` describes model parallelism, numerical
precision, and recipe optimizations shared by generation, scoring,
profiles, and embeddings. Task-specific sampling, pooling, strand, and
batch arguments stay on the corresponding inference function.

## Usage

``` r
evo2_inference_control(
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
)
```

## Arguments

- tensor_parallel_size, context_parallel_size:

  Model-parallel rank counts. Their product cannot exceed the GPUs
  allocated by
  [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md).
  Generation requires the product to equal the allocated GPU count.
  Positional profiles and embeddings require context parallelism of one.
  Evo 2 inference uses one pipeline stage.

- max_sequence_length:

  Optional generation context limit. `NULL` lets the recipe and model
  determine the limit.

- max_batch_size:

  Maximum prompt records admitted to one generation call and used to
  size upstream buffers.

- precision:

  Semantic inference precision. `"auto"` follows the checkpoint and
  model registry; `"bf16"` and `"fp8"` request an explicit policy.

- mixed_precision_recipe:

  Optional exact upstream precision recipe. When supplied, it takes
  precedence over the semantic `precision` mapping.

- vortex_style_fp8:

  Whether to use the Vortex-compatible FP8 path. `"auto"` follows
  checkpoint provenance and the model registry.

- cuda_graphs:

  CUDA graph implementation. `"auto"` selects local graphs unless
  `subquadratic_ops = TRUE`.

- subquadratic_ops:

  Whether to enable the fused Hyena kernels used by batch prediction and
  generation prefill. They may incur a one-time compilation cost.

- chunked_prefill:

  Whether generation should prefill long prompts in chunks.

- dynamic_max_tokens:

  Optional dynamic-batching token limit.

- dynamic_block_size:

  Dynamic-batching block size.

- extra:

  Named advanced prediction settings. The pinned prediction entry point
  applies `no_sequence_parallel` and `min_length`. Generation rejects
  non-empty `extra`; settings accepted by an upstream parser but not
  applied by the pinned entry point are rejected before launch.

## Value

An S7 `Evo2InferenceControl`.

## References

[Pinned BioNeMo Recipes Evo 2 inference and
prediction](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#autoregressive-generation-infer_evo2)

## Examples

``` r
control <- evo2_inference_control(
  tensor_parallel_size = 2L,
  precision = "bf16",
  subquadratic_ops = TRUE
)
control@tensor_parallel_size
#> [1] 2
control@precision
#> [1] "bf16"
```
