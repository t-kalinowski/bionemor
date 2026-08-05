# Generate DNA continuations with Evo 2

All prompts are written to one request and processed while the recipe
keeps the model loaded. Prompt tokens plus `num_tokens` must fit the
model context and any smaller `max_sequence_length` in
[`evo2_inference_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_inference_control.md).

## Usage

``` r
evo2_generate(
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
)
```

## Arguments

- object:

  An Evo 2 model with an explicit checkpoint.

- prompt:

  Prompts as a character vector, data frame, FASTA path, or
  `DNAStringSet`. Character-vector names or an `id` data-frame column
  become output IDs.

- compute:

  A BioNeMo compute descriptor. `NULL` uses the descriptor attached by
  [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
  or a previous fine-tuning run.

- num_tokens:

  Maximum number of new tokens per prompt.

- temperature, top_k, top_p:

  Sampling temperature and top-k or nucleus filtering. At most one of
  `top_k` and `top_p` may be positive. Set `top_k = 0` when using top-p
  sampling.

- seed:

  Optional positive random seed. The pinned recipe maps zero to its
  default seed, so zero is rejected rather than recorded as if it were
  used.

- return_probabilities:

  Whether to retain generated-token log probabilities and probabilities
  in list columns.

- normalize:

  Sequence normalization mode. `"evo2"` accepts a leading
  [`evo2_phylo_tag()`](https://t-kalinowski.github.io/bionemor/reference/evo2_phylo_tag.md),
  validates the DNA portion, and uppercases the complete prompt,
  including the tag. `"dna"` accepts IUPAC DNA only, and `"none"` sends
  text unchanged.

- validate:

  Mechanical output validation level. `"basic"` records warnings for
  non-ACGT symbols, extreme GC fraction, long homopolymers, low
  complexity, and duplicate completions. `"strict"` also rejects
  non-ACGT output. `"none"` skips these checks.

- control:

  Controls from
  [`evo2_inference_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_inference_control.md).

- output:

  Optional directory for a copy of portable outputs.

- name:

  Optional run name.

- async:

  Whether to return before completion.

## Value

With `async = FALSE`, an `evo2_generation` data frame with one row per
prompt and these 17 columns:

- `id`, `input_id`, and `sample` identify the request and sample.

- `prompt`, `completion`, and `sequence` contain the input, generated
  suffix, and their concatenation.

- `finish_reason` records why generation stopped.

- `prompt_tokens`, `generated_tokens`, and `total_tokens` report token
  counts.

- `log_probabilities` and `probabilities` are list columns containing
  per-generated-token values when `return_probabilities = TRUE`, and
  `NULL` otherwise.

- `generated_bases`, `gc_fraction`, `ambiguous_fraction`, and
  `longest_homopolymer` contain mechanical sequence summaries.

- `validation_warnings` is a list column of zero or more mechanical
  validation messages.

With `async = TRUE`, a `BioNeMoJob`.

## References

[BioNeMo Recipes autoregressive
generation](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#autoregressive-generation-infer_evo2)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
compute <- bionemo_install(compute)
model <- evo2_model("7b", compute)

generated <- evo2_generate(
  model,
  c(reference = "ACGTACGT"),
  num_tokens = 32L,
  seed = 17L
)
generated[c("input_id", "prompt", "completion", "finish_reason")]
} # }
```
