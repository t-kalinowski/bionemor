# Score DNA sequences with Evo 2

Scores are sums or means of token log probabilities selected by the
recipe's loss mask. Higher values indicate that the model assigns
greater likelihood to the sequence under the requested reduction.

## Usage

``` r
evo2_score(
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
)
```

## Arguments

- object:

  An Evo 2 model with an explicit checkpoint.

- newdata:

  Sequences or a FASTA path.

- compute:

  A BioNeMo compute descriptor. `NULL` uses the descriptor attached by
  [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
  or a previous fine-tuning run.

- reduction:

  `"mean"` divides each strand's log-probability sum by its scored token
  count; `"sum"` retains the total.

- strand:

  Score the supplied sequence, its reverse complement, or both.

- batch_size:

  Prediction micro-batch size.

- prepend_bos:

  Whether the prediction entry point should prepend the
  beginning-of-sequence token before scoring.

- normalize:

  Sequence normalization mode. With `normalize = "none"`, reverse-strand
  operations require uppercase IUPAC DNA.

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

With `async = FALSE`, an `evo2_scores` data frame with columns `id`,
`sequence_length`, `tokens_scored`, `score`, `forward_score`,
`reverse_score`, `reduction`, and `strand`. With `async = TRUE`, a
`BioNeMoJob`.

## Details

The recipe first computes reduced log probabilities separately for each
requested strand. `forward_score` and `reverse_score` retain those
strand-specific values and are `NA` for strands that were not requested.
`score` equals the available strand score, or the arithmetic mean of
both values when `strand = "both"`. The `reduction` column records
whether each strand was reduced by its token mean or sum.

## References

[BioNeMo Recipes batch sequence
scoring](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#batch-sequence-scoring-predict_evo2)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
compute <- bionemo_install(compute)
model <- evo2_model("7b", compute)

scores <- evo2_score(
  model,
  c(reference = "ACGTACGT", variant = "ACGTTCGT"),
  reduction = "mean",
  strand = "both"
)
scores
} # }
```
