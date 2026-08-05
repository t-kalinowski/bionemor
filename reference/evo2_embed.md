# Extract Evo 2 sequence embeddings

Extract Evo 2 sequence embeddings

## Usage

``` r
evo2_embed(
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

- layer:

  `"last"` or a one-based decoder layer. R layer `1` maps to upstream
  layer `0`; `"last"` maps to upstream `-1`.

- pool:

  Pooling across non-padding sequence positions. `"mean"` and `"max"`
  aggregate values; `"first"` and `"last"` select an endpoint. With
  `pool = "none"`, `output` is required and `strand = "both"` is not
  supported.

- strand:

  Score the supplied sequence, its reverse complement, or both.

- batch_size:

  Prediction micro-batch size.

- normalize:

  Sequence normalization mode. With `normalize = "none"`, reverse-strand
  operations require uppercase IUPAC DNA.

- control:

  Controls from
  [`evo2_inference_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_inference_control.md).

- output:

  Optional output file. Required when `pool = "none"`.

- name:

  Optional run name.

- async:

  Whether to return before completion.

## Value

With `async = FALSE`, an `evo2_embeddings` numeric matrix for pooled
output or a `BioNeMoArtifact` for unpooled output. With `async = TRUE`,
a `BioNeMoJob`.

## Details

Embeddings currently require `context_parallel_size = 1`. With a pooling
rule, token embeddings are pooled independently for each strand. With
`strand = "both"`, the forward and reverse pooled vectors are then
averaged. The returned matrix preserves input IDs as row names and names
its columns `dim_1`, `dim_2`, and so on.

With `pool = "none"`, the result is a Parquet artifact with columns `id`
(string), `position` (int64), `embedding` (list of doubles), and
`strand` (string). Unpooled output requires `output` and does not
support `strand = "both"`.

## References

[BioNeMo Recipes embedding
extraction](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#batch-sequence-scoring-predict_evo2)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
compute <- bionemo_install(compute)
model <- evo2_model("7b", compute)

embeddings <- evo2_embed(
  model,
  c(reference = "ACGTACGT", variant = "ACGTTCGT"),
  pool = "mean",
  strand = "both"
)
dim(embeddings)
embeddings[, 1:4, drop = FALSE]
} # }
```
