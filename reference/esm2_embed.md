# Extract pooled ESM-2 protein embeddings

`esm2_embed()` runs the ESM-2 model through the package-pinned native
Transformers and Transformer Engine runtime. It returns one last-token,
L2-normalized embedding row per input protein. Model weights are
downloaded from the model's pinned Hugging Face revision on first use
and cached below the compute workspace.

## Usage

``` r
esm2_embed(
  object,
  newdata,
  compute = NULL,
  output = NULL,
  name = NULL,
  async = FALSE
)
```

## Arguments

- object:

  An ESM-2 model descriptor from
  [`esm2()`](https://t-kalinowski.github.io/bionemor/reference/esm2.md)
  or
  [`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md).

- newdata:

  A character vector of protein sequences, an `XStringSet`, a data frame
  with a `sequence` column, or a FASTA path. Named inputs retain their
  names as matrix row names.

- compute:

  A BioNeMo compute descriptor using
  [`esm2_recipe()`](https://t-kalinowski.github.io/bionemor/reference/esm2_recipe.md).
  `NULL` uses the compute target attached by
  [`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md).

- output:

  Optional path for the portable JSONL result. Container outputs must be
  inside the compute workspace.

- name:

  Optional run name.

- async:

  Whether to return a `BioNeMoJob` before completion.

## Value

A numeric matrix with class `esm2_embeddings`, or a `BioNeMoJob` when
`async = TRUE`. The matrix keeps ordinary matrix behavior. Its
`provenance` attribute records the model, source and recipe revisions,
pooling method, and path where the job was saved.

## Details

Use the embedding rows for sequence similarity, clustering, or as
features in downstream R models. They are model representations, not
measurements of protein function. ESM-2 currently requires `gpus = 1`.
You may supply multiple proteins; the runtime processes them one at a
time on the selected GPU to preserve their bidirectional attention
boundaries.

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(
  recipe = esm2_recipe(),
  engine = "container",
  workspace = "~/bionemor-workspace"
)
compute <- bionemo_install(compute)
model <- esm2_model("8m", compute)
esm2_embed(model, c(reference = "MKT", variant = "MNT"))
} # }
```
