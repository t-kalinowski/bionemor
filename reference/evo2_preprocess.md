# Preprocess training data for Evo 2 fine-tuning

`evo2_preprocess()` is a training-data preprocessing step. It writes
each dataset partition as FASTA, calls the pinned `preprocess_evo2`
entry point, and returns an `Evo2Dataset` that points to the resulting
indexed files. It does not prepare model weights, fit the model, or run
inference. The model argument supplies the model size and compatible
tokenizer configuration; its checkpoint weights are not read.

## Usage

``` r
evo2_preprocess(
  data,
  model,
  compute = NULL,
  path,
  control = evo2_preprocess_control(),
  overwrite = FALSE,
  async = FALSE
)
```

## Arguments

- data:

  An
  [`evo2_dataset()`](https://t-kalinowski.github.io/bionemor/reference/evo2_dataset.md)
  result or any input accepted by its `train` argument.

- model:

  An Evo 2 model descriptor.

- compute:

  A BioNeMo compute descriptor. `NULL` uses the descriptor attached by
  [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
  or a previous fine-tuning run.

- path:

  Destination path. Relative paths resolve below the compute workspace.
  Container execution requires the destination to remain inside that
  workspace.

- control:

  Preprocessing controls from
  [`evo2_preprocess_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess_control.md).

- overwrite:

  Whether to replace an existing destination.

- async:

  Whether to return a `BioNeMoJob` before preprocessing completes.

## Value

With `async = FALSE`, a prepared `Evo2Dataset`. With `async = TRUE`, a
`BioNeMoJob`;
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
materializes the dataset.

## Details

Most users can pass raw data directly to
[`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md).
[`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md)
performs this step automatically with default controls. Call
`evo2_preprocess()` first when preprocessing must be customized or when
the same indexed data will be reused across fitting runs. Its manifest
records input digests, model size, tokenizer and recipe revisions,
preprocessing controls, and output digests. Before training,
[`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md)
checks that the prepared path and manifest exist and verifies the
recorded model size, tokenizer, tokenizer revision, and recipe revision.

## References

[BioNeMo Recipes Evo 2
preprocessing](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#data-preprocessing-preprocess_evo2)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
compute <- bionemo_install(compute)
model <- evo2_model("1b", compute)
data <- evo2_dataset(c(first = "ACGT", second = "TGCA"))

prepared <- evo2_preprocess(
  data,
  model,
  path = "datasets/example",
  control = evo2_preprocess_control(sample_length = 1024L)
)
} # }
```
