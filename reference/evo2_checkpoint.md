# Prepare an Evo 2 checkpoint

Convert a Savanna or NeMo2 checkpoint to the MBridge format used by the
BioNeMo Evo 2 recipe, or inspect and register an existing MBridge
checkpoint. The model registry supplies the recommended source, its
exact revision, and tokenizer for each supported model size.

## Usage

``` r
evo2_checkpoint(
  model,
  source = "recommended",
  format = c("auto", "savanna", "nemo2", "mbridge"),
  path,
  compute,
  revision = "recommended",
  tokenizer = "recommended",
  precision = "auto",
  overwrite = FALSE,
  trust = FALSE,
  async = FALSE
)
```

## Arguments

- model:

  An Evo 2 model specification.

- source:

  `"recommended"`, an `hf://` or `ngc://` URI, an existing local path,
  or a `BioNeMoCheckpoint`.

- format:

  Source checkpoint format. `"auto"` uses registry and URI metadata.

- path:

  Destination path. Relative paths resolve below the compute workspace.
  Container and Slurm execution require the destination to remain inside
  that workspace.

- compute:

  A compute specification.

- revision:

  Exact source revision.

- tokenizer:

  Tokenizer path or `"recommended"`.

- precision:

  Conversion precision recipe.

- overwrite:

  Whether to replace an existing destination.

- trust:

  Whether to allow an unknown local or remote pickle-based checkpoint.
  The exact source and revision in the model registry are trusted.

- async:

  Whether to return a running job.

## Value

A `BioNeMoCheckpoint`, or a `BioNeMoJob` when `async = TRUE`.

## Details

`hf://` sources are treated as Savanna checkpoints and `ngc://` sources
as NeMo2 checkpoints unless `format` is explicit. A local source without
a bionemor manifest also requires an explicit format. Existing MBridge
checkpoints are inspected and registered without conversion. Relative
source and destination paths are resolved below the compute workspace.

Checkpoint preparation records source, recipe, tokenizer, precision,
trust, and inspection metadata in a manifest beside the completed
checkpoint. A later call reuses the destination only when that manifest
matches the request. Unknown pickle-based sources require
`trust = TRUE`; exact sources and revisions from the package registry
are trusted automatically.

Savanna conversion uses the pinned recipe's Transformer Engine key
mapping. Convert no-TE checkpoints explicitly upstream, then register
the resulting MBridge checkpoint.

## References

[Pinned BioNeMo Recipes Evo 2 checkpoint
documentation](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#fine-tuning-from-an-existing-checkpoint)

## See also

[`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md),
[`checkpoint_path()`](https://t-kalinowski.github.io/bionemor/reference/checkpoint_metadata.md),
[`checkpoint_manifest()`](https://t-kalinowski.github.io/bionemor/reference/checkpoint_metadata.md)

## Examples

``` r
model <- evo2("7b")
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
checkpoint <- evo2_checkpoint(
  model,
  source = "recommended",
  path = "checkpoints/evo2-7b",
  compute = compute
)
} # }
```
