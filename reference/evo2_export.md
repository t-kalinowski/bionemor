# Export an Evo 2 checkpoint

Export a dense MBridge checkpoint to the weights-only Vortex format used
by optimized inference runtimes. When `strip_optimizer = TRUE`, the
recipe first writes an intermediate MBridge checkpoint without optimizer
state and exports that checkpoint. LoRA checkpoints cannot be exported
because they depend on their dense base checkpoint.

## Usage

``` r
evo2_export(
  model,
  path,
  strip_optimizer = TRUE,
  compute,
  overwrite = FALSE,
  async = FALSE
)
```

## Arguments

- model:

  An Evo 2 model bound to a dense MBridge checkpoint.

- path:

  Destination `.pt` path.

- strip_optimizer:

  Whether to create a weights-only MBridge intermediate.

- compute:

  A compute specification.

- overwrite:

  Whether to replace an existing export.

- async:

  Whether to return a running job.

## Value

A Vortex `BioNeMoCheckpoint`, or a `BioNeMoJob` when `async = TRUE`.

## Details

The upstream exporter writes a shared `config.json` beside the `.pt`
file, so each destination directory may contain only one Vortex
checkpoint. The checkpoint inspector selects the Transformer Engine key
mapping and adds `--no-te` for a non-TE MBridge checkpoint. An
unidentified key layout is rejected before export.

## References

[Pinned BioNeMo Recipes Evo 2 Vortex export
documentation](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#exporting-to-vortex-format)

## See also

[`evo2_checkpoint()`](https://t-kalinowski.github.io/bionemor/reference/evo2_checkpoint.md),
[`checkpoint_manifest()`](https://t-kalinowski.github.io/bionemor/reference/checkpoint_metadata.md)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
model <- evo2_model("7b", compute)
vortex <- evo2_export(
  model,
  path = "exports/evo2-7b/model.pt",
  compute = compute
)
} # }
```
