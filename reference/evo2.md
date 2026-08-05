# Describe an Evo 2 model

`evo2()` creates a model descriptor without downloading or loading
weights. By default it is compute-independent, so operations require an
explicit compute descriptor. Supply `compute` to bind a custom
checkpoint, or use
[`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
for the direct, synchronous path to a model backed by the recommended
checkpoint and bound to compute.

## Usage

``` r
evo2(
  size = "7b",
  checkpoint = NULL,
  revision = "recommended",
  config = list(),
  compute = NULL
)
```

## Arguments

- size:

  Canonical model name from
  [`evo2_models()`](https://t-kalinowski.github.io/bionemor/reference/evo2_models.md)
  or a known upstream alias such as `"evo2_7b"`.

- checkpoint:

  `NULL`, an existing MBridge checkpoint path, or a checkpoint returned
  by
  [`evo2_checkpoint()`](https://t-kalinowski.github.io/bionemor/reference/evo2_checkpoint.md).
  A path is inspected and registered immediately.

- revision:

  Exact source checkpoint revision. `"recommended"` uses the package
  model registry.

- config:

  Named model-level settings. Supported names are `tokenizer`,
  `tokenizer_revision`, and `mixed_precision_recipe`.

- compute:

  Optional
  [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md)
  descriptor to attach to the model. `NULL` leaves it unbound.

## Value

An S7 `Evo2Model`.

## Details

A model records the architecture, context length, source revision, model
settings, and optional checkpoint. A
[`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md)
descriptor records where and how work runs. Keeping `evo2()` unbound
lets an existing checkpoint be used with another compatible runtime;
execution still checks checkpoint visibility, recipe revision, and GPU
compatibility.

## Examples

``` r
model <- evo2("7b")
model
#> <Evo 2 model>
#> Size:       7B
#> Context:    1,048,576 nt
#> Checkpoint: not attached
#> Recipe:     BioNeMo Evo 2 2.4 @ e8e7f597
#> Ready:      no
model@context_length
#> [1] 1048576
```
