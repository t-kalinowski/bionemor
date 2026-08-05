# Prepare a recommended Evo 2 model

`evo2_model()` prepares or reuses the registry-recommended dense MBridge
checkpoint and returns an Evo 2 model with that checkpoint and `compute`
attached. Inference and fine-tuning functions use that compute
descriptor by default. The function does not install or diagnose the
runtime. Preparation is synchronous.

## Usage

``` r
evo2_model(size = "7b", compute, path = NULL)
```

## Arguments

- size:

  Canonical Evo 2 model name or a known upstream alias.

- compute:

  A compute specification.

- path:

  Optional checkpoint destination or reuse location. Relative paths are
  resolved below the compute workspace. `NULL` uses a revision-qualified
  path below `checkpoints/`.

## Value

An S7 `Evo2Model` with a `BioNeMoCheckpoint` attached.

## Details

With `path = NULL`, the destination below the compute workspace is keyed
by the canonical model name, source revision, and recipe revision. An
explicit `path` changes only that destination or reuse location; it does
not change the registered source. Existing incomplete or mismatched
destinations are rejected without being overwritten.

## See also

[`evo2()`](https://t-kalinowski.github.io/bionemor/reference/evo2.md),
[`evo2_checkpoint()`](https://t-kalinowski.github.io/bionemor/reference/evo2_checkpoint.md),
[`evo2_models()`](https://t-kalinowski.github.io/bionemor/reference/evo2_models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
model <- evo2_model("7b", compute)

# The prepared model remembers where it runs.
evo2_score(model, c(example = "ACGTACGT"))
} # }
```
