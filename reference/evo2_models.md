# List the package's pinned Evo 2 model registry

`evo2_models()` reports available model sizes and compatibility
metadata; it does not construct or prepare a model. Use
[`evo2()`](https://t-kalinowski.github.io/bionemor/reference/evo2.md)
for an offline descriptor or
[`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
for a ready model.

## Usage

``` r
evo2_models(compute = NULL, compatible = FALSE)
```

## Arguments

- compute:

  Optional BioNeMo compute descriptor with an advertised GPU count and
  per-GPU compute capability in its cached capability report.

- compatible:

  If `TRUE`, return only models compatible with `compute`.

## Value

A data frame with one row per canonical model and these columns:

- `name` and `model_size` are the R and upstream recipe identifiers.

- `parameters` and `context_length` describe model scale and maximum
  nucleotide context.

- `source`, `source_revision`, and `source_format` identify the
  registered checkpoint input.

- `precision_policy` and `training_precision_policy` record supported
  inference and fine-tuning policies.

- `download_size` is the approximate registered source download size.

- `compatible` and `compatibility_note` report the result of checking
  the supplied compute descriptor. They are `NA` or explanatory when no
  compute descriptor is supplied.

## Details

Compatibility checks the advertised GPU count, compute capability, and
inference precision policy. It does not measure available GPU memory,
disk capacity, or fine-tuning requirements.

## Examples

``` r
models <- evo2_models()
models[c("name", "parameters", "context_length", "download_size")]
#>       name parameters context_length download_size
#> 1  1b-base      1e+09           8192    2710000000
#> 2  7b-base      7e+09           8192   15800000000
#> 3       7b      7e+09        1048576   23428959022
#> 4      20b      2e+10        1048576  110000000000
#> 5 40b-base      4e+10           8192  225000000000
#> 6      40b      4e+10        1048576  225000000000
```
