# Describe an ESM-2 model

`esm2()` creates an offline model descriptor. By default, inference
obtains the exact checkpoint listed by
[`esm2_models()`](https://t-kalinowski.github.io/bionemor/reference/esm2_models.md)
when it first runs. Supply a runtime-visible, Hugging Face
Transformers-compatible local checkpoint directory whose architecture
matches `size` to use local weights.

## Usage

``` r
esm2(size = "8m", checkpoint = NULL, compute = NULL)
```

## Arguments

- size:

  A canonical model name or upstream model alias.

- checkpoint:

  `NULL` or one compatible, runtime-visible local checkpoint directory.

- compute:

  Optional
  [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md)
  descriptor.

## Value

An S7 `Esm2Model`.

## Examples

``` r
model <- esm2("8m")
model
#> <ESM-2 model>
#> Size:       8M
#> Context:    1,024 residues
#> Embedding:  320 dimensions
#> Source:     nvidia/esm2_t6_8M_UR50D
```
