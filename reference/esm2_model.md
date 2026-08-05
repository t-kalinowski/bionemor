# Bind an ESM-2 model to compute

`esm2_model()` returns a model ready for embedding on `compute`. With
`path = NULL`, Transformers obtains the package-pinned Hugging Face
checkpoint on first use. Set `path` to a runtime-visible, Transformers-
compatible checkpoint directory whose architecture matches `size` to
avoid that download.

## Usage

``` r
esm2_model(size = "8m", compute, path = NULL)
```

## Arguments

- size:

  A canonical model name or upstream model alias.

- compute:

  A
  [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md)
  descriptor.

- path:

  Optional compatible, runtime-visible local checkpoint directory.

## Value

An S7 `Esm2Model` bound to `compute`.

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(
  recipe = esm2_recipe(),
  workspace = "~/bionemor-esm2"
)
compute <- bionemo_install(compute)
model <- esm2_model("8m", compute)
} # }
```
