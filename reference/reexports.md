# Model compatibility generics

[`fit()`](https://generics.r-lib.org/reference/fit.html) delegates Evo 2
models to
[`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md).
[`predict()`](https://rdrr.io/r/stats/predict.html) provides
cross-family inference. For Evo 2, it delegates to
[`evo2_score()`](https://t-kalinowski.github.io/bionemor/reference/evo2_score.md),
[`evo2_generate()`](https://t-kalinowski.github.io/bionemor/reference/evo2_generate.md),
or
[`evo2_embed()`](https://t-kalinowski.github.io/bionemor/reference/evo2_embed.md)
according to `type`. For ESM-2, it delegates to
[`esm2_embed()`](https://t-kalinowski.github.io/bionemor/reference/esm2_embed.md);
only `type = "embedding"` is supported.

## Usage

``` r
fit(object, ...)
predict(object, ...)
```

## Arguments

- object:

  An Evo 2 or ESM-2 model.
  [`fit()`](https://generics.r-lib.org/reference/fit.html) supports only
  Evo 2.

- ...:

  Method arguments. Evo 2
  [`fit()`](https://generics.r-lib.org/reference/fit.html) accepts
  `data`, `compute`, `steps`, `control`, `method`, `name`, `output`,
  `timeout`, and `async`.
  [`predict()`](https://rdrr.io/r/stats/predict.html) accepts `newdata`,
  `type`, `compute`, and arguments for the selected task-specific
  function.

## Value

[`fit()`](https://generics.r-lib.org/reference/fit.html) returns an
`Evo2Model` or `BioNeMoJob`.
[`predict()`](https://rdrr.io/r/stats/predict.html) returns the selected
task-specific result.
