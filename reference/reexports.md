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
fit(
  object,
  data,
  compute = NULL,
  steps,
  control = evo2_fit_control(),
  method = evo2_lora(),
  name = NULL,
  output = NULL,
  timeout = Inf,
  async = FALSE,
  ...
)

predict(
  object,
  newdata,
  type = c("score", "generate", "embedding"),
  compute = NULL,
  ...
)
```

## Arguments

- object:

  An Evo 2 or ESM-2 model.
  [`fit()`](https://generics.r-lib.org/reference/fit.html) supports only
  Evo 2.

- data:

  An `Evo2Dataset` or accepted raw sequence input.

- compute:

  A BioNeMo compute descriptor. `NULL` uses the descriptor attached by
  [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md),
  [`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md),
  or a previous fine-tuning run.

- steps:

  Positive training steps.

- control:

  Fine-tuning controls.

- method:

  A LoRA or full fine-tuning descriptor.

- name:

  Optional run name.

- output:

  Optional result path.

- timeout:

  Complete operation timeout in seconds.

- async:

  Whether to return a `BioNeMoJob` before the operation completes.

- newdata:

  DNA sequences or prompts for Evo 2, or protein sequences for ESM-2.

- type:

  Inference operation. Evo 2 supports `"score"`, `"generate"`, and
  `"embedding"`; ESM-2 supports `"embedding"`.

- ...:

  Reserved for future compatibility and must be empty for
  [`fit()`](https://generics.r-lib.org/reference/fit.html); passed to
  the selected task-specific function for
  [`predict()`](https://rdrr.io/r/stats/predict.html).

## Value

[`fit()`](https://generics.r-lib.org/reference/fit.html) returns an
`Evo2Model` or `BioNeMoJob`.
[`predict()`](https://rdrr.io/r/stats/predict.html) returns the selected
task-specific result.
