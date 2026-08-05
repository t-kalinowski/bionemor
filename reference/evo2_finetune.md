# Fine-tune an Evo 2 model

`evo2_finetune()` runs `train_evo2` from an MBridge checkpoint. Raw
sequence inputs and unprepared
[`evo2_dataset()`](https://t-kalinowski.github.io/bionemor/reference/evo2_dataset.md)
objects are preprocessed automatically using default preprocessing
controls. Call
[`evo2_preprocess()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess.md)
first when preprocessing must be customized or reused.

## Usage

``` r
evo2_finetune(
  object,
  data,
  compute = NULL,
  steps,
  method = evo2_lora(),
  control = evo2_fit_control(),
  path = NULL,
  name = NULL,
  async = TRUE,
  timeout = Inf
)
```

## Arguments

- object:

  An Evo 2 model with an MBridge checkpoint.

- data:

  An
  [`evo2_dataset()`](https://t-kalinowski.github.io/bionemor/reference/evo2_dataset.md)
  result or any input accepted by its `train` argument.

- compute:

  A BioNeMo compute descriptor. `NULL` uses the descriptor attached by
  [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
  or a previous fine-tuning run.

- steps:

  Positive number of optimizer steps.

- method:

  Fine-tuning strategy from
  [`evo2_lora()`](https://t-kalinowski.github.io/bionemor/reference/evo2_lora.md)
  or
  [`evo2_full()`](https://t-kalinowski.github.io/bionemor/reference/evo2_full.md).

- control:

  Training controls from
  [`evo2_fit_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_fit_control.md).

- path:

  Result directory. Relative paths resolve below the compute workspace,
  and `NULL` uses `artifacts/<name>`. Container execution requires the
  result to remain inside the workspace.

- name:

  Optional run name. When `data` must be preprocessed automatically, its
  preprocessing run uses `<name>-data`.

- async:

  Whether to return a `BioNeMoJob` before fine-tuning completes.

- timeout:

  Complete operation timeout in seconds. This limits the launched
  operation;
  [`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
  has a separate client-side wait timeout.

## Value

With `async = FALSE`, a fitted `Evo2Model` bound to the same compute
descriptor. With `async = TRUE`, a `BioNeMoJob`;
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
materializes the fitted model.

## Details

`steps` counts optimizer steps, not epochs. The fitting control
determines data parallelism and gradient accumulation from the allocated
GPU count and model-parallel settings. Training requires GPUs with
compute capability 8.0 or newer; model-specific precision restrictions
are enforced before launch.

A LoRA result contains adapter weights and records the dense base
checkpoint; that base checkpoint must remain at the recorded path for
later inference. LoRA-on-LoRA fine-tuning and full fine-tuning from a
LoRA checkpoint are not supported.

## References

[BioNeMo Recipes Evo 2
fine-tuning](https://github.com/NVIDIA-BioNeMo/bionemo-recipes/blob/e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e/recipes/evo2_megatron/README.md#fine-tuning-from-an-existing-checkpoint)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(), workspace = "~/evo2-work")
compute <- bionemo_install(compute)
model <- evo2_model("1b", compute)
data <- evo2_dataset(c(first = "ACGT", second = "TGCA"))

run <- evo2_finetune(
  model,
  data,
  steps = 500L,
  method = evo2_lora(targets = c("attention", "mlp")),
  control = evo2_fit_control(
    sequence_length = 1024L,
    precision = "bf16"
  ),
  async = TRUE
)
fitted <- job_wait(run)
} # }
```
