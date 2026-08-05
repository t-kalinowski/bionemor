# Run biological foundation models from R

`bionemor` supports Evo 2 operations on DNA and ESM-2 protein embeddings
through a shared R interface for configuring compute, running and
monitoring jobs, returning results to R, and recording how they were
produced.

## Details

Installing the R package and inspecting its model registries do not
require a GPU. Preparing weights, inference, preprocessing, and
fine-tuning require a supported CUDA-capable NVIDIA GPU; there is no CPU
fallback. The package-managed local runtime also requires Linux, Docker,
and NVIDIA Container Toolkit. A remote Linux GPU from a provider such as
Brev can be used when no local GPU is available.

## How the pieces fit together

A model descriptor identifies a family, variant, configuration, and
optional checkpoint without loading weights into R. A recipe describes
the pinned family-specific source, dependencies, and commands. A compute
descriptor combines that recipe with a workspace, backend, runtime
engine, and GPU resources; a container is one engine selected by
compute. Family-specific R functions request operations. Each operation
creates a job directory that stores its request, state, logs, outputs,
and details about how it was run. Save the job path to reopen it in
another R session.

## Start here

- [`evo2_models()`](https://t-kalinowski.github.io/bionemor/reference/evo2_models.md)
  and
  [`esm2_models()`](https://t-kalinowski.github.io/bionemor/reference/esm2_models.md)
  list the supported model checkpoints.

- [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md)
  describes the selected recipe, GPU runtime, and workspace.

- [`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
  prepares or checks that runtime.

## Evo 2 DNA operations

- [`evo2_models()`](https://t-kalinowski.github.io/bionemor/reference/evo2_models.md)
  lists the packaged model registry, and
  [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
  prepares a checkpoint and binds it to compute.

- [`evo2_generate()`](https://t-kalinowski.github.io/bionemor/reference/evo2_generate.md)
  extends DNA prompts.

- [`evo2_score()`](https://t-kalinowski.github.io/bionemor/reference/evo2_score.md)
  scores complete sequences.

- [`evo2_profile()`](https://t-kalinowski.github.io/bionemor/reference/evo2_profile.md)
  writes per-position log probabilities.

- [`evo2_embed()`](https://t-kalinowski.github.io/bionemor/reference/evo2_embed.md)
  returns pooled embeddings or writes positional embeddings.

- [`evo2_dataset()`](https://t-kalinowski.github.io/bionemor/reference/evo2_dataset.md),
  [`evo2_preprocess()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess.md),
  and
  [`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md)
  describe and preprocess training data, then fine-tune a model.

- [`evo2_checkpoint()`](https://t-kalinowski.github.io/bionemor/reference/evo2_checkpoint.md)
  and
  [`evo2_export()`](https://t-kalinowski.github.io/bionemor/reference/evo2_export.md)
  convert and export checkpoints.

## ESM-2 protein embeddings

- [`esm2_models()`](https://t-kalinowski.github.io/bionemor/reference/esm2_models.md)
  lists pinned NVIDIA ESM-2 checkpoints.

- [`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md)
  binds a checkpoint to an ESM-2 recipe runtime.

- [`esm2_embed()`](https://t-kalinowski.github.io/bionemor/reference/esm2_embed.md)
  returns last-token, L2-normalized protein embeddings.

## Run and monitor jobs

Operations save their requests, logs, state, and portable results below
the compute workspace. Use
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md)
to reopen a run after the R session ends and
[`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md)
to retrieve its result.

## See also

Useful links:

- <https://t-kalinowski.github.io/bionemor/>

- <https://github.com/t-kalinowski/bionemor>

- Report bugs at <https://github.com/t-kalinowski/bionemor/issues>

## Author

**Maintainer**: Tomasz Kalinowski <kalinowskit@gmail.com>

Authors:

- Tomasz Kalinowski <kalinowskit@gmail.com>
