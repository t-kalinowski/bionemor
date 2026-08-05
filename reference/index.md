# Package index

## Start here

- [`bionemor`](https://t-kalinowski.github.io/bionemor/reference/bionemor-package.md)
  [`bionemor-package`](https://t-kalinowski.github.io/bionemor/reference/bionemor-package.md)
  : Run biological foundation models from R

## Configure compute and runtimes

- [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md)
  : Describe where BioNeMo operations run
- [`evo2_recipe()`](https://t-kalinowski.github.io/bionemor/reference/evo2_recipe.md)
  : Describe the Evo 2 BioNeMo recipe
- [`esm2_recipe()`](https://t-kalinowski.github.io/bionemor/reference/esm2_recipe.md)
  : Describe the BioNeMo ESM-2 embedding recipe
- [`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
  : Install or verify a BioNeMo recipe runtime
- [`bionemo_doctor()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_doctor.md)
  : Diagnose a BioNeMo Recipes execution environment
- [`bionemo_capabilities()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_capabilities.md)
  : Report capabilities of a BioNeMo recipe runtime

## Evo 2 models and checkpoints

- [`evo2_models()`](https://t-kalinowski.github.io/bionemor/reference/evo2_models.md)
  : List the package's pinned Evo 2 model registry
- [`evo2()`](https://t-kalinowski.github.io/bionemor/reference/evo2.md)
  : Describe an Evo 2 model
- [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
  : Prepare a recommended Evo 2 model
- [`evo2_checkpoint()`](https://t-kalinowski.github.io/bionemor/reference/evo2_checkpoint.md)
  : Prepare an Evo 2 checkpoint
- [`evo2_export()`](https://t-kalinowski.github.io/bionemor/reference/evo2_export.md)
  : Export an Evo 2 checkpoint
- [`checkpoint_path()`](https://t-kalinowski.github.io/bionemor/reference/checkpoint_metadata.md)
  [`checkpoint_manifest()`](https://t-kalinowski.github.io/bionemor/reference/checkpoint_metadata.md)
  : Inspect checkpoint metadata

## Evo 2 inference

- [`evo2_generate()`](https://t-kalinowski.github.io/bionemor/reference/evo2_generate.md)
  : Generate DNA continuations with Evo 2
- [`evo2_score()`](https://t-kalinowski.github.io/bionemor/reference/evo2_score.md)
  : Score DNA sequences with Evo 2
- [`evo2_profile()`](https://t-kalinowski.github.io/bionemor/reference/evo2_profile.md)
  : Write positional Evo 2 log-probability profiles
- [`evo2_embed()`](https://t-kalinowski.github.io/bionemor/reference/evo2_embed.md)
  : Extract Evo 2 sequence embeddings
- [`evo2_phylo_tag()`](https://t-kalinowski.github.io/bionemor/reference/evo2_phylo_tag.md)
  : Construct an Evo 2 phylogenetic prompt tag
- [`evo2_inference_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_inference_control.md)
  : Construct typed Evo 2 inference controls

## Evo 2 fine-tuning

- [`evo2_dataset()`](https://t-kalinowski.github.io/bionemor/reference/evo2_dataset.md)
  : Describe an Evo 2 dataset
- [`evo2_preprocess_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess_control.md)
  : Construct typed Evo 2 preprocessing controls
- [`evo2_preprocess()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess.md)
  : Preprocess training data for Evo 2 fine-tuning
- [`evo2_lora()`](https://t-kalinowski.github.io/bionemor/reference/evo2_lora.md)
  : Describe Evo 2 LoRA fine-tuning
- [`evo2_full()`](https://t-kalinowski.github.io/bionemor/reference/evo2_full.md)
  : Describe full Evo 2 fine-tuning
- [`evo2_fit_control()`](https://t-kalinowski.github.io/bionemor/reference/evo2_fit_control.md)
  : Construct typed Evo 2 fitting controls
- [`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md)
  : Fine-tune an Evo 2 model

## ESM-2 protein embeddings

- [`esm2_models()`](https://t-kalinowski.github.io/bionemor/reference/esm2_models.md)
  : List the pinned ESM-2 models
- [`esm2()`](https://t-kalinowski.github.io/bionemor/reference/esm2.md)
  : Describe an ESM-2 model
- [`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md)
  : Bind an ESM-2 model to compute
- [`esm2_embed()`](https://t-kalinowski.github.io/bionemor/reference/esm2_embed.md)
  : Extract pooled ESM-2 protein embeddings

## Run and monitor jobs

- [`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md)
  : Reopen a persisted BioNeMo job
- [`job_path()`](https://t-kalinowski.github.io/bionemor/reference/job_path.md)
  : Return the run directory for a BioNeMo job
- [`job_status()`](https://t-kalinowski.github.io/bionemor/reference/job_status.md)
  : Return a BioNeMo job state
- [`job_logs()`](https://t-kalinowski.github.io/bionemor/reference/job_logs.md)
  : Read BioNeMo job logs
- [`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
  : Wait for a BioNeMo job and return its result
- [`job_result()`](https://t-kalinowski.github.io/bionemor/reference/job_result.md)
  : Return a completed BioNeMo job result
- [`job_cancel()`](https://t-kalinowski.github.io/bionemor/reference/job_cancel.md)
  : Cancel a BioNeMo job

## Compatibility generics

- [`fit()`](https://t-kalinowski.github.io/bionemor/reference/reexports.md)
  [`predict()`](https://t-kalinowski.github.io/bionemor/reference/reexports.md)
  : Model compatibility generics
