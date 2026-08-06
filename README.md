# bionemor

Evo 2 is a biological foundation model trained on DNA sequences. ESM-2 is a
biological foundation model trained on protein sequences.

`bionemor` supports Evo 2 and ESM-2 on NVIDIA GPUs from R. Evo 2 can generate
and score DNA, calculate positional profiles and embeddings, and be
[fine-tuned](https://t-kalinowski.github.io/bionemor/articles/evo2-finetune.html)
on your own sequences. ESM-2 creates protein embeddings for similarity,
clustering, and downstream models.

Inference inputs are ordinary R character vectors or Biostrings sequence sets.
Generation and scoring return data frames, while pooled embeddings return
numeric matrices. Longer runs can be saved, monitored, and reopened from
another R session.

> Running a model requires a supported CUDA-capable NVIDIA GPU. There is no CPU fallback.
> You can install `bionemor` and inspect the available models without a GPU.

## Install bionemor

```r
pak::pak("t-kalinowski/bionemor")
```

## Quick start: generate DNA with Evo 2

This example takes DNA prompts and predicts continuations with Evo 2. Run it on
a Linux machine with a supported NVIDIA GPU and Docker configured for GPU
access. The [setup guide](https://t-kalinowski.github.io/bionemor/articles/bionemor.html)
covers both local machines and NVIDIA Brev.

First, prepare the package-managed Evo 2 runtime:

``` r
library(bionemor)

workspace <- "~/workspace/bionemor"
evo2_compute <- bionemo_compute(
  recipe = evo2_recipe(),
  workspace = workspace
)
evo2_compute <- bionemo_install(evo2_compute)
```

Then select and prepare the model:

```r
model <- evo2_model("7b", evo2_compute)
```

Then generate a continuation for each prompt:

``` r
dna <- c(
  reference = "ACGTACGTACGT",
  variant = "ACGTACGTTCGT"
)

generated <- evo2_generate(
  model,
  dna,
  num_tokens = 8L,
  seed = 17L
)
generated[c(
  "input_id", "prompt", "completion", "generated_tokens", "finish_reason"
)]
#>    input_id       prompt completion generated_tokens finish_reason
#> 1 reference ACGTACGTACGT   ATACGTAT                8        length
#> 2   variant ACGTACGTTCGT   ATACGTTT                8        length
```

The output above was captured on 2026-08-06 with an NVIDIA L40S using package
revision 6970e338a742.

The first install prepares the runtime, and the first model call downloads and
prepares the checkpoint. Later calls reuse both from the workspace.

The same model can score sequences with `evo2_score()`, calculate positional
profiles with `evo2_profile()`, create embeddings with `evo2_embed()`, and be
fine-tuned with `evo2_finetune()`. See the
[fine-tuning guide](https://t-kalinowski.github.io/bionemor/articles/evo2-finetune.html)
for the complete workflow.

## How the pieces fit together

The quick start combines a few package objects that record how operations run.
Saved jobs can be reopened later:

| Concept | Role |
|---|---|
| Model descriptor | Identifies a model family, size, configuration, and optional checkpoint. |
| Recipe | Pins the software environment for a model family. |
| Compute descriptor | Combines a recipe with a workspace and execution settings. |
| Operation | Runs work such as `evo2_generate()` or `esm2_embed()`. |
| Job | Saves an operation's request, logs, state, result, and provenance. |

The quick start uses the defaults `backend = "local"` and
`engine = "container"`. The backend says where commands run: on the current
machine or through Slurm. The engine says how the runtime is supplied: in a
container or in a compatible external environment. The local Docker and Brev
setup guides use these defaults.

## Score and embed DNA with Evo 2

Scoring returns reduced token log probabilities; higher values indicate greater
model likelihood. Pooled embeddings return a numeric matrix whose row names
preserve the input IDs.

``` r
scores <- evo2_score(
  model,
  dna,
  reduction = "mean",
  strand = "both"
)
scores
#>          id sequence_length tokens_scored     score forward_score reverse_score reduction
#> 1 reference              12            11 -1.408769     -1.408769     -1.408769      mean
#> 2   variant              12            11 -1.411028     -1.422442     -1.399613      mean
#>   strand
#> 1   both
#> 2   both

dna_embeddings <- evo2_embed(
  model,
  dna,
  pool = "mean",
  strand = "both"
)
dim(dna_embeddings)
#> [1]    2 4096
round(dna_embeddings[, 1:4, drop = FALSE], 4)
#>                 dim_1        dim_2       dim_3       dim_4
#> reference 41006313472 -25541912576 25635586048 14001286144
#> variant   41308303360 -25804754944 26007482368 14023655424
```

Scores are model likelihoods, not direct measurements of biological function.

## Embed proteins with ESM-2

ESM-2 turns protein sequences into vectors for similarity, clustering, or use
as features in downstream R models:

``` r
esm2_compute <- bionemo_compute(
  recipe = esm2_recipe(),
  workspace = file.path(workspace, "esm2")
)
esm2_compute <- bionemo_install(esm2_compute)
protein_model <- esm2_model("8m", esm2_compute)

proteins <- c(
  protein_1 = "MKTAYIAKQRQISFVKSHFSRQ",
  protein_2 = "GAVLILKKKGHHEAELKPLAQSHATK"
)
protein_embeddings <- esm2_embed(protein_model, proteins)

dim(protein_embeddings)
#> [1]   2 320
round(protein_embeddings[, 1:4, drop = FALSE], 4)
#>             dim_1   dim_2  dim_3  dim_4
#> protein_1 -0.0048 -0.0239 0.0659 0.0467
#> protein_2 -0.0147  0.0020 0.0397 0.0330
```

Each row is one last-token, L2-normalized protein embedding. The result behaves
like an ordinary numeric matrix and includes provenance for the model and saved
job.

## Run longer work

Operations that accept `async = TRUE` return a saved job. Save its path to
monitor it or reopen it in a later R session:

```r
run <- evo2_generate(
  model,
  dna,
  num_tokens = 8L,
  async = TRUE
)

path <- job_path(run)

# In a new R session:
same_run <- bionemo_job(path)
job_status(same_run)
result <- job_wait(same_run)
```

## Learn more

- [Set up a GPU runtime and work on Brev](https://t-kalinowski.github.io/bionemor/articles/bionemor.html)
  covers Docker, model setup, custom images, and the full Evo 2 and ESM-2
  workflows.
- [Fine-tune Evo 2 and retain the checkpoint](https://t-kalinowski.github.io/bionemor/articles/evo2-finetune.html)
  covers preprocessing, LoRA, and fitted inference.
- [Browse the function reference](https://t-kalinowski.github.io/bionemor/reference/index.html)
  for all supported operations and controls.
