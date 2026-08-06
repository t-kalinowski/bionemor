# bionemor

Biological foundation models are trained on large collections of DNA or
protein sequences. The resulting models can be used across different
sequence tasks.

`bionemor` lets bioinformaticians and other researchers use these models
on NVIDIA GPUs from R. With Evo 2, you can generate and score DNA,
calculate positional profiles and embeddings, and fine-tune a model on
your own sequences. With ESM-2, you can create embeddings that represent
proteins as numerical vectors for similarity, clustering, and downstream
models.

Sequence inputs can be character vectors or sequence sets from
Biostrings. Generation and scoring return data frames, while pooled
embeddings return numeric matrices. Longer runs can be saved, monitored,
and reopened from another R session.

A model descriptor identifies the model and optional checkpoint. A
compute descriptor records where and how it runs. Family-specific
functions use those objects for running and monitoring jobs without
loading model weights into the R process.

> Running a model requires a supported CUDA-capable NVIDIA GPU. There is
> no CPU fallback. You can install `bionemor` and inspect the available
> models without a GPU.

## Install bionemor

``` r

pak::pak("t-kalinowski/bionemor")
```

For the local container setup below, install the package in the R
environment on the Linux GPU machine, then load it:

``` r

library(bionemor)
```

## Supported models and operations

The supported models and their main functions are:

| Model family | Input | What you can do | Main functions |
|----|----|----|----|
| Evo 2 | DNA sequences | Generation, scoring, positional profiles, embeddings, fine-tuning, training-data preprocessing, and checkpoint preparation and export | [`evo2_generate()`](https://t-kalinowski.github.io/bionemor/reference/evo2_generate.md), [`evo2_score()`](https://t-kalinowski.github.io/bionemor/reference/evo2_score.md), [`evo2_profile()`](https://t-kalinowski.github.io/bionemor/reference/evo2_profile.md), [`evo2_embed()`](https://t-kalinowski.github.io/bionemor/reference/evo2_embed.md), [`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md), [`evo2_preprocess()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess.md), [`evo2_models()`](https://t-kalinowski.github.io/bionemor/reference/evo2_models.md), [`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md), [`evo2_checkpoint()`](https://t-kalinowski.github.io/bionemor/reference/evo2_checkpoint.md), [`evo2_export()`](https://t-kalinowski.github.io/bionemor/reference/evo2_export.md) |
| ESM-2 | Protein sequences | Pooled protein embeddings | [`esm2_models()`](https://t-kalinowski.github.io/bionemor/reference/esm2_models.md), [`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md), [`esm2_embed()`](https://t-kalinowski.github.io/bionemor/reference/esm2_embed.md) |

The examples below begin with Evo 2 DNA, followed by ESM-2 protein
embeddings.

## Get access to a GPU

The local setup below runs models in Docker on a Linux machine with an
NVIDIA GPU. Docker must be configured so containers can use the GPU. GPU
requirements vary by model, sequence length, batch size, and operation.

If you do not have a suitable local GPU, [NVIDIA
Brev](https://brev.nvidia.com/) lets you rent one and manage the
instance from its CLI. Download the package’s startup script before
creating the VM:

``` bash
curl --fail --location --output bionemor-brev-setup.sh \
  https://raw.githubusercontent.com/t-kalinowski/bionemor/main/tools/brev/setup.sh
brev search --stoppable --gpu-name L40S --min-vram 48 --sort price
brev create bionemor-gpu --mode vm \
  --stoppable --gpu-name L40S --min-vram 48 \
  --startup-script @bionemor-brev-setup.sh
brev shell bionemor-gpu
```

The startup script installs the current R release with rig, installs
`bionemor` with pak, and creates `~/workspace/bionemor`. R runs directly
on the Brev VM; the BioNeMo recipe runtime runs in Docker. Brev supplies
the machine, so use `backend = "local"` inside it rather than treating
Brev as another backend.

Data and checkpoints remain user-managed. Copy local files into the
persistent workspace with the CLI, or download them from the VM:

``` bash
brev copy ./data/ \
  bionemor-gpu:/home/ubuntu/workspace/bionemor/data/
```

Files below `~/workspace` persist when a stoppable instance is stopped.
Deleting the instance deletes that storage, so retain another copy of
important data.

When you finish, leave the remote shell and stop the instance from your
local terminal:

``` bash
brev stop bionemor-gpu
```

## Set up Evo 2

Evo 2 operations run through NVIDIA BioNeMo Recipes and Megatron Bridge.
`bionemor` uses versions tested with this release. The local Docker
environment is built from an NVIDIA NGC base image. Create an NGC API
key and authenticate Docker to `nvcr.io` on the GPU machine, including
when using Brev:

Megatron Bridge is the training and inference stack used by the pinned
Evo 2 recipe. MBridge is its checkpoint format.

``` bash
echo "$NGC_API_KEY" | docker login nvcr.io \
  --username '$oauthtoken' --password-stdin
```

[`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md)
records the workspace and execution settings.
[`evo2_recipe()`](https://t-kalinowski.github.io/bionemor/reference/evo2_recipe.md)
selects that software environment; `backend = "local"` and
`engine = "container"` run it in Docker on the current machine:

``` r

workspace <- normalizePath(
  Sys.getenv("BIONEMOR_DOCS_WORKSPACE", "~/workspace/bionemor"),
  mustWork = FALSE
)
evo2_compute <- bionemo_compute(
  recipe = evo2_recipe(),
  backend = "local",
  engine = "container",
  workspace = workspace
)

evo2_compute <- bionemo_install(evo2_compute)
```

[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
builds or verifies the selected environment and checks its GPU
capabilities. If setup fails, use
[`bionemo_doctor()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_doctor.md)
to inspect the environment.

### Extend the recipe image

Add site-specific system or Python dependencies by building on the
package’s verified image, not directly on the raw NVIDIA base image.
After installation, `evo2_compute@image` prints the managed recipe
image. Export the same value in a shell:

``` bash
export BIONEMOR_RECIPE_IMAGE="$(
  Rscript --vanilla -e '
    library(bionemor)
    compute <- bionemo_compute(
      recipe = evo2_recipe(),
      workspace = "~/workspace/bionemor"
    )
    cat(compute@image)
  '
)"
```

Then build a Dockerfile such as:

``` dockerfile
ARG BIONEMOR_RECIPE_IMAGE
FROM ${BIONEMOR_RECIPE_IMAGE}

# Add site-specific Dockerfile instructions here.
```

``` bash
docker build \
  --build-arg BIONEMOR_RECIPE_IMAGE \
  --tag example/bionemor-evo2:site .
```

Select that local tag explicitly and verify the resulting prebuilt
recipe image:

``` r

custom_compute <- bionemo_compute(
  recipe = evo2_recipe(),
  backend = "local",
  engine = "container",
  workspace = workspace,
  image = "example/bionemor-evo2:site"
)
custom_compute <- bionemo_install(custom_compute)
```

[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
inspects an explicit image but does not build or pull it. The R package
stays on the VM; it does not need to be installed in this runtime image.

## Evo 2: generate, score, and embed DNA

[`evo2_models()`](https://t-kalinowski.github.io/bionemor/reference/evo2_models.md)
lists models without downloading weights. With a configured compute
object, it can filter by GPU count, compute capability, and precision
policy. Check GPU memory and disk requirements separately.

The output below was captured on 2026-08-01 with an NVIDIA L40S using
package revision 7da249b5f346.

``` r

models <- evo2_models(evo2_compute, compatible = TRUE)
str(models)
#> 'data.frame':    2 obs. of  12 variables:
#>  $ name                     : chr  "7b-base" "7b"
#>  $ model_size               : chr  "evo2_7b_base" "evo2_7b"
#>  $ parameters               : num  7e+09 7e+09
#>  $ context_length           : int  8192 1048576
#>  $ source                   : chr  "hf://arcinstitute/savanna_evo2_7b_base" "hf://arcinstitute/savanna_evo2_7b"
#>  $ source_revision          : chr  "eb0a7478e5f3c291f31e2b3d9ec14fc067f9982a" "9e69aeeaacf4d11fdbabfa73da65a770e5031f02"
#>  $ source_format            : chr  "savanna" "savanna"
#>  $ precision_policy         : chr  "bf16-or-fp8" "bf16-or-fp8"
#>  $ training_precision_policy: chr  "bf16-or-fp8" "bf16-or-fp8"
#>  $ download_size            : num  1.58e+10 1.58e+10
#>  $ compatible               : logi  TRUE TRUE
#>  $ compatibility_note       : chr  "advertised GPUs support the validated BF16 or FP8 policy" "advertised GPUs support the validated BF16 or FP8 policy"
```

[`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
downloads and prepares the selected model the first time you use it.
Later calls from the same workspace reuse the prepared checkpoint:

``` r

model <- evo2_model("7b", evo2_compute)
model
```

Pass the model to the generation, scoring, and embedding functions.
Generation and scoring return data frames; pooled embeddings return a
numeric matrix:

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

Scores are reduced token log probabilities; higher values indicate
greater model likelihood. Interpreting them in terms of biological
function generally requires further analysis. Embedding row names
preserve the input IDs.

## ESM-2: embed proteins

`bionemor` supports pooled protein embeddings with ESM-2. You can use
these vectors for sequence similarity, clustering, or as features in
downstream R models.
[`esm2_models()`](https://t-kalinowski.github.io/bionemor/reference/esm2_models.md)
lists the available pinned NVIDIA checkpoints and their embedding
dimensions without downloading weights, and
[`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md)
selects one for use with a compute descriptor.

The ESM-2 environment uses native Transformers and Transformer Engine.
On first use, Transformers downloads the selected weights from Hugging
Face and caches them below the workspace.
[`esm2_embed()`](https://t-kalinowski.github.io/bionemor/reference/esm2_embed.md)
currently requires `gpus = 1` and returns one embedding per protein:

``` r

esm2_compute <- bionemo_compute(
  recipe = esm2_recipe(),
  backend = "local",
  engine = "container",
  gpus = 1L,
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
str(protein_embeddings)
#>  'esm2_embeddings' num [1:2, 1:320] -0.00477 -0.0147 -0.02391 0.002 0.06589 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : chr [1:2] "protein_1" "protein_2"
#>   ..$ : chr [1:320] "dim_1" "dim_2" "dim_3" "dim_4" ...
#>  - attr(*, "provenance")=List of 6
#>   ..$ run_path       : chr "/home/ubuntu/bionemor-recipes-workspace/esm2/.bionemor/runs/esm2-embedding-20260801T223010-834352"
#>   ..$ model          : chr "8m"
#>   ..$ source         : chr "nvidia/esm2_t6_8M_UR50D"
#>   ..$ source_revision: chr "3674a6acb6c217bbeff709d182a11b196125dfc3"
#>   ..$ pooling        : chr "last-token-l2"
#>   ..$ recipe_revision: chr "e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e"
round(protein_embeddings[, 1:4, drop = FALSE], 4)
#>             dim_1   dim_2  dim_3  dim_4
#> protein_1 -0.0048 -0.0239 0.0659 0.0467
#> protein_2 -0.0147  0.0020 0.0397 0.0330
```

Each row is one last-token, L2-normalized protein embedding. The result
is a numeric matrix for use with ordinary R tools. Its `provenance`
attribute records the model, recipe revision, and path where the job was
saved.

## Run longer work

Operations that accept `async = TRUE` return a saved job. Save its path
to monitor it or reopen it in a later R session:

``` r

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

## How the pieces fit together

The examples above use the following package concepts:

| Concept | Role in bionemor |
|----|----|
| Model descriptor | Identifies the model family, size, configuration, and optional checkpoint. |
| Checkpoint | Model weights obtained from a registered source, a supplied path, or an earlier run. |
| Recipe | Describes the versioned software environment and commands for a model family. |
| Compute descriptor | Combines a recipe with a workspace, execution settings, and requested GPU resources. |
| Operation | Work requested through a family-specific R function such as [`evo2_score()`](https://t-kalinowski.github.io/bionemor/reference/evo2_score.md) or [`esm2_embed()`](https://t-kalinowski.github.io/bionemor/reference/esm2_embed.md). |
| Job | A saved record of an operation’s request, logs, state, outputs, and provenance. |

## More guides

- [Get started with
  bionemor](https://t-kalinowski.github.io/bionemor/articles/bionemor.html)
  walks through setup and Evo 2 and ESM-2 examples.
- [Fine-tune Evo 2 and retain the
  checkpoint](https://t-kalinowski.github.io/bionemor/articles/evo2-finetune.html)
  covers preprocessing, LoRA, and fitted inference.
