

# bionemor

`bionemor` lets R users prepare biological foundation models, run inference,
and fine-tune them with NVIDIA BioNeMo Recipes. BioNeMo Recipes provides the
model-specific training and inference programs. This package currently supports
Evo 2, a family of models for DNA sequences.

A **checkpoint** is a stored set of model weights. A **compute** object records
where those weights are used: the runtime, workspace, execution backend, and GPU
resources. After one-time system setup, model inputs and results are ordinary R
vectors, data frames, matrices, artifacts, and durable job objects.

> Model operations require a CUDA-capable NVIDIA GPU. There is no CPU fallback.
> You can install the package and inspect model metadata without a GPU.

## What you can do

| Goal | Main functions | Result |
|---|---|---|
| List supported Evo 2 models | `evo2_models()` | Data frame |
| Prepare or reuse model weights | `evo2_model()`, `evo2_checkpoint()` | Model or checkpoint |
| Build phylogenetic prompts and generate DNA | `evo2_phylo_tag()`, `evo2_generate()` | Tag or data frame |
| Score whole sequences or individual positions | `evo2_score()`, `evo2_profile()` | Data frame or Parquet artifact |
| Configure precision, parallelism, and inference optimizations | `evo2_inference_control()` | Inference control |
| Extract sequence embeddings | `evo2_embed()` | Matrix or Parquet artifact |
| Prepare data and fine-tune with LoRA or full training | `evo2_dataset()`, `evo2_prepare()`, `evo2_finetune()`, `evo2_lora()`, `evo2_full()` | Dataset, model, or durable job |
| Export weights in Vortex format | `evo2_export()` | Checkpoint artifact |
| Monitor, reopen, retrieve, or cancel longer work | `job_status()`, `bionemo_job()`, `job_result()`, `job_cancel()` | Durable job or typed result |

The captured output below was rendered on 2026-08-01 with an NVIDIA L40S using
package revision 8819b8ec4d86.

## Install bionemor

```r
pak::pak("t-kalinowski/bionemor")
```

Install the package on the Linux GPU machine where you will run the model.

## Get access to a GPU

The package-managed local runtime requires Linux, Git, `tar`, Docker, and the
NVIDIA Container Toolkit. Docker must be able to expose the GPU to containers.
The 7B examples below were captured on one NVIDIA L40S with 48 GB of GPU memory;
requirements vary by model, sequence length, batch size, and operation.

If you do not have a suitable local GPU, [NVIDIA Brev](https://brev.nvidia.com/)
is one way to rent one. Brev instances are billable. Inspect the available type
and hourly price before creating an instance:

```bash
brev search --gpu-name L40S --min-vram 48 --sort price
brev create bionemor-evo2 --gpu-name L40S --min-vram 48
brev shell bionemor-evo2
```

The Brev instance supplies the Linux GPU machine; it is not a
`bionemo_compute()` backend. Inside the instance, use the `local` backend shown
below.

Run the package installation and the remaining setup from an R session on the
Brev instance. Install R and the required system tools there if the selected
image does not include them. Leave the remote shell before running this command
from your local terminal:

```bash
brev stop bionemor-evo2
```

Stop the instance when you finish so you do not leave compute running. Check
the selected instance type's billing and storage terms.

## Set up the recipe runtime once

NVIDIA NGC is the container registry that supplies the runtime's base image.
Create an NGC API key and authenticate Docker to `nvcr.io` on the GPU machine,
including when using Brev:

```bash
echo "$NGC_API_KEY" | docker login nvcr.io \
  --username '$oauthtoken' --password-stdin
```

Then configure a workspace and install the package-pinned Evo 2 runtime:


``` r
library(bionemor)

workspace <- normalizePath(
  Sys.getenv("BIONEMOR_DOCS_WORKSPACE", "~/evo2-work"),
  mustWork = FALSE
)
compute <- bionemo_compute(
  backend = "local",
  engine = "container",
  workspace = workspace
)

compute <- bionemo_install(compute)
```

`bionemo_install()` builds or verifies the runtime and performs GPU-backed
capability checks. Use `bionemo_doctor()` to diagnose a setup problem. A
site-managed recipe environment can use `engine = "external"`.

## Prepare a model

`evo2_models()` lists models without downloading weights. With a configured
compute object, it can filter by GPU count, compute capability, and precision
policy. This compatibility check does not estimate GPU memory or disk use.


``` r
models <- evo2_models(compute, compatible = TRUE)
str(models)
#> 'data.frame':	2 obs. of  12 variables:
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

`evo2_model()` prepares the recommended dense Megatron Bridge (MBridge)
checkpoint on its first call and reuses a complete matching checkpoint later:

```r
model <- evo2_model("7b", compute)
model
```

The returned model retains its compute object, so later inference and
fine-tuning calls do not repeat it.

## Run inference

Generation and scoring return data frames. Pooled embeddings return a numeric
matrix:


``` r
sequences <- c(
  reference = "ACGTACGTACGT",
  variant = "ACGTACGTTCGT"
)

generated <- evo2_generate(
  model,
  sequences,
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
  sequences,
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

embeddings <- evo2_embed(
  model,
  sequences,
  pool = "mean",
  strand = "both"
)
dim(embeddings)
#> [1]    2 4096
str(embeddings)
#>  'evo2_embeddings' num [1:2, 1:4096] 4.10e+10 4.13e+10 -2.55e+10 -2.58e+10 2.56e+10 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : chr [1:2] "reference" "variant"
#>   ..$ : chr [1:4096] "dim_1" "dim_2" "dim_3" "dim_4" ...
#>  - attr(*, "provenance")=List of 6
#>   ..$ run_path       : chr "/home/ubuntu/bionemor-recipes-workspace/.bionemor/runs/evo2-embedding-20260801T031605-312966"
#>   ..$ checkpoint     : chr "/home/ubuntu/bionemor-recipes-workspace/checkpoints/evo2-7b-mbridge-recipes-e8e7"
#>   ..$ layer          : chr "last"
#>   ..$ pool           : chr "mean"
#>   ..$ strand         : chr "both"
#>   ..$ recipe_revision: chr "e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e"
round(embeddings[, 1:4, drop = FALSE], 4)
#>                 dim_1        dim_2       dim_3       dim_4
#> reference 41006313472 -25541912576 25635586048 14001286144
#> variant   41308303360 -25804754944 26007482368 14023655424
```

Scores are reduced token log probabilities; higher values indicate greater
model likelihood. With `strand = "both"`, forward and reverse values are
reduced independently and averaged. Embedding row names preserve the input IDs.

## Fine-tune and run longer jobs

`evo2_finetune()` accepts raw R sequence inputs or an `evo2_dataset()`. It can
train LoRA adapters with `evo2_lora()` or update the supported base-model
parameters with `evo2_full()`. See
[Fine-tune Evo 2 and retain the checkpoint](vignettes/evo2-finetune.Rmd) for a
complete GPU-captured example with fitted scoring and generation.

For longer work, set `async = TRUE` and save the durable job path:

```r
run <- evo2_generate(
  model,
  sequences,
  num_tokens = 8L,
  async = TRUE
)

path <- job_path(run)
same_run <- bionemo_job(path)
job_status(same_run)
result <- job_wait(same_run)
```

## More guides

- [Evo 2 workflows from R](vignettes/bionemor.Rmd) covers scoring, generation,
  embeddings, and durable jobs.
- [Fine-tune Evo 2 and retain the checkpoint](vignettes/evo2-finetune.Rmd)
  covers preprocessing, LoRA, and fitted inference.
- [Run BioNeMo Recipes jobs with Slurm](vignettes/slurm.Rmd) is an experimental
  cluster reference. It has not been executed on Brev and must be validated on
  the target cluster.

Generated sequences are model output, not validated biological designs. The
package reports mechanical sequence checks; downstream biological validation
remains the user's responsibility.
