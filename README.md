

# bionemor

`bionemor` runs biological foundation-model workflows from R with NVIDIA
BioNeMo Recipes. BioNeMo Recipes is a collection of model-specific training and
inference programs. The package installs a pinned recipe runtime, translates R
inputs into its command-line interface, and returns results as R objects and
durable jobs.

> Model operations require a supported CUDA-capable NVIDIA GPU. There is no CPU fallback.
> You can install the package and inspect recipes, workflows, and model metadata
> without a GPU.

## Find a workflow

Start with `bionemo_workflows()`. It lists the model families and operations in
the installed package without downloading model weights:


``` r
library(bionemor)

workflows <- bionemo_workflows()
workflows[c("id", "family", "task")]
#>                id family       task
#> 1      esm2/embed   esm2      embed
#> 2 evo2/checkpoint   evo2 checkpoint
#> 3     evo2/export   evo2     export
#> 4    evo2/prepare   evo2    prepare
#> 5  evo2/fine-tune   evo2  fine-tune
#> 6   evo2/generate   evo2   generate
#> 7      evo2/score   evo2      score
#> 8    evo2/profile   evo2    profile
#> 9      evo2/embed   evo2      embed
```

The package currently provides these family-specific R interfaces:

| Model family | Input | Supported workflows | Main functions |
|---|---|---|---|
| Evo 2 | DNA sequences | Checkpoint preparation and export, generation, scoring, positional profiles, embeddings, data preparation, and fine-tuning | `evo2_models()`, `evo2_model()`, `evo2_checkpoint()`, `evo2_export()`, `evo2_prepare()`, `evo2_generate()`, `evo2_score()`, `evo2_profile()`, `evo2_embed()`, `evo2_finetune()` |
| ESM-2 | Protein sequences | Pooled protein embeddings | `esm2_models()`, `esm2_model()`, `esm2_embed()` |

Evo 2 has the broadest interface and is the detailed example below. ESM-2
embedding uses the same compute, workflow, and durable-job abstractions. The
lower-level `bionemo_run()` interface also accepts an ID from
`bionemo_workflows()` when you need to select a workflow programmatically.

The captured output below was rendered on 2026-08-01 with an NVIDIA L40S
using package revision 7da249b5f346.

## Install bionemor

```r
pak::pak("t-kalinowski/bionemor")
```

Install the package on the Linux GPU machine where you will run the model.

## Get access to a GPU

The package-managed local runtime requires Linux, Git, `tar`, Docker, and the
NVIDIA Container Toolkit. Docker must be able to expose the GPU to containers.
The Evo 2 7B examples below were captured on one NVIDIA L40S with 48 GB of GPU
memory. Requirements vary by model, sequence length, batch size, and operation.

If you do not have a suitable local GPU, [NVIDIA Brev](https://brev.nvidia.com/)
is one way to rent one. Brev instances are billable. Inspect the available type
and hourly price before creating an instance:

```bash
brev search --stoppable --gpu-name L40S --min-vram 48 --sort price
brev create bionemor-gpu --stoppable --gpu-name L40S --min-vram 48
brev shell bionemor-gpu
```

Brev supplies the Linux GPU machine; it is not a `bionemo_compute()` backend.
Inside the instance, use the `local` backend shown below. Install R and the
required system tools if the selected image does not include them.

Leave the remote shell and stop the instance from your local terminal when you
finish:

```bash
brev stop bionemor-gpu
```

Check the selected instance type's billing and storage terms.

## Install a recipe runtime

NVIDIA NGC is the container registry that supplies the runtime's base image.
Create an NGC API key and authenticate Docker to `nvcr.io` on the GPU machine,
including when using Brev:

```bash
echo "$NGC_API_KEY" | docker login nvcr.io \
  --username '$oauthtoken' --password-stdin
```

A compute descriptor records where jobs run, where their files are stored, and
which recipe supplies the model-specific runtime. The recipe is required so
that changing model families is visible in user code.

This descriptor selects the Evo 2 recipe:


``` r
workspace <- normalizePath(
  Sys.getenv("BIONEMOR_DOCS_WORKSPACE", "~/bionemor-work"),
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

`bionemo_install()` builds or verifies the selected runtime and performs
GPU-backed capability checks. Use `bionemo_doctor()` to diagnose a setup
problem. A site-managed recipe environment can use `engine = "external"`.

## Evo 2: score, generate, and embed DNA

`evo2_models()` lists models without downloading weights. With a configured
compute object, it can filter by GPU count, compute capability, and precision
policy. This compatibility check does not estimate GPU memory or disk use.


``` r
models <- evo2_models(evo2_compute, compatible = TRUE)
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

`evo2_model()` prepares the recommended dense Megatron Bridge checkpoint on
its first call and reuses a complete matching checkpoint later:

```r
model <- evo2_model("7b", evo2_compute)
model
```



The returned model retains its compute object. Generation and scoring return
data frames; pooled embeddings return a numeric matrix:


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

Scores are reduced token log probabilities; higher values indicate greater
model likelihood. They are not calibrated measurements of biological
function. Embedding row names preserve the input IDs.

## ESM-2: embed proteins

ESM-2 uses its own recipe and compute descriptor. `esm2_model()` binds a pinned
ESM-2 checkpoint to that compute target. On first use, Transformers downloads
the selected weights from Hugging Face and caches them below the workspace.
Installation uses the recipe's native Transformers and Transformer Engine path
and does not compile vLLM. ESM-2 inference currently requires `gpus = 1`. You
may supply multiple proteins; the runtime processes them one at a time on the
selected GPU to preserve their bidirectional attention boundaries.


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

Each row is one last-token, L2-normalized protein embedding. You can use these
vectors for sequence similarity, clustering, or as features in downstream R
models. They are model representations, not measurements of protein function.
`esm2_models()` lists the available pinned checkpoints and their embedding
dimensions without downloading weights.

## Run longer work

Family-specific functions use the same durable job interface. Set
`async = TRUE`, retain the job path, and reopen it in a later R session:

```r
run <- evo2_generate(
  model,
  dna,
  num_tokens = 8L,
  async = TRUE
)

path <- job_path(run)
same_run <- bionemo_job(path)
job_status(same_run)
result <- job_wait(same_run)
```

## More guides

- [BioNeMo workflows from R](vignettes/bionemor.Rmd) explains the shared API,
  then walks through Evo 2 and ESM-2 workflows.
- [Fine-tune Evo 2 and retain the checkpoint](vignettes/evo2-finetune.Rmd)
  covers preprocessing, LoRA, and fitted inference.
- [Run BioNeMo Recipes jobs with Slurm](vignettes/slurm.Rmd) is an experimental
  cluster reference. Its Evo 2 example has not been executed on Slurm and must
  be validated on the target cluster.

Generated sequences are model output, not validated biological designs. The
package reports mechanical sequence checks; downstream biological validation
remains the user's responsibility.
