# bionemor

`bionemor` lets you prepare Evo 2 models, run batch inference, and fine-tune
with NVIDIA BioNeMo Recipes without leaving R. You work with R vectors, data
frames, matrices, and durable job objects. You do not need to write Python,
handle Python objects, or move the rest of your workflow to Python.

The package supports a direct path from setup to useful work:

1. Configure and verify a pinned recipe runtime once.
2. Prepare or reuse the recommended checkpoint with `evo2_model()`.
3. Generate, score, embed, or fine-tune DNA sequences from R.

## Install bionemor

```r
pak::pak("t-kalinowski/bionemor")
```

## Set up the recipe runtime once

The default local setup builds the verified Evo 2 recipe container. It requires
Git, `tar`, Docker, and an NVIDIA GPU exposed through Docker.

Authenticate Docker to `nvcr.io` with an NGC API key before installing:

```bash
echo "$NGC_API_KEY" | docker login nvcr.io \
  --username '$oauthtoken' --password-stdin
```

Then inspect the exact setup steps, install the runtime, and verify it:

```r
library(bionemor)

compute <- bionemo_compute(
  backend = "local",
  engine = "container",
  workspace = normalizePath("~/evo2-work", mustWork = FALSE)
)

plan <- bionemo_install_plan(compute)
plan
as.data.frame(plan)

compute <- bionemo_install(compute)
bionemo_doctor(compute, target = "all")
```

`bionemo_install()` performs GPU-backed capability and command probes, so this
is not a CPU-only setup step. A site-managed recipe environment can use
`engine = "external"`.

## Prepare a model

List the package's pinned model registry with `evo2_models()`. The plural
function reports available sizes and compatibility metadata; it does not
prepare weights.

```r
evo2_models()
```

`evo2_model()` is the shortest path to a ready model. It prepares the
registry-recommended dense checkpoint on the first call and reuses it on later
calls when its manifest matches the model and pinned runtime:

```r
model <- evo2_model("7b", compute)
```

Checkpoint preparation is synchronous and may take substantial time and
storage. Pass `path` to choose the checkpoint destination:

```r
model <- evo2_model(
  "7b",
  compute,
  path = "checkpoints/evo2-7b-mbridge"
)
```

The names are deliberately distinct: `evo2_models()` lists the registry,
`evo2()` creates an offline descriptor, and `evo2_model()` prepares a ready
model. For custom sources, explicit conversion controls, or asynchronous
preparation, use `evo2()` with `evo2_checkpoint()`.

## Run inference

Generation, scoring, and pooled embeddings return ordinary R objects:

```r
sequences <- c(
  reference = "ACGTACGTACGT",
  variant = "ACGTACGTTCGT"
)

generated <- evo2_generate(
  model,
  sequences,
  compute = compute,
  num_tokens = 64L,
  seed = 1L
)

scores <- evo2_score(
  model,
  sequences,
  compute = compute,
  reduction = "mean",
  strand = "both"
)

embeddings <- evo2_embed(
  model,
  sequences,
  compute = compute,
  pool = "mean",
  strand = "both"
)
```

Generation returns one data-frame row per prompt, including the prompt,
completion, token counts, optional per-token probabilities, and mechanical
sequence checks. Scores include the reduced log probability for each requested
strand and their average when `strand = "both"`. Pooled embeddings are a
numeric matrix whose row names preserve the input IDs.

## Fine-tune with LoRA

Fine-tuning accepts named character vectors, FASTA files, or an
`Evo2Dataset`. Raw inputs are prepared automatically:

```r
data <- evo2_dataset(
  train = "data/train.fa",
  validation = "data/validation.fa",
  test = "data/test.fa"
)

run <- evo2_finetune(
  model,
  data,
  compute = compute,
  steps = 500L,
  method = evo2_lora(
    rank = 16L,
    alpha = 32,
    targets = c("hyena", "attention", "mlp")
  ),
  control = evo2_fit_control(
    sequence_length = 8192L,
    global_batch_size = 8L,
    micro_batch_size = 1L,
    precision = "bf16"
  ),
  path = "runs/splice-lora",
  async = TRUE
)

job_status(run)
job_logs(run, tail = 50L)
fitted <- job_wait(run)
```

The fitted result is another `Evo2Model`, so the same inference functions work
without changing interfaces:

```r
fitted_scores <- evo2_score(fitted, sequences, compute)
```

Two complete examples cover the details:

- [Fine-tune Evo 2 and retain the checkpoint](vignettes/evo2-finetune.Rmd)
- [Run Evo 2 inference from R](vignettes/evo2-inference.Rmd)

## Runtime and checkpoint details

BioNeMo Recipes runs in an external process. R writes portable JSONL, FASTA,
YAML, Parquet, and manifest files, launches the pinned recipe, and converts its
outputs back to R objects. Python remains an implementation detail of that
runtime; using `bionemor` does not require a Python-facing workflow.

MBridge is the runtime checkpoint format. The recommended open-source path
converts the registered Arc Savanna source from Hugging Face. The default
checkpoint path includes the canonical model name, source revision, and recipe
revision so an updated package does not silently reuse incompatible weights.

NIM is not used. The package wraps the open-source Evo 2 Megatron recipe; NIM
only informed a few generation defaults and output checks.

Automatic image builds are limited to the verified package recipe. A custom
repository, revision, or base image requires `engine = "external"` or an
explicit prebuilt `image`. Slurm support is experimental and requires an
existing Apptainer image visible on the shared filesystem.

## Durable runs and explicit failures

Every operation keeps its request, command plan, state, logs, outputs, and
provenance under the compute workspace. Reopen a run after the launching R
session exits:

```r
same_run <- bionemo_job(job_path(run))
job_status(same_run)
result <- job_wait(same_run)
```

Operational failures inherit from `bionemor_error` and a stable code-specific
class such as `BN_CHECKPOINT_INCOMPLETE`, `BN_NO_GPU`, or `BN_TIMEOUT`.

Generated sequence is model output, not a validated biological design. The
package reports mechanical sequence checks; downstream biological validation
remains the user's responsibility.
