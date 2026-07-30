# bionemor

`bionemor` prepares MBridge checkpoints, fine-tunes Evo 2 models, and runs
batch inference with NVIDIA BioNeMo Recipes from R. R writes portable inputs
and manifests, then invokes the pinned recipe in an external process. It does
not initialize Python inside R or return Python objects.

NIM is not used. The package wraps the open-source Evo 2 Megatron recipe. NIM
only informed a few generation defaults and output checks.

## Install the recipe runtime

The package locks BioNeMo Recipes to an exact source revision. The default
container is built from the verified upstream Evo 2 Dockerfile. Its `FROM`
image is bound to the locked digest before the package helper and provenance
labels are appended. The package also activates the Dockerfile's documented
`uv` fallback with a digest-pinned image because the locked NGC base does not
contain `uv`. The NGC PyTorch base image alone does not contain the recipe
commands.

The local container path requires Git, `tar`, and an NVIDIA GPU exposed through
Docker.
Authenticate Docker to `nvcr.io` with an NGC API key before installing:

```bash
echo "$NGC_API_KEY" | docker login nvcr.io \
  --username '$oauthtoken' --password-stdin
```

`bionemo_install()` pulls the locked NGC PyTorch base image, builds the recipe
image, and runs GPU-backed capability and command probes. It is not a CPU-only
setup step.

```r
library(bionemor)

compute <- bionemo_compute(
  backend = "local",
  engine = "container",
  workspace = normalizePath("~/evo2-work", mustWork = FALSE)
)

plan <- bionemo_install_plan(compute)
compute <- bionemo_install(compute)
bionemo_doctor(compute, target = "all")
```

`bionemo_install_plan()` shows the pinned source, build, and verification steps
without running them. A site-managed recipe environment can instead use
`engine = "external"`. Slurm container jobs require an existing Apptainer image
visible on the shared filesystem.

Automatic image builds are limited to the verified package recipe. A custom
repository, revision, or base image requires `engine = "external"` or an
explicit prebuilt `image`; `bionemo_install()` verifies that image's recipe and
helper labels before use.

## Prepare an Evo 2 checkpoint

MBridge is the runtime checkpoint format. The recommended open-source path
converts an Arc Savanna checkpoint from Hugging Face:

```r
base <- evo2("7b")

checkpoint <- evo2_checkpoint(
  base,
  source = "recommended",
  path = "checkpoints/evo2-7b-mbridge",
  compute = compute
)

model <- evo2("7b", checkpoint = checkpoint)
```

Checkpoint preparation is explicit. Constructing `evo2()` does not download or
load weights. Registration requires the current MBridge distributed-checkpoint
metadata and at least one non-empty weight shard. Recommended tokenizers resolve
to an absolute path in the selected container or external recipe runtime.

## Generate and score sequences

Generation batches all prompts into one recipe invocation and returns a data
frame. The run directory preserves the request JSONL, raw recipe response,
generated FASTA, validation summary, command plan, logs, and provenance.

```r
generated <- evo2_generate(
  model,
  c(first = "ACGTACGTACGT", second = "GCTAGCTAGCTA"),
  compute = compute,
  num_tokens = 64L,
  seed = 1L
)

scores <- evo2_score(
  model,
  c(reference = "ACGTACGTACGT", variant = "ACGTACGTTCGT"),
  compute = compute,
  reduction = "mean",
  strand = "both"
)
```

`predict()` remains a compatibility wrapper for generation, scoring, and
pooled embeddings. Raw PyTorch prediction tensors are not a public result
format.

## Fine-tune with LoRA

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
    precision = "auto"
  ),
  path = "runs/splice-lora",
  async = TRUE
)

job_status(run)
job_logs(run, tail = 50L)
fitted <- job_wait(run)
```

Complete worked examples:

- [Fine-tune Evo 2 and retain the checkpoint](vignettes/evo2-finetune.Rmd)
- [Load an Evo 2 checkpoint for inference](vignettes/evo2-inference.Rmd)

The documented original 1B fine-tuning path uses BF16 even though inference
from that source requires Vortex-style FP8. `train_evo2` does not expose a
Vortex-style training mode, so 20B, 40B, and MBridge checkpoints configured for
that mode fail before launch. FP8 delayed scaling is also unavailable in the
pinned recipe. Full fine-tuning cannot start from a LoRA checkpoint.

Jobs persist their request, plan, state, logs, and outputs under the compute
workspace. `bionemo_job(job_path(run))` reopens a run after the launching R
session exits. Terminal manifests include parallel origin maps that distinguish
values supplied by the user, package defaults, adapter defaults, and values
resolved from the model, checkpoint, data, or compute environment. Checkpoint
trust decisions and original inference-input source metadata remain attached to
downstream runs.

Operational failures inherit from `bionemor_error` and from a stable
code-specific class such as `BN_CHECKPOINT_INCOMPLETE`, `BN_NO_GPU`, or
`BN_TIMEOUT`. The condition's `code` field contains the same value. When
available, conditions also retain the run path, operation, model, checkpoint,
recipe revision, log paths, and upstream exit status.

Generated sequence is model output, not a validated biological design. The
package reports mechanical sequence checks; downstream biological validation
remains the user's responsibility.
