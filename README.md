# bionemor

`bionemor` prepares checkpoints, fits models, and runs batch inference with
NVIDIA BioNeMo Framework from R. BioNeMo runs as a versioned external compute
system; the package does not expose Python trainer or tensor objects.

The initial implementation supports Evo 2 with BioNeMo Framework 2.6.3.

| Model | Sizes | Profile | Checkpoints | Operations |
| --- | --- | --- | --- | --- |
| Evo 2 | 1B, 7B, 40B | `bionemo-2.6.3` | Hugging Face, NGC, local | full fit, generation, score, raw logits |

The package installs and loads without Python or a GPU. Runtime diagnostics are
available through `bionemo_doctor()`.

## NGC-free container workflow

The BioNeMo Framework image and the public Evo 2 checkpoint below do not
require an NGC API key:

```r
library(bionemor)

compute <- bionemo_compute(
  backend = "local",
  engine = "container",
  image = "nvcr.io/nvidia/clara/bionemo-framework:2.6.3",
  workspace = "/home/ubuntu/bionemor-workspace"
)

spec <- evo2("1b")
base_checkpoint <- evo2_checkpoint(
  spec,
  source = "hf://arcinstitute/savanna_evo2_1b_base",
  path = "checkpoints/evo2-1b-8k",
  compute = compute
)
model <- evo2("1b", checkpoint = base_checkpoint)

baseline <- predict(
  model,
  "ACGTACGTACGT",
  type = "response",
  compute = compute,
  num_tokens = 8L
)

fitted <- generics::fit(
  model,
  data = c("ACGTACGTACGT", "TGCATGCATGCA"),
  compute = compute,
  steps = 3L,
  timeout = 300
)

after <- predict(
  fitted,
  "ACGTACGTACGT",
  type = "response",
  compute = compute,
  num_tokens = 8L
)
```

Checkpoint preparation is explicit. `fit()` and `predict()` never select or
download another checkpoint.

Entitled NGC checkpoints are also explicit. Set `NGC_CLI_API_KEY` (or
`NGC_API_KEY`) and use an `ngc://` resource:

```r
checkpoint <- evo2_checkpoint(
  evo2("7b"),
  source = "ngc://evo2/7b-1m:1.0",
  path = "checkpoints/evo2-7b-1m-ngc",
  compute = compute
)
```

The credential is passed only to the checkpoint preparation process. It is not
stored in commands, provenance, manifests, or logs.

## Execution boundary

The ordinary path is:

```text
R objects and FASTA
  -> BioNeMo 2.6.3 CLI commands
  -> external Python, Docker, or Slurm process
  -> checkpoints and prediction files
  -> typed R results
```

Generated sequences and sequence scores are materialized as R values. Raw
logits remain a `BioNeMoArtifact` pointing to a PyTorch file.

See the package vignettes for local/container setup, Slurm, the tested Brev
L40S workflow, and the optional plain-path handoff to `nimr`.
