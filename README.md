

# bionemor

`bionemor` prepares Evo 2 models, runs batch inference, and fine-tunes with
NVIDIA BioNeMo Recipes without leaving R. Inputs and results are R vectors,
data frames, matrices, and durable job objects.

The main workflow is:

1. Configure and verify a pinned recipe runtime once.
2. Prepare or reuse the recommended checkpoint with `evo2_model()`.
3. Generate, score, embed, or fine-tune DNA sequences.

The output below was rendered on 2026-07-31 with an NVIDIA L40S using
package revision df19000ec8ed. See
[`vignettes-src/`](vignettes-src/) and
[`tools/render-gpu-docs.R`](tools/render-gpu-docs.R) for the executable sources
and manual render command.

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

Then inspect the setup plan, install the runtime, and verify it:


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

plan <- bionemo_install_plan(compute)
plan
#> <BioNeMo install plan>
#> Target: install
#> Steps:  17
#> Status: planned
#> 1. source-init: create a content-addressed BioNeMo Recipes checkout
#> 2. source-fetch: fetch the exact locked recipe revision
#> 3. source-checkout: check out the fetched recipe revision
#> 4. dockerfile-verify: verify the official locked recipe Dockerfile
#> 5. base-image-pull: pull the official NGC PyTorch base image
#> 6. base-image-verify: verify the locked base-image digest
#> 7. image-build: build the official recipe with the package helper appended
#> 8. image-inspect: resolve the built image ID
#> 9. runtime-capabilities: verify the helper protocol and current recipe commands
#> 10. probe-infer_evo2: verify infer_evo2
#> 11. probe-predict_evo2: verify predict_evo2
#> 12. probe-preprocess_evo2: verify preprocess_evo2
#> 13. probe-train_evo2: verify train_evo2
#> 14. probe-evo2_convert_savanna_to_mbridge: verify evo2_convert_savanna_to_mbridge
#> 15. probe-evo2_convert_nemo2_to_mbridge: verify evo2_convert_nemo2_to_mbridge
#> 16. probe-evo2_export_mbridge_to_vortex: verify evo2_export_mbridge_to_vortex
#> 17. probe-evo2_remove_optimizer: verify evo2_remove_optimizer

compute <- bionemo_install(compute)
doctor <- bionemo_doctor(compute, target = "all", verbose = FALSE)
stopifnot(doctor@ok)
doctor
#> <BioNeMo doctor>
#> Target: all
#> Status: pass
#>                            check status
#>                          backend   pass
#>                       host tools   pass
#>                        workspace   pass
#>                  helper protocol   pass
#>                           recipe   pass
#>                   runtime Python   pass
#>                  runtime PyTorch   pass
#>                     runtime CUDA   pass
#>       runtime Transformer Engine   pass
#>          runtime Megatron Bridge   pass
#>                  runtime BioNeMo   pass
#>                              GPU   pass
#>                            image   pass
#>                       base image   pass
#>        container GPU passthrough   pass
#>                       infer_evo2   pass
#>                     predict_evo2   pass
#>                  preprocess_evo2   pass
#>                       train_evo2   pass
#>  evo2_convert_savanna_to_mbridge   pass
#>    evo2_convert_nemo2_to_mbridge   pass
#>    evo2_export_mbridge_to_vortex   pass
#>            evo2_remove_optimizer   pass
#>                                                                                                    detail
#>                                                                                      docker are available
#>                                                                           bash, awk, mkfifo are available
#>                                                                   /home/ubuntu/bionemor-recipes-workspace
#>                                                                                                protocol 1
#>                                                                                    recipe 2.4 at e8e7f597
#>                                                                                                    3.12.3
#>                                                                               2.13.0a0+8145d630e8.nv26.06
#>                                                                                                      13.3
#>                                                                                           2.16.0+4220403e
#>                                                                                                     0.4.1
#>                                                                                          import available
#>                                                        NVIDIA L40S compute 8.9, 44.4 GiB driver 610.43.02
#>        bionemor/evo2:e8e7f597363c sha256:83328bd1c26aa314548c10b3cee0af567d7c7ebbf3e9d9f4b5bc32cafc345789
#>  nvcr.io/nvidia/pytorch:26.06-py3 sha256:abd110b23600e877173dafc3078385b7c13ddacd7e0c6a6acb0a864586d59622
#>                                                                                  CUDA devices are visible
#>                                                                                                 available
#>                                                                                                 available
#>                                                                                                 available
#>                                                                                                 available
#>                                                                                                 available
#>                                                                                                 available
#>                                                                                                 available
#>                                                                                                 available
```

`bionemo_install()` performs GPU-backed capability and command probes. A
site-managed recipe environment can use `engine = "external"`.

## Prepare a model

`evo2_models()` lists the registry and compatibility metadata without preparing
weights:


``` r
models <- evo2_models(compute, compatible = TRUE)
models[models$name %in% c("1b-base", "7b"), c(
  "name", "parameters", "context_length", "compatible", "download_size"
)]
#>   name parameters context_length compatible download_size
#> 2   7b      7e+09        1048576       TRUE      1.58e+10
```

`evo2_model()` prepares the recommended dense MBridge checkpoint on its first
call and reuses a complete matching checkpoint later:

```r
model <- evo2_model("7b", compute)
```

An already prepared custom checkpoint can be attached and compute-bound without
copying or converting it. The rendered capture uses this path:


``` r
checkpoint <- Sys.getenv("BIONEMOR_DOCS_CHECKPOINT")
stopifnot(dir.exists(checkpoint))
model <- evo2(
  "7b",
  checkpoint = checkpoint,
  compute = compute
)
model
#> <Evo 2 model>
#> Size:       7B
#> Context:    1,048,576 nt
#> Checkpoint: MBridge at /home/ubuntu/bionemor-recipes-workspace/checkpoints/evo2-7b-mbridge-recipes-e8e7
#> Recipe:     BioNeMo Evo 2 2.4 @ e8e7f597
#> Ready:      yes
#> Compute:    local/container
```

The model now retains its compute descriptor. Inference and fine-tuning calls
therefore do not need a repeated `compute` argument. The separation still
matters at construction: a model describes architecture and weights, while
`bionemo_compute()` describes the runtime, workspace, backend, recipe revision,
and GPU allocation. `evo2_model()` couples the recommended checkpoint and
compute; `evo2(..., compute = compute)` does the same for a custom checkpoint.

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
scores[c("id", "score", "forward_score", "reverse_score")]
#>          id     score forward_score reverse_score
#> 1 reference -1.408769     -1.408769     -1.408769
#> 2   variant -1.411028     -1.422442     -1.399613

embeddings <- evo2_embed(
  model,
  sequences,
  pool = "mean",
  strand = "both"
)
dim(embeddings)
#> [1]    2 4096
round(embeddings[, 1:4, drop = FALSE], 4)
#>                 dim_1        dim_2       dim_3       dim_4
#> reference 41006313472 -25541912576 25635586048 14001286144
#> variant   41308303360 -25804754944 26007482368 14023655424
```

Scores are reduced token log probabilities; higher values indicate greater
model likelihood. With `strand = "both"`, forward and reverse values are
reduced independently and averaged. Embedding row names preserve the input IDs.

## Fine-tune with LoRA

Raw R sequence vectors are prepared automatically. This small run exists to
exercise the API; two optimizer steps on repeated synthetic sequences do not
produce a useful biological model.


``` r
sequence_127 <- function(pattern) {
  substr(strrep(pattern, ceiling(127 / nchar(pattern))), 1L, 127L)
}

data <- evo2_dataset(
  train = c(
    train_1 = sequence_127("ACGT"),
    train_2 = sequence_127("TGCA"),
    train_3 = sequence_127("GATTACA"),
    train_4 = sequence_127("CCGTA")
  ),
  validation = c(validation_1 = sequence_127("AGCT")),
  test = c(test_1 = sequence_127("TGCAT"))
)
run_id <- Sys.getenv(
  "BIONEMOR_DOCS_RUN_ID",
  format(Sys.time(), "docs-%Y%m%d-%H%M%S")
)

run <- evo2_finetune(
  model,
  data,
  steps = 2L,
  method = evo2_lora(
    rank = 4L,
    alpha = 8,
    dropout = 0,
    targets = c("hyena", "attention", "mlp")
  ),
  control = evo2_fit_control(
    sequence_length = 128L,
    global_batch_size = 1L,
    micro_batch_size = 1L,
    learning_rate = 1e-4,
    minimum_learning_rate = 0,
    warmup_steps = 0L,
    decay_steps = 2L,
    constant_steps = 0L,
    eval_interval = 1L,
    eval_iters = 1L,
    log_interval = 1L,
    precision = "bf16",
    keep_checkpoints = 1L,
    workers = 1L,
    seed = 17L,
    dataset_seed = 17L
  ),
  path = file.path("artifacts", run_id, "readme-lora"),
  name = paste0(run_id, "-readme-lora"),
  async = TRUE,
  timeout = 3600
)

fitted <- job_wait(run, timeout = 3600)
job_status(run, refresh = TRUE)
#> [1] "succeeded"
fitted
#> <Evo 2 model>
#> Size:       7B
#> Context:    1,048,576 nt
#> Checkpoint: MBridge at /home/ubuntu/bionemor-recipes-workspace/artifacts/docs-20260731-175839/readme-lora/docs-20260731-175839-readme-lora/checkpoints
#> Recipe:     BioNeMo Evo 2 2.4 @ e8e7f597
#> Ready:      yes
#> Compute:    local/container

manifest <- checkpoint_manifest(fitted)
data.frame(
  kind = manifest$kind,
  format = manifest$format,
  base_checkpoint = basename(manifest$base_checkpoint_path),
  recipe_revision = substr(manifest$recipe_revision, 1L, 12L)
)
#>   kind  format base_checkpoint recipe_revision
#> 1 lora mbridge    iter_0000001    e8e7f597363c
```

`targets` selects module groups in every matching model layer. `"hyena"`
expands to `dense_projection` and `dense`; `"attention"` expands to
`linear_qkv` and `linear_proj`; and `"mlp"` expands to `linear_fc1` and
`linear_fc2`. The effective LoRA scale is `alpha / rank`.

The fitted result is another compute-bound `Evo2Model`:


``` r
probe <- c(probe = sequence_127("ACGTGCAA"))

evo2_score(
  fitted,
  probe,
  reduction = "mean",
  strand = "forward"
)[c("id", "score")]
#>      id      score
#> 1 probe -0.1700051

evo2_generate(
  fitted,
  probe,
  num_tokens = 8L,
  seed = 17L
)[c("input_id", "prompt", "completion", "finish_reason")]
#>   input_id
#> 1    probe
#>                                                                                                                            prompt
#> 1 ACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCA
#>   completion finish_reason
#> 1   AACGTGCA        length
```

The LoRA checkpoint records its dense base checkpoint. Keep both paths
available for later inference.

## Vignettes and durable runs

The package includes three articles:

- [Evo 2 workflows from R](vignettes/bionemor.Rmd)
- [Fine-tune Evo 2 and retain the checkpoint](vignettes/evo2-finetune.Rmd)
- [Run BioNeMo Recipes jobs with Slurm](vignettes/slurm.Rmd)

The first two contain output captured in the target GPU environment. The Slurm
article remains a reference because scheduler behavior must be validated on the
target cluster. Slurm support is experimental.

Every operation stores its request, command plan, state, logs, outputs, and
provenance under the compute workspace. Reopen a run after the launching R
session exits:

```r
same_run <- bionemo_job(job_path(run))
job_status(same_run)
result <- job_wait(same_run)
```

Generated sequence is model output, not a validated biological design. The
package reports mechanical sequence checks; downstream biological validation
remains the user's responsibility.
