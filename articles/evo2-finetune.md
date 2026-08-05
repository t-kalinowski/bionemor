# Fine-tune Evo 2 with LoRA

This article adapts Evo 2 to a DNA sequence collection with low-rank
adaptation (LoRA), saves the fitted checkpoint, and uses it for scoring
and generation. LoRA trains small adapter matrices while leaving most of
the base model unchanged, so it requires less GPU memory than updating
every model weight.

Complete the setup in
[`vignette("bionemor")`](https://t-kalinowski.github.io/bionemor/articles/bionemor.md)
first. Fine-tuning still requires an NVIDIA CUDA GPU, an installed
BioNeMo Recipes runtime, and a prepared Evo 2 checkpoint. The example
below uses repeated toy sequences and only two optimizer steps to
exercise the workflow. It does not produce a useful biological model.

The output was captured on 2026-08-01 with an NVIDIA L40S using package
revision 7da249b5f346. The executable source is
`vignettes-src/evo2-finetune.Rmd`; the package vignette is pre-rendered
so it can be read without a GPU.

## Set up the runtime and base model

``` r

library(bionemor)

workspace <- normalizePath(
  Sys.getenv("BIONEMOR_DOCS_WORKSPACE", "~/evo2-work"),
  mustWork = FALSE
)
run_id <- Sys.getenv(
  "BIONEMOR_DOCS_RUN_ID",
  format(Sys.time(), "docs-%Y%m%d-%H%M%S")
)

compute <- bionemo_compute(
  recipe = evo2_recipe(),
  backend = "local",
  engine = "container",
  workspace = workspace
)
compute <- bionemo_install(compute)
```

Prepare the recommended checkpoint once and reuse it from the workspace:

``` r

model <- evo2_model("7b", compute)
```

## Preprocess a tiny dataset

[`evo2_dataset()`](https://t-kalinowski.github.io/bionemor/reference/evo2_dataset.md)
accepts named sequence vectors or FASTA files. Supply independent
validation and test sequences so training and evaluation do not measure
memorization of closely related inputs.

[`evo2_preprocess()`](https://t-kalinowski.github.io/bionemor/reference/evo2_preprocess.md)
is specifically a training-data preprocessing step. It writes the
sequence partitions as FASTA, runs the Evo 2 indexing command, and
returns an `Evo2Dataset` that points to the resulting `.bin` and `.idx`
files. It does not prepare model weights, fit the model, or run
inference. Calling it explicitly lets you customize preprocessing or
reuse the same indexed data across fitting runs.
[`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md)
performs this step automatically when given raw data.

The nucleotide tokenizer uses one token per base. Each toy sequence
below has 127 bases; preprocessing appends an end-of-document token to
create the 128-token samples requested by the fitting control.

``` r

sequence_127 <- function(pattern) {
  stopifnot(is.character(pattern), length(pattern) == 1L, nzchar(pattern))
  substr(strrep(pattern, ceiling(127 / nchar(pattern))), 1L, 127L)
}

train <- c(
  train_1 = sequence_127("ACGT"),
  train_2 = sequence_127("TGCA"),
  train_3 = sequence_127("GATTACA"),
  train_4 = sequence_127("CCGTA")
)
validation <- c(validation_1 = sequence_127("AGCT"))
test <- c(test_1 = sequence_127("TGCAT"))

data <- evo2_dataset(
  train = train,
  validation = validation,
  test = test
)

prepared <- evo2_preprocess(
  data,
  model,
  path = file.path("datasets", paste0(run_id, "-tiny-evo2-128")),
  control = evo2_preprocess_control(
    append_eod = TRUE,
    sample_length = 128L,
    workers = 1L,
    seed = 17L
  ),
  overwrite = TRUE
)

data.frame(
  split = names(prepared@manifest$inputs),
  records = vapply(
    prepared@manifest$inputs,
    function(input) input$records,
    integer(1L)
  ),
  row.names = NULL
)
#>        split records
#> 1      train       4
#> 2 validation       1
#> 3       test       1
```

Explicit validation and test inputs keep this small example
deterministic. For real work, choose held-out splits that reflect the
biological question and avoid leakage between related sequences.

## Choose LoRA targets

[`evo2_lora()`](https://t-kalinowski.github.io/bionemor/reference/evo2_lora.md)
describes which parts of the model receive trainable adapters:

- `"hyena"` selects the Hyena sequence-mixing blocks.
- `"attention"` selects attention blocks.
- `"mlp"` selects feed-forward blocks.

These are adapter locations, not response variables. `rank` controls
adapter capacity, and the effective adapter scale is `alpha / rank`.

``` r

method <- evo2_lora(
  rank = 4L,
  alpha = 8,
  dropout = 0,
  targets = c("hyena", "attention", "mlp")
)
method
#> <bionemor::Evo2LoRA>
#>  @ kind           : chr "lora"
#>  @ rank           : int 4
#>  @ alpha          : num 8
#>  @ dropout        : num 0
#>  @ targets        : chr [1:3] "hyena" "attention" "mlp"
#>  @ fully_trainable: chr(0)
```

## Fine-tune the model

``` r

run <- evo2_finetune(
  model,
  prepared,
  steps = 2L,
  method = method,
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
  path = file.path("artifacts", run_id, "vignette-lora"),
  name = paste0(run_id, "-vignette-lora"),
  async = TRUE,
  timeout = 3600
)
run
#> <bionemor_job>
#> ID: docs-20260801-7da249b-vignette-lora
#> Kind: fine-tune
#> State: starting
#> Path: /home/ubuntu/bionemor-recipes-workspace/.bionemor/runs/docs-20260801-7da249b-vignette-lora

fitted <- job_wait(run, timeout = 3600)
job_status(run, refresh = TRUE)
#> [1] "succeeded"
fitted
#> <Evo 2 model>
#> Size:       7B
#> Context:    1,048,576 nt
#> Checkpoint: MBridge at /home/ubuntu/bionemor-recipes-workspace/artifacts/docs-20260801-7da249b/vignette-lora/docs-20260801-7da249b-vignette-lora/checkpoints
#> Recipe:     BioNeMo Evo 2 2.4 @ e8e7f597
#> Ready:      yes
#> Compute:    local/container
```

With `async = TRUE`,
[`evo2_finetune()`](https://t-kalinowski.github.io/bionemor/reference/evo2_finetune.md)
returns a job immediately. The job is saved on disk, so it can be
monitored or reopened in another R session.
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
returns a fitted `Evo2Model` that points to the saved checkpoint and
retains the compute descriptor. A wait timeout does not cancel training.

## Inspect and reuse the fitted checkpoint

``` r

fitted_checkpoint <- checkpoint_path(fitted)
manifest <- checkpoint_manifest(fitted)

data.frame(
  format = manifest$format,
  method = manifest$kind,
  base_checkpoint = basename(manifest$base_checkpoint_path)
)
#>    format method base_checkpoint
#> 1 mbridge   lora    iter_0000001
```

Keep the checkpoint root returned by
[`checkpoint_path()`](https://t-kalinowski.github.io/bionemor/reference/checkpoint_metadata.md),
which contains `bionemor-checkpoint.json`. A LoRA checkpoint records its
dense base checkpoint and requires that base to remain available. Pass
`fitted` directly to inference functions while working in the same R
session.

``` r

probe <- c(probe = sequence_127("ACGTGCAA"))

fitted_scores <- evo2_score(
  fitted,
  probe,
  reduction = "mean",
  strand = "forward"
)
fitted_scores[c("id", "score", "forward_score")]
#>      id      score forward_score
#> 1 probe -0.1705485    -0.1705485

fitted_generation <- evo2_generate(
  fitted,
  probe,
  num_tokens = 8L,
  seed = 17L
)
fitted_generation[c("input_id", "prompt", "completion", "finish_reason")]
#>   input_id
#> 1    probe
#>                                                                                                                            prompt
#> 1 ACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCAAACGTGCA
#>   completion finish_reason
#> 1   AACGTGCA        length
```

These calls confirm that the adapter and dense base load together and
return well-formed results. The values only validate the workflow; two
toy training steps are not evidence of improved biological performance.

## Use the fitted model in a fresh R session

Save the training job’s path, then pass that path to
[`bionemo_job()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_job.md)
in another session.
[`job_wait()`](https://t-kalinowski.github.io/bionemor/reference/job_wait.md)
waits if training is still active and otherwise reconstructs the fitted
model from its saved checkpoint and compute descriptor.

The following [`callr::r()`](https://callr.r-lib.org/reference/r.html)
call starts a separate R process and scores a sequence with the fitted
model:

``` r

fit_job_path <- job_path(run)

fresh_scores <- callr::r(
  function(path, probe) {
    library(bionemor)

    # In a new R session:
    fitted <- job_wait(bionemo_job(path), timeout = 3600)
    evo2_score(
      fitted,
      probe,
      reduction = "mean",
      strand = "forward"
    )
  },
  args = list(path = fit_job_path, probe = probe)
)
fresh_scores
```

Keep the run directory, fitted checkpoint, and—for LoRA—the dense base
checkpoint referenced by its manifest. A separate machine also needs
access to those paths and to a compatible recipe runtime.
