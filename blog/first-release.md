# bionemor: biological foundation models from R

We are releasing the first version of bionemor, an R package for running biological foundation models on NVIDIA GPUs. It is intended for bioinformaticians whose sequence data and downstream analysis already live in R, including users of Bioconductor packages such as Biostrings.

Evo 2 operations use programs from NVIDIA BioNeMo Recipes. ESM-2 embeddings use a helper supplied by bionemor and built on Transformers and Transformer Engine. bionemor configures the version-pinned GPU runtime for each model family, runs the selected operation, and returns its results to R.

The first release supports Evo 2 operations on DNA and ESM-2 protein embeddings. Evo 2 support includes generation, scoring, positional profiles, embeddings, dataset preprocessing, and LoRA or full fine-tuning. ESM-2 support provides pooled protein embeddings for similarity, clustering, and downstream modeling.

## A small Evo 2 example

The package separates the model from the environment where it runs. A recipe selects a pinned model-specific runtime. A compute descriptor combines that recipe with a workspace and execution settings. The model descriptor identifies the architecture and checkpoint; it does not load the weights into the R process. For local container execution, bionemor uses Docker by default.

```r
library(bionemor)

compute <- bionemo_compute(
  recipe = evo2_recipe(),
  backend = "local",
  engine = "container",
  workspace = "~/bionemor-work"
)
compute <- bionemo_install(compute)

model <- evo2_model("7b", compute)

dna <- Biostrings::DNAStringSet(c(
  reference = "ACGTACGTACGT",
  variant = "ACGTACGTTCGT"
))

scores <- evo2_score(
  model,
  dna,
  reduction = "mean",
  strand = "both"
)
scores
```

Megatron Bridge is the training and inference stack used by the pinned Evo 2 recipe. MBridge is its checkpoint format. `evo2_model()` downloads the registered weights when needed, converts them in the selected runtime, stores the checkpoint in the compute workspace, and returns an R model descriptor that points to it.

`evo2_score()` returns a data frame, and the embedding functions return ordinary numeric matrices. These results can move directly into familiar R and Bioconductor analyses. Each operation also records its inputs, logs, outputs, and provenance in a durable run directory.

Model operations require a supported CUDA-capable NVIDIA GPU. The package can still be installed without one to inspect recipes and model metadata.

## Fine-tuning and reuse

Evo 2 fine-tuning follows the same interface. `evo2_dataset()` describes the training, validation, and test sequences. `evo2_preprocess()` converts those sequences into the indexed dataset consumed by the pinned Evo 2 training program. By default, `evo2_finetune()` returns a durable job, and `job_wait()` waits for that job and returns a fitted model backed by a saved checkpoint.

Longer operations can return a durable job immediately. Saving its path is enough to reopen it in a later R session:

```r
sequence_127 <- function(pattern) {
  substr(strrep(pattern, ceiling(127 / nchar(pattern))), 1L, 127L)
}

data <- evo2_dataset(
  train = Biostrings::DNAStringSet(c(
    train_1 = sequence_127("ACGT"),
    train_2 = sequence_127("TGCA")
  )),
  validation = Biostrings::DNAStringSet(c(
    validation_1 = sequence_127("AGCT")
  )),
  test = Biostrings::DNAStringSet(c(
    test_1 = sequence_127("TGCAT")
  ))
)
prepared <- evo2_preprocess(
  data,
  model,
  path = "datasets/my-study",
  control = evo2_preprocess_control(sample_length = 128L)
)

run <- evo2_finetune(
  model,
  prepared,
  steps = 100L,
  control = evo2_fit_control(sequence_length = 128L),
  async = TRUE
)
path <- job_path(run)

# In a new R session:
run <- bionemo_job(path)
fitted <- job_wait(run)
```

The fitted checkpoint can then be passed to the same scoring, generation, and embedding functions as the base model.

## Protein embeddings with ESM-2

`bionemor` also supports ESM-2, a protein language model used to create numerical representations of protein sequences. It follows the same model-descriptor, compute, and durable-job interface as the Evo 2 examples:

```r
esm2_compute <- bionemo_compute(
  recipe = esm2_recipe(),
  backend = "local",
  engine = "container",
  workspace = "~/bionemor-work/esm2"
)
esm2_compute <- bionemo_install(esm2_compute)
protein_model <- esm2_model("8m", esm2_compute)

proteins <- Biostrings::AAStringSet(c(
  protein_1 = "MKTAYIAKQRQISFVKSHFSRQ",
  protein_2 = "GAVLILKKKGHHEAELKPLAQSHATK"
))
protein_embeddings <- esm2_embed(protein_model, proteins)
```

The rows of `protein_embeddings` can be used for sequence similarity, clustering, or as features in downstream R models. They are model representations, not measurements of protein function.

## One R interface for DNA and protein models

`bionemor` brings Evo 2 DNA operations and ESM-2 protein embeddings into one R package. Both use the same execution lifecycle: select a pinned runtime, describe compute, stage sequence inputs, run work on a GPU, retain jobs and checkpoints, and return portable R results.

The model-specific functions remain explicit through names such as `evo2_score()` and `esm2_embed()`. The package handles the compute, job, provenance, and result lifecycle shared by both model families.

## Install bionemor

The package is available from GitHub:

```r
pak::pak("t-kalinowski/bionemor")
```

See the [getting-started guide](https://t-kalinowski.github.io/bionemor/articles/bionemor.html) for GPU setup and complete examples. Generated sequences and scores are model outputs; their biological interpretation and validation remain part of the downstream analysis.
