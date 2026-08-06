# Brev GPU validation

This directory contains maintainer workflows that require billable GPU compute.

## Package tests on an existing GPU instance

The regular test suite uses a small local runtime substitute. The global GPU
gate exercises the installed Evo 2 and ESM-2 helpers, including their real
pooled embedding writers. From the repository root on a fresh instance, install
the package's development dependencies and `devtools`:

```bash
Rscript --vanilla -e \
  'pak::local_install_dev_deps(); pak::pkg_install("devtools")'
```

Then run the package tests with current images and an Evo 2 checkpoint:

```bash
BIONEMOR_TEST_GPU=true \
BIONEMOR_EVO2_IMAGE=sha256:<evo2-image-id> \
BIONEMOR_EVO2_WORKSPACE=/home/ubuntu/workspace/bionemor \
BIONEMOR_EVO2_CHECKPOINT=/home/ubuntu/workspace/bionemor/checkpoints/evo2-7b-mbridge-recipes-e8e7 \
BIONEMOR_ESM2_IMAGE=sha256:<esm2-image-id> \
BIONEMOR_ESM2_WORKSPACE=/home/ubuntu/workspace/bionemor \
R -q -e 'devtools::test()'
```

Set `BIONEMOR_EVO2_MODEL` when the checkpoint is not the default `"7b"` model.
The ESM-2 test uses the pinned public `"8m"` model, so network access or a warm
model cache is required. Both images must use bridge protocol 2 from the
current package sources. Build or rebuild the package-managed Evo 2 and ESM-2
computes first, then supply their immutable image IDs above. The explicit
images used by these tests are verification-only and cannot be rebuilt. The
tests do not provision or start an instance. Without `BIONEMOR_TEST_GPU=true`,
they skip the real runtimes and run the local suite.

## Acceptance capture

Dated subdirectories are produced by the opt-in
`validation/brev-evo2/scripts/brev-evo2-run.sh --run` workflow. Each capture must come from a
completed mechanical acceptance run and contain its full-precision evidence
record, compact outputs, redacted terminal manifests, and retained LoRA
inspection metadata.

The workflow provisions billable compute only when invoked explicitly. It
keeps the existing quick inference smoke test, then runs scoring, generation,
pooled embeddings, deterministic preprocessing, two BF16 rank-4 LoRA optimizer
steps, and fitted scoring and generation. It copies the evidence locally before
stopping the instance.

Do not add illustrative output here. A capture documents one synthetic
end-to-end run; it is not a benchmark or a biological-quality evaluation.

## README and vignette output

The executable documentation lives in `README.Rmd` and `vignettes-src/`.
Render it only in the target recipe environment, after committing the sources:

```bash
R CMD INSTALL .
capture_date="$(date -u +%F)"
capture_time="$(date -u +%Y%m%d-%H%M%S)"
BIONEMOR_DOCS_RENDER=1 \
BIONEMOR_DOCS_WORKSPACE=/home/ubuntu/workspace/bionemor \
BIONEMOR_DOCS_CHECKPOINT=/home/ubuntu/workspace/bionemor/checkpoints/evo2-7b-mbridge-recipes-e8e7 \
BIONEMOR_DOCS_GPU='NVIDIA L40S' \
BIONEMOR_DOCS_RENDER_DATE="$capture_date" \
BIONEMOR_DOCS_RUN_ID="docs-$capture_time" \
Rscript tools/render-gpu-docs.R
```

The renderer fails when the source tree is dirty, renders into a staging
directory, verifies that GPU-backed documents contain captured output, and
then replaces `README.md` and two static package vignettes. The generated
vignettes contain ordinary fenced R code and output, not executable knitr
chunks, so package installation and vignette rendering do not require a GPU.

If an existing package-managed image fails its helper-label check after the
package helper changes, rebuild it once with
`bionemo_install(compute, rebuild = TRUE)` before rendering again.

The README and `bionemor` article run Evo 2 generation, scoring, and embeddings,
then run ESM-2 protein embeddings through its separate pinned runtime. The
fine-tuning article preprocesses fixed 127-base synthetic inputs, runs two BF16
rank-4 LoRA optimizer steps, and checks fitted scoring and generation.
