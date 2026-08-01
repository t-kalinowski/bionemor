# Brev Evo 2 validation

This directory contains maintainer workflows that require billable GPU compute.

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
BIONEMOR_DOCS_WORKSPACE=/home/ubuntu/bionemor-recipes-workspace \
BIONEMOR_DOCS_CHECKPOINT=/home/ubuntu/bionemor-recipes-workspace/checkpoints/evo2-7b-mbridge-recipes-e8e7 \
BIONEMOR_DOCS_GPU='NVIDIA L40S' \
BIONEMOR_DOCS_RENDER_DATE="$capture_date" \
BIONEMOR_DOCS_RUN_ID="docs-$capture_time" \
Rscript tools/render-gpu-docs.R
```

The renderer fails when the source tree is dirty, renders into a staging
directory, verifies that GPU-backed documents contain captured output, and
then replaces `README.md` and three static package vignettes. The generated
vignettes contain ordinary fenced R code and output, not executable knitr
chunks, so package installation and vignette rendering do not require a GPU.

If an existing image fails its helper-label check after the package helper
changes, rebuild it once with `bionemo_install(compute, rebuild = TRUE)` before
rendering again.

The README and `bionemor` article run dense generation, scoring, and pooled
embeddings. The fine-tuning article prepares fixed 127-base synthetic inputs,
runs two BF16 rank-4 LoRA optimizer steps, and checks fitted scoring and
generation. The Slurm article is rendered as reference material but is not
executed on Brev.
