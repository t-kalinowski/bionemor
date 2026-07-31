# Brev Evo 2 validation

Dated subdirectories here are produced by the opt-in
`inst/scripts/brev-evo2-run.sh --run` workflow. Each capture must come from a
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
