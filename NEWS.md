# bionemor 0.0.0.9000

* Replaced the retired BioNeMo Framework runtime with a pinned BioNeMo Recipes
  Evo 2 runtime built from the upstream recipe Dockerfile.
* Made MBridge the native checkpoint format and added explicit Savanna and
  NeMo2 conversion plus Vortex export.
* Added separate inference and fine-tuning precision policies, including the
  documented 1B BF16 fine-tuning path and explicit Vortex-style boundaries.
* Added task-specific generation, sequence scoring, embeddings, dataset
  preparation, and LoRA or full fine-tuning APIs.
* Added `evo2_model()` as the direct path to a ready model backed by the
  recommended dense MBridge checkpoint.
* Made install plans inspectable as ordered data frames with redacted,
  shell-quoted commands.
* Added structured command plans and durable run directories that can be
  reopened after the launching R session exits.
* Added manifest value-origin maps and propagated checkpoint trust and input
  source provenance into downstream runs.
* Added stable `BN_*` condition classes with operation and run context for
  model, runtime, checkpoint, protocol, and output failures.
* Kept Python inside the external recipe runtime: R users work with R inputs,
  durable jobs, and portable results without interacting with Python objects.
  NIM remains outside this package.
