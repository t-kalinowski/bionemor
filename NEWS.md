# bionemor 0.0.0.9000

* Added task-specific generation, sequence scoring, embeddings, dataset
  preparation, and LoRA or full fine-tuning APIs.
* Added `evo2_model()` as the direct path to a ready model backed by the
  recommended dense MBridge checkpoint. Prepared, fitted, and explicitly bound
  custom models retain their compute descriptor, so downstream operations do
  not require it again.
* Made MBridge the native checkpoint format and added explicit Savanna and
  NeMo2 conversion plus Vortex export.
* Added separate inference and fine-tuning precision policies, including the
  documented 1B BF16 fine-tuning path and explicit Vortex-style boundaries.
* Added structured command plans and durable run directories that can be
  reopened after the launching R session exits.
* Added stable `BN_*` condition classes with operation and run context for
  model, runtime, checkpoint, protocol, and output failures.
* Replaced the retired BioNeMo Framework runtime with a pinned BioNeMo Recipes
  Evo 2 runtime built from the upstream recipe Dockerfile.
* Separated generic BioNeMo model classes from Evo 2 adapter classes and model
  registry code.
* Removed deprecated or unsupported API placeholders, old persisted-request
  migration, and per-field value-origin metadata. Resolved requests, plans,
  checkpoint identity, and precision values remain in durable runs.
* Moved billable Brev acceptance tooling out of the installed package and into
  the maintainer validation directory.
