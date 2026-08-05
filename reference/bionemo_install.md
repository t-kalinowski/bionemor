# Install or verify a BioNeMo recipe runtime

`bionemo_install()` makes the runtime described by `compute` ready for
bionemor operations. For the verified local/container configuration, it
can build the package-pinned recipe image. For an explicit local image,
an external environment, or either Slurm engine, it verifies the
existing runtime instead. Installation is independent of model weights:
prepare, select, or attach model weights separately with the
family-specific model functions.

## Usage

``` r
bionemo_install(compute, rebuild = FALSE, pull = TRUE, keep_source = FALSE)
```

## Arguments

- compute:

  A BioNeMo compute descriptor from
  [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md).
  Assign the return value of `bionemo_install()` before using it for
  model operations.

- rebuild:

  Whether to rebuild an existing deterministic local image. This is only
  supported for the package-managed verified recipe image; an explicit
  prebuilt image is always verified in place.

- pull:

  Whether to pull the locked digest-qualified base image before a local
  build. The base-image digest is still verified when `FALSE`.

- keep_source:

  Whether to keep the synthetic local build context after a successful
  build. The content-addressed recipe source checkout remains in the
  workspace recipe cache.

## Value

The supplied `BioNeMoCompute` descriptor with resolved runtime metadata.
For containers this includes `image_digest`; all paths include a
validated capability report in `config$capabilities`.

## Setup lifecycle

A typical setup has two explicit stages:

1.  Create a descriptor with
    [`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md).
    This chooses the backend, engine, workspace, recipe, image, and
    requested resources without probing the runtime.

2.  Call `bionemo_install()` and retain its returned compute descriptor.
    The return value contains the resolved image digest when applicable
    and a validated capability report in `compute@config$capabilities`.

[`bionemo_doctor()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_doctor.md)
can then check the environment for a particular operation group and,
optionally, a particular model checkpoint. Re-run installation after
changing the recipe runtime or image. Use
[`bionemo_capabilities()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_capabilities.md)
with `refresh = TRUE` when only a fresh runtime report is needed.

## What installation does

For the verified local/container path, installation checks for the
deterministic derived image. When it must build, it fetches the exact
locked BioNeMo Recipes revision, verifies the upstream Dockerfile blob,
prepares a synthetic build context containing the package helper,
optionally pulls the digest-qualified base image, verifies that digest,
and builds the image. Existing explicit images are not rebuilt; their
immutable ID and required provenance labels are verified.

Every path performs a GPU-backed helper capability probe and verifies
the commands advertised by the selected recipe. The helper's protocol,
recipe version, and recipe revision must match the compute descriptor.
Local containers are probed by the configured Docker-compatible engine.
Local external runtimes are probed directly.

Slurm installation does not install packages or build an image. It
submits synchronous probe jobs so the checks run in allocations rather
than on the login host. External Slurm environments must already expose
the helper and recipe commands. Slurm/container requires Apptainer and
an existing image; a local SIF is hashed before probing and its digest
is checked again inside each allocation.

## See also

[`bionemo_capabilities()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_capabilities.md),
[`bionemo_doctor()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_doctor.md),
[`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md),
[`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Build and verify the package-pinned local container runtime.
compute <- bionemo_compute(recipe = evo2_recipe(),
  backend = "local",
  engine = "container",
  workspace = "~/evo2-work"
)
compute <- bionemo_install(compute)

# The returned descriptor carries immutable image and capability metadata.
compute@image_digest
compute@config$capabilities$runtime$gpus
bionemo_doctor(compute, target = "inference", verbose = FALSE)
} # }
```
