# Describe where BioNeMo operations run

A compute descriptor records where commands run, how the BioNeMo recipe
runtime is supplied, which workspace is shared with that runtime, and
which GPU or scheduler resources to request. Constructing the descriptor
creates the workspace if needed, but does not install software, start a
container, probe a GPU, or submit a Slurm job.

## Usage

``` r
bionemo_compute(
  recipe,
  backend = c("local", "slurm"),
  engine = c("container", "external"),
  workspace = getwd(),
  image = NULL,
  gpus = 1L,
  queue = NULL,
  account = NULL,
  walltime = NULL,
  config = list()
)
```

## Arguments

- recipe:

  Recipe descriptor such as
  [`evo2_recipe()`](https://t-kalinowski.github.io/bionemor/reference/evo2_recipe.md)
  or
  [`esm2_recipe()`](https://t-kalinowski.github.io/bionemor/reference/esm2_recipe.md).

- backend:

  Where commands are launched: `"local"` or `"slurm"`.

- engine:

  How the recipe runtime is supplied: `"container"` or `"external"`.

- workspace:

  Writable workspace used by R and the runtime. It must not be the
  filesystem root. Slurm requires the same absolute path on the
  submitting host and compute nodes.

- image:

  Container image. For local/container execution, `NULL` or the verified
  recipe's base-image tag or digest-qualified reference selects the
  deterministic derived recipe image; another reference selects a
  prebuilt recipe image. Slurm/container requires a readable SIF on
  shared storage or a digest-qualified image URI. Unverified recipes
  require an explicit image unless `engine = "external"`.

- gpus:

  Positive integer GPU count.

- queue, account, walltime:

  Optional Slurm partition, account, and time limit. These values are
  passed to `sbatch` without interpretation.

- config:

  Named list of site-specific settings. Use `container_engine` to
  replace the default `"docker"` command for local containers.

## Value

An S7 `BioNeMoCompute` descriptor. The returned object describes
execution but is not installed or verified until
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
runs.

## Models and compute

A model and compute have separate roles. A family constructor such as
[`evo2()`](https://t-kalinowski.github.io/bionemor/reference/evo2.md) or
[`esm2()`](https://t-kalinowski.github.io/bionemor/reference/esm2.md)
describes model identity, checkpoint or source, and model-level
settings. `bionemo_compute()` describes the execution environment and
its recipe. An operation uses the supplied compute descriptor or one
bound to a model by
[`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md)
or
[`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md).
Keeping them separate lets a runtime be installed and diagnosed before a
model is prepared and lets an offline model descriptor run in more than
one compatible environment.

## Backends and engines

`backend` selects where bionemor launches commands:

- `"local"` launches them from the current machine.

- `"slurm"` writes a job script, submits it with `sbatch`, reads its
  state with `sacct`, and cancels it with `scancel`. Slurm support is
  experimental; please report problems at the package's issue tracker.

`engine` selects how the recipe runtime is provided:

- `"container"` runs commands in an image. Local execution uses Docker
  by default; set `config = list(container_engine = "podman")` to use
  another Docker-compatible command. Slurm execution uses Apptainer and
  requires an existing SIF path or digest-qualified image URI. Apptainer
  support is experimental; please report problems through the package
  issue tracker.

- `"external"` runs the recipe commands and package helper directly in
  an environment managed outside bionemor. Those commands must be on the
  execution environment's `PATH`. `image` must be `NULL`.

bionemor can build the verified recipe image only for the
local/container combination. For either external combination, and for
Slurm/container,
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
verifies an existing environment instead of creating it.

## Workspace and image

`workspace` is normalized, created if absent, and required to be
writable. It is the working directory for external commands and is
mounted at the same absolute path inside containers. Relative
checkpoint, dataset, and artifact destinations resolve below this
directory, and bionemor stores run metadata in its `.bionemor`
subdirectory. For container and Slurm execution, these inputs and
outputs must remain visible at their recorded paths. The submitting
process and Slurm compute nodes must therefore resolve the workspace to
the same shared storage.

With the verified recipe and local/container execution, `image = NULL`
or the recipe's locked base-image tag or digest-qualified reference
selects the deterministic derived image that
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
builds. Any other image reference is treated as a prebuilt recipe image
and inspected rather than rebuilt. Slurm/container requires `image`: a
local SIF must be readable on shared storage, while a digest-qualified
URI records its supplied digest. External runtimes do not use an image
property.

## Resources and site settings

`gpus` is the required GPU count used for compatibility checks and Slurm
requests. Local containers expose all available GPUs. Execution uses one
node. `queue`, `account`, and `walltime` become Slurm `--partition`,
`--account`, and `--time` directives and are ignored by the local
backend.

`config` is for site-specific runtime metadata. The supported
user-facing setting is `container_engine` for local containers.
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
stores a validated capability report in `config$capabilities` on the
returned descriptor; users normally should not populate that entry
themselves.

## See also

[`evo2()`](https://t-kalinowski.github.io/bionemor/reference/evo2.md),
[`evo2_model()`](https://t-kalinowski.github.io/bionemor/reference/evo2_model.md),
[`esm2()`](https://t-kalinowski.github.io/bionemor/reference/esm2.md),
[`esm2_model()`](https://t-kalinowski.github.io/bionemor/reference/esm2_model.md),
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md),
[`bionemo_doctor()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_doctor.md)

## Examples

``` r
# Descriptor construction is offline and does not require a GPU.
workspace <- file.path(tempdir(), "bionemor-compute-example")
compute <- bionemo_compute(
  recipe = evo2_recipe(),
  engine = "external",
  workspace = workspace
)
compute
#> <BioNeMo compute>
#> Backend:   local
#> Engine:    external
#> Workspace: /tmp/RtmpQjQknc/bionemor-compute-example
#> Recipe:    2.4 @ e8e7f597
#> Resources: 1 node(s), 1 GPU(s)

if (FALSE) { # \dontrun{
# A site-managed runtime on a Slurm cluster.
slurm_compute <- bionemo_compute(
  recipe = evo2_recipe(),
  backend = "slurm",
  engine = "external",
  workspace = "/shared/projects/evo2",
  gpus = 4L,
  queue = "gpu",
  account = "biology",
  walltime = "01:00:00"
)
} # }
```
