# Diagnose a BioNeMo Recipes execution environment

`bionemo_doctor()` checks whether a compute descriptor is ready for one
or more groups of operations supplied by its recipe runtime. It performs
live runtime probes; it does not rely on the cached capability report
stored by
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md).
A model is optional because the runtime can be diagnosed before weights
are prepared. Supply one to add the recipe's model compatibility and
checkpoint checks. The doctor does not install software, build
containers, or prepare checkpoints.

## Usage

``` r
bionemo_doctor(
  compute,
  model = NULL,
  target = c("all", "inference", "training", "conversion"),
  verbose = TRUE
)
```

## Arguments

- compute:

  A BioNeMo compute descriptor whose runtime has been built or selected,
  usually the value returned by
  [`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md).

- model:

  Optional BioNeMo model. Supplying a model adds the checks implemented
  by the compute recipe.

- target:

  Operation group to check: `"all"`, `"inference"`, `"training"`, or
  `"conversion"`.

- verbose:

  Whether printing the result should include complete retained probe
  output in addition to the check table. This does not change the checks
  performed or the columns returned by
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

A `BioNeMoDoctor` with `target`, logical `ok`, a `checks` data frame,
and the selected `verbose` print setting. Use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to
inspect the `check`, `status`, `detail`, and `output` columns
programmatically.

## Targets

`target` selects a command group declared by the selected recipe.
`"inference"`, `"training"`, and `"conversion"` check the commands
needed for that operation group; `"all"` checks every command
implemented by the recipe. An error is reported when a requested group
is unsupported.

Every target also checks the backend commands, required host tools,
writable workspace, helper protocol, recipe identity, runtime software
versions, GPU visibility and count, and image metadata and digest
availability. When `model` is supplied, the doctor also checks registry
compatibility, model source or checkpoint readiness. These checks do not
run a model operation on user data.

With a Slurm backend, host and runtime checks submit short synchronous
jobs so that tools, GPUs, and the runtime are inspected on compute
nodes. This can create probe files under `workspace/.bionemor` and
consume scheduler time.

## Results

Printing the returned `BioNeMoDoctor` shows its target, overall status,
and a table of `check`, `status`, and `detail`. The overall `ok`
property is `TRUE` only when no row has `status = "fail"`. Set
`verbose = TRUE` to print any complete credential-redacted probe output
recorded for failed checks.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
all check data with four columns:

- `check`: the component or command inspected.

- `status`: `"pass"` or `"fail"`.

- `detail`: a concise explanation of the result.

- `output`: credential-redacted probe output when one was retained,
  otherwise `""`.

## See also

[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md),
[`bionemo_capabilities()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_capabilities.md)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(),
  engine = "external",
  workspace = "/shared/projects/evo2"
)
compute <- bionemo_install(compute)

doctor <- bionemo_doctor(
  compute,
  target = "inference",
  verbose = FALSE
)
doctor

checks <- as.data.frame(doctor)
checks[checks$status == "fail", c("check", "detail")]

# Add checkpoint and model compatibility checks.
model <- evo2("7b", checkpoint = "/shared/models/evo2-7b-mbridge")
bionemo_doctor(compute, model, target = "inference")
} # }
```
