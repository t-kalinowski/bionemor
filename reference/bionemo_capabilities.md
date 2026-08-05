# Report capabilities of a BioNeMo recipe runtime

`bionemo_capabilities()` runs the package helper in the environment
described by `compute` and parses its JSON report. The probe verifies
that the helper's protocol, recipe version, and recipe revision match
the compute descriptor. It also reports whether the selected recipe's
commands for its implemented inference, training, or
checkpoint-conversion operations are available.

## Usage

``` r
bionemo_capabilities(compute, refresh = FALSE)
```

## Arguments

- compute:

  A BioNeMo compute descriptor. Its runtime must expose the package
  helper. Use
  [`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
  to verify the helper and every required recipe command.

- refresh:

  Whether to ignore `compute@config$capabilities` and run a new runtime
  probe.

## Value

A named list parsed from the helper capability report, with `image`,
`image_digest`, and `probed_at` runtime provenance added by bionemor.

## Details

This is a live GPU-backed probe when no cached report is used. A local
container is started with GPU access, an external runtime runs the
helper directly, and a Slurm backend submits a synchronous probe
allocation. The probe does not prepare a model or inspect model weights.

## Report contents

The returned named list contains helper, protocol, recipe, command,
feature, and runtime information, plus recipe-specific entries.
`runtime` includes software versions, CUDA availability, the GPU count,
driver information, and per-GPU properties. bionemor adds `image`,
`image_digest`, and the UTC `probed_at` time so the report records where
it came from.

[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
saves the validated report in `compute@config$capabilities` on the
compute descriptor it returns. With `refresh = FALSE`, this function
returns that cached list when present; otherwise it probes the runtime.
`refresh = TRUE` always probes again. Calling this function does not
modify the supplied compute descriptor.

## See also

[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md),
[`bionemo_doctor()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_doctor.md)

## Examples

``` r
if (FALSE) { # \dontrun{
compute <- bionemo_compute(recipe = evo2_recipe(),
  engine = "external",
  workspace = "/shared/projects/evo2"
)
compute <- bionemo_install(compute)

capabilities <- bionemo_capabilities(compute)
capabilities[c("protocol_version", "recipe_version", "probed_at")]
capabilities$commands
capabilities$runtime$gpus

# Probe again after the runtime or GPU allocation changes.
capabilities <- bionemo_capabilities(compute, refresh = TRUE)
} # }
```
