# Describe the Evo 2 BioNeMo recipe

`evo2_recipe()` identifies the versioned BioNeMo Recipes environment
used for Evo 2 checkpoint preparation, inference, and fine-tuning. It is
offline: it reads the lock bundled with bionemor without cloning source,
pulling an image, or starting a runtime.

## Usage

``` r
evo2_recipe(
  revision = "recommended",
  repository = NULL,
  base_image = NULL,
  allow_mutable = FALSE
)
```

## Arguments

- revision:

  BioNeMo Recipes revision. `"recommended"` uses the package lock.
  Otherwise, supply a full commit SHA unless `allow_mutable = TRUE`.

- repository:

  BioNeMo Recipes repository URL. `NULL` uses the lock.

- base_image:

  Base image reference. `NULL` uses the lock.

- allow_mutable:

  Whether `revision` may be a branch or tag.

## Value

A `BioNeMoRecipe` descriptor.

## Details

Use the result as the required `recipe` argument to
[`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md),
then use
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)
to build or verify the runtime. Changing a locked source field creates
an unverified descriptor. Unverified recipes require an externally
managed runtime or an explicit prebuilt image.

## See also

[`bionemo_compute()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_compute.md),
[`bionemo_install()`](https://t-kalinowski.github.io/bionemor/reference/bionemo_install.md)

## Examples

``` r
recipe <- evo2_recipe()
recipe
#> <BioNeMo recipe>
#> Adapter:    evo2-megatron
#> Version:    2.4
#> Revision:   e8e7f597
#> Repository: https://github.com/NVIDIA-BioNeMo/bionemo-recipes
#> Verified:   yes
```
