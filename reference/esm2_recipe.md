# Describe the BioNeMo ESM-2 embedding recipe

`esm2_recipe()` is an offline descriptor for the package-pinned BioNeMo
Recipes environment used for ESM-2 embeddings. It does not download
source, pull an image, or start a runtime. Installation uses native
Transformers and the Transformer Engine runtime.

## Usage

``` r
esm2_recipe(
  revision = "recommended",
  repository = NULL,
  base_image = NULL,
  allow_mutable = FALSE
)
```

## Arguments

- revision:

  BioNeMo Recipes revision. `"recommended"` uses the exact package lock.
  Otherwise, supply a full commit SHA unless `allow_mutable = TRUE`.

- repository:

  BioNeMo Recipes repository URL. `NULL` uses the lock.

- base_image:

  Base container image. `NULL` uses the locked image.

- allow_mutable:

  Whether `revision` may be a branch or tag.

## Value

An S7 `BioNeMoRecipe` descriptor.

## Examples

``` r
recipe <- esm2_recipe()
recipe
#> <BioNeMo recipe>
#> Adapter:    esm2-transformers
#> Version:    transformers-5.14.1
#> Revision:   e8e7f597
#> Repository: https://github.com/NVIDIA-BioNeMo/bionemo-recipes
#> Verified:   yes
```
