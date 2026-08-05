esm2_recipe_lock <- function() {
  read_recipe_lock("esm2", "ESM-2")
}

#' Describe the BioNeMo ESM-2 embedding recipe
#'
#' `esm2_recipe()` is an offline descriptor for the package-pinned BioNeMo
#' Recipes ESM-2 workflow. It does not download source, pull an image, or start
#' a runtime. Installation uses the recipe's native Transformers and
#' Transformer Engine runtime.
#'
#' @param revision BioNeMo Recipes revision. `"recommended"` uses the exact
#'   package lock. Otherwise, supply a full commit SHA unless
#'   `allow_mutable = TRUE`.
#' @param repository BioNeMo Recipes repository URL. `NULL` uses the lock.
#' @param base_image Base container image. `NULL` uses the locked image.
#' @param allow_mutable Whether `revision` may be a branch or tag.
#'
#' @return An S7 `BioNeMoRecipe` descriptor.
#'
#' @examples
#' recipe <- esm2_recipe()
#' recipe
#' @export
esm2_recipe <- function(
  revision = "recommended",
  repository = NULL,
  base_image = NULL,
  allow_mutable = FALSE
) {
  recipe_descriptor(
    esm2_recipe_lock(),
    adapter = "esm2-transformers",
    revision = revision,
    repository = repository,
    base_image = base_image,
    allow_mutable = allow_mutable
  )
}
