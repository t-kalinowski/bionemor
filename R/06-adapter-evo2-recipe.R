evo2_recipe_lock <- function() {
  read_recipe_lock("evo2", "Evo 2")
}

#' Describe the Evo 2 BioNeMo recipe
#'
#' `evo2_recipe()` identifies the versioned BioNeMo Recipes environment used
#' for Evo 2 checkpoint preparation, inference, and fine-tuning. It is offline:
#' it reads the lock bundled with bionemor without cloning source, pulling an
#' image, or starting a runtime.
#'
#' Use the result as the required `recipe` argument to [bionemo_compute()], then
#' use [bionemo_install()] to build or verify the runtime. Changing a locked
#' source field creates an unverified descriptor. Unverified recipes require an
#' externally managed runtime or an explicit prebuilt image.
#'
#' @param revision BioNeMo Recipes revision. `"recommended"` uses the package
#'   lock. Otherwise, supply a full commit SHA unless `allow_mutable = TRUE`.
#' @param repository BioNeMo Recipes repository URL. `NULL` uses the lock.
#' @param base_image Base image reference. `NULL` uses the lock.
#' @param allow_mutable Whether `revision` may be a branch or tag.
#'
#' @return A `BioNeMoRecipe` descriptor.
#'
#' @examples
#' recipe <- evo2_recipe()
#' recipe
#'
#' @seealso [bionemo_compute()], [bionemo_install()]
#' @export
evo2_recipe <- function(
  revision = "recommended",
  repository = NULL,
  base_image = NULL,
  allow_mutable = FALSE
) {
  recipe_descriptor(
    evo2_recipe_lock(),
    adapter = "evo2-megatron",
    revision = revision,
    repository = repository,
    base_image = base_image,
    allow_mutable = allow_mutable
  )
}
