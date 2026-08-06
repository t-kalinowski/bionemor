#' Model compatibility generics
#'
#' `fit()` delegates Evo 2 models to [evo2_finetune()]. `predict()` provides
#' cross-family inference. For Evo 2, it delegates to [evo2_score()],
#' [evo2_generate()], or [evo2_embed()] according to `type`. For ESM-2, it
#' delegates to [esm2_embed()]; only `type = "embedding"` is supported.
#'
#' @param object An Evo 2 or ESM-2 model. `fit()` supports only Evo 2.
#' @param ... Method arguments. Evo 2 `fit()` accepts `data`, `compute`, `steps`,
#'   `control`, `method`, `name`, `output`, `timeout`, and `async`. `predict()`
#'   accepts `newdata`, `type`, `compute`, and arguments for the selected
#'   task-specific function.
#'
#' @return `fit()` returns an `Evo2Model` or `BioNeMoJob`. `predict()` returns
#'   the selected task-specific result.
#'
#' @usage
#' fit(object, ...)
#' predict(object, ...)
#'
#' @name reexports
#' @aliases fit predict
NULL

#' @noRd
#' @importFrom generics fit
#' @export
generics::fit

#' @noRd
#' @importFrom stats predict
#' @export
stats::predict
