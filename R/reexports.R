#' Model compatibility generics
#'
#' `fit()` delegates Evo 2 models to [evo2_finetune()]. `predict()` provides
#' cross-family inference. For Evo 2, it delegates to [evo2_score()],
#' [evo2_generate()], or [evo2_embed()] according to `type`. For ESM-2, it
#' delegates to [esm2_embed()]; only `type = "embedding"` is supported.
#'
#' @param object An Evo 2 or ESM-2 model. `fit()` supports only Evo 2.
#' @param data An `Evo2Dataset` or accepted raw sequence input.
#' @param compute A BioNeMo compute descriptor. `NULL` uses the descriptor
#'   attached by [evo2_model()], [esm2_model()], or a previous fine-tuning run.
#' @param steps Positive training steps.
#' @param control Fine-tuning controls.
#' @param method A LoRA or full fine-tuning descriptor.
#' @param name Optional run name.
#' @param output Optional result path.
#' @param timeout Complete operation timeout in seconds.
#' @param async Whether to return a durable job.
#' @param newdata DNA sequences or prompts for Evo 2, or protein sequences for
#'   ESM-2.
#' @param type Inference operation. Evo 2 supports `"score"`, `"generate"`, and
#'   `"embedding"`; ESM-2 supports `"embedding"`.
#' @param ... Reserved for future compatibility and must be empty for `fit()`;
#'   passed to the selected task-specific function for `predict()`.
#'
#' @return `fit()` returns an `Evo2Model` or `BioNeMoJob`. `predict()` returns
#'   the selected task-specific result.
#'
#' @usage
#' fit(
#'   object,
#'   data,
#'   compute = NULL,
#'   steps,
#'   control = evo2_fit_control(),
#'   method = evo2_lora(),
#'   name = NULL,
#'   output = NULL,
#'   timeout = Inf,
#'   async = FALSE,
#'   ...
#' )
#'
#' predict(
#'   object,
#'   newdata,
#'   type = c("score", "generate", "embedding"),
#'   compute = NULL,
#'   ...
#' )
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
