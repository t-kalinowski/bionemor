#' Evo 2 compatibility generics
#'
#' `fit()` delegates to [evo2_finetune()]. `predict()` delegates to
#' [evo2_score()], [evo2_generate()], or [evo2_embed()] according to `type`.
#'
#' @param object An Evo 2 model.
#' @param data An `Evo2Dataset` or accepted raw sequence input.
#' @param compute A BioNeMo compute descriptor. `NULL` uses the descriptor
#'   attached by [evo2_model()] or a previous fine-tuning run.
#' @param steps Positive training steps.
#' @param control Fine-tuning controls.
#' @param method A LoRA or full fine-tuning descriptor.
#' @param name Optional run name.
#' @param output Optional result path.
#' @param timeout Complete operation timeout in seconds.
#' @param async Whether to return a durable job.
#' @param newdata Sequences or prompts.
#' @param type Inference operation: `"score"`, `"generate"`, or `"embedding"`.
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
