#' BioNeMo Recipes Evo 2 workflows from R
#'
#' `bionemor` prepares MBridge checkpoints, fine-tunes models, and runs
#' file-backed inference with the pinned NVIDIA BioNeMo Recipes Evo 2 runtime.
#' NIM is not used.
#'
#' Operational failures inherit from `bionemor_error` and a stable `BN_*`
#' code-specific class. Conditions retain run and model context when available.
#'
#' Terminal run manifests retain checkpoint trust and input-source metadata.
#' Their `value_origins` maps distinguish user requests, package defaults,
#' adapter defaults, and auto-resolved values.
#'
#' @import S7
#' @importFrom stats predict
#' @importFrom utils str
#' @keywords internal
"_PACKAGE"
