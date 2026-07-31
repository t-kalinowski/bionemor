#' Use BioNeMo Recipes Evo 2 without leaving R
#'
#' `bionemor` prepares MBridge checkpoints, runs generation, scoring, and
#' embeddings, and fine-tunes Evo 2 models with the pinned NVIDIA BioNeMo
#' Recipes runtime. Inputs and results remain ordinary R objects; users do not
#' need to write Python or interact with Python objects.
#'
#' [evo2_model()] provides the direct path to a ready model using the
#' recommended checkpoint. [evo2()] and [evo2_checkpoint()] expose the
#' lower-level workflow for custom sources and asynchronous preparation.
#'
#' The Recipes runtime runs out of process and writes portable files and
#' manifests. NIM is not used.
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
