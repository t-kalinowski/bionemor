#' Run BioNeMo Recipes Evo 2 workflows from R
#'
#' `bionemor` prepares MBridge checkpoints, runs generation, scoring, and
#' embeddings, and fine-tunes Evo 2 models with the pinned NVIDIA BioNeMo
#' Recipes runtime. Inputs and results are ordinary R vectors, data frames,
#' matrices, and S7 objects.
#'
#' [evo2_model()] is the direct path to a ready model. It prepares or reuses the
#' recommended checkpoint and binds the compute descriptor used for later calls
#' to [evo2_generate()], [evo2_score()], [evo2_embed()], and [evo2_finetune()].
#' [evo2()] and [evo2_checkpoint()] expose the lower-level workflow for custom
#' checkpoint sources and explicit compute overrides.
#'
#' Runtime operations dispatched through the job runner write a durable run
#' directory containing their request, logs, state, and portable result. A
#' [BioNeMoJob][bionemo_job()] can be reopened after the R session ends.
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
