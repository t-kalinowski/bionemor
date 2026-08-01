#' Use BioNeMo Recipes from R
#'
#' NVIDIA BioNeMo Recipes provides runnable training and inference workflows for
#' biological foundation models. `bionemor` controls those workflows from R and
#' returns R data frames, matrices, and durable job objects. The package supports
#' Evo 2 DNA workflows and ESM-2 protein embeddings.
#'
#' Installing the R package and inspecting its model registries do not require a
#' GPU. Preparing weights, inference, preprocessing, and fine-tuning require a
#' supported CUDA-capable NVIDIA GPU; there is no CPU fallback. The
#' package-managed local runtime also requires Linux, Docker, and NVIDIA
#' Container Toolkit. A remote
#' Linux GPU from a provider such as Brev can be used when no local GPU is
#' available.
#'
#' @section Start here:
#'
#' - [bionemo_workflows()] lists installed model-family workflows.
#' - [bionemo_compute()] describes the selected recipe, GPU runtime, and
#'   workspace.
#' - [bionemo_install()] prepares or checks that runtime.
#'
#' @section Evo 2 DNA workflows:
#'
#' - [evo2_models()] lists the packaged model registry, and [evo2_model()]
#'   prepares a checkpoint and binds it to compute.
#' - [evo2_generate()] extends DNA prompts.
#' - [evo2_score()] scores complete sequences.
#' - [evo2_profile()] writes per-position log probabilities.
#' - [evo2_embed()] returns pooled embeddings or writes positional embeddings.
#' - [evo2_dataset()], [evo2_prepare()], and [evo2_finetune()] prepare data and
#'   fine-tune a model.
#' - [evo2_checkpoint()] and [evo2_export()] convert and export checkpoints.
#'
#' @section ESM-2 protein embeddings:
#'
#' - [esm2_models()] lists pinned NVIDIA ESM-2 checkpoints.
#' - [esm2_model()] binds a checkpoint to an ESM-2 recipe runtime.
#' - [esm2_embed()] returns last-token, L2-normalized protein embeddings.
#'
#' Operations write durable requests, logs, state, and portable results below
#' the compute workspace. Use [bionemo_job()] to reopen a run after the R session
#' ends and [job_result()] to retrieve its result.
#'
#' @import S7
#' @importFrom stats predict
#' @importFrom utils str
#' @keywords internal
"_PACKAGE"
