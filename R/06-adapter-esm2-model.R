esm2_model_registry_records <- function() {
  read_model_registry_records("esm2", "ESM-2", esm2_recipe_lock())
}

esm2_model_record <- function(size) {
  select_model_registry_record(esm2_model_registry_records(), size)
}

#' List the pinned ESM-2 models
#'
#' `esm2_models()` describes the NVIDIA ESM-2 checkpoints available to the
#' package. It is offline and does not download weights.
#'
#' @return A data frame with model names, sizes, embedding dimensions, and
#'   immutable Hugging Face source identifiers.
#'
#' @examples
#' esm2_models()
#' @export
esm2_models <- function() {
  records <- esm2_model_registry_records()
  data.frame(
    name = pluck_chr(records, "name"),
    model_size = pluck_chr(records, "model_size"),
    parameters = pluck_dbl(records, "parameters"),
    context_length = pluck_int(records, "context_length"),
    embedding_size = pluck_int(records, "embedding_size"),
    source = pluck_chr(records, "source"),
    source_revision = pluck_chr(records, "source_revision"),
    source_format = pluck_chr(records, "source_format"),
    stringsAsFactors = FALSE
  )
}

#' Describe an ESM-2 model
#'
#' `esm2()` creates an offline model descriptor. By default, inference obtains
#' the exact checkpoint listed by [esm2_models()] when it first runs. Supply a
#' runtime-visible, Hugging Face/vLLM-compatible local checkpoint directory
#' whose architecture matches `size` to use local weights.
#'
#' @param size A canonical model name or upstream model alias.
#' @param checkpoint `NULL` or one compatible, runtime-visible local checkpoint
#'   directory.
#' @param compute Optional [bionemo_compute()] descriptor.
#'
#' @return An S7 `Esm2Model`.
#'
#' @examples
#' model <- esm2("8m")
#' model
#' @export
esm2 <- function(size = "8m", checkpoint = NULL, compute = NULL) {
  record <- esm2_model_record(size)
  stopifnot(
    "checkpoint must be NULL or one path" = is.null(checkpoint) ||
      is_scalar_string(checkpoint),
    "compute must be NULL or a BioNeMo compute descriptor" = is.null(compute) ||
      S7_inherits(compute, BioNeMoCompute)
  )
  Esm2Model(
    family = "esm2",
    checkpoint = checkpoint,
    compute = compute,
    config = list(),
    provenance = list(
      source = record$source,
      source_revision = record$source_revision
    ),
    size = record$name,
    model_size = record$model_size,
    context_length = as.integer(record$context_length),
    embedding_size = as.integer(record$embedding_size),
    revision = record$source_revision
  )
}

#' Bind an ESM-2 model to compute
#'
#' `esm2_model()` returns a model ready for embedding on `compute`. With
#' `path = NULL`, vLLM obtains the package-pinned Hugging Face checkpoint on
#' first use. Set `path` to a runtime-visible, Hugging Face/vLLM-compatible
#' checkpoint directory whose architecture matches `size` to avoid that
#' download.
#'
#' @param size A canonical model name or upstream model alias.
#' @param compute A [bionemo_compute()] descriptor.
#' @param path Optional compatible, runtime-visible local checkpoint directory.
#'
#' @return An S7 `Esm2Model` bound to `compute`.
#'
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(
#'   recipe = esm2_recipe(),
#'   workspace = "~/bionemor-esm2"
#' )
#' compute <- bionemo_install(compute)
#' model <- esm2_model("8m", compute)
#' }
#' @export
esm2_model <- function(size = "8m", compute, path = NULL) {
  stopifnot(
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "path must be NULL or one non-empty string" = is.null(path) ||
      is_scalar_string(path)
  )
  esm2(size = size, checkpoint = path, compute = compute)
}

method(print, Esm2Model) <- function(x, ...) {
  record <- esm2_model_record(x@size)
  checkpoint <- model_checkpoint_path(x)
  cat("<ESM-2 model>\n", sep = "")
  cat("Size:       ", toupper(x@size), "\n", sep = "")
  cat(
    "Context:    ",
    format(x@context_length, big.mark = ",", scientific = FALSE),
    " residues\n",
    sep = ""
  )
  cat("Embedding:  ", x@embedding_size, " dimensions\n", sep = "")
  cat(
    "Source:     ",
    if (is.null(checkpoint)) record$source else checkpoint,
    "\n",
    sep = ""
  )
  invisible(x)
}
