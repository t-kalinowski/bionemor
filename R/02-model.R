#' Describe an Evo 2 model
#'
#' `evo2()` creates a compute-independent model specification for the
#' BioNeMo 2.6.3 Evo 2 adapter.
#'
#' @param size Evo 2 model size.
#' @param checkpoint `NULL`, one checkpoint path, or a checkpoint returned by
#'   [evo2_checkpoint()].
#' @param pretrained Whether fitting starts from pretrained weights.
#' @param config Additional adapter configuration.
#'
#' @return An S7 `Evo2Model`.
#' @export
evo2 <- function(size, checkpoint = NULL, pretrained = TRUE, config = list()) {
  if (missing(size)) {
    stop("size is required", call. = FALSE)
  }
  stopifnot(
    "size must be one string" = is_scalar_string(size),
    "checkpoint must be NULL, one path, or a BioNeMo checkpoint" =
      is.null(checkpoint) ||
        is_scalar_string(checkpoint) ||
        S7_inherits(checkpoint, BioNeMoCheckpoint),
    "pretrained must be TRUE or FALSE" = is_scalar_logical(pretrained),
    "config must be a list" = is.list(config),
    "checkpoint must be NULL when pretrained is FALSE" =
      pretrained || is.null(checkpoint)
  )

  size <- switch(
    tolower(size),
    `1b` = "1b",
    `1b-8k` = "1b",
    `7b` = "7b",
    `7b-1m` = "7b",
    `40b` = "40b",
    `40b-1m` = "40b",
    stop("size must be one of '1b', '7b', or '40b'", call. = FALSE)
  )
  if (S7_inherits(checkpoint, BioNeMoCheckpoint)) {
    stopifnot(
      "checkpoint family must be 'evo2'" = checkpoint@family == "evo2",
      "checkpoint size does not match model size" = checkpoint@variant == size
    )
  }

  Evo2Model(
    family = "evo2",
    checkpoint = checkpoint,
    pretrained = pretrained,
    task = NULL,
    config = config,
    provenance = list(),
    size = size
  )
}

method(print, BioNeMoModel) <- function(x, ...) {
  cat("<bionemor_model>\n", sep = "")
  cat("Family: ", x@family, "\n", sep = "")
  if (S7_inherits(x, Evo2Model)) {
    cat("Size: ", x@size, "\n", sep = "")
  }
  checkpoint <- model_checkpoint_path(x)
  cat(
    "Checkpoint: ",
    if (is.null(checkpoint)) {
      if (x@pretrained) "not prepared" else "random initialization"
    } else {
      checkpoint
    },
    "\n",
    sep = ""
  )
  invisible(x)
}

method(print, BioNeMoCheckpoint) <- function(x, ...) {
  cat("<bionemor_checkpoint>\n", sep = "")
  cat("Model: ", x@family, " ", x@variant, "\n", sep = "")
  cat("Profile: ", x@profile, "\n", sep = "")
  cat("Path: ", x@path, "\n", sep = "")
  invisible(x)
}

method(print, BioNeMoPrediction) <- function(x, ...) {
  cat("<bionemor_prediction>\n", sep = "")
  cat("Type: ", x@type, "\n", sep = "")
  rows <- if (is.null(dim(x@data))) length(x@data) else NROW(x@data)
  cat("Rows: ", rows, "\n", sep = "")
  invisible(x)
}

method(as.data.frame, BioNeMoPrediction) <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  stopifnot(
    "this prediction is not tabular" =
      is.data.frame(x@data) || is.matrix(x@data)
  )
  base::as.data.frame(
    x@data,
    row.names = row.names,
    optional = optional,
    ...
  )
}
