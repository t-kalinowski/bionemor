resolve_model_compute <- function(model, compute = NULL) {
  stopifnot(
    "model must be a BioNeMo model" = S7_inherits(model, BioNeMoModel)
  )
  if (is.null(compute)) {
    compute <- model@compute
    if (!S7_inherits(compute, BioNeMoCompute)) {
      stop("compute is required for an unbound model; supply compute")
    }
  }
  if (!S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be a BioNeMo compute specification")
  }
  compute
}

method(print, BioNeMoRecipe) <- function(x, ...) {
  cat("<BioNeMo recipe>\n", sep = "")
  cat("Adapter:    ", x@adapter, "\n", sep = "")
  cat("Version:    ", x@recipe_version, "\n", sep = "")
  cat("Revision:   ", substr(x@revision, 1L, 8L), "\n", sep = "")
  cat("Repository: ", x@repository, "\n", sep = "")
  cat("Verified:   ", if (x@verified) "yes" else "no", "\n", sep = "")
  invisible(x)
}

method(print, BioNeMoModel) <- function(x, ...) {
  cat("<BioNeMo model>\n", sep = "")
  cat("Family: ", x@family, "\n", sep = "")
  invisible(x)
}

method(print, BioNeMoCheckpoint) <- function(x, ...) {
  cat("<BioNeMo checkpoint>\n", sep = "")
  cat("Model:  ", x@family, " ", x@variant, "\n", sep = "")
  cat("Format: ", x@format, "\n", sep = "")
  cat("Kind:   ", x@kind, "\n", sep = "")
  cat("Path:   ", x@path, "\n", sep = "")
  invisible(x)
}
