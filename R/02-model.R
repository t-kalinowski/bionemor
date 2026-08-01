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

read_model_registry_records <- function(name, label, lock) {
  stopifnot(
    is_scalar_string(name),
    is_scalar_string(label),
    is.list(lock),
    identical(lock$model_registry_version, 1L),
    is_scalar_string(lock$revision)
  )
  path <- system.file(
    "recipes",
    paste0(name, "-models.json"),
    package = "bionemor",
    mustWork = TRUE
  )
  registry <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!identical(registry$schema_version, lock$model_registry_version)) {
    stop("unsupported ", label, " model registry schema")
  }
  if (!is.list(registry$models) || length(registry$models) == 0L) {
    stop(label, " model registry is empty")
  }
  if (!identical(registry$recipe_revision, lock$revision)) {
    stop(label, " model registry does not match the recipe lock")
  }
  registry$models
}

select_model_registry_record <- function(records, size) {
  if (!is_scalar_string(size)) {
    stop("size must be one non-empty string")
  }
  size <- tolower(size)
  matches <- vapply(
    records,
    function(record) size %in% c(record$name, record$aliases),
    logical(1)
  )
  if (sum(matches) != 1L) {
    choices <- pluck_chr(records, "name")
    bionemor_abort(
      "BN_MODEL_UNKNOWN",
      paste0(
        "supported sizes are ",
        paste(sprintf("'%s'", choices), collapse = ", ")
      ),
      model = size
    )
  }
  records[[which(matches)]]
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
