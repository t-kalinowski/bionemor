adapter_registry <- function() {
  list(
    `evo2-megatron` = list(
      family = "evo2",
      operations = list(
        generation = c(generation = 1L),
        score = c(score = 1L),
        profile = c(profile = 1L),
        embedding = c(
          `embedding-pooled` = 1L,
          `embedding-unpooled` = 1L
        ),
        checkpoint = c(checkpoint = 1L),
        export = c(checkpoint = 1L),
        preprocess = c(preprocess = 1L),
        `fine-tune` = c(`fine-tune` = 1L)
      ),
      materialize = bionemor_adapter_evo2_megatron_materialize,
      provenance = recipe_runtime_provenance,
      install_spec = bionemor_adapter_evo2_megatron_install_spec,
      doctor_model = bionemor_adapter_evo2_megatron_doctor_model
    ),
    `esm2-transformers` = list(
      family = "esm2",
      operations = list(embedding = c(`esm2-pooled` = 1L)),
      materialize = bionemor_adapter_esm2_transformers_materialize,
      provenance = recipe_runtime_provenance,
      install_spec = bionemor_adapter_esm2_transformers_install_spec,
      doctor_model = bionemor_adapter_esm2_transformers_doctor_model
    )
  )
}

adapter_record <- function(adapter) {
  stopifnot(
    "adapter must be one safe identifier" = is_scalar_string(adapter) &&
      grepl("^[a-z][a-z0-9-]*$", adapter)
  )
  record <- adapter_registry()[[adapter]]
  hooks <- c(
    "materialize",
    "provenance",
    "install_spec",
    "doctor_model"
  )
  if (is.null(record)) {
    stop("BioNeMo adapter implementation is unavailable: ", adapter)
  }
  stopifnot(
    "adapter family must be one non-empty string" = is_scalar_string(
      record$family
    ),
    "adapter operations must be a non-empty named list" = is.list(
      record$operations
    ) &&
      length(record$operations) > 0L &&
      !is.null(names(record$operations)) &&
      all(nzchar(names(record$operations))),
    "adapter hooks must be functions" = all(vapply(
      record[hooks],
      is.function,
      logical(1)
    ))
  )
  record
}

adapter_function <- function(adapter, action) {
  stopifnot(
    "adapter action must be one safe identifier" = is_scalar_string(action) &&
      grepl("^[a-z][a-z0-9_]*$", action)
  )
  implementation <- adapter_record(adapter)[[action]]
  if (!is.function(implementation)) {
    stop("BioNeMo adapter capability is unavailable: ", action)
  }
  implementation
}

result_versions <- function(adapter, kind) {
  stopifnot(
    "operation must be one safe name" = is_scalar_string(kind) &&
      grepl("^[A-Za-z0-9_.-]+$", kind)
  )
  record <- adapter_record(adapter)
  operation <- record$operations[[kind]]
  if (is.null(operation)) {
    stop("BioNeMo operation is unavailable for this recipe: ", kind)
  }
  stopifnot(
    "result versions must be a named numeric vector" = is.numeric(operation) &&
      length(operation) > 0L &&
      !is.null(names(operation)) &&
      all(nzchar(names(operation))) &&
      all(vapply(operation, is_scalar_integerish, logical(1), min = 1L))
  )
  operation
}

validate_result_contract <- function(
  adapter,
  kind,
  descriptor,
  run_path,
  request_id,
  operation
) {
  result_type <- if (is.list(descriptor)) descriptor$type else NULL
  result_version <- if (is.list(descriptor)) {
    descriptor$result_version
  } else {
    NULL
  }
  valid <- is_scalar_string(result_type) &&
    is_scalar_integerish(result_version, min = 1L)
  versions <- tryCatch(
    result_versions(adapter, kind),
    error = function(error) NULL
  )
  expected <- if (valid && !is.null(versions)) {
    versions[[result_type]]
  } else {
    NULL
  }
  if (
    is.null(expected) ||
      !identical(as.integer(expected), result_version)
  ) {
    bionemor_abort(
      "BN_PROTOCOL",
      "persisted run contains an unsupported result contract",
      run_path = run_path,
      request_id = request_id,
      operation = operation,
      adapter = adapter,
      kind = kind,
      result_type = result_type,
      result_version = result_version
    )
  }
  invisible(descriptor)
}

read_operation_record <- function(run_path) {
  operation <- read_json_file(
    file.path(run_path, "request.json"),
    simplify = FALSE
  )
  valid <- identical(operation$schema_version, 4L) &&
    identical(
      names(operation),
      c(
        "schema_version",
        "id",
        "kind",
        "compute",
        "request",
        "execution",
        "result",
        "context",
        "cleanup",
        "timeout"
      )
    ) &&
    is_scalar_string(operation$id) &&
    is_scalar_string(operation$kind) &&
    is.list(operation$compute) &&
    is.list(operation$request) &&
    is.list(operation$execution) &&
    is.list(operation$result) &&
    is.list(operation$context) &&
    identical(
      names(operation$context),
      c("model", "checkpoint", "tokenizer", "precision", "warnings")
    ) &&
    all(vapply(
      operation$context,
      is.list,
      logical(1)
    )) &&
    (is.null(operation$cleanup) || is.list(operation$cleanup)) &&
    (is.null(operation$timeout) ||
      is_scalar_number(operation$timeout) && operation$timeout > 0)
  if (!valid) {
    bionemor_abort(
      "BN_PROTOCOL",
      "persisted operation schema is unsupported",
      run_path = run_path,
      request_id = operation$id %||% basename(run_path),
      operation = "job-reopen"
    )
  }
  operation
}
