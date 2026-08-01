workflow_identity_fields <- c(
  "id",
  "adapter",
  "adapter_version",
  "family",
  "task",
  "protocol_version",
  "input_schema",
  "result_schema"
)

adapter_registry <- function() {
  list(
    `evo2-megatron` = list(
      adapter_version = 1L,
      run = bionemor_adapter_evo2_megatron_run,
      materialize = bionemor_adapter_evo2_megatron_materialize,
      manifest_context = bionemor_adapter_evo2_megatron_manifest_context,
      provenance = bionemor_adapter_evo2_megatron_provenance,
      install_spec = bionemor_adapter_evo2_megatron_install_spec,
      doctor_model = bionemor_adapter_evo2_megatron_doctor_model
    )
  )
}

adapter_record <- function(adapter) {
  record <- adapter_registry()[[adapter]]
  hooks <- c("run", "materialize", "manifest_context", "provenance")
  if (
    is.null(record) ||
      !is_scalar_integerish(record$adapter_version, min = 1L) ||
      !all(vapply(record[hooks], is.function, logical(1)))
  ) {
    bionemor_abort(
      "BN_WORKFLOW_UNKNOWN",
      paste0("BioNeMo adapter implementation is unavailable: ", adapter),
      operation = "workflow-discovery"
    )
  }
  record
}

workflow_registry_paths <- function() {
  root <- system.file("workflows", package = "bionemor")
  if (!nzchar(root)) {
    root <- file.path("inst", "workflows")
  }
  paths <- sort(list.files(root, pattern = "[.]json$", full.names = TRUE))
  if (!length(paths)) {
    stop("the installed BioNeMo workflow registry is empty")
  }
  paths
}

workflow_registry_records <- function() {
  records <- lapply(workflow_registry_paths(), function(path) {
    manifest <- jsonlite::read_json(path, simplifyVector = TRUE)
    adapter <- adapter_record(manifest$adapter)
    stopifnot(
      "workflow manifest schema is unsupported" = identical(
        manifest$schema_version,
        1L
      ),
      "workflow manifest is invalid" = is_scalar_string(manifest$adapter) &&
        is_scalar_integerish(manifest$adapter_version, min = 1L) &&
        identical(manifest$adapter_version, adapter$adapter_version) &&
        is_scalar_string(manifest$family) &&
        is_scalar_integerish(manifest$protocol_version, min = 1L) &&
        is.data.frame(manifest$workflows) &&
        nrow(manifest$workflows) > 0L
    )
    workflows <- manifest$workflows
    workflows$adapter <- manifest$adapter
    workflows$adapter_version <- as.integer(manifest$adapter_version)
    workflows$family <- manifest$family
    workflows$protocol_version <- as.integer(manifest$protocol_version)
    workflows[workflow_identity_fields]
  })
  records <- do.call(rbind, records)
  stopifnot(
    "workflow IDs must be unique" = !anyDuplicated(records$id),
    "workflow IDs must be family-qualified" = identical(
      records$id,
      paste0(records$family, "/", records$task)
    )
  )
  rownames(records) <- NULL
  records
}

workflow_record <- function(id) {
  stopifnot(
    "workflow ID must be one non-empty string" = is_scalar_string(id)
  )
  records <- workflow_registry_records()
  index <- match(id, records$id)
  if (is.na(index)) {
    bionemor_abort(
      "BN_WORKFLOW_UNKNOWN",
      paste0("BioNeMo workflow is unsupported: ", id),
      operation = "workflow-discovery"
    )
  }
  as.list(records[index, , drop = FALSE])
}

workflow_identity <- function(workflow) {
  stopifnot(
    "workflow must be a BioNeMo workflow" = S7_inherits(
      workflow,
      BioNeMoWorkflow
    )
  )
  S7::props(workflow)[workflow_identity_fields]
}

workflow_from_identity <- function(identity) {
  required <- workflow_identity_fields
  if (!is.list(identity) || !all(required %in% names(identity))) {
    bionemor_abort(
      "BN_PROTOCOL",
      "persisted run does not contain a complete workflow identity",
      operation = "workflow-resolution"
    )
  }
  workflow <- bionemo_workflow(identity$id)
  current <- workflow_identity(workflow)
  mismatched <- required[
    !vapply(
      required,
      function(field) identical(identity[[field]], current[[field]]),
      logical(1)
    )
  ]
  if (length(mismatched)) {
    bionemor_abort(
      "BN_PROTOCOL",
      paste0(
        "persisted workflow identity does not match this installation: ",
        paste(mismatched, collapse = ", ")
      ),
      operation = "workflow-resolution",
      workflow = identity$id,
      fields = mismatched
    )
  }
  workflow
}

read_run_request <- function(run_path) {
  request <- read_json_file(
    file.path(run_path, "request.json"),
    simplify = FALSE
  )
  if (!identical(request$schema_version, 2L)) {
    bionemor_abort(
      "BN_PROTOCOL",
      "persisted run request schema is unsupported",
      run_path = run_path,
      request_id = request$id %||% basename(run_path),
      operation = "job-reopen"
    )
  }
  request
}

job_workflow <- function(job) {
  stopifnot(
    "job must be a BioNeMo job" = S7_inherits(job, BioNeMoJob)
  )
  request <- read_run_request(job@path)
  workflow_from_identity(request$workflow)
}

adapter_function <- function(adapter, action) {
  stopifnot(
    "adapter must be one safe identifier" = is_scalar_string(adapter) &&
      grepl("^[a-z][a-z0-9-]*$", adapter),
    "adapter action must be one safe identifier" = is_scalar_string(action) &&
      grepl("^[a-z][a-z0-9_]*$", action)
  )
  implementation <- adapter_record(adapter)[[action]]
  if (!is.function(implementation)) {
    bionemor_abort(
      "BN_WORKFLOW_UNKNOWN",
      paste0("BioNeMo adapter capability is unavailable: ", action),
      operation = "workflow-discovery"
    )
  }
  implementation
}

#' Discover installed BioNeMo workflows
#'
#' `bionemo_workflows()` lists the versioned workflows implemented by this
#' installation of bionemor. A workflow describes one model-family operation,
#' its portable input and result schemas, and the versioned adapter that
#' implements it. It is independent of a model checkpoint and compute target.
#'
#' @param family Optional model family. When supplied, only workflows for that
#'   family are returned.
#'
#' @return A data frame with one row per installed workflow.
#' @export
bionemo_workflows <- function(family = NULL) {
  stopifnot(
    "family must be NULL or one non-empty string" = is.null(family) ||
      is_scalar_string(family)
  )
  records <- workflow_registry_records()
  families <- unique(records$family)
  if (!is.null(family) && !family %in% families) {
    bionemor_abort(
      "BN_WORKFLOW_UNKNOWN",
      paste0("BioNeMo workflow family is unsupported: ", family),
      operation = "workflow-discovery"
    )
  }
  if (!is.null(family)) {
    records <- records[records$family == family, , drop = FALSE]
  }
  records[workflow_identity_fields]
}

#' Select one installed BioNeMo workflow
#'
#' @param id Family-qualified workflow ID, such as `"evo2/score"`.
#'
#' @return A `BioNeMoWorkflow` descriptor. The descriptor is not bound to a
#'   model checkpoint or compute target.
#' @export
bionemo_workflow <- function(id) {
  record <- workflow_record(id)
  do.call(BioNeMoWorkflow, record)
}

#' Run a versioned BioNeMo workflow
#'
#' `bionemo_run()` is the low-level automation interface for installed workflow
#' adapters. Family-specific functions such as [evo2_score()] provide the
#' ordinary R interface and use the same workflow contract.
#'
#' @param workflow A workflow returned by [bionemo_workflow()].
#' @param model Model descriptor accepted by the workflow adapter.
#' @param input Workflow input.
#' @param compute Optional compute target. A compute-bound model can supply it.
#' @param parameters Named list of workflow-specific parameters.
#' @param async Whether to return a durable job before completion.
#' @param name Optional durable run name.
#'
#' @return The workflow's portable R result, or a `BioNeMoJob` when
#'   `async = TRUE`.
#' @export
bionemo_run <- function(
  workflow,
  model,
  input,
  compute = NULL,
  parameters = list(),
  async = FALSE,
  name = NULL
) {
  stopifnot(
    "workflow must be a BioNeMo workflow" = S7_inherits(
      workflow,
      BioNeMoWorkflow
    ),
    "parameters must be a named list" = is.list(parameters) &&
      (length(parameters) == 0L ||
        !is.null(names(parameters)) &&
          all(nzchar(names(parameters))) &&
          !anyDuplicated(names(parameters))),
    "async must be TRUE or FALSE" = is_scalar_logical(async),
    "name must be NULL or one non-empty string" = is.null(name) ||
      is_scalar_string(name)
  )
  workflow <- workflow_from_identity(workflow_identity(workflow))
  run <- adapter_function(workflow@adapter, "run")
  run(
    workflow = workflow,
    model = model,
    input = input,
    compute = compute,
    parameters = parameters,
    async = async,
    name = name
  )
}

method(format, BioNeMoWorkflow) <- function(x, ...) {
  x@id
}

method(print, BioNeMoWorkflow) <- function(x, ...) {
  cat("<BioNeMo workflow>\n")
  cat("ID:       ", x@id, "\n", sep = "")
  cat(
    "Adapter:  ",
    x@adapter,
    " v",
    x@adapter_version,
    " protocol ",
    x@protocol_version,
    "\n",
    sep = ""
  )
  cat("Input:    ", x@input_schema, "\n", sep = "")
  cat("Result:   ", x@result_schema, "\n", sep = "")
  invisible(x)
}
