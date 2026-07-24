copy_materializer <- function(compute, name) {
  source <- system.file(
    "scripts",
    "materialize-evo2.py",
    package = "bionemor"
  )
  if (!nzchar(source)) {
    source <- file.path("inst", "scripts", "materialize-evo2.py")
  }
  stopifnot("Evo 2 materializer is not installed" = file.exists(source))
  root <- file.path(compute@workspace, ".bionemor", "jobs", name)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(root, "materialize-evo2.py")
  file.copy(source, destination, overwrite = TRUE)
  destination
}

#' Run batch prediction with an Evo 2 checkpoint
#'
#' @param object An Evo 2 model with an explicit checkpoint.
#' @param newdata Sequences or a FASTA path.
#' @param type Prediction type.
#' @param compute A BioNeMo compute specification.
#' @param async Whether to return a job before completion.
#' @param output Optional output path.
#' @param name Optional job name.
#' @param reduction Score reduction.
#' @param num_tokens,temperature,top_k,top_p Generation controls.
#' @param precision Numerical precision.
#' @param extra_args Explicit additional CLI arguments.
#' @param ... Reserved for future adapter arguments.
#'
#' @return A `BioNeMoJob` asynchronously or a `BioNeMoPrediction`.
#' @noRd
method(predict, Evo2Model) <- function(
  object,
  newdata,
  type = c("response", "score", "raw"),
  compute,
  async = FALSE,
  output = NULL,
  name = NULL,
  reduction = c("sum", "mean"),
  num_tokens = 100L,
  temperature = 0.7,
  top_k = 3L,
  top_p = 0,
  precision = c("bf16", "fp8"),
  extra_args = character(),
  ...
) {
  dots <- list(...)
  stopifnot(
    "`...` is reserved and must be empty" = length(dots) == 0L,
    "object must be an Evo 2 model" = S7_inherits(object, Evo2Model),
    "compute must be a BioNeMo compute specification" =
      S7_inherits(compute, BioNeMoCompute),
    "compute workspace must exist" = dir.exists(compute@workspace),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  type <- match.arg(type)
  reduction <- match.arg(reduction)
  precision <- match.arg(precision)
  extra_args <- validate_prediction_extra_args(extra_args, precision)
  checkpoint <- model_checkpoint_path(object, base = compute@workspace)
  stopifnot(
    "batch prediction requires an explicit checkpoint" =
      is_scalar_string(checkpoint),
    "checkpoint does not exist" = dir.exists(checkpoint)
  )
  if (type == "response") {
    validate_generation_controls(num_tokens, temperature, top_k, top_p)
  }
  name <- name %||% basename(tempfile("evo2-predict-"))
  stopifnot(
    "name must be one safe job name" =
      is_scalar_string(name) &&
        grepl("^[A-Za-z0-9_.-]+$", name) &&
        !(name %in% c(".", "..")),
    "output must be NULL or one non-empty string" =
      is.null(output) || is_scalar_string(output)
  )
  input <- prepare_sequence_input(newdata, compute@workspace, name)
  output <- normalize_path(
    output %||% file.path(compute@workspace, "artifacts", name),
    base = compute@workspace
  )
  if (compute@engine == "container") {
    stopifnot(
      "container checkpoint must be inside the compute workspace" =
        path_is_within(checkpoint, compute@workspace),
      "container input must be inside the compute workspace" =
        path_is_within(input$path, compute@workspace),
      "container output must be inside the compute workspace" =
        path_is_within(output, compute@workspace)
    )
  }
  helper <- copy_materializer(compute, name)
  base_command <- evo2_predict_command(
    object,
    input,
    type,
    compute,
    output,
    reduction,
    as.integer(num_tokens),
    as.double(temperature),
    as.integer(top_k),
    as.double(top_p),
    precision,
    extra_args,
    helper
  )
  wrapped <- wrap_compute_command(base_command, compute, name)
  result <- BioNeMoPrediction(
    type = type,
    data = NULL,
    provenance = list(
      checkpoint = checkpoint,
      profile = compute@profile,
      precision = precision
    ),
    metadata = list(
      input = input$path,
      ids = input$ids,
      output = output,
      checkpoint = normalize_path(checkpoint),
      profile = compute@profile,
      reduction = reduction
    )
  )
  job <- submit_job(
    wrapped$command,
    compute,
    name,
    kind = "predict",
    expected_result = result,
    metadata = list(
      input = input$path,
      output = output,
      container_name = wrapped$container_name
    )
  )
  if (async) job else job_wait(job, poll = 0.05)
}
