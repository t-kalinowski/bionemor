#' Fit or fine-tune an Evo 2 model
#'
#' @param object An Evo 2 model.
#' @param data A FASTA path, character vector, data frame with a `sequence`
#'   column, or `Biostrings::DNAStringSet`.
#' @param compute A BioNeMo compute specification.
#' @param steps Positive integer training steps.
#' @param control Typed controls from [evo2_fit_control()].
#' @param name Optional job name.
#' @param output Optional output path, relative to the compute workspace.
#' @param timeout Complete operation timeout in seconds. A finite timeout
#'   requires the `timeout` command for local execution.
#' @param async Whether to return a job before completion.
#' @param ... Reserved for future adapter arguments.
#'
#' @return A `BioNeMoJob` asynchronously or a fitted `Evo2Model`.
#' @noRd
method(fit, Evo2Model) <- function(
  object,
  data,
  compute,
  steps,
  control = evo2_fit_control(),
  name = NULL,
  output = NULL,
  timeout = Inf,
  async = FALSE,
  ...
) {
  dots <- list(...)
  stopifnot(
    "`...` is reserved and must be empty" = length(dots) == 0L,
    "object must be an Evo 2 model" = S7_inherits(object, Evo2Model),
    "compute must be a BioNeMo compute specification" =
      S7_inherits(compute, BioNeMoCompute),
    "compute workspace must exist" = dir.exists(compute@workspace),
    "steps must be a positive integer" =
      is_scalar_integerish(steps, min = 1),
    "control must be an Evo2FitControl" =
      S7_inherits(control, Evo2FitControl),
    "timeout must be positive" =
      is_scalar_number(timeout) && timeout > 0 || identical(timeout, Inf),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  checkpoint <- model_checkpoint_path(object, base = compute@workspace)
  if (object@pretrained && is.null(checkpoint)) {
    stop(
      "fitting with pretrained weights requires an explicit checkpoint",
      call. = FALSE
    )
  }
  stopifnot(
    "explicit checkpoint does not exist" =
      is.null(checkpoint) || dir.exists(checkpoint)
  )
  name <- name %||% basename(tempfile("evo2-fit-"))
  stopifnot(
    "name must be one safe job name" =
      is_scalar_string(name) &&
        grepl("^[A-Za-z0-9_.-]+$", name) &&
        !(name %in% c(".", "..")),
    "output must be NULL or one non-empty string" =
      is.null(output) || is_scalar_string(output)
  )
  output <- normalize_path(
    output %||% file.path(compute@workspace, "artifacts", name),
    base = compute@workspace
  )
  input <- prepare_sequence_input(data, compute@workspace, name)
  if (compute@engine == "container") {
    stopifnot(
      "container input must be inside the compute workspace" =
        path_is_within(input$path, compute@workspace),
      "container output must be inside the compute workspace" =
        path_is_within(output, compute@workspace),
      "container checkpoint must be inside the compute workspace" =
        is.null(checkpoint) || path_is_within(checkpoint, compute@workspace)
    )
  }
  configs <- evo2_fit_configs(input, compute, name, control)
  base_command <- evo2_train_command(
    object,
    configs,
    compute,
    as.integer(steps),
    control,
    output,
    name
  )
  wrapped <- wrap_compute_command(
    base_command,
    compute,
    name
  )
  command <- bound_operation_command(wrapped$command, compute, timeout)
  result <- Evo2Model(
    family = "evo2",
    checkpoint = file.path(output, "checkpoint"),
    pretrained = TRUE,
    task = "causal_lm",
    config = object@config,
    provenance = list(
      parent_checkpoint = if (is.null(checkpoint)) NULL else normalize_path(checkpoint),
      profile = compute@profile,
      precision = control@precision,
      control = control,
      steps = as.integer(steps),
      data = list(
        source = input$source,
        path = input$path,
        ids = input$ids
      )
    ),
    size = object@size
  )
  job <- submit_job(
    command,
    compute,
    name,
    kind = "fit",
    expected_result = result,
    timeout = timeout,
    metadata = list(
      input = input$path,
      preprocess = configs$preprocess,
      dataset = configs$dataset,
      output = output,
      container_name = wrapped$container_name
    )
  )
  if (async) job else job_wait(job, poll = 0.05)
}
