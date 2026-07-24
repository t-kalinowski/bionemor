#' Describe BioNeMo execution resources
#'
#' @param backend Execution backend.
#' @param workspace Workspace visible to the selected execution engine. It
#'   must not be a filesystem root.
#' @param profile Versioned BioNeMo command profile.
#' @param engine External execution engine.
#' @param image Container image for Docker or Apptainer execution.
#' @param gpus,nodes Positive integer resource counts. The initial profile
#'   supports one node.
#' @param queue,account,walltime Optional Slurm fields.
#' @param config Additional site-specific configuration.
#'
#' @return An S7 `BioNeMoCompute`.
#' @export
bionemo_compute <- function(
  backend = c("local", "slurm"),
  workspace = getwd(),
  profile = "bionemo-2.6.3",
  engine = c("python", "container"),
  image = NULL,
  gpus = 1L,
  nodes = 1L,
  queue = NULL,
  account = NULL,
  walltime = NULL,
  config = list()
) {
  backend <- match.arg(backend)
  engine <- match.arg(engine)
  stopifnot(
    "workspace must be one non-empty string" = is_scalar_string(workspace),
    "profile must be 'bionemo-2.6.3'" = identical(profile, "bionemo-2.6.3"),
    "image must be NULL or one non-empty string" =
      is.null(image) || is_scalar_string(image),
    "gpus must be a positive integer" = is_scalar_integerish(gpus, min = 1),
    "nodes must be a positive integer" = is_scalar_integerish(nodes, min = 1),
    "the bionemo-2.6.3 profile supports a single node" = nodes == 1,
    "queue must be NULL or one non-empty string" =
      is.null(queue) || is_scalar_string(queue),
    "account must be NULL or one non-empty string" =
      is.null(account) || is_scalar_string(account),
    "walltime must be NULL or one non-empty string" =
      is.null(walltime) || is_scalar_string(walltime),
    "config must be a list" = is.list(config)
  )
  workspace <- normalize_path(workspace)
  stopifnot(
    "workspace must not be the filesystem root" =
      !identical(dirname(workspace), workspace)
  )

  BioNeMoCompute(
    backend = backend,
    engine = engine,
    workspace = workspace,
    image = image,
    gpus = as.integer(gpus),
    nodes = as.integer(nodes),
    queue = queue,
    account = account,
    walltime = walltime,
    profile = profile,
    config = config
  )
}

method(print, BioNeMoCompute) <- function(x, ...) {
  cat("<bionemor_compute>\n", sep = "")
  cat("Backend: ", x@backend, "\n", sep = "")
  cat("Engine: ", x@engine, "\n", sep = "")
  cat("Workspace: ", x@workspace, "\n", sep = "")
  cat("Resources: ", x@nodes, " node(s), ", x@gpus, " GPU(s)\n", sep = "")
  invisible(x)
}

#' Report implemented BioNeMo operations
#'
#' @param x A model, checkpoint, or compute specification.
#'
#' @return A data frame of operations and support status.
#' @export
bionemo_capabilities <- function(x) {
  stopifnot(
    "x must be a BioNeMo model, checkpoint, or compute specification" =
      S7_inherits(x, BioNeMoModel) ||
        S7_inherits(x, BioNeMoCheckpoint) ||
        S7_inherits(x, BioNeMoCompute)
  )
  data.frame(
    operation = c("checkpoint", "fit", "response", "score", "raw"),
    supported = TRUE,
    profile = "bionemo-2.6.3",
    stringsAsFactors = FALSE
  )
}
