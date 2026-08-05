esm2_huggingface_environment <- function(compute) {
  cache <- file.path(
    compute@workspace,
    ".bionemor",
    "cache",
    "huggingface"
  )
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  credentials <- Sys.getenv(
    c("HF_TOKEN", "HUGGING_FACE_HUB_TOKEN"),
    unset = ""
  )
  credentials <- credentials[nzchar(credentials)]
  c(HF_HOME = cache, credentials)
}

esm2_gpu_count <- function(compute) {
  stopifnot(
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    )
  )
  ok <- identical(compute@gpus, 1L)
  list(
    ok = ok,
    detail = paste0(
      "ESM-2 currently requires gpus = 1 because the native Transformers ",
      "helper runs on one CUDA device; compute requests gpus = ",
      compute@gpus
    )
  )
}

esm2_embedding_plan <- function(
  input,
  portable,
  source,
  source_revision,
  compute
) {
  args <- c(
    "embed",
    "--model",
    source,
    if (!is.null(source_revision)) c("--revision", source_revision),
    "--input",
    input,
    "--output",
    portable
  )
  command_plan(
    list(command_spec(
      "bionemor-esm2-helper",
      args,
      env = esm2_huggingface_environment(compute),
      cwd = compute@workspace
    ))
  )
}

#' Extract pooled ESM-2 protein embeddings
#'
#' `esm2_embed()` runs the ESM-2 model through the package-pinned native
#' Transformers and Transformer Engine runtime. It returns one last-token,
#' L2-normalized embedding row per input protein. Model weights are downloaded
#' from the model's pinned Hugging Face revision on first use and cached below
#' the compute workspace.
#'
#' Use the embedding rows for sequence similarity, clustering, or as features
#' in downstream R models. They are model representations, not measurements of
#' protein function. ESM-2 currently requires `gpus = 1`. You may supply
#' multiple proteins; the runtime processes them one at a time on the selected
#' GPU to preserve their bidirectional attention boundaries.
#'
#' @param object An ESM-2 model descriptor from [esm2()] or [esm2_model()].
#' @param newdata A character vector of protein sequences, an `XStringSet`, a
#'   data frame with a `sequence` column, or a FASTA path. Named inputs retain
#'   their names as matrix row names.
#' @param compute A BioNeMo compute descriptor using [esm2_recipe()]. `NULL`
#'   uses the compute target attached by [esm2_model()].
#' @param output Optional prefix for the compressed float32 data (`.f32.gz`)
#'   and JSON metadata (`.json`). Container outputs must be inside the compute
#'   workspace.
#' @param name Optional run name.
#' @param async Whether to return a `BioNeMoJob` before completion.
#'
#' @return A numeric matrix with class `esm2_embeddings`, or a `BioNeMoJob`
#'   when `async = TRUE`. The matrix keeps ordinary matrix behavior. Its
#'   `provenance` attribute records the model, source and recipe revisions,
#'   pooling method, and path where the job was saved.
#'
#' @details Portable embedding files store little-endian, row-major float32
#'   values. R reads these values into its native double-precision numeric
#'   matrix.
#' @examples
#' \dontrun{
#' compute <- bionemo_compute(
#'   recipe = esm2_recipe(),
#'   engine = "container",
#'   workspace = "~/bionemor-workspace"
#' )
#' compute <- bionemo_install(compute)
#' model <- esm2_model("8m", compute)
#' esm2_embed(model, c(reference = "MKT", variant = "MNT"))
#' }
#' @export
esm2_embed <- function(
  object,
  newdata,
  compute = NULL,
  output = NULL,
  name = NULL,
  async = FALSE
) {
  stopifnot(
    "object must be an ESM-2 model" = S7_inherits(object, Esm2Model),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  compute <- resolve_model_compute(object, compute)
  stopifnot(
    "compute must use the ESM-2 Transformers recipe" = identical(
      compute@recipe@adapter,
      "esm2-transformers"
    )
  )
  output <- validate_output_path(
    output,
    compute,
    c(".f32.gz", ".json")
  )
  record <- esm2_model_record(object@size)
  gpu_count <- esm2_gpu_count(compute)
  if (!gpu_count$ok) {
    stop(gpu_count$detail, call. = FALSE)
  }
  checkpoint <- model_checkpoint_path(object, base = compute@workspace)
  if (is.null(checkpoint)) {
    source <- record$source
    source_revision <- record$source_revision
  } else {
    if (!dir.exists(checkpoint)) {
      stop("ESM-2 checkpoint path must be an existing directory")
    }
    if (
      compute@engine == "container" &&
        !path_is_within(checkpoint, compute@workspace)
    ) {
      stop("container checkpoint must be inside the compute workspace")
    }
    source <- normalizePath(checkpoint, mustWork = TRUE)
    source_revision <- NULL
  }

  request <- list(
    model = object@size,
    pooling = "last-token-l2",
    output = output
  )
  run <- create_run(compute, "embedding", name)
  run_path <- run$path
  input <- prepare_sequence_input(
    newdata,
    run_path,
    normalize = "protein",
    filename = "proteins.fasta"
  )
  request$input_source <- input$input_source
  too_long <- which(
    nchar(input$sequences, type = "chars") > object@context_length
  )
  if (length(too_long)) {
    index <- too_long[[1L]]
    bionemor_abort(
      "BN_CONTEXT_LIMIT",
      paste0(
        "embedding request '",
        input$ids[[index]],
        "' has ",
        nchar(input$sequences[[index]], type = "chars"),
        " residues but the context limit is ",
        object@context_length
      ),
      run_path = run_path,
      request_id = input$ids[[index]],
      operation = "embedding",
      model = object@size,
      recipe_revision = compute@recipe@revision,
      context_length = object@context_length
    )
  }
  portable <- file.path(run_path, "outputs", "embeddings")
  plan <- esm2_embedding_plan(
    input = input$path,
    portable = portable,
    source = source,
    source_revision = source_revision,
    compute = compute
  )
  source_format <- if (is.null(checkpoint)) {
    record$source_format
  } else {
    "huggingface"
  }
  source_digest <- if (is.null(checkpoint)) {
    list(algorithm = "git-revision", value = source_revision)
  } else {
    list(algorithm = "md5", value = path_digest(checkpoint))
  }
  operation <- operation_spec(
    run = run,
    request = request,
    plan = plan,
    result = list(type = "esm2-pooled", portable = portable),
    execution = list(
      input_ids = input$ids,
      embedding_size = object@embedding_size
    ),
    context = operation_context(
      model = list(
        name = object@size,
        model_size = object@model_size,
        revision = source_revision
      ),
      checkpoint = list(
        path = checkpoint,
        source = source,
        format = source_format,
        kind = "pretrained",
        revision = source_revision,
        digest = source_digest
      ),
      tokenizer = list(
        identity = source,
        revision = source_revision,
        digest = source_digest
      ),
      precision = list(
        semantic = "float32",
        resolved_recipe = "float32"
      )
    )
  )
  submit_operation(operation, async = async)
}

esm2_materialize_embedding <- function(job, operation) {
  descriptor <- operation$result
  execution <- operation$execution
  result <- read_pooled_embedding_matrix(
    descriptor$portable,
    unlist(execution$input_ids, use.names = FALSE),
    width = execution$embedding_size
  )
  class(result) <- c("esm2_embeddings", "matrix", "array")
  attr(result, "provenance") <- list(
    run_path = job@path,
    model = operation$context$model$name,
    source = operation$context$checkpoint$source,
    source_revision = operation$context$checkpoint$revision,
    pooling = operation$request$pooling,
    recipe_revision = job@compute@recipe@revision
  )
  if (!is.null(operation$request$output)) {
    copy_output_files(
      descriptor$portable,
      operation$request$output,
      c(".f32.gz", ".json")
    )
  }
  result
}

#' Run ESM-2 inference through the compatibility generic
#'
#' @param object An ESM-2 model.
#' @param newdata Protein sequences.
#' @param type Inference operation. ESM-2 currently supports `"embedding"`.
#' @param compute A BioNeMo compute descriptor.
#' @param ... Arguments passed to [esm2_embed()].
#'
#' @return An ESM-2 embedding matrix or job.
#' @noRd
method(predict, Esm2Model) <- function(
  object,
  newdata,
  type = c("embedding"),
  compute = NULL,
  ...
) {
  match.arg(type)
  esm2_embed(object, newdata, compute = compute, ...)
}

bionemor_adapter_esm2_transformers_install_spec <- function(recipe) {
  stopifnot(
    "recipe must use the ESM-2 Transformers adapter" = S7_inherits(
      recipe,
      BioNeMoRecipe
    ) &&
      identical(recipe@adapter, "esm2-transformers")
  )
  list(
    lock = esm2_recipe_lock(),
    helper = "bionemor-esm2-helper",
    helper_asset = c("scripts", "embed-esm2.py"),
    helper_filename = "embed-esm2.py",
    semantic_operations = "embed",
    docker_appendage = c(
      "docker",
      "esm2-transformers",
      "Dockerfile.append"
    ),
    image_repository = "bionemor/esm2-transformers",
    image_version = "esm2-transformers-5.14.1",
    probes = list(
      inference = "bionemor-esm2-helper",
      training = character(),
      conversion = character()
    ),
    command_keys = c(`bionemor-esm2-helper` = "embed")
  )
}

bionemor_adapter_esm2_transformers_doctor_model <- function(
  compute,
  model,
  report
) {
  stopifnot(
    "model must be an ESM-2 model" = S7_inherits(model, Esm2Model),
    "compute must use the ESM-2 Transformers adapter" = identical(
      compute@recipe@adapter,
      "esm2-transformers"
    ),
    "capability report must be a list" = is.list(report)
  )
  gpus <- report$runtime$gpus
  available_bytes <- if (
    is.data.frame(gpus) &&
      "total_memory_bytes" %in% names(gpus) &&
      nrow(gpus) >= compute@gpus
  ) {
    sum(as.double(utils::head(gpus$total_memory_bytes, compute@gpus)))
  } else {
    NA_real_
  }
  required_bytes <- 4 * esm2_model_record(model@size)$parameters
  memory_ok <- is.finite(available_bytes) && available_bytes >= required_bytes
  memory_detail <- if (is.finite(available_bytes)) {
    paste0(
      format(round(available_bytes / 1024^3, 1), nsmall = 1, trim = TRUE),
      " GiB available; ",
      format(round(required_bytes / 1024^3, 1), nsmall = 1, trim = TRUE),
      " GiB required for float32 weights before runtime overhead"
    )
  } else {
    "GPU memory is not reported"
  }
  checkpoint <- model_checkpoint_path(model, base = compute@workspace)
  source_ok <- is.null(checkpoint) || dir.exists(checkpoint)
  gpu_count <- esm2_gpu_count(compute)
  rbind(
    doctor_row(
      "model GPU count",
      if (gpu_count$ok) "pass" else "fail",
      gpu_count$detail
    ),
    doctor_row(
      "model memory floor",
      if (memory_ok) "pass" else "fail",
      memory_detail
    ),
    doctor_row(
      "model source",
      if (source_ok) "pass" else "fail",
      if (is.null(checkpoint)) {
        paste0(
          esm2_model_record(model@size)$source,
          "@",
          model@revision
        )
      } else if (source_ok) {
        normalizePath(checkpoint, mustWork = TRUE)
      } else {
        paste("checkpoint is not available:", checkpoint)
      }
    )
  )
}


bionemor_adapter_esm2_transformers_materialize <- function(
  job,
  operation
) {
  descriptor <- operation$result
  if (!identical(descriptor$type, "esm2-pooled")) {
    bionemor_abort(
      "BN_PROTOCOL",
      paste0("ESM-2 result contract is unsupported: ", descriptor$type),
      run_path = job@path,
      request_id = job@id,
      operation = job@kind
    )
  }
  esm2_materialize_embedding(job, operation)
}
