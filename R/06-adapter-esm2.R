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

esm2_tensor_parallelism <- function(object, compute) {
  stopifnot(
    "object must be an ESM-2 model" = S7_inherits(object, Esm2Model),
    "compute must be a BioNeMo compute descriptor" = S7_inherits(
      compute,
      BioNeMoCompute
    )
  )
  heads <- as.integer(esm2_model_record(object@size)$attention_heads)
  ok <- heads %% compute@gpus == 0L
  suffix <- if (ok) {
    "divides the attention-head count"
  } else {
    paste(
      "is unsupported because the tensor parallel size must divide",
      "the attention-head count"
    )
  }
  list(
    ok = ok,
    detail = sprintf(
      "ESM-2 model '%s' has %d attention heads; gpus = %d %s",
      object@size,
      heads,
      compute@gpus,
      suffix
    )
  )
}

esm2_embedding_plan <- function(
  input,
  portable,
  source,
  source_revision,
  max_num_batched_tokens,
  tensor_parallel_size,
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
    portable,
    "--max-num-batched-tokens",
    as.character(max_num_batched_tokens),
    "--tensor-parallel-size",
    as.character(tensor_parallel_size)
  )
  command_plan(
    list(command_spec(
      "bionemor-esm2-helper",
      args,
      env = esm2_huggingface_environment(compute),
      cwd = compute@workspace
    )),
    metadata = list(operation = "embedding")
  )
}

#' Extract pooled ESM-2 protein embeddings
#'
#' `esm2_embed()` runs the ESM-2 pooling model through the package-pinned vLLM
#' recipe and returns one last-token, L2-normalized embedding row per input
#' protein. Model weights are downloaded from the model's pinned Hugging Face
#' revision on first use and cached below the compute workspace.
#'
#' Use the embedding rows for sequence similarity, clustering, or as features
#' in downstream R models. They are model representations, not measurements of
#' protein function. The compute descriptor's GPU count is vLLM's tensor
#' parallel size and must divide the model's attention-head count reported by
#' [esm2_models()].
#'
#' @param object An ESM-2 model descriptor from [esm2()] or [esm2_model()].
#' @param newdata A character vector of protein sequences, an `XStringSet`, a
#'   data frame with a `sequence` column, or a FASTA path. Named inputs retain
#'   their names as matrix row names.
#' @param compute A BioNeMo compute descriptor using [esm2_recipe()]. `NULL`
#'   uses the compute target attached by [esm2_model()].
#' @param output Optional path for the portable JSONL result. Container outputs
#'   must be inside the compute workspace.
#' @param name Optional durable run name.
#' @param async Whether to return a `BioNeMoJob` before completion.
#'
#' @return A numeric matrix with class `esm2_embeddings`, or a `BioNeMoJob`
#'   when `async = TRUE`.
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
    "compute must use the ESM-2 vLLM recipe" = identical(
      compute@recipe@adapter,
      "esm2-vllm"
    )
  )
  output <- validate_output_path(output, compute)
  record <- esm2_model_record(object@size)
  tensor_parallelism <- esm2_tensor_parallelism(object, compute)
  if (!tensor_parallelism$ok) {
    stop(tensor_parallelism$detail, call. = FALSE)
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
    source = source,
    source_revision = source_revision,
    pooling = "last-token-l2"
  )
  workflow <- bionemo_workflow("esm2/embed")
  run_path <- create_run(
    compute,
    "embedding",
    name,
    request = request,
    workflow = workflow_identity(workflow)
  )
  input <- prepare_sequence_input(
    newdata,
    run_path,
    normalize = "protein",
    filename = "proteins.fasta"
  )
  persist_inference_input_source(run_path, input)
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
  portable <- file.path(run_path, "outputs", "embeddings.jsonl")
  plan <- esm2_embedding_plan(
    input = input$path,
    portable = portable,
    source = source,
    source_revision = source_revision,
    max_num_batched_tokens = as.integer(object@context_length + 2L),
    tensor_parallel_size = compute@gpus,
    compute = compute
  )
  submit_plan(
    plan,
    compute,
    run_path,
    "embedding",
    list(
      type = "esm2-pooled",
      portable = portable,
      input_ids = input$ids,
      model = object@size,
      model_size = object@model_size,
      embedding_size = object@embedding_size,
      source = source,
      source_revision = source_revision,
      source_format = if (is.null(checkpoint)) {
        record$source_format
      } else {
        "huggingface"
      },
      checkpoint = checkpoint,
      pooling = "last-token-l2",
      output = output
    ),
    async = async
  )
}

esm2_materialize_embedding <- function(job, descriptor) {
  if (!file.exists(descriptor$portable)) {
    stop("ESM-2 helper did not write its portable output")
  }
  rows <- read_jsonl_rows(descriptor$portable)
  if (!length(rows)) {
    stop("ESM-2 helper wrote no embedding rows")
  }
  ids <- pluck_chr(rows, "id")
  if (!identical(ids, unlist(descriptor$input_ids, use.names = FALSE))) {
    stop("ESM-2 embedding IDs do not match input order")
  }
  values <- lapply(rows, function(row) {
    as.double(unlist(row$embedding, use.names = FALSE))
  })
  if (
    any(lengths(values) != descriptor$embedding_size) ||
      any(!is.finite(unlist(values, use.names = FALSE)))
  ) {
    stop("ESM-2 embeddings have an invalid shape or non-finite values")
  }
  result <- do.call(rbind, values)
  rownames(result) <- ids
  colnames(result) <- paste0("dim_", seq_len(ncol(result)))
  class(result) <- c("esm2_embeddings", "matrix", "array")
  attr(result, "provenance") <- list(
    run_path = job@path,
    model = descriptor$model,
    source = descriptor$source,
    source_revision = descriptor$source_revision,
    pooling = descriptor$pooling,
    recipe_revision = job@compute@recipe@revision
  )
  if (!is.null(descriptor$output)) {
    copy_output_file(descriptor$portable, descriptor$output)
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

bionemor_adapter_esm2_vllm_install_spec <- function(recipe) {
  stopifnot(
    "recipe must use the ESM-2 vLLM adapter" = S7_inherits(
      recipe,
      BioNeMoRecipe
    ) &&
      identical(recipe@adapter, "esm2-vllm")
  )
  list(
    lock = esm2_recipe_lock(),
    helper = "bionemor-esm2-helper",
    helper_asset = c("scripts", "embed-esm2.py"),
    helper_filename = "embed-esm2.py",
    semantic_operations = "embed",
    docker_appendage = c("docker", "esm2-vllm", "Dockerfile.append"),
    uv_after_from = TRUE,
    image_repository = "bionemor/esm2",
    image_version = "esm2-vllm-0.15.1",
    build_args = c(INSTALL_VLLM = "true"),
    probes = list(
      inference = "bionemor-esm2-helper",
      training = character(),
      conversion = character()
    ),
    command_keys = c(`bionemor-esm2-helper` = "embed")
  )
}

bionemor_adapter_esm2_vllm_install_build_args <- function(
  compute,
  build_args
) {
  stopifnot(
    "compute must use the ESM-2 vLLM adapter" = S7_inherits(
      compute,
      BioNeMoCompute
    ) &&
      identical(compute@recipe@adapter, "esm2-vllm"),
    "build arguments must be a named character vector" = is.character(
      build_args
    ) &&
      !is.null(names(build_args))
  )
  probe <- run_install_command(
    "nvidia-smi",
    c("--query-gpu=compute_cap", "--format=csv,noheader,nounits"),
    error = "failed to detect the GPU compute capability",
    code = "BN_RUNTIME_MISSING",
    recipe_revision = compute@recipe@revision,
    hint = "Run nvidia-smi and verify that the requested GPUs are available."
  )
  architectures <- trimws(strsplit(trimws(probe$stdout), "\n")[[1L]])
  selected <- utils::head(architectures, compute@gpus)
  if (
    length(selected) != compute@gpus ||
      any(!grepl("^[0-9]+[.][0-9]+$", selected)) ||
      length(unique(selected)) != 1L
  ) {
    bionemor_abort(
      "BN_RUNTIME_MISSING",
      "ESM-2 image builds require the selected GPUs to report one shared compute capability",
      operation = "install",
      recipe_revision = compute@recipe@revision,
      hint = "Run nvidia-smi and select GPUs with the same compute capability."
    )
  }
  c(build_args, TORCH_CUDA_ARCH_LIST = selected[[1L]])
}

bionemor_adapter_esm2_vllm_install_image_suffix <- function(
  compute,
  build_args
) {
  stopifnot(
    "compute must use the ESM-2 vLLM adapter" = S7_inherits(
      compute,
      BioNeMoCompute
    ) &&
      identical(compute@recipe@adapter, "esm2-vllm"),
    "build arguments must contain one CUDA architecture" =
      is_scalar_string(build_args[["TORCH_CUDA_ARCH_LIST"]]) &&
      grepl(
        "^[0-9]+[.][0-9]+$",
        build_args[["TORCH_CUDA_ARCH_LIST"]]
      )
  )
  paste0(
    "sm",
    gsub(".", "", build_args[["TORCH_CUDA_ARCH_LIST"]], fixed = TRUE)
  )
}

bionemor_adapter_esm2_vllm_doctor_model <- function(compute, model, report) {
  stopifnot(
    "model must be an ESM-2 model" = S7_inherits(model, Esm2Model),
    "compute must use the ESM-2 vLLM adapter" = identical(
      compute@recipe@adapter,
      "esm2-vllm"
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
  tensor_parallelism <- esm2_tensor_parallelism(model, compute)
  rbind(
    doctor_row(
      "model tensor parallelism",
      if (tensor_parallelism$ok) "pass" else "fail",
      tensor_parallelism$detail
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

bionemor_adapter_esm2_vllm_manifest_context <- function(
  workflow,
  job,
  request,
  plan
) {
  stopifnot(
    "workflow must use the ESM-2 vLLM adapter" = identical(
      workflow@adapter,
      "esm2-vllm"
    ),
    "manifest request and plan must be lists" = is.list(request) &&
      is.list(plan)
  )
  descriptor <- job@expected_result
  revision <- descriptor$source_revision
  checkpoint_digest <- if (
    !is.null(descriptor$checkpoint) && file.exists(descriptor$checkpoint)
  ) {
    path_digest(descriptor$checkpoint)
  } else {
    NULL
  }
  source_digest <- if (!is.null(checkpoint_digest)) {
    list(algorithm = "md5", value = checkpoint_digest)
  } else if (!is.null(revision)) {
    list(algorithm = "git-revision", value = revision)
  } else {
    NULL
  }
  list(
    checkpoint = list(
      path = descriptor$checkpoint,
      source = descriptor$source,
      format = descriptor$source_format,
      kind = "pretrained",
      revision = revision,
      digest = source_digest
    ),
    model = list(
      name = descriptor$model,
      model_size = descriptor$model_size,
      revision = revision
    ),
    tokenizer = list(
      identity = descriptor$source,
      revision = revision,
      digest = source_digest
    ),
    precision = list(semantic = "float32", resolved_recipe = "float32"),
    warnings = list()
  )
}

bionemor_adapter_esm2_vllm_materialize <- function(
  workflow,
  job,
  descriptor
) {
  stopifnot(
    "workflow must use the ESM-2 vLLM adapter" = identical(
      workflow@adapter,
      "esm2-vllm"
    ),
    "job result descriptor must be a list" = is.list(descriptor)
  )
  if (!identical(workflow@task, "embed")) {
    bionemor_abort(
      "BN_PROTOCOL",
      paste0("ESM-2 result workflow is unsupported: ", workflow@id),
      run_path = job@path,
      request_id = job@id,
      operation = workflow@task
    )
  }
  esm2_materialize_embedding(job, descriptor)
}

bionemor_adapter_esm2_vllm_run <- function(
  workflow,
  model,
  input,
  compute,
  parameters,
  async,
  name
) {
  stopifnot(
    "workflow must use the ESM-2 vLLM adapter" = identical(
      workflow@adapter,
      "esm2-vllm"
    ),
    "model must be an ESM-2 model" = S7_inherits(model, Esm2Model),
    "workflow and model families must match" = identical(
      workflow@family,
      model@family
    )
  )
  compute <- resolve_model_compute(model, compute)
  stopifnot(
    "workflow and compute adapters must match" = identical(
      workflow@adapter,
      compute@recipe@adapter
    )
  )
  if (!identical(workflow@task, "embed")) {
    bionemor_abort(
      "BN_WORKFLOW_UNKNOWN",
      paste0("ESM-2 workflow is unsupported: ", workflow@id),
      operation = "workflow-dispatch"
    )
  }
  arguments <- list(
    object = model,
    newdata = input,
    compute = compute,
    async = async,
    name = name
  )
  workflow_call(esm2_embed, arguments, parameters)
}
