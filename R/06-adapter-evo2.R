validate_sequence_context <- function(
  input,
  object,
  compute,
  operation,
  run_path,
  checkpoint,
  additional_tokens = 0L,
  max_sequence_length = NULL
) {
  stopifnot(
    "input must be a prepared sequence input" = is.list(input) &&
      is.character(input$ids) &&
      is.character(input$sequences),
    "object must be an Evo 2 model" = S7_inherits(object, Evo2Model),
    "compute must be a BioNeMo compute specification" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "operation must be one inference operation" = operation %in%
      c("generation", "score", "profile", "embedding"),
    "run path must exist" = is_scalar_string(run_path) && dir.exists(run_path),
    "checkpoint must be one non-empty string" = is_scalar_string(checkpoint),
    "additional tokens must be a non-negative integer" = is_scalar_integerish(
      additional_tokens,
      min = 0
    ),
    "maximum sequence length must be NULL or a positive integer" = is.null(
      max_sequence_length
    ) ||
      is_scalar_integerish(max_sequence_length, min = 1)
  )
  model_context_length <- as.integer(object@context_length)
  context_length <- if (is.null(max_sequence_length)) {
    model_context_length
  } else {
    min(model_context_length, as.integer(max_sequence_length))
  }
  sequence_lengths <- nchar(input$sequences, type = "chars")
  required_lengths <- sequence_lengths + as.integer(additional_tokens)
  invalid <- which(required_lengths > context_length)
  if (length(invalid)) {
    index <- invalid[[1L]]
    bionemor_abort(
      "BN_CONTEXT_LIMIT",
      paste0(
        operation,
        " request '",
        input$ids[[index]],
        "' requires ",
        required_lengths[[index]],
        " tokens but the context limit is ",
        context_length
      ),
      run_path = run_path,
      request_id = input$ids[[index]],
      operation = operation,
      model = object@size,
      checkpoint = checkpoint,
      recipe_revision = compute@recipe@revision,
      context_length = as.integer(context_length),
      model_context_length = model_context_length,
      sequence_length = as.integer(sequence_lengths[[index]]),
      additional_tokens = as.integer(additional_tokens),
      required_length = as.integer(required_lengths[[index]]),
      hint = paste0(
        "shorten the input",
        if (additional_tokens > 0L) " or request fewer generated tokens" else ""
      )
    )
  }
  invisible(input)
}

reverse_complement <- function(x, request_id = NULL) {
  complement <- c(
    A = "T",
    C = "G",
    G = "C",
    T = "A",
    R = "Y",
    Y = "R",
    S = "S",
    W = "W",
    K = "M",
    M = "K",
    B = "V",
    D = "H",
    H = "D",
    V = "B",
    N = "N"
  )
  vapply(
    strsplit(x, "", fixed = TRUE),
    function(bases) {
      if (!all(bases %in% names(complement))) {
        abort_invalid_sequence(
          paste0(
            "reverse-complement input may contain only uppercase IUPAC DNA symbols"
          ),
          request_id = request_id
        )
      }
      paste0(rev(unname(complement[bases])), collapse = "")
    },
    character(1)
  )
}

prepare_stranded_input <- function(input, run_path, strand) {
  strand <- match.arg(strand, c("forward", "reverse", "both"))
  records <- vector("list", 0L)
  sequences <- character()
  for (index in seq_along(input$sequences)) {
    id <- input$ids[[index]]
    sequence <- unname(input$sequences[[index]])
    selected <- switch(
      strand,
      forward = "forward",
      reverse = "reverse",
      both = c("forward", "reverse")
    )
    for (direction in selected) {
      derived_id <- if (strand == "forward") {
        id
      } else {
        paste0(id, "::", direction)
      }
      derived <- if (direction == "forward") {
        sequence
      } else {
        reverse_complement(sequence, request_id = id)
      }
      sequences[[derived_id]] <- derived
      records[[length(records) + 1L]] <- list(
        derived_id = derived_id,
        id = id,
        input_index = as.integer(index),
        strand = direction,
        sequence = derived,
        sequence_length = nchar(sequence, type = "chars")
      )
    }
  }
  input$path <- write_fasta(
    sequences,
    file.path(run_path, "inputs", "sequences.fasta")
  )
  input$derived_sequences <- sequences
  input$map <- records
  atomic_write_json(
    records,
    file.path(run_path, "inputs", "sequence-map.json"),
    auto_unbox = TRUE
  )
  input
}

#' Construct an Evo 2 phylogenetic prompt tag
#'
#' Evo 2 represents taxonomy context as a pipe-enclosed, semicolon-delimited tag
#' with the short rank keys `d`, `p`, `c`, `o`, `f`, `g`, and `s`. Missing ranks
#' are serialized as `None`. With `uppercase = TRUE`, the complete serialized
#' tag, including missing-rank markers, is converted to uppercase.
#'
#' @param domain,phylum,class,order,family,genus,species Optional taxonomy
#'   ranks. Values cannot contain `;`, `|`, or line breaks.
#' @param uppercase Whether to uppercase the serialized tag.
#'
#' @return One Evo 2 phylogenetic prompt tag.
#'
#' @examples
#' evo2_phylo_tag(
#'   domain = "Bacteria",
#'   phylum = "Proteobacteria",
#'   genus = "Escherichia"
#' )
#'
#' @export
evo2_phylo_tag <- function(
  domain = NULL,
  phylum = NULL,
  class = NULL,
  order = NULL,
  family = NULL,
  genus = NULL,
  species = NULL,
  uppercase = TRUE
) {
  ranks <- list(
    d = domain,
    p = phylum,
    c = class,
    o = order,
    f = family,
    g = genus,
    s = species
  )
  if (
    !all(vapply(
      ranks,
      function(x) is.null(x) || is_scalar_string(x),
      logical(1)
    ))
  ) {
    stop("taxonomy ranks must be NULL or one non-empty string")
  }
  if (
    !all(vapply(
      ranks,
      function(x) is.null(x) || !grepl("[;|\r\n]", x),
      logical(1)
    ))
  ) {
    stop("taxonomy ranks must not contain separators or line breaks")
  }
  if (!is_scalar_logical(uppercase)) {
    stop("uppercase must be TRUE or FALSE")
  }
  values <- vapply(ranks, function(x) x %||% "None", character(1))
  tag <- paste0(
    "|",
    paste0(names(values), "__", values, collapse = ";"),
    "|"
  )
  if (uppercase) toupper(tag) else tag
}

control_property <- function(control, name, default = NULL) {
  tryCatch(S7::prop(control, name), error = function(...) default)
}

inference_checkpoint_defaults <- function(checkpoint, manifest = NULL) {
  if (is.null(checkpoint) && is.null(manifest)) {
    return(list(
      mixed_precision_recipe = NULL,
      precision_policy = NULL,
      vortex_style_fp8 = NULL
    ))
  }
  if (!is.list(manifest)) {
    manifest <- tryCatch(
      read_checkpoint_manifest(checkpoint),
      error = function(...) NULL
    )
  }
  if (!is.list(manifest)) {
    return(list(
      mixed_precision_recipe = NULL,
      precision_policy = NULL,
      vortex_style_fp8 = NULL
    ))
  }
  record <- tryCatch(
    evo2_model_record(manifest$variant),
    error = function(...) NULL
  )
  list(
    mixed_precision_recipe = manifest$mixed_precision_recipe %||%
      record$mixed_precision_recipe %||%
      NULL,
    precision_policy = record$precision_policy %||% NULL,
    vortex_style_fp8 = isTRUE(manifest$inspection$vortex_style_fp8)
  )
}

verified_hopper_runtime <- function(compute) {
  report <- compute@config$capabilities
  if (!is.list(report)) {
    report <- runtime_capabilities(compute, refresh = TRUE)
  }
  gpus <- report$runtime$gpus
  majors <- if (
    is.data.frame(gpus) && "compute_capability_major" %in% names(gpus)
  ) {
    as.integer(gpus$compute_capability_major)
  } else if (is.list(gpus)) {
    vapply(
      gpus,
      function(gpu) as.integer(gpu$compute_capability_major %||% NA_integer_),
      integer(1)
    )
  } else {
    integer()
  }
  length(majors) >= compute@gpus &&
    !anyNA(majors) &&
    all(majors == 9L)
}

resolved_inference_control <- function(
  control,
  compute,
  operation,
  checkpoint = NULL,
  checkpoint_manifest = NULL
) {
  tensor <- control_property(control, "tensor_parallel_size", 1L)
  pipeline <- 1L
  context <- control_property(control, "context_parallel_size", 1L)
  precision <- control_property(control, "precision", "auto")
  mixed <- control_property(control, "mixed_precision_recipe", NULL)
  defaults <- inference_checkpoint_defaults(checkpoint, checkpoint_manifest)
  vortex_setting <- control_property(control, "vortex_style_fp8", "auto")
  automatic_vortex <- identical(vortex_setting, "auto") &&
    (isTRUE(defaults$vortex_style_fp8) ||
      identical(defaults$precision_policy, "vortex-fp8-on-hopper"))
  vortex <- switch(
    vortex_setting,
    yes = TRUE,
    no = FALSE,
    auto = automatic_vortex
  )
  if (is.null(mixed)) {
    mixed <- switch(
      precision,
      auto = defaults$mixed_precision_recipe,
      bf16 = "bf16_mixed",
      fp8 = if (vortex) {
        "bf16_mixed"
      } else {
        "bf16_with_fp8_current_scaling_mixed"
      }
    )
  }
  if (automatic_vortex) {
    if (!verified_hopper_runtime(compute)) {
      stop(
        "automatic Vortex FP8 requires a verified Hopper GPU capability report"
      )
    }
  }
  subquadratic <- control_property(control, "subquadratic_ops", FALSE)
  cuda_graphs <- control_property(control, "cuda_graphs", "auto")
  if (identical(cuda_graphs, "auto")) {
    cuda_graphs <- if (subquadratic) "none" else "local"
  }
  world_size <- as.integer(compute@gpus * compute@nodes)
  model_parallel <- as.integer(tensor * pipeline * context)
  if (!is_scalar_integerish(tensor, min = 1)) {
    stop("tensor parallelism must be a positive integer")
  }
  if (!identical(as.integer(pipeline), 1L)) {
    stop("pipeline parallelism must equal one")
  }
  if (!is_scalar_integerish(context, min = 1)) {
    stop("context parallelism must be a positive integer")
  }
  if (model_parallel > world_size) {
    stop("model parallelism cannot exceed the allocated world size")
  }
  if (operation == "generation") {
    if (model_parallel != world_size) {
      stop("generation world size must equal the model-parallel product")
    }
  }
  list(
    tensor_parallel_size = as.integer(tensor),
    pipeline_parallel_size = as.integer(pipeline),
    context_parallel_size = as.integer(context),
    world_size = world_size,
    processes_per_node = as.integer(world_size / compute@nodes),
    precision = precision,
    mixed_precision_recipe = mixed,
    vortex_style_fp8 = vortex,
    max_sequence_length = control_property(
      control,
      "max_sequence_length",
      NULL
    ),
    max_batch_size = as.integer(
      control_property(control, "max_batch_size", 1L)
    ),
    cuda_graphs = cuda_graphs,
    subquadratic_ops = subquadratic,
    chunked_prefill = control_property(
      control,
      "chunked_prefill",
      FALSE
    ),
    dynamic_max_tokens = control_property(
      control,
      "dynamic_max_tokens",
      NULL
    ),
    dynamic_block_size = as.integer(
      control_property(control, "dynamic_block_size", 256L)
    ),
    extra = control_property(control, "extra", list())
  )
}

parallel_command_args <- function(resolved) {
  c(
    "--tensor-parallel-size",
    as.character(resolved$tensor_parallel_size),
    "--pipeline-model-parallel-size",
    as.character(resolved$pipeline_parallel_size),
    "--context-parallel-size",
    as.character(resolved$context_parallel_size)
  )
}

precision_command_args <- function(resolved) {
  args <- character()
  if (!is.null(resolved$mixed_precision_recipe)) {
    args <- c(
      args,
      "--mixed-precision-recipe",
      resolved$mixed_precision_recipe
    )
  }
  if (isTRUE(resolved$vortex_style_fp8)) {
    args <- c(args, "--vortex-style-fp8")
  }
  args
}

prediction_extra_args <- function(extra) {
  mapping <- c(
    no_sequence_parallel = "--no-sequence-parallel",
    min_length = "--min-length"
  )
  unlist(
    Map(
      function(name, value) {
        flag <- unname(mapping[[name]])
        if (is.logical(value)) {
          if (isTRUE(value)) flag else character()
        } else {
          c(
            flag,
            if (is.numeric(value)) format_number(value) else as.character(value)
          )
        }
      },
      names(extra),
      extra
    ),
    use.names = FALSE
  )
}

validate_generation_control <- function(control) {
  if (length(control_property(control, "extra", list())) != 0L) {
    stop("inference extra settings are not supported for generation")
  }
  invisible(control)
}

torchrun_command <- function(operation, args, resolved, cwd) {
  command_spec(
    executable = "torchrun",
    args = c(
      "--nproc-per-node",
      as.character(resolved$processes_per_node),
      "--no-python",
      operation,
      args
    ),
    cwd = cwd
  )
}

evo2_generation_plan <- function(
  run_path,
  checkpoint,
  prompts,
  upstream,
  portable,
  fasta,
  validation,
  compute,
  control,
  num_tokens,
  temperature,
  top_k,
  top_p,
  seed,
  return_probabilities,
  validate,
  checkpoint_manifest = NULL
) {
  resolved <- resolved_inference_control(
    control,
    compute,
    "generation",
    checkpoint,
    checkpoint_manifest
  )
  run_path <- normalizePath(run_path, mustWork = TRUE)
  relative_path <- function(path) {
    path <- normalize_path(path)
    prefix <- paste0(run_path, .Platform$file.sep)
    stopifnot(
      "execution paths must be inside the run directory" = startsWith(
        path,
        prefix
      )
    )
    substring(path, nchar(prefix) + 1L)
  }
  execution <- list(
    schema_version = 1L,
    driver = "evo2-megatron",
    operation = "generate",
    checkpoint = checkpoint,
    inputs = list(prompts = relative_path(prompts)),
    outputs = list(
      upstream = relative_path(upstream),
      portable = relative_path(portable),
      fasta = relative_path(fasta),
      validation = relative_path(validation)
    ),
    parameters = list(
      max_new_tokens = as.integer(num_tokens),
      temperature = as.double(temperature),
      top_k = as.integer(top_k),
      top_p = as.double(top_p),
      seed = seed,
      return_probabilities = return_probabilities,
      validate = validate
    ),
    resolved = resolved[c(
      "processes_per_node",
      "tensor_parallel_size",
      "pipeline_parallel_size",
      "context_parallel_size",
      "mixed_precision_recipe",
      "vortex_style_fp8",
      "max_sequence_length",
      "max_batch_size",
      "cuda_graphs",
      "subquadratic_ops",
      "chunked_prefill",
      "dynamic_max_tokens",
      "dynamic_block_size"
    )]
  )
  request_path <- file.path(run_path, "request.json")
  request <- read_json_file(request_path, simplify = FALSE)
  request$execution <- execution
  atomic_write_json(request, request_path)
  command_plan(
    list(
      command_spec(
        "bionemor-evo2-helper",
        c("run", "--request", request_path),
        cwd = compute@workspace
      )
    ),
    metadata = list(
      operation = "generation",
      resolved_control = resolved,
      failure_contract = list(
        active_step = "generation-validation",
        exit_codes = list(
          `65` = "BN_OUTPUT_SCHEMA",
          `66` = "BN_NONFINITE_OUTPUT",
          `67` = "BN_INVALID_SEQUENCE"
        )
      )
    )
  )
}

evo2_prediction_plan <- function(
  mode,
  checkpoint,
  input,
  upstream,
  portable,
  compute,
  control,
  batch_size,
  reduction = NULL,
  layer = NULL,
  pool = NULL,
  prepend_bos = FALSE,
  checkpoint_manifest = NULL
) {
  if (
    !mode %in% c("score", "profile", "embedding-pooled", "embedding-unpooled")
  ) {
    stop("prediction mode is unsupported")
  }
  resolved <- resolved_inference_control(
    control,
    compute,
    mode,
    checkpoint,
    checkpoint_manifest
  )
  stopifnot(
    "max_sequence_length is supported only for generation" = is.null(
      resolved$max_sequence_length
    ),
    "max_batch_size is supported only for generation" = identical(
      resolved$max_batch_size,
      1L
    ),
    "CUDA graph controls are supported only for generation" = identical(
      control_property(control, "cuda_graphs", "auto"),
      "auto"
    ),
    "chunked prefill is supported only for generation" = !resolved$chunked_prefill,
    "dynamic_max_tokens is supported only for generation" = is.null(
      resolved$dynamic_max_tokens
    ),
    "dynamic_block_size is supported only for generation" = identical(
      resolved$dynamic_block_size,
      256L
    )
  )
  predict_args <- c(
    "--fasta",
    input$path,
    "--ckpt-dir",
    checkpoint,
    "--output-dir",
    upstream,
    "--micro-batch-size",
    as.character(batch_size),
    "--write-interval",
    "epoch",
    parallel_command_args(resolved),
    precision_command_args(resolved),
    if (resolved$subquadratic_ops) "--use-subquadratic-ops",
    prediction_extra_args(resolved$extra),
    if (prepend_bos) "--prepend-bos"
  )
  if (mode == "score") {
    predict_args <- c(
      predict_args,
      "--output-log-prob-seqs",
      "--log-prob-collapse-option",
      "per_token"
    )
  } else if (mode == "profile") {
    predict_args <- c(
      predict_args,
      "--output-log-prob-seqs",
      "--log-prob-collapse-option",
      "per_token"
    )
  } else {
    predict_args <- c(
      predict_args,
      "--embedding-layer",
      as.character(layer)
    )
  }
  helper_args <- c(
    "materialize-predictions",
    "--mode",
    mode,
    "--input",
    upstream,
    "--sequence-map",
    file.path(
      dirname(dirname(input$path)),
      "inputs",
      "sequence-map.json"
    ),
    "--output",
    portable,
    if (mode == "score") c("--reduction", reduction),
    if (!is.null(pool)) c("--pool", pool)
  )
  command_plan(
    list(
      torchrun_command(
        "predict_evo2",
        predict_args,
        resolved,
        compute@workspace
      ),
      command_spec(
        "bionemor-evo2-helper",
        helper_args,
        cwd = compute@workspace
      )
    ),
    metadata = list(
      operation = mode,
      resolved_control = resolved,
      cleanup = list(directory = "upstream", suffix = ".pt")
    )
  )
}

bionemor_adapter_evo2_megatron_install_spec <- function(recipe) {
  stopifnot(
    "recipe must use the Evo 2 Megatron adapter" = S7_inherits(
      recipe,
      BioNeMoRecipe
    ) &&
      identical(recipe@adapter, "evo2-megatron")
  )
  list(
    lock = evo2_recipe_lock(),
    helper = "bionemor-evo2-helper",
    helper_asset = c("scripts", "materialize-evo2.py"),
    helper_filename = "materialize-evo2.py",
    semantic_operations = "generate",
    docker_appendage = c("docker", "evo2-recipes", "Dockerfile.append"),
    uv_fallback = "#COPY --from=ghcr.io/astral-sh/uv:latest /uv /uvx /bin/",
    image_repository = "bionemor/evo2",
    image_version = paste0("evo2-recipe-", recipe@recipe_version),
    probes = list(
      inference = c("infer_evo2", "predict_evo2"),
      training = c("preprocess_evo2", "train_evo2"),
      conversion = c(
        "evo2_convert_savanna_to_mbridge",
        "evo2_convert_nemo2_to_mbridge",
        "evo2_export_mbridge_to_vortex",
        "evo2_remove_optimizer"
      )
    ),
    command_keys = c(
      evo2_convert_savanna_to_mbridge = "savanna_to_mbridge",
      evo2_convert_nemo2_to_mbridge = "nemo2_to_mbridge",
      evo2_export_mbridge_to_vortex = "mbridge_to_vortex",
      evo2_remove_optimizer = "remove_optimizer"
    )
  )
}

bionemor_adapter_evo2_megatron_doctor_model <- function(
  compute,
  model,
  report
) {
  stopifnot(
    "model must be an Evo 2 model" = S7_inherits(model, Evo2Model),
    "compute must use the Evo 2 Megatron adapter" = identical(
      compute@recipe@adapter,
      "evo2-megatron"
    ),
    "capability report must be a list" = is.list(report)
  )
  compatible_compute <- compute
  config <- compute@config
  config$capabilities <- report
  compatible_compute@config <- config
  models <- evo2_models(compatible_compute)
  selected <- models[models$name == model@size, , drop = FALSE]
  stopifnot(
    "model must be present in the Evo 2 registry" = nrow(selected) == 1L
  )
  rows <- list(doctor_row(
    "model compatibility",
    if (isTRUE(selected$compatible[[1L]])) "pass" else "fail",
    selected$compatibility_note[[1L]]
  ))

  checkpoint <- model_checkpoint_path(model, base = compute@workspace)
  present <- is_scalar_string(checkpoint) && dir.exists(checkpoint)
  record <- evo2_model_record(model@size)
  rows[[2L]] <- doctor_row(
    "checkpoint storage",
    if (present) "pass" else "fail",
    if (present) {
      "checkpoint is already present"
    } else {
      paste(
        "checkpoint is unavailable;",
        format(round(record$download_size / 1024^3, 1), nsmall = 1),
        "GiB download required"
      )
    }
  )
  if (!present) {
    rows[[3L]] <- doctor_row(
      "model checkpoint",
      "fail",
      if (is.null(checkpoint)) {
        "an explicit MBridge checkpoint is required"
      } else {
        paste("checkpoint is not available:", checkpoint)
      }
    )
    return(do.call(rbind, rows))
  }

  root <- file.path(compute@workspace, ".bionemor", "doctor")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  output <- tempfile("checkpoint-", tmpdir = root, fileext = ".json")
  probe <- runtime_probe(
    compute,
    recipe_install_spec(compute@recipe)$helper,
    c("inspect-checkpoint", "--path", checkpoint, "--output", output)
  )
  detail <- redact_credentials(trimws(paste(probe$stdout, probe$stderr)))
  if (probe$status != 0L || !file.exists(output)) {
    rows[[3L]] <- doctor_row(
      "model checkpoint",
      "fail",
      if (nzchar(detail)) detail else "checkpoint inspection failed",
      detail
    )
    return(do.call(rbind, rows))
  }
  inspection <- read_json_file(output)
  correct <- identical(inspection$model_size, model@model_size)
  rows[[3L]] <- doctor_row(
    "model checkpoint",
    if (correct) "pass" else "fail",
    if (correct) {
      paste(inspection$model_size, inspection$kind %||% "unknown")
    } else {
      "checkpoint model size does not match the model"
    }
  )
  do.call(rbind, rows)
}

evo2_manifest_precision <- function(plan, request, checkpoint) {
  resolved <- plan$metadata$resolved_control %||% list()
  request_precision <- request$precision_request %||%
    request$precision %||%
    NULL
  request_semantic_precision <- if (is.list(request_precision)) {
    request_precision$semantic %||% NULL
  } else {
    request_precision
  }
  request_control <- request$control %||% list()
  if (!is.list(request_control)) {
    request_control <- list()
  }
  semantic <- resolved$semantic_precision %||%
    resolved$precision %||%
    request_control$precision %||%
    request_semantic_precision %||%
    NULL
  resolved_recipe <- resolved$mixed_precision_recipe %||%
    checkpoint$mixed_precision_recipe %||%
    NULL
  list(
    semantic = semantic,
    resolved_recipe = resolved_recipe
  )
}

bionemor_adapter_evo2_megatron_manifest_context <- function(
  workflow,
  job,
  request,
  plan
) {
  stopifnot(
    "workflow must use the Evo 2 Megatron adapter" = identical(
      workflow@adapter,
      "evo2-megatron"
    ),
    "manifest request and plan must be lists" = is.list(request) &&
      is.list(plan)
  )
  descriptor <- job@expected_result
  candidates <- c(
    descriptor$checkpoint %||% character(),
    descriptor$path %||% character(),
    descriptor$checkpoint_root %||% character()
  )
  candidates <- candidates[
    vapply(candidates, is_scalar_string, logical(1))
  ]
  path <- if (length(candidates)) candidates[[1L]] else NULL
  path_exists <- !is.null(path) && file.exists(path)
  if (path_exists) {
    path <- normalizePath(path, mustWork = TRUE)
  } else if (!is.null(path)) {
    path <- normalize_path(path)
  }
  manifest_path <- if (path_exists) checkpoint_manifest_path(path) else NULL
  metadata <- if (!is.null(manifest_path) && file.exists(manifest_path)) {
    read_checkpoint_manifest(path, manifest_path)
  } else {
    list()
  }
  expected <- descriptor$expected %||% list()
  variant <- metadata$variant %||%
    descriptor$variant %||%
    expected$variant %||%
    NULL
  record <- if (is_scalar_string(variant)) {
    tryCatch(evo2_model_record(variant), error = function(error) list())
  } else {
    list()
  }
  checkpoint_digest <- metadata$checkpoint_digest %||%
    if (path_exists) checkpoint_payload_digest(path) else NULL
  base_path <- metadata$base_checkpoint_path %||%
    descriptor$base_checkpoint %||%
    NULL
  base_digest <- metadata$base_checkpoint_digest %||%
    if (!is.null(base_path) && file.exists(base_path)) {
      path_digest(base_path)
    } else {
      NULL
    }
  tokenizer_revision <- metadata$tokenizer_revision %||%
    record$tokenizer_revision %||%
    NULL
  validation <- file.path(job@path, "outputs", "validation.json")
  warnings <- if (file.exists(validation)) {
    read_json_file(validation, simplify = FALSE)$warnings
  } else {
    list()
  }
  list(
    checkpoint = if (is.null(path)) {
      list()
    } else {
      list(
        path = path,
        source = metadata$source %||% expected$source %||% NULL,
        source_trust = metadata$source_trust %||%
          expected$source_trust %||%
          NULL,
        source_verified = metadata$source_verified %||%
          expected$source_verified %||%
          NULL,
        format = metadata$format %||%
          descriptor$format %||%
          expected$format %||%
          "mbridge",
        kind = metadata$kind %||% descriptor$checkpoint_kind %||% NULL,
        revision = metadata$source_revision %||%
          expected$source_revision %||%
          NULL,
        digest = if (is.null(checkpoint_digest)) {
          NULL
        } else {
          list(algorithm = "md5", value = checkpoint_digest)
        },
        base_checkpoint = list(
          path = base_path,
          source = metadata$base_checkpoint_source %||%
            descriptor$base_checkpoint_source %||%
            NULL,
          source_trust = metadata$base_checkpoint_source_trust %||%
            descriptor$base_checkpoint_source_trust %||%
            NULL,
          source_verified = metadata$base_checkpoint_source_verified %||%
            descriptor$base_checkpoint_source_verified %||%
            NULL,
          digest = base_digest
        )
      )
    },
    model = list(
      name = variant,
      model_size = metadata$model_size %||%
        descriptor$model_size %||%
        expected$model_size %||%
        NULL,
      revision = metadata$source_revision %||%
        record$source_revision %||%
        NULL
    ),
    tokenizer = list(
      identity = metadata$tokenizer %||% record$tokenizer %||% NULL,
      revision = tokenizer_revision,
      digest = if (is.null(tokenizer_revision)) {
        NULL
      } else {
        list(algorithm = "git-revision", value = tokenizer_revision)
      }
    ),
    precision = evo2_manifest_precision(
      plan,
      request,
      metadata
    ),
    warnings = warnings
  )
}

bionemor_adapter_evo2_megatron_materialize <- function(
  workflow,
  job,
  descriptor
) {
  stopifnot(
    "workflow must use the Evo 2 Megatron adapter" = identical(
      workflow@adapter,
      "evo2-megatron"
    ),
    "job result descriptor must be a list" = is.list(descriptor)
  )
  materialize <- switch(
    workflow@task,
    generate = materialize_generation_job,
    score = materialize_score_job,
    profile = materialize_profile_job,
    embed = materialize_embedding_job,
    checkpoint = materialize_checkpoint_job,
    export = materialize_checkpoint_job,
    prepare = materialize_prepare_job,
    `fine-tune` = materialize_finetune_job,
    NULL
  )
  if (is.null(materialize)) {
    bionemor_abort(
      "BN_PROTOCOL",
      paste0("Evo 2 result workflow is unsupported: ", workflow@id),
      run_path = job@path,
      request_id = job@id,
      operation = workflow@task,
      log_paths = file.path(job@path, c("stdout.log", "stderr.log"))
    )
  }
  materialize(job, descriptor)
}

bionemor_adapter_evo2_megatron_run <- function(
  workflow,
  model,
  input,
  compute,
  parameters,
  async,
  name
) {
  stopifnot(
    "workflow must use the Evo 2 Megatron adapter" = identical(
      workflow@adapter,
      "evo2-megatron"
    ),
    "model must be an Evo 2 model" = S7_inherits(model, Evo2Model),
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
  if (
    workflow@task %in% c("checkpoint", "export", "prepare") && !is.null(name)
  ) {
    stop(paste0("name is not supported for workflow ", workflow@id))
  }
  routes <- list(
    generate = list(evo2_generate, "object", "prompt", TRUE),
    score = list(evo2_score, "object", "newdata", TRUE),
    profile = list(evo2_profile, "object", "newdata", TRUE),
    embed = list(evo2_embed, "object", "newdata", TRUE),
    prepare = list(evo2_prepare, "model", "data", FALSE),
    `fine-tune` = list(evo2_finetune, "object", "data", TRUE),
    checkpoint = list(evo2_checkpoint, "model", "source", FALSE),
    export = list(evo2_export, "model", "path", FALSE)
  )
  route <- routes[[workflow@task]]
  if (is.null(route)) {
    bionemor_abort(
      "BN_WORKFLOW_UNKNOWN",
      paste0("Evo 2 workflow is unsupported: ", workflow@id),
      operation = "workflow-dispatch"
    )
  }
  arguments <- list(compute = compute, async = async)
  arguments[[route[[2L]]]] <- model
  arguments[[route[[3L]]]] <- input
  if (isTRUE(route[[4L]])) {
    arguments$name <- name
  }
  workflow_call(route[[1L]], arguments, parameters)
}

format_number <- function(x) {
  format(
    x,
    scientific = FALSE,
    trim = TRUE,
    digits = 15L,
    decimal.mark = "."
  )
}
