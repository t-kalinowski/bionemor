evo2_model_registry_records <- function() {
  path <- system.file(
    "recipes",
    "evo2-models.json",
    package = "bionemor",
    mustWork = TRUE
  )
  registry <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (!identical(registry$schema_version, 1L)) {
    stop("unsupported Evo 2 model registry schema")
  }
  if (!is.list(registry$models) || length(registry$models) <= 0L) {
    stop("Evo 2 model registry is empty")
  }
  registry$models
}

evo2_model_registry <- function() {
  records <- evo2_model_registry_records()
  data.frame(
    name = pluck_chr(records, "name"),
    model_size = pluck_chr(records, "model_size"),
    parameters = pluck_dbl(records, "parameters"),
    context_length = pluck_int(records, "context_length"),
    source = pluck_chr(records, "source"),
    source_revision = pluck_chr(records, "source_revision"),
    source_format = pluck_chr(records, "source_format"),
    tokenizer = pluck_chr(records, "tokenizer"),
    tokenizer_revision = pluck_chr(records, "tokenizer_revision"),
    mixed_precision_recipe = pluck_chr(records, "mixed_precision_recipe"),
    precision_policy = pluck_chr(records, "precision_policy"),
    training_precision_policy = pluck_chr(
      records,
      "training_precision_policy"
    ),
    download_size = pluck_dbl(records, "download_size"),
    aliases = I(lapply(records, `[[`, "aliases")),
    stringsAsFactors = FALSE
  )
}

evo2_model_record <- function(size) {
  if (!is_scalar_string(size)) {
    stop("size must be one non-empty string")
  }
  size <- tolower(size)
  records <- evo2_model_registry_records()
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
        paste0("'", choices, "'", collapse = ", ")
      ),
      model = size
    )
  }
  records[[which(matches)]]
}

evo2_gpu_advertisement <- function(compute) {
  report <- compute@config$capabilities
  if (!is.list(report) || !is.list(report$runtime)) {
    return(list(count = NA_integer_, capability_majors = integer()))
  }

  runtime <- report$runtime
  gpus <- runtime$gpus
  capability_majors <- if (
    is.data.frame(gpus) &&
      "compute_capability_major" %in% names(gpus)
  ) {
    values <- gpus$compute_capability_major
    vapply(
      values,
      function(value) {
        if (is_scalar_integerish(value, min = 0)) {
          as.integer(value)
        } else {
          NA_integer_
        }
      },
      integer(1)
    )
  } else if (is.list(gpus)) {
    vapply(
      gpus,
      function(gpu) {
        value <- gpu$compute_capability_major
        if (is_scalar_integerish(value, min = 0)) {
          as.integer(value)
        } else {
          NA_integer_
        }
      },
      integer(1)
    )
  } else {
    integer()
  }
  count <- runtime$gpu_count
  count <- if (is_scalar_integerish(count, min = 0)) {
    as.integer(count)
  } else {
    NA_integer_
  }
  list(count = count, capability_majors = capability_majors)
}

evo2_model_compatibility <- function(registry, compute) {
  policies <- c("bf16-or-fp8", "vortex-fp8-on-hopper", "unverified")
  if (!all(registry$precision_policy %in% policies)) {
    stop("model registry contains an unsupported precision policy")
  }

  advertisement <- evo2_gpu_advertisement(compute)
  requested <- compute@gpus
  enough_gpus <- !is.na(advertisement$count) &&
    advertisement$count >= requested
  enough_capabilities <-
    length(advertisement$capability_majors) >= requested &&
    !anyNA(advertisement$capability_majors[seq_len(requested)])
  selected_majors <- if (enough_capabilities) {
    advertisement$capability_majors[seq_len(requested)]
  } else {
    integer()
  }

  compatible <- logical(nrow(registry))
  notes <- character(nrow(registry))
  for (index in seq_len(nrow(registry))) {
    policy <- registry$precision_policy[[index]]
    if (identical(policy, "unverified")) {
      notes[[index]] <- paste(
        "the original Arc 40B-base checkpoint is not verified",
        "on any hardware and precision combination"
      )
    } else if (is.na(advertisement$count)) {
      notes[[index]] <-
        "runtime GPU count and compute capability are not advertised"
    } else if (!enough_gpus) {
      notes[[index]] <- sprintf(
        "compute requests %d GPUs but the runtime advertises %d GPU(s)",
        requested,
        advertisement$count
      )
    } else if (!enough_capabilities) {
      notes[[index]] <-
        "runtime does not advertise compute capability for all requested GPUs"
    } else if (identical(policy, "bf16-or-fp8")) {
      compatible[[index]] <- all(selected_majors >= 8L)
      notes[[index]] <- if (compatible[[index]]) {
        "advertised GPUs support the validated BF16 or FP8 policy"
      } else {
        "requires GPUs with compute capability 8.0 or newer"
      }
    } else {
      compatible[[index]] <- all(selected_majors == 9L)
      notes[[index]] <- if (compatible[[index]]) {
        "advertised Hopper GPUs support the validated Vortex-style FP8 policy"
      } else {
        paste(
          "requires Hopper GPUs with compute capability 9.x",
          "and the Vortex-style FP8 policy"
        )
      }
    }
  }
  list(compatible = compatible, note = notes)
}

evo2_execution_model_record <- function(object, operation) {
  if (!S7_inherits(object, Evo2Model)) {
    stop("object must be an Evo 2 model")
  }
  if (!is_scalar_string(operation)) {
    stop("operation must be one non-empty string")
  }
  record <- evo2_model_record(object@size)
  if (identical(record$precision_policy, "unverified")) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      paste0(
        operation,
        " is unavailable for model '",
        record$name,
        "': no verified precision and hardware combination is registered"
      ),
      operation = operation,
      model = record$name
    )
  }
  record
}

evo2_has_gpu_capabilities <- function(compute) {
  report <- compute@config$capabilities
  runtime <- if (is.list(report)) report$runtime else NULL
  is.list(runtime) &&
    !is.null(runtime$gpu_count) &&
    !is.null(runtime$gpus)
}

evo2_compute_with_gpu_capabilities <- function(compute) {
  if (!S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be a BioNeMo compute descriptor")
  }
  if (evo2_has_gpu_capabilities(compute)) {
    return(compute)
  }
  config <- compute@config
  config$capabilities <- runtime_capabilities(compute, refresh = TRUE)
  compute@config <- config
  compute
}

evo2_model_preflight <- function(object, compute, operation, record = NULL) {
  if (!S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be a BioNeMo compute descriptor")
  }
  record <- record %||% evo2_execution_model_record(object, operation)
  compute <- evo2_compute_with_gpu_capabilities(compute)
  registry <- evo2_model_registry()
  index <- match(record$name, registry$name)
  if (length(index) != 1L || is.na(index)) {
    stop("model is missing from the compatibility registry")
  }
  compatibility <- evo2_model_compatibility(registry, compute)
  if (!isTRUE(compatibility$compatible[[index]])) {
    advertisement <- evo2_gpu_advertisement(compute)
    code <- if (identical(advertisement$count, 0L)) {
      "BN_NO_GPU"
    } else {
      "BN_GPU_INCOMPATIBLE"
    }
    bionemor_abort(
      code,
      paste0(
        operation,
        " is unavailable for model '",
        record$name,
        "': ",
        compatibility$note[[index]]
      ),
      operation = operation,
      model = record$name,
      recipe_revision = compute@recipe@revision,
      hint = compatibility$note[[index]]
    )
  }
  list(record = record, compute = compute)
}

evo2_verified_hopper <- function(compute) {
  advertisement <- evo2_gpu_advertisement(compute)
  requested <- compute@gpus
  !is.na(advertisement$count) &&
    advertisement$count >= requested &&
    length(advertisement$capability_majors) >= requested &&
    !anyNA(advertisement$capability_majors[seq_len(requested)]) &&
    all(advertisement$capability_majors[seq_len(requested)] == 9L)
}

checkpoint_nested_value <- function(value, keys) {
  if (!is.list(value)) {
    return(NULL)
  }
  value_names <- names(value)
  if (!is.null(value_names)) {
    normalized <- tolower(gsub("-", "_", value_names, fixed = TRUE))
    for (index in which(normalized %in% keys)) {
      if (!is.null(value[[index]])) {
        return(value[[index]])
      }
    }
  }
  for (item in value) {
    found <- checkpoint_nested_value(item, keys)
    if (!is.null(found)) {
      return(found)
    }
  }
  NULL
}

checkpoint_model_size <- function(config) {
  explicit <- checkpoint_nested_value(
    config,
    c("model_size", "model_name")
  )
  if (is_scalar_string(explicit)) {
    return(explicit)
  }

  model <- config[["model"]] %||% config
  provider <- checkpoint_nested_value(model, c("_target_", "target"))
  if (is_scalar_string(provider)) {
    provider_name <- sub("^.*\\.", "", provider)
    providers <- c(
      Hyena1bModelProvider = "evo2_1b_base",
      Hyena7bModelProvider = "evo2_7b_base",
      Hyena7bARCLongContextModelProvider = "evo2_7b",
      Hyena20bARCModelProvider = "evo2_20b",
      Hyena40bModelProvider = "evo2_40b_base",
      Hyena40bARCLongContextModelProvider = "evo2_40b"
    )
    if (provider_name %in% names(providers)) {
      return(unname(providers[[provider_name]]))
    }
  }

  layers <- checkpoint_nested_value(model, "num_layers")
  hidden <- checkpoint_nested_value(model, "hidden_size")
  sequence <- checkpoint_nested_value(
    model,
    c("seq_length", "sequence_length")
  )
  if (
    !is_scalar_integerish(layers, min = 1) ||
      !is_scalar_integerish(hidden, min = 1)
  ) {
    return(NULL)
  }
  dimensions <- paste(as.integer(layers), as.integer(hidden), sep = ":")
  long_context <- is_scalar_integerish(sequence, min = 1) && sequence > 8192
  switch(
    dimensions,
    `25:1920` = "evo2_1b_base",
    `32:4096` = if (long_context) "evo2_7b" else "evo2_7b_base",
    `24:8192` = "evo2_20b",
    `50:8192` = if (long_context) "evo2_40b" else "evo2_40b_base",
    NULL
  )
}

checkpoint_config_inspection <- function(config, resolved_path) {
  model <- config[["model"]] %||% config
  provider <- checkpoint_nested_value(model, c("_target_", "target"))
  explicit_kind <- checkpoint_nested_value(
    config,
    c("kind", "checkpoint_kind")
  )
  peft <- config$peft
  kind <- if (
    !is.null(peft) &&
      !identical(peft, FALSE) &&
      !(is.list(peft) && length(peft) == 0L)
  ) {
    "lora"
  } else {
    "dense"
  }
  if (
    is_scalar_string(explicit_kind) &&
      explicit_kind %in% c("dense", "lora", "training", "weights_only")
  ) {
    kind <- explicit_kind
  }

  tokenizer_directory <- file.path(resolved_path, "tokenizer")
  tokenizer_config <- config$tokenizer
  tokenizer <- if (dir.exists(tokenizer_directory)) {
    normalizePath(tokenizer_directory, mustWork = TRUE)
  } else if (is_scalar_string(tokenizer_config)) {
    tokenizer_config
  } else {
    checkpoint_nested_value(
      tokenizer_config %||% config,
      c(
        "hf_tokenizer_model_path",
        "hf_tokenizer_model_or_path",
        "tokenizer_path",
        "tokenizer_model"
      )
    )
  }

  precision <- checkpoint_nested_value(
    config,
    c("mixed_precision_recipe", "precision_config", "mixed_precision")
  )
  if (is.list(precision)) {
    precision <- precision$`_target_` %||% precision$name
  }
  if (!is_scalar_string(precision)) {
    precision <- NULL
  }

  list(
    model_provider = if (is_scalar_string(provider)) provider else NULL,
    model_size = checkpoint_model_size(config),
    kind = kind,
    vortex_style_fp8 = identical(
      checkpoint_nested_value(model, "vortex_style_fp8"),
      TRUE
    ),
    tokenizer = if (is_scalar_string(tokenizer)) tokenizer else NULL,
    mixed_precision_recipe = precision,
    base_checkpoint = checkpoint_nested_value(
      config,
      c("pretrained_checkpoint", "base_checkpoint", "base_checkpoint_path")
    ),
    base_checkpoint_source = checkpoint_nested_value(
      config,
      "base_checkpoint_source"
    ),
    source_revision = checkpoint_nested_value(config, "source_revision")
  )
}

inspect_model_checkpoint <- function(path, model_size) {
  if (!is_scalar_string(path) || !dir.exists(path)) {
    stop("checkpoint must be an existing MBridge directory")
  }
  path <- normalize_path(path)
  run_config <- file.path(path, "run_config.yaml")
  resolved_path <- path

  if (!file.exists(run_config)) {
    latest <- file.path(path, "latest_checkpointed_iteration.txt")
    if (file.exists(latest)) {
      iteration <- readLines(latest, warn = FALSE)
      if (length(iteration) != 1L) {
        stop("latest_checkpointed_iteration.txt must contain one iteration")
      }
      if (!grepl("^[0-9]+$", iteration)) {
        stop("latest checkpoint iteration must be a non-negative integer")
      }
      iteration <- sub("^0+", "", iteration)
      if (!nzchar(iteration)) {
        iteration <- "0"
      }
      resolved_path <- file.path(
        path,
        paste0(
          "iter_",
          strrep("0", max(0L, 7L - nchar(iteration))),
          iteration
        )
      )
      if (!dir.exists(resolved_path)) {
        stop("latest checkpoint iteration does not exist")
      }
      if (!file.exists(file.path(resolved_path, "run_config.yaml"))) {
        stop("latest checkpoint iteration must contain run_config.yaml")
      }
    } else {
      candidates <- list.dirs(path, recursive = FALSE, full.names = TRUE)
      candidates <- candidates[
        grepl("^iter_[0-9]+$", basename(candidates)) &
          file.exists(file.path(candidates, "run_config.yaml"))
      ]
      if (length(candidates) == 0L) {
        stop(
          "checkpoint has no direct run_config.yaml or valid iter_* checkpoint"
        )
      }
      iterations <- sub("^iter_", "", basename(candidates))
      iterations <- sub("^0+", "", iterations)
      iterations[!nzchar(iterations)] <- "0"
      if (anyDuplicated(iterations)) {
        stop("checkpoint contains duplicate numeric iteration directories")
      }
      order <- order(nchar(iterations), iterations, basename(candidates))
      resolved_path <- candidates[[order[[length(order)]]]]
    }
    run_config <- file.path(resolved_path, "run_config.yaml")
  }

  resolved_path <- normalizePath(resolved_path, mustWork = TRUE)
  run_config <- normalizePath(run_config, mustWork = TRUE)
  assert_mbridge_dcp_weights(resolved_path)
  config <- yaml12::read_yaml(run_config, simplify = FALSE)
  if (!is.list(config) || is.null(names(config))) {
    stop("checkpoint run_config.yaml must contain a mapping")
  }
  details <- checkpoint_config_inspection(config, resolved_path)
  if (!is_scalar_string(details$model_size)) {
    stop("checkpoint run_config.yaml does not identify a supported model size")
  }
  if (!identical(details$model_size, model_size)) {
    stop("checkpoint model size does not match the requested model")
  }
  c(
    list(
      path = path,
      resolved_path = resolved_path,
      run_config = run_config,
      config = config
    ),
    details
  )
}

register_model_checkpoint <- function(inspection, record) {
  path <- inspection$path
  manifest_path <- checkpoint_manifest_path(path, "mbridge")
  if (file.exists(manifest_path)) {
    manifest <- read_checkpoint_manifest(path, manifest_path)
    if (!identical(manifest$model_size, record$model_size)) {
      stop("checkpoint manifest model size does not match the requested model")
    }
    return(checkpoint_from_manifest(path, manifest, manifest_path))
  }

  recipe <- evo2_recipe()
  kind <- inspection$kind
  if (!kind %in% c("dense", "lora", "training", "weights_only")) {
    stop("checkpoint kind must be dense, LoRA, training, or weights-only")
  }
  source_revision <- inspection$source_revision %||% path_digest(path)
  base_checkpoint <- inspection$base_checkpoint
  if (identical(kind, "lora")) {
    if (!is_scalar_string(base_checkpoint)) {
      stop("LoRA checkpoint run_config.yaml must identify its base checkpoint")
    }
  }
  base_checkpoint_digest <- NULL
  if (is_scalar_string(base_checkpoint) && file.exists(base_checkpoint)) {
    base_checkpoint <- normalizePath(base_checkpoint, mustWork = TRUE)
    base_checkpoint_digest <- path_digest(base_checkpoint)
  }
  checkpoint_inspection <- list(
    path = path,
    resolved_path = inspection$resolved_path,
    run_config = inspection$run_config,
    model_provider = inspection$model_provider,
    model_size = record$model_size,
    kind = kind,
    vortex_style_fp8 = inspection$vortex_style_fp8,
    tokenizer = inspection$tokenizer %||% record$tokenizer,
    mixed_precision_recipe = inspection$mixed_precision_recipe,
    base_checkpoint = base_checkpoint
  )
  manifest <- list(
    schema_version = 1L,
    family = "evo2",
    variant = record$name,
    model_size = record$model_size,
    format = "mbridge",
    kind = kind,
    source = path,
    source_format = "mbridge",
    source_revision = source_revision,
    recipe_revision = recipe@revision,
    source_trust = "not-required",
    source_verified = FALSE,
    tokenizer = checkpoint_inspection$tokenizer,
    mixed_precision_recipe = inspection$mixed_precision_recipe %||%
      record$mixed_precision_recipe,
    base_checkpoint_path = base_checkpoint,
    base_checkpoint_digest = base_checkpoint_digest,
    base_checkpoint_source = inspection$base_checkpoint_source %||% NULL,
    inspection = checkpoint_inspection,
    provenance = list(
      registered_at = base::format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )
  atomic_write_json(manifest, manifest_path)
  atomic_write_json(
    list(schema_version = 1L, manifest = basename(manifest_path)),
    checkpoint_completion_path(path, "mbridge")
  )
  checkpoint_from_manifest(path, manifest, manifest_path)
}

#' Describe an Evo 2 model
#'
#' `evo2()` creates a compute-independent model descriptor. It does not
#' download or load weights, and operations on the returned model require an
#' explicit compute descriptor. Use [evo2_model()] for the direct, synchronous
#' path to a model backed by the recommended checkpoint and bound to compute.
#'
#' @param size Canonical Evo 2 model name or a known upstream alias.
#' @param checkpoint `NULL`, an existing MBridge checkpoint path, or a
#'   checkpoint returned by [evo2_checkpoint()].
#' @param revision Exact source checkpoint revision. `"recommended"` uses the
#'   package model registry.
#' @param config Named model-level settings.
#'
#' @return An S7 `Evo2Model`.
#' @export
evo2 <- function(
  size = "7b",
  checkpoint = NULL,
  revision = "recommended",
  config = list()
) {
  record <- evo2_model_record(size)
  allowed_config <- c(
    "tokenizer",
    "tokenizer_revision",
    "mixed_precision_recipe"
  )
  stopifnot(
    "checkpoint must be NULL, one path, or a BioNeMo checkpoint" = is.null(
      checkpoint
    ) ||
      is_scalar_string(checkpoint) ||
      S7_inherits(checkpoint, BioNeMoCheckpoint),
    "revision must be 'recommended' or a full commit SHA" = identical(
      revision,
      "recommended"
    ) ||
      is_scalar_string(revision) &&
        grepl("^[0-9a-fA-F]{40}$", revision),
    "config must be a named list" = is.list(config) &&
      (length(config) == 0L ||
        !is.null(names(config)) &&
          all(nzchar(names(config))) &&
          !anyDuplicated(names(config))),
    "config contains an unsupported model setting" = all(
      names(config) %in% allowed_config
    )
  )

  if (is_scalar_string(checkpoint)) {
    inspection <- inspect_model_checkpoint(checkpoint, record$model_size)
    checkpoint <- register_model_checkpoint(inspection, record)
  }
  if (S7_inherits(checkpoint, BioNeMoCheckpoint)) {
    checkpoint_record <- evo2_model_record(checkpoint@variant)
    if (!identical(checkpoint@family, "evo2")) {
      stop("checkpoint family must be 'evo2'")
    }
    if (!identical(checkpoint@format, "mbridge")) {
      stop("checkpoint must be in MBridge format")
    }
    if (!identical(checkpoint_record$name, record$name)) {
      stop("checkpoint model size does not match the requested model")
    }
  }

  revision <- if (identical(revision, "recommended")) {
    record$source_revision
  } else {
    tolower(revision)
  }

  Evo2Model(
    family = "evo2",
    checkpoint = checkpoint,
    compute = NULL,
    task = "causal_lm",
    config = config,
    provenance = list(),
    size = record$name,
    model_size = record$model_size,
    context_length = as.integer(record$context_length),
    revision = revision
  )
}

resolve_model_compute <- function(model, compute = NULL) {
  stopifnot("model must be an Evo 2 model" = S7_inherits(model, Evo2Model))
  if (is.null(compute)) {
    compute <- model@compute
    if (!S7_inherits(compute, BioNeMoCompute)) {
      stop(
        "compute is required for an unbound model; use evo2_model() or supply compute"
      )
    }
  }
  if (!S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be a BioNeMo compute specification")
  }
  compute
}

#' List the package's pinned Evo 2 model registry
#'
#' `evo2_models()` reports available model sizes and compatibility metadata; it
#' does not construct or prepare a model. Use [evo2()] for an offline
#' descriptor or [evo2_model()] for a ready model.
#'
#' @param compute Optional BioNeMo compute descriptor with an advertised GPU
#'   count and per-GPU compute capability in its cached capability report.
#' @param compatible If `TRUE`, return only models compatible with `compute`.
#'
#' @return A data frame with one row per canonical model.
#' @export
evo2_models <- function(compute = NULL, compatible = FALSE) {
  if (!is.null(compute) && !S7_inherits(compute, BioNeMoCompute)) {
    stop("compute must be NULL or a BioNeMo compute descriptor")
  }
  if (!is_scalar_logical(compatible)) {
    stop("compatible must be TRUE or FALSE")
  }
  if (compatible && is.null(compute)) {
    stop("compute is required when compatible is TRUE")
  }
  registry <- evo2_model_registry()
  registry$prepared <- FALSE
  compatibility <- if (is.null(compute)) {
    list(
      compatible = rep(NA, nrow(registry)),
      note = rep("compute not supplied", nrow(registry))
    )
  } else {
    evo2_model_compatibility(registry, compute)
  }
  registry$compatible <- compatibility$compatible
  registry$compatibility_note <- compatibility$note
  registry <- registry[c(
    "name",
    "model_size",
    "parameters",
    "context_length",
    "source",
    "source_revision",
    "source_format",
    "precision_policy",
    "training_precision_policy",
    "download_size",
    "prepared",
    "compatible",
    "compatibility_note"
  )]
  if (compatible) {
    registry <- registry[registry$compatible, , drop = FALSE]
  }
  rownames(registry) <- NULL
  registry
}

method(print, BioNeMoRecipe) <- function(x, ...) {
  cat("<BioNeMo recipe>\n", sep = "")
  cat("Recipe:     Evo 2 ", x@recipe_version, "\n", sep = "")
  cat("Revision:   ", substr(x@revision, 1L, 8L), "\n", sep = "")
  cat("Repository: ", x@repository, "\n", sep = "")
  cat("Verified:   ", if (x@verified) "yes" else "no", "\n", sep = "")
  invisible(x)
}

method(print, BioNeMoModel) <- function(x, ...) {
  if (S7_inherits(x, Evo2Model)) {
    checkpoint <- model_checkpoint_path(x)
    recipe <- evo2_recipe()
    cat("<Evo 2 model>\n", sep = "")
    cat("Size:       ", toupper(x@size), "\n", sep = "")
    cat(
      "Context:    ",
      format(x@context_length, big.mark = ",", scientific = FALSE),
      " nt\n",
      sep = ""
    )
    cat(
      "Checkpoint: ",
      if (is.null(checkpoint)) {
        "not attached"
      } else {
        paste("MBridge at", checkpoint)
      },
      "\n",
      sep = ""
    )
    cat(
      "Recipe:     BioNeMo Evo 2 ",
      recipe@recipe_version,
      " @ ",
      substr(recipe@revision, 1L, 8L),
      "\n",
      sep = ""
    )
    cat(
      "Ready:      ",
      if (is.null(checkpoint)) "no" else "yes",
      "\n",
      sep = ""
    )
    if (S7_inherits(x@compute, BioNeMoCompute)) {
      cat(
        "Compute:    ",
        x@compute@backend,
        "/",
        x@compute@engine,
        "\n",
        sep = ""
      )
    }
    return(invisible(x))
  }
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

method(print, BioNeMoPrediction) <- function(x, ...) {
  cat("<BioNeMo prediction>\n", sep = "")
  cat("Type: ", x@type, "\n", sep = "")
  rows <- if (is.null(dim(x@data))) length(x@data) else NROW(x@data)
  cat("Rows: ", rows, "\n", sep = "")
  invisible(x)
}

method(as.data.frame, BioNeMoPrediction) <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  if (!is.data.frame(x@data) && !is.matrix(x@data)) {
    stop("this prediction is not tabular")
  }
  base::as.data.frame(
    x@data,
    row.names = row.names,
    optional = optional,
    ...
  )
}
