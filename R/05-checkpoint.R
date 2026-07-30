checkpoint_manifest_file <- "bionemor-checkpoint.json"
checkpoint_completion_file <- ".bionemor-complete"

checkpoint_manifest_path <- function(path, format = NULL) {
  file_checkpoint <- identical(format, "vortex") ||
    (file.exists(path) && !dir.exists(path))
  if (file_checkpoint) {
    paste0(path, ".", checkpoint_manifest_file)
  } else {
    file.path(path, checkpoint_manifest_file)
  }
}

checkpoint_completion_path <- function(path, format = NULL) {
  file_checkpoint <- identical(format, "vortex") ||
    (file.exists(path) && !dir.exists(path))
  if (file_checkpoint) {
    paste0(path, checkpoint_completion_file)
  } else {
    file.path(path, checkpoint_completion_file)
  }
}

read_checkpoint_manifest <- function(path, manifest_path = NULL) {
  manifest_path <- manifest_path %||% checkpoint_manifest_path(path)
  if (!is_scalar_string(manifest_path) || !file.exists(manifest_path)) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      "checkpoint manifest does not exist",
      operation = "checkpoint",
      checkpoint = path
    )
  }
  read_json_file(manifest_path)
}

checkpoint_is_complete <- function(path, format = NULL) {
  manifest_path <- checkpoint_manifest_path(path, format)
  marker_path <- checkpoint_completion_path(path, format)
  if (
    !file.exists(path) ||
      !file.exists(manifest_path) ||
      !file.exists(marker_path)
  ) {
    return(FALSE)
  }
  manifest <- tryCatch(
    read_checkpoint_manifest(path, manifest_path),
    error = function(error) NULL
  )
  is.list(manifest) &&
    identical(manifest$schema_version, 1L) &&
    identical(manifest$format, format %||% manifest$format) &&
    is_scalar_string(manifest$source) &&
    is_scalar_string(manifest$source_revision) &&
    is_scalar_string(manifest$recipe_revision) &&
    is.list(manifest$inspection)
}

checkpoint_from_manifest <- function(
  path,
  manifest,
  manifest_path = checkpoint_manifest_path(path, manifest$format)
) {
  BioNeMoCheckpoint(
    path = normalizePath(path, mustWork = TRUE),
    format = manifest$format,
    kind = manifest$kind,
    family = manifest$family,
    variant = manifest$variant,
    source = manifest$source,
    source_format = manifest$source_format,
    source_revision = manifest$source_revision,
    recipe_revision = manifest$recipe_revision,
    base_checkpoint = manifest$base_checkpoint_path %||% NULL,
    manifest = normalizePath(manifest_path, mustWork = TRUE),
    provenance = manifest$provenance %||% list()
  )
}

assert_manifest_matches <- function(manifest, expected) {
  fields <- c(
    "family",
    "variant",
    "model_size",
    "format",
    "source",
    "source_format",
    "source_revision",
    "recipe_revision",
    "source_trust",
    "source_verified"
  )
  checkpoint_identity_fields <- c(
    "tokenizer_identity",
    "tokenizer_revision",
    "mixed_precision_recipe"
  )
  fields <- c(
    fields,
    checkpoint_identity_fields[checkpoint_identity_fields %in% names(expected)]
  )
  for (field in fields) {
    if (!identical(manifest[[field]], expected[[field]])) {
      bionemor_abort(
        "BN_CHECKPOINT_SOURCE",
        paste0(
          "checkpoint manifest ",
          field,
          " does not match the requested ",
          field
        ),
        operation = "checkpoint",
        checkpoint = manifest$path %||% NULL,
        recipe_revision = expected$recipe_revision
      )
    }
  }
  invisible(manifest)
}

evo2_checkpoint_model_record <- function(model) {
  stopifnot(
    "model must be an Evo 2 model specification" = S7_inherits(model, Evo2Model)
  )
  evo2_model_record(model@size)
}

evo2_checkpoint_model_size <- function(model, record) {
  value <- tryCatch(model@model_size, error = function(error) NULL)
  value %||% record$model_size
}

evo2_checkpoint_context_length <- function(model, record) {
  value <- tryCatch(model@context_length, error = function(error) NULL)
  as.integer(value %||% record$context_length)
}

normalize_checkpoint_uri <- function(source, model = NULL) {
  stopifnot("source must be one non-empty string" = is_scalar_string(source))
  if (startsWith(source, "hf://") || startsWith(source, "ngc://")) {
    return(source)
  }
  if (grepl("^[[:alpha:]][[:alnum:]+.-]*://", source)) {
    bionemor_abort(
      "BN_CHECKPOINT_SOURCE",
      "source must be 'recommended', a BioNeMo checkpoint, an hf:// or ngc:// URI, or an existing local path",
      operation = "checkpoint",
      model = model,
      checkpoint = source
    )
  }
  source
}

checkpoint_source_info <- function(
  model,
  source,
  format,
  revision,
  compute,
  trust
) {
  record <- evo2_checkpoint_model_record(model)
  source_checkpoint <- S7_inherits(source, BioNeMoCheckpoint)

  if (source_checkpoint) {
    if (source@family != "evo2" || source@variant != model@size) {
      bionemor_abort(
        "BN_CHECKPOINT_SOURCE",
        if (source@family != "evo2") {
          "checkpoint source family must be 'evo2'"
        } else {
          "checkpoint source size does not match model size"
        },
        operation = "checkpoint",
        model = model@size,
        checkpoint = source@path
      )
    }
    resolved_source <- source@path
    detected_format <- source@format
    source_revision <- source@source_revision
  } else if (identical(source, "recommended")) {
    resolved_source <- record$source
    detected_format <- record$source_format
    source_revision <- if (!identical(revision, "recommended")) {
      revision
    } else {
      model@revision %||% record$source_revision
    }
  } else {
    source <- normalize_checkpoint_uri(source, model@size)
    remote <- startsWith(source, "hf://") || startsWith(source, "ngc://")
    resolved_source <- if (remote) {
      source
    } else {
      normalized <- normalize_path(source, base = compute@workspace)
      if (!file.exists(normalized)) {
        bionemor_abort(
          "BN_CHECKPOINT_SOURCE",
          "local checkpoint source does not exist",
          operation = "checkpoint",
          model = model@size,
          checkpoint = normalized
        )
      }
      normalizePath(normalized, mustWork = TRUE)
    }
    detected_format <- if (!identical(format, "auto")) {
      format
    } else if (startsWith(source, "hf://")) {
      "savanna"
    } else if (startsWith(source, "ngc://")) {
      "nemo2"
    } else if (
      !dir.exists(resolved_source) && grepl("[.]pt$", resolved_source)
    ) {
      "savanna"
    } else {
      bionemor_abort(
        "BN_CHECKPOINT_FORMAT",
        "format must be supplied for a local checkpoint without a bionemor manifest",
        operation = "checkpoint",
        model = model@size,
        checkpoint = resolved_source
      )
    }
    source_revision <- if (!identical(revision, "recommended")) {
      revision
    } else if (
      identical(resolved_source, record$source) &&
        identical(detected_format, record$source_format)
    ) {
      record$source_revision
    } else if (remote) {
      bionemor_abort(
        "BN_CHECKPOINT_SOURCE",
        "revision must be an exact revision for a custom remote checkpoint source",
        operation = "checkpoint",
        model = model@size,
        checkpoint = resolved_source
      )
    } else {
      path_digest(resolved_source)
    }
  }

  if (!identical(format, "auto") && !identical(format, detected_format)) {
    bionemor_abort(
      "BN_CHECKPOINT_FORMAT",
      "format does not match the checkpoint source format",
      operation = "checkpoint",
      model = model@size,
      checkpoint = resolved_source
    )
  }
  if (identical(detected_format, "vortex")) {
    bionemor_abort(
      "BN_CHECKPOINT_FORMAT",
      "Vortex checkpoints cannot be converted to MBridge",
      operation = "checkpoint",
      model = model@size,
      checkpoint = resolved_source
    )
  }
  if (!detected_format %in% c("savanna", "nemo2", "mbridge")) {
    bionemor_abort(
      "BN_CHECKPOINT_FORMAT",
      "checkpoint source format must be Savanna, NeMo2, or MBridge",
      operation = "checkpoint",
      model = model@size,
      checkpoint = resolved_source
    )
  }
  if (!is_scalar_string(source_revision)) {
    bionemor_abort(
      "BN_CHECKPOINT_SOURCE",
      "source revision must be one non-empty string",
      operation = "checkpoint",
      model = model@size,
      checkpoint = resolved_source
    )
  }
  if (
    startsWith(resolved_source, "hf://") &&
      !grepl("^[0-9a-fA-F]{40}$", source_revision)
  ) {
    bionemor_abort(
      "BN_CHECKPOINT_SOURCE",
      "Hugging Face revision must be a full commit SHA",
      operation = "checkpoint",
      model = model@size,
      checkpoint = resolved_source
    )
  }
  trusted_registry_source <-
    identical(resolved_source, record$source) &&
    identical(detected_format, record$source_format) &&
    identical(tolower(source_revision), tolower(record$source_revision))
  explicit_trust_required <-
    detected_format %in% c("savanna", "nemo2") && !trusted_registry_source
  if (
    explicit_trust_required &&
      !trust
  ) {
    bionemor_abort(
      "BN_CHECKPOINT_SOURCE",
      "trust = TRUE is required for an unknown local or remote pickle-based checkpoint",
      operation = "checkpoint",
      model = model@size,
      checkpoint = resolved_source,
      recipe_revision = compute@recipe@revision,
      hint = "Set trust = TRUE only after verifying the checkpoint source."
    )
  }

  list(
    source = resolved_source,
    format = detected_format,
    revision = source_revision,
    source_trust = if (trusted_registry_source) {
      "registry"
    } else if (explicit_trust_required) {
      "explicit"
    } else {
      "not-required"
    },
    source_verified = trusted_registry_source,
    record = record
  )
}

evo2_checkpoint_tokenizer <- function(tokenizer, record, compute) {
  stopifnot(
    "tokenizer must be 'recommended' or one non-empty path" = is_scalar_string(
      tokenizer
    )
  )
  if (identical(tokenizer, "recommended")) {
    if (compute@engine == "container") {
      return(file.path("/workspace/bionemo", record$tokenizer))
    }
    report <- compute@config$capabilities
    if (!is.list(report) || is.null(report$tokenizers)) {
      report <- runtime_capabilities(compute, refresh = TRUE)
    }
    key <- basename(record$tokenizer)
    tokenizers <- report$tokenizers
    value <- if (
      (is.list(tokenizers) || is.character(tokenizers)) &&
        key %in% names(tokenizers)
    ) {
      tokenizers[[key]]
    } else {
      NULL
    }
    if (
      !is_scalar_string(value) ||
        !startsWith(value, "/") ||
        !dir.exists(value)
    ) {
      bionemor_abort(
        "BN_TOKENIZER_MISMATCH",
        "external runtime did not report the recommended tokenizer path",
        operation = "checkpoint",
        model = record$name,
        checkpoint = record$source,
        recipe_revision = compute@recipe@revision
      )
    }
    return(normalizePath(value, mustWork = TRUE))
  }
  value <- normalize_path(tokenizer, base = compute@workspace)
  if (!dir.exists(value)) {
    bionemor_abort(
      "BN_TOKENIZER_MISMATCH",
      "explicit tokenizer path does not exist",
      operation = "checkpoint",
      model = record$name,
      checkpoint = record$source
    )
  }
  if (
    !(compute@backend == "local" && compute@engine == "external") &&
      !path_is_within(value, compute@workspace)
  ) {
    bionemor_abort(
      "BN_TOKENIZER_MISMATCH",
      "container or Slurm tokenizer path must be inside the compute workspace",
      operation = "checkpoint",
      model = record$name,
      checkpoint = record$source
    )
  }
  normalizePath(value, mustWork = TRUE)
}

evo2_checkpoint_tokenizer_provenance <- function(
  model,
  tokenizer,
  tokenizer_path,
  record
) {
  configured_revision <- model@config$tokenizer_revision %||% NULL
  revision <- if (!is.null(configured_revision)) {
    configured_revision
  } else if (identical(tokenizer, "recommended")) {
    record$tokenizer_revision
  } else {
    path_digest(tokenizer_path)
  }
  stopifnot(
    "tokenizer revision must be one non-empty string" = is_scalar_string(
      revision
    )
  )
  list(
    identity = if (identical(tokenizer, "recommended")) {
      record$tokenizer
    } else {
      tokenizer_path
    },
    revision = revision
  )
}

assert_mbridge_dcp_weights <- function(path) {
  if (!is_scalar_string(path) || !dir.exists(path)) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      "MBridge checkpoint path must be an existing directory",
      operation = "checkpoint",
      checkpoint = path
    )
  }
  if (!file.exists(file.path(path, ".metadata"))) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      "MBridge checkpoint is missing distributed checkpoint metadata",
      operation = "checkpoint",
      checkpoint = path
    )
  }
  shards <- list.files(
    path,
    pattern = "[.]distcp$",
    full.names = TRUE,
    recursive = FALSE
  )
  shards <- shards[
    file.exists(shards) &
      !dir.exists(shards) &
      unname(file.info(shards)$size) > 0
  ]
  if (length(shards) == 0L) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      "MBridge checkpoint has no distributed checkpoint weight shard",
      operation = "checkpoint",
      checkpoint = path
    )
  }
  invisible(shards)
}

checkpoint_manifest_resolved_path <- function(path, manifest) {
  resolved <- manifest$inspection$resolved_path %||% path
  if (!is_scalar_string(resolved) || !dir.exists(resolved)) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      "checkpoint manifest has no resolved MBridge path",
      operation = "checkpoint",
      checkpoint = path
    )
  }
  normalizePath(resolved, mustWork = TRUE)
}

assert_checkpoint_manifest_weights <- function(path, manifest) {
  if (identical(manifest$format, "mbridge")) {
    assert_mbridge_dcp_weights(
      checkpoint_manifest_resolved_path(path, manifest)
    )
  }
  invisible(manifest)
}

evo2_checkpoint_precision <- function(precision, record) {
  stopifnot(
    "precision must be one non-empty string" = is_scalar_string(precision)
  )
  if (identical(precision, "auto")) {
    return(record$mixed_precision_recipe %||% "bf16_mixed")
  }
  switch(
    precision,
    bf16 = "bf16_mixed",
    fp8 = "bf16_with_fp8_current_scaling_mixed",
    bf16_mixed = "bf16_mixed",
    bf16_with_fp8_current_scaling_mixed = "bf16_with_fp8_current_scaling_mixed",
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      "precision must be 'auto', 'bf16', 'fp8', or a supported recipe precision",
      operation = "checkpoint"
    )
  )
}

ngc_checkpoint_key <- function() {
  key <- Sys.getenv("NGC_CLI_API_KEY")
  if (!nzchar(key)) {
    key <- Sys.getenv("NGC_API_KEY")
  }
  key
}

checkpoint_conversion_command <- function(
  source,
  source_format,
  destination,
  model_size,
  tokenizer,
  sequence_length,
  precision,
  revision,
  compute
) {
  common <- c(
    "--tokenizer-path",
    tokenizer,
    "--mbridge-ckpt-dir",
    destination,
    "--model-size",
    model_size,
    "--seq-length",
    as.character(sequence_length),
    "--mixed-precision-recipe",
    precision
  )
  if (source_format == "savanna") {
    converter_source <- if (startsWith(source, "hf://")) {
      sub("^hf://", "", source)
    } else {
      source
    }
    args <- c(
      "--savanna-ckpt-path",
      converter_source,
      common,
      "--revision",
      revision
    )
    return(command_spec(
      "evo2_convert_savanna_to_mbridge",
      args,
      cwd = compute@workspace
    ))
  }
  if (source_format == "nemo2" && startsWith(source, "ngc://")) {
    key <- ngc_checkpoint_key()
    if (!nzchar(key)) {
      bionemor_abort(
        "BN_CHECKPOINT_SOURCE",
        "NGC_CLI_API_KEY or NGC_API_KEY is required for an ngc:// checkpoint source",
        operation = "checkpoint",
        checkpoint = source,
        hint = "Set NGC_CLI_API_KEY or NGC_API_KEY."
      )
    }
    script <- paste(
      "set -euo pipefail",
      "source_path=$(download_bionemo_data \"$1\")",
      "shift",
      "exec evo2_convert_nemo2_to_mbridge --nemo2-ckpt-dir \"$source_path\" \"$@\"",
      sep = "\n"
    )
    return(command_spec(
      "bash",
      c("-c", script, "bionemor-ngc", sub("^ngc://", "", source), common),
      env = c(NGC_CLI_API_KEY = key),
      cwd = compute@workspace,
      redactions = key
    ))
  }
  if (source_format == "nemo2") {
    return(command_spec(
      "evo2_convert_nemo2_to_mbridge",
      c("--nemo2-ckpt-dir", source, common),
      cwd = compute@workspace
    ))
  }
  bionemor_abort(
    "BN_CHECKPOINT_FORMAT",
    "unsupported checkpoint conversion",
    operation = "checkpoint",
    checkpoint = source
  )
}

checkpoint_copy_command <- function(source, destination, compute) {
  script <- paste(
    "set -euo pipefail",
    "mkdir -p \"$2\"",
    paste0(
      "find \"$1\" -mindepth 1 -maxdepth 1 ",
      "! -name '",
      checkpoint_manifest_file,
      "' ",
      "! -name '",
      checkpoint_completion_file,
      "' ",
      "-exec cp -a {} \"$2/\" \\;"
    ),
    sep = "\n"
  )
  command_spec(
    "bash",
    c("-c", script, "bionemor-checkpoint-copy", source, destination),
    cwd = compute@workspace
  )
}

checkpoint_inspection_command <- function(path, output, compute) {
  command_spec(
    "bionemor-evo2-helper",
    c("inspect-checkpoint", "--path", path, "--output", output),
    cwd = compute@workspace
  )
}

checkpoint_expected_manifest <- function(
  model,
  source,
  source_format,
  source_revision,
  recipe_revision,
  source_trust,
  source_verified,
  tokenizer_identity,
  tokenizer_revision,
  mixed_precision_recipe
) {
  record <- evo2_checkpoint_model_record(model)
  list(
    family = "evo2",
    variant = model@size,
    model_size = evo2_checkpoint_model_size(model, record),
    format = "mbridge",
    source = source,
    source_format = source_format,
    source_revision = source_revision,
    recipe_revision = recipe_revision,
    source_trust = source_trust,
    source_verified = source_verified,
    tokenizer_identity = tokenizer_identity,
    tokenizer_revision = tokenizer_revision,
    mixed_precision_recipe = mixed_precision_recipe
  )
}

materialize_checkpoint_job <- function(job, descriptor) {
  stopifnot(
    "checkpoint result descriptor is invalid" = is.list(descriptor) &&
      identical(descriptor$type, "checkpoint"),
    "checkpoint result path must be one non-empty string" = is_scalar_string(
      descriptor$path
    ),
    "checkpoint result format must be MBridge or Vortex" = descriptor$format %in%
      c("mbridge", "vortex")
  )
  path <- descriptor$path
  manifest_path <- checkpoint_manifest_path(path, descriptor$format)

  if (checkpoint_is_complete(path, descriptor$format)) {
    manifest <- read_checkpoint_manifest(path, manifest_path)
    assert_manifest_matches(manifest, descriptor$expected)
    assert_checkpoint_manifest_weights(path, manifest)
    return(checkpoint_from_manifest(path, manifest, manifest_path))
  }

  if (!file.exists(path)) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      "checkpoint operation did not create its output",
      run_path = job@path,
      operation = if (descriptor$format == "vortex") "export" else "checkpoint",
      model = descriptor$variant %||% NULL,
      checkpoint = path
    )
  }
  if (
    !is_scalar_string(descriptor$inspection) ||
      !file.exists(descriptor$inspection)
  ) {
    bionemor_abort(
      "BN_OUTPUT_SCHEMA",
      "checkpoint inspection output is missing",
      run_path = job@path,
      operation = "checkpoint",
      model = descriptor$variant %||% NULL,
      checkpoint = path
    )
  }
  inspection <- read_json_file(descriptor$inspection)
  if (descriptor$format == "mbridge") {
    schema_message <- if (!is_scalar_string(inspection$path)) {
      "checkpoint inspector did not report an MBridge path"
    } else if (!is_scalar_string(inspection$model_size)) {
      "checkpoint inspector did not report the model size"
    } else if (
      !identical(
        inspection$model_size,
        descriptor$expected$model_size
      )
    ) {
      "checkpoint model size does not match the requested model"
    } else if (!inspection$kind %in% c("dense", "lora")) {
      "checkpoint inspector did not report dense or LoRA weights"
    } else {
      NULL
    }
    if (!is.null(schema_message)) {
      bionemor_abort(
        "BN_OUTPUT_SCHEMA",
        schema_message,
        run_path = job@path,
        operation = "checkpoint",
        model = descriptor$variant %||% NULL,
        checkpoint = path
      )
    }
  } else {
    if (!file.exists(file.path(dirname(path), "config.json"))) {
      bionemor_abort(
        "BN_CHECKPOINT_INCOMPLETE",
        "Vortex export is missing config.json",
        run_path = job@path,
        operation = "export",
        model = descriptor$variant %||% NULL,
        checkpoint = path
      )
    }
  }

  kind <- if (descriptor$format == "mbridge") {
    inspection$kind
  } else {
    "dense"
  }
  base_checkpoint <- inspection$base_checkpoint %||%
    descriptor$base_checkpoint %||%
    NULL
  base_digest <- NULL
  if (identical(kind, "lora")) {
    if (!is_scalar_string(base_checkpoint) || !file.exists(base_checkpoint)) {
      bionemor_abort(
        "BN_BASE_CHECKPOINT_MISSING",
        if (!is_scalar_string(base_checkpoint)) {
          "LoRA checkpoint inspector did not report its base checkpoint"
        } else {
          "LoRA base checkpoint is not available"
        },
        run_path = job@path,
        operation = "checkpoint",
        model = descriptor$variant %||% NULL,
        checkpoint = path
      )
    }
    base_checkpoint <- normalizePath(base_checkpoint, mustWork = TRUE)
    base_digest <- path_digest(base_checkpoint)
  }

  manifest <- c(
    list(schema_version = 1L),
    descriptor$expected,
    list(
      kind = kind,
      tokenizer = inspection$tokenizer %||% descriptor$tokenizer,
      base_checkpoint_path = base_checkpoint,
      base_checkpoint_digest = base_digest,
      base_checkpoint_source = descriptor$base_checkpoint_source %||% NULL,
      inspection = inspection,
      provenance = list(
        run_path = descriptor$run_path,
        plan = descriptor$plan,
        created_at = base::format(Sys.time(), tz = "UTC", usetz = TRUE)
      )
    )
  )
  if (is.null(manifest$mixed_precision_recipe)) {
    manifest$mixed_precision_recipe <-
      inspection$mixed_precision_recipe %||% descriptor$precision
  }
  atomic_write_json(manifest, manifest_path)
  atomic_write_lines(
    jsonlite::toJSON(
      list(
        schema_version = 1L,
        manifest = basename(manifest_path)
      ),
      auto_unbox = TRUE
    ),
    checkpoint_completion_path(path, descriptor$format)
  )
  checkpoint_from_manifest(path, manifest, manifest_path)
}

inspect_checkpoint_for_export <- function(path, compute) {
  root <- file.path(compute@workspace, ".bionemor", "inspection")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  output <- tempfile("checkpoint-", tmpdir = root, fileext = ".json")
  on.exit(unlink(output), add = TRUE)
  result <- runtime_probe(
    compute,
    "bionemor-evo2-helper",
    c("inspect-checkpoint", "--path", path, "--output", output)
  )
  detail <- redact_credentials(trimws(paste(result$stdout, result$stderr)))
  if (result$status != 0L || !file.exists(output)) {
    bionemor_abort(
      "BN_CHECKPOINT_FORMAT",
      paste0(
        "MBridge checkpoint inspection failed",
        if (nzchar(detail)) paste0(": ", detail) else ""
      ),
      operation = "export",
      checkpoint = path,
      upstream_exit_status = result$status
    )
  }
  read_json_file(output)
}

assert_vortex_export_layout <- function(path, allow_existing = FALSE) {
  stopifnot(
    "allow_existing must be TRUE or FALSE" = is_scalar_logical(allow_existing)
  )
  directory <- dirname(path)
  if (!dir.exists(directory)) {
    return(invisible(path))
  }
  checkpoints <- list.files(
    directory,
    pattern = "[.]pt$",
    full.names = TRUE,
    recursive = FALSE
  )
  destination <- normalize_path(path)
  siblings <- checkpoints[
    vapply(
      checkpoints,
      function(checkpoint) {
        !identical(normalize_path(checkpoint), destination)
      },
      logical(1)
    )
  ]
  if (length(siblings) != 0L) {
    bionemor_abort(
      "BN_CHECKPOINT_FORMAT",
      "a Vortex export directory may contain only one Vortex checkpoint",
      operation = "export",
      checkpoint = path
    )
  }
  if (
    file.exists(file.path(directory, "config.json")) &&
      !(allow_existing && file.exists(path))
  ) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      "Vortex config.json already exists without its checkpoint",
      operation = "export",
      checkpoint = path
    )
  }
  invisible(path)
}

#' Prepare an Evo 2 checkpoint
#'
#' Convert a Savanna or NeMo2 checkpoint to the MBridge format used by the
#' BioNeMo Evo 2 recipe, or inspect and register an existing MBridge checkpoint.
#'
#' @param model An Evo 2 model specification.
#' @param source `"recommended"`, an `hf://` or `ngc://` URI, an existing local
#'   path, or a `BioNeMoCheckpoint`.
#' @param format Source checkpoint format. `"auto"` uses registry and URI
#'   metadata.
#' @param path Destination path, relative to the compute workspace.
#' @param compute A compute specification.
#' @param revision Exact source revision.
#' @param tokenizer Tokenizer path or `"recommended"`.
#' @param precision Conversion precision recipe.
#' @param overwrite Whether to replace an existing destination.
#' @param trust Whether to allow an unknown local or remote pickle-based
#'   checkpoint. The exact source and revision in the model registry are trusted.
#' @param async Whether to return a running job.
#'
#' Version 1 uses the pinned recipe's Transformer Engine key mapping for
#' Savanna conversion. Convert no-TE checkpoints explicitly upstream, then
#' register the resulting MBridge checkpoint.
#'
#' @return A `BioNeMoCheckpoint`, or a `BioNeMoJob` when `async = TRUE`.
#' @export
evo2_checkpoint <- function(
  model,
  source = "recommended",
  format = c("auto", "savanna", "nemo2", "mbridge"),
  path,
  compute,
  revision = "recommended",
  tokenizer = "recommended",
  precision = "auto",
  overwrite = FALSE,
  trust = FALSE,
  async = FALSE
) {
  invocation <- match.call(expand.dots = FALSE)
  format <- match.arg(format)
  stopifnot(
    "model must be an Evo 2 model specification" = S7_inherits(
      model,
      Evo2Model
    ),
    "source must be 'recommended', one source string, or a BioNeMo checkpoint" = is_scalar_string(
      source
    ) ||
      S7_inherits(source, BioNeMoCheckpoint),
    "path must be one non-empty string" = is_scalar_string(path),
    "compute must be a BioNeMo compute specification" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "compute workspace must exist" = dir.exists(compute@workspace),
    "revision must be one non-empty string" = is_scalar_string(revision),
    "tokenizer must be one non-empty string" = is_scalar_string(tokenizer),
    "overwrite must be TRUE or FALSE" = is_scalar_logical(overwrite),
    "trust must be TRUE or FALSE" = is_scalar_logical(trust),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )

  info <- checkpoint_source_info(
    model,
    source,
    format,
    revision,
    compute,
    trust
  )
  if (
    identical(info$format, "nemo2") &&
      identical(info$record$precision_policy, "vortex-fp8-on-hopper")
  ) {
    bionemor_abort(
      "BN_PRECISION_INCOMPATIBLE",
      paste0(
        "Vortex-sensitive NeMo2 conversion requires an explicit upstream ",
        "--vortex-style-fp8 decision; convert it upstream and register the ",
        "resulting MBridge checkpoint"
      ),
      operation = "checkpoint",
      model = model@size,
      checkpoint = info$source,
      recipe_revision = compute@recipe@revision
    )
  }
  destination <- normalize_path(path, base = compute@workspace)
  stopifnot(
    "checkpoint destination must not be the filesystem root" = !identical(
      dirname(destination),
      destination
    ),
    "checkpoint destination must not be the compute workspace itself" = !identical(
      destination,
      normalize_path(compute@workspace)
    )
  )
  remote_source <- startsWith(info$source, "hf://") ||
    startsWith(info$source, "ngc://")
  if (compute@engine == "container" || compute@backend == "slurm") {
    stopifnot(
      "checkpoint path must be inside the compute workspace" = path_is_within(
        destination,
        compute@workspace
      ),
      "local source must be inside the compute workspace for container or Slurm execution" = remote_source ||
        path_is_within(info$source, compute@workspace)
    )
  }
  source_is_destination <- !remote_source &&
    identical(normalize_path(info$source), destination)
  if (
    !remote_source &&
      !source_is_destination &&
      (path_is_within(info$source, destination) ||
        path_is_within(destination, info$source))
  ) {
    bionemor_abort(
      "BN_CHECKPOINT_SOURCE",
      "local checkpoint source and destination must not overlap",
      operation = "checkpoint",
      model = model@size,
      checkpoint = info$source
    )
  }
  if (source_is_destination && info$format != "mbridge") {
    bionemor_abort(
      "BN_CHECKPOINT_FORMAT",
      "only an existing MBridge checkpoint can be registered in place",
      operation = "checkpoint",
      model = model@size,
      checkpoint = destination
    )
  }

  record <- info$record
  model_size <- evo2_checkpoint_model_size(model, record)
  sequence_length <- evo2_checkpoint_context_length(model, record)
  tokenizer_path <- evo2_checkpoint_tokenizer(tokenizer, record, compute)
  tokenizer_provenance <- evo2_checkpoint_tokenizer_provenance(
    model,
    tokenizer,
    tokenizer_path,
    record
  )
  precision_recipe <- evo2_checkpoint_precision(precision, record)
  expected <- checkpoint_expected_manifest(
    model,
    info$source,
    info$format,
    info$revision,
    compute@recipe@revision,
    info$source_trust,
    info$source_verified,
    tokenizer_provenance$identity,
    tokenizer_provenance$revision,
    precision_recipe
  )
  if (checkpoint_is_complete(destination, "mbridge") && !overwrite) {
    manifest_path <- checkpoint_manifest_path(destination, "mbridge")
    manifest <- read_checkpoint_manifest(destination, manifest_path)
    assert_manifest_matches(manifest, expected)
    assert_checkpoint_manifest_weights(destination, manifest)
    return(checkpoint_from_manifest(destination, manifest, manifest_path))
  }
  if (file.exists(destination) && !overwrite && !source_is_destination) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      paste0("checkpoint destination exists but is incomplete: ", destination),
      operation = "checkpoint",
      model = model@size,
      checkpoint = destination
    )
  }
  if (file.exists(destination) && overwrite) {
    if (source_is_destination) {
      bionemor_abort(
        "BN_CHECKPOINT_SOURCE",
        "cannot overwrite a checkpoint that is also the conversion source",
        operation = "checkpoint",
        model = model@size,
        checkpoint = destination
      )
    }
    unlink(destination, recursive = TRUE, force = TRUE)
    unlink(checkpoint_manifest_path(destination, "mbridge"), force = TRUE)
    unlink(checkpoint_completion_path(destination, "mbridge"), force = TRUE)
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  source_request <- if (S7_inherits(source, BioNeMoCheckpoint)) {
    checkpoint_path(source)
  } else {
    source
  }
  request <- list(
    operation = "checkpoint",
    model = model@size,
    model_size = model_size,
    source_request = source_request,
    source = info$source,
    format_request = format,
    source_format = info$format,
    revision_request = revision,
    source_revision = info$revision,
    trust = trust,
    source_trust = info$source_trust,
    source_verified = info$source_verified,
    destination = destination,
    tokenizer_request = tokenizer,
    tokenizer = tokenizer_path,
    precision_request = precision,
    precision = precision_recipe,
    overwrite = overwrite
  )
  request_origins <- argument_origin_map(
    request,
    invocation,
    argument_map = c(
      model = "model",
      source_request = "source",
      format_request = "format",
      revision_request = "revision",
      trust = "trust",
      destination = "path",
      tokenizer_request = "tokenizer",
      precision_request = "precision",
      overwrite = "overwrite"
    ),
    adapter_defaults = "operation",
    auto_resolved = c(
      "model_size",
      "source_trust",
      "source_verified"
    )
  )
  request_origins$source <- if (identical(source_request, info$source)) {
    request_origins$source_request
  } else {
    "auto_resolved"
  }
  request_origins$source_format <- if (identical(format, "auto")) {
    "auto_resolved"
  } else {
    request_origins$format_request
  }
  request_origins$source_revision <- if (identical(revision, "recommended")) {
    "auto_resolved"
  } else {
    request_origins$revision_request
  }
  request_origins$tokenizer <- if (identical(tokenizer, "recommended")) {
    "auto_resolved"
  } else {
    request_origins$tokenizer_request
  }
  request_origins$precision <- if (identical(precision, "auto")) {
    "auto_resolved"
  } else {
    request_origins$precision_request
  }
  run_path <- create_run(
    compute,
    kind = "checkpoint",
    request = request,
    request_origins = request_origins
  )
  inspection <- file.path(run_path, "outputs", "checkpoint-inspection.json")
  steps <- list()
  if (info$format %in% c("savanna", "nemo2")) {
    steps[[length(steps) + 1L]] <- checkpoint_conversion_command(
      info$source,
      info$format,
      destination,
      model_size,
      tokenizer_path,
      sequence_length,
      precision_recipe,
      info$revision,
      compute
    )
  } else if (!source_is_destination) {
    steps[[length(steps) + 1L]] <- checkpoint_copy_command(
      info$source,
      destination,
      compute
    )
  }
  steps[[length(steps) + 1L]] <- checkpoint_inspection_command(
    destination,
    inspection,
    compute
  )
  plan <- command_plan(
    steps,
    metadata = list(
      operation = "checkpoint",
      recipe_revision = compute@recipe@revision
    )
  )
  descriptor <- list(
    type = "checkpoint",
    path = destination,
    format = "mbridge",
    expected = expected,
    variant = model@size,
    model_size = model_size,
    inspection = inspection,
    tokenizer = tokenizer_path,
    tokenizer_revision = record$tokenizer_revision,
    precision = precision_recipe,
    base_checkpoint = NULL,
    base_checkpoint_source = NULL,
    source = info$source,
    source_format = info$format,
    source_revision = info$revision,
    source_trust = info$source_trust,
    source_verified = info$source_verified,
    run_path = run_path,
    plan = serializable_plan(plan)
  )
  submit_plan(
    plan,
    compute,
    run_path,
    kind = "checkpoint",
    expected_result = descriptor,
    async = async
  )
}

#' Export an Evo 2 checkpoint
#'
#' @param model An Evo 2 model bound to a dense MBridge checkpoint.
#' @param path Destination `.pt` path.
#' @param format Export format. Version 1 supports `"vortex"`.
#' @param strip_optimizer Whether to create a weights-only MBridge intermediate.
#' @param compute A compute specification.
#' @param overwrite Whether to replace an existing export.
#' @param async Whether to return a running job.
#'
#' @return A Vortex `BioNeMoCheckpoint`, or a `BioNeMoJob` when
#'   `async = TRUE`.
#'
#' The upstream exporter writes a shared `config.json` beside the `.pt` file,
#' so each destination directory may contain only one Vortex checkpoint.
#' The checkpoint inspector selects the Transformer Engine key mapping and
#' adds `--no-te` for a non-TE MBridge checkpoint. An unidentified key layout
#' is rejected before export.
#' @export
evo2_export <- function(
  model,
  path,
  format = "vortex",
  strip_optimizer = TRUE,
  compute,
  overwrite = FALSE,
  async = FALSE
) {
  invocation <- match.call(expand.dots = FALSE)
  stopifnot(
    "model must be an Evo 2 model specification" = S7_inherits(
      model,
      Evo2Model
    ),
    "path must be one non-empty string" = is_scalar_string(path),
    "format must be 'vortex'" = identical(format, "vortex"),
    "strip_optimizer must be TRUE or FALSE" = is_scalar_logical(
      strip_optimizer
    ),
    "compute must be a BioNeMo compute specification" = S7_inherits(
      compute,
      BioNeMoCompute
    ),
    "overwrite must be TRUE or FALSE" = is_scalar_logical(overwrite),
    "async must be TRUE or FALSE" = is_scalar_logical(async)
  )
  checkpoint <- model@checkpoint
  if (
    !is_scalar_string(checkpoint) &&
      !S7_inherits(checkpoint, BioNeMoCheckpoint)
  ) {
    bionemor_abort(
      "BN_BASE_CHECKPOINT_MISSING",
      "model must have a prepared MBridge checkpoint",
      operation = "export",
      model = model@size
    )
  }
  if (S7_inherits(checkpoint, BioNeMoCheckpoint)) {
    if (checkpoint@format != "mbridge") {
      bionemor_abort(
        "BN_CHECKPOINT_FORMAT",
        "model checkpoint must use MBridge format",
        operation = "export",
        model = model@size,
        checkpoint = checkpoint@path
      )
    }
  }
  source <- checkpoint_path(checkpoint)
  source_inspection <- inspect_checkpoint_for_export(source, compute)
  inspection_message <- if (!is_scalar_string(source_inspection$model_size)) {
    "checkpoint inspector did not report the model size"
  } else if (!identical(source_inspection$model_size, model@model_size)) {
    "checkpoint model size does not match the requested model"
  } else if (identical(source_inspection$kind, "lora")) {
    "LoRA checkpoints cannot be exported to Vortex in version 1"
  } else if (!is_scalar_logical(source_inspection$transformer_engine)) {
    paste(
      "checkpoint inspector did not identify a Transformer Engine key layout;",
      "export it upstream with an explicit --no-te decision"
    )
  } else {
    NULL
  }
  if (!is.null(inspection_message)) {
    bionemor_abort(
      "BN_CHECKPOINT_FORMAT",
      inspection_message,
      operation = "export",
      model = model@size,
      checkpoint = source
    )
  }
  transformer_engine <- source_inspection$transformer_engine
  destination <- normalize_path(path, base = compute@workspace)
  stopifnot(
    "export destination must not be a filesystem root" = !identical(
      dirname(destination),
      destination
    ),
    "export destination must differ from the source checkpoint" = !identical(
      destination,
      source
    )
  )
  if (compute@engine == "container" || compute@backend == "slurm") {
    stopifnot(
      "export destination must be inside the compute workspace" = path_is_within(
        destination,
        compute@workspace
      ),
      "checkpoint must be inside the compute workspace" = path_is_within(
        source,
        compute@workspace
      )
    )
  }
  record <- evo2_checkpoint_model_record(model)
  model_size <- evo2_checkpoint_model_size(model, record)
  source_manifest <- if (
    S7_inherits(checkpoint, BioNeMoCheckpoint) &&
      !is.null(checkpoint@manifest) &&
      file.exists(checkpoint@manifest)
  ) {
    checkpoint_manifest(checkpoint)
  } else {
    list()
  }
  source_revision <- source_manifest$source_revision
  if (
    is.null(source_revision) &&
      S7_inherits(checkpoint, BioNeMoCheckpoint)
  ) {
    source_revision <- checkpoint@source_revision
  }
  source_revision <- source_revision %||% path_digest(source)
  expected <- list(
    family = "evo2",
    variant = model@size,
    model_size = model_size,
    format = "vortex",
    source = source,
    source_format = "mbridge",
    source_revision = source_revision,
    recipe_revision = compute@recipe@revision,
    source_trust = source_manifest$source_trust %||% "not-recorded",
    source_verified = isTRUE(source_manifest$source_verified)
  )
  if (checkpoint_is_complete(destination, "vortex") && !overwrite) {
    manifest_path <- checkpoint_manifest_path(destination, "vortex")
    manifest <- read_checkpoint_manifest(destination, manifest_path)
    assert_manifest_matches(manifest, expected)
    stopifnot(
      "Vortex export is missing config.json" = file.exists(file.path(
        dirname(destination),
        "config.json"
      ))
    )
    return(checkpoint_from_manifest(destination, manifest, manifest_path))
  }
  assert_vortex_export_layout(destination, allow_existing = overwrite)
  if (file.exists(destination) && !overwrite) {
    bionemor_abort(
      "BN_CHECKPOINT_INCOMPLETE",
      paste0("export destination exists but is incomplete: ", destination),
      operation = "export",
      model = model@size,
      checkpoint = destination
    )
  }
  if (file.exists(destination) && overwrite) {
    unlink(destination, force = TRUE)
    unlink(checkpoint_manifest_path(destination, "vortex"), force = TRUE)
    unlink(checkpoint_completion_path(destination, "vortex"), force = TRUE)
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)

  request <- list(
    operation = "export",
    model = model@size,
    source = source,
    source_trust = expected$source_trust,
    source_verified = expected$source_verified,
    destination = destination,
    format = format,
    strip_optimizer = strip_optimizer,
    overwrite = overwrite
  )
  request_origins <- argument_origin_map(
    request,
    invocation,
    argument_map = c(
      model = "model",
      destination = "path",
      format = "format",
      strip_optimizer = "strip_optimizer",
      overwrite = "overwrite"
    ),
    adapter_defaults = "operation",
    auto_resolved = c(
      "source",
      "source_trust",
      "source_verified"
    )
  )
  run_path <- create_run(
    compute,
    kind = "export",
    request = request,
    request_origins = request_origins
  )
  export_source <- source
  steps <- list()
  if (strip_optimizer) {
    export_source <- file.path(run_path, "upstream", "weights-only")
    steps[[length(steps) + 1L]] <- command_spec(
      "evo2_remove_optimizer",
      c(
        "--src-ckpt-dir",
        source,
        "--dst-ckpt-dir",
        export_source
      ),
      cwd = compute@workspace
    )
  }
  steps[[length(steps) + 1L]] <- command_spec(
    "evo2_export_mbridge_to_vortex",
    c(
      "--mbridge-ckpt-dir",
      export_source,
      "--output-path",
      destination,
      "--model-size",
      model_size,
      if (!transformer_engine) "--no-te"
    ),
    cwd = compute@workspace
  )
  inspection <- file.path(run_path, "outputs", "export-inspection.json")
  steps[[length(steps) + 1L]] <- command_spec(
    "bionemor-evo2-helper",
    c(
      "write-manifest-fragment",
      "--path",
      destination,
      "--output",
      inspection
    ),
    cwd = compute@workspace
  )
  plan <- command_plan(
    steps,
    metadata = list(
      operation = "export",
      recipe_revision = compute@recipe@revision,
      transformer_engine = transformer_engine
    )
  )
  descriptor <- list(
    type = "checkpoint",
    path = destination,
    format = "vortex",
    expected = expected,
    inspection = inspection,
    tokenizer = source_manifest$tokenizer %||% NULL,
    precision = source_manifest$mixed_precision_recipe %||% NULL,
    base_checkpoint = NULL,
    base_checkpoint_source = NULL,
    run_path = run_path,
    plan = serializable_plan(plan)
  )
  submit_plan(
    plan,
    compute,
    run_path,
    kind = "export",
    expected_result = descriptor,
    async = async
  )
}

#' Return a checkpoint path
#'
#' @param x A checkpoint, model, or one checkpoint path.
#'
#' @return One normalized path.
#' @export
checkpoint_path <- function(x) {
  path <- if (S7_inherits(x, BioNeMoCheckpoint)) {
    x@path
  } else if (S7_inherits(x, BioNeMoModel)) {
    model_checkpoint_path(x)
  } else {
    x
  }
  stopifnot(
    "x does not contain one checkpoint path" = is_scalar_string(path)
  )
  normalize_path(path)
}

#' Read checkpoint provenance
#'
#' @param x A checkpoint, model, or one checkpoint path.
#'
#' @return A named list.
#' @export
checkpoint_manifest <- function(x) {
  if (S7_inherits(x, BioNeMoCheckpoint) && !is.null(x@manifest)) {
    return(read_checkpoint_manifest(x@path, x@manifest))
  }
  path <- checkpoint_path(x)
  read_checkpoint_manifest(path)
}
