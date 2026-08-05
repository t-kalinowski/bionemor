write_executable <- function(path, lines) {
  writeLines(c("#!/usr/bin/env bash", "set -euo pipefail", lines), path)
  Sys.chmod(path, "0755")
  path
}

write_r_executable <- function(path, lines) {
  writeLines(c(paste0("#!", file.path(R.home("bin"), "Rscript")), lines), path)
  Sys.chmod(path, "0755")
  path
}

fake_pooled_embedding_writer <- c(
  "write_pooled_embeddings <- function(prefix, ids, values, source_dtype) {",
  "  stopifnot(is.matrix(values), all(is.finite(values)))",
  "  dir.create(dirname(prefix), recursive = TRUE, showWarnings = FALSE)",
  "  data <- paste0(prefix, '.f32.gz')",
  "  connection <- gzfile(data, 'wb', compression = 1L)",
  "  writeBin(as.double(t(values)), connection, size = 4L, endian = 'little')",
  "  close(connection)",
  "  metadata <- list(format = 'bionemor-pooled-embeddings', version = 1L, shape = as.integer(dim(values)), ids = ids, source_dtype = source_dtype, storage_dtype = 'float32', byte_order = 'little', order = 'row-major', compression = 'gzip', compression_level = 1L, uncompressed_bytes = as.double(length(values) * 4), data_md5 = unname(tools::md5sum(data)))",
  "  jsonlite::write_json(metadata, paste0(prefix, '.json'), auto_unbox = TRUE, pretty = TRUE)",
  "}"
)

expect_pooled_embedding_output <- function(prefix, shape, ids, source_dtype) {
  expect_false(file.exists(prefix))
  expect_true(all(file.exists(paste0(prefix, c(".f32.gz", ".json")))))
  metadata <- jsonlite::read_json(
    paste0(prefix, ".json"),
    simplifyVector = TRUE
  )
  expected <- list(
    format = "bionemor-pooled-embeddings",
    version = 1L,
    shape = shape,
    ids = ids,
    storage_dtype = "float32",
    byte_order = "little",
    order = "row-major",
    compression = "gzip",
    compression_level = 1L
  )
  expect_identical(metadata[names(expected)], expected)
  expect_true(metadata$source_dtype %in% source_dtype)
  expect_equal(metadata$uncompressed_bytes, prod(shape) * 4)
  expect_identical(
    metadata$data_md5,
    unname(tools::md5sum(paste0(prefix, ".f32.gz")))
  )
  invisible(metadata)
}

write_fake_dcp_weights <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  writeLines("metadata", file.path(path, ".metadata"))
  writeLines("weights", file.path(path, "__0_0.distcp"))
  writeLines("common", file.path(path, "common.pt"))
  writeLines("{}", file.path(path, "metadata.json"))
  invisible(path)
}

fake_recipes_runtime <- function(bin) {
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  materializer <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "materialize-evo2.py"
  )
  if (!file.exists(materializer)) {
    materializer <- system.file(
      "scripts",
      "materialize-evo2.py",
      package = "bionemor"
    )
  }
  materializer <- normalizePath(materializer, mustWork = TRUE)
  python <- Sys.which("python3")
  stopifnot(nzchar(python))
  python_modules <- file.path(bin, "python-modules")
  dir.create(python_modules)
  writeLines("# Generation conversion does not use torch.", file.path(
    python_modules,
    "torch.py"
  ))
  tokenizer_root <- file.path(bin, "tokenizers")
  dir.create(
    file.path(tokenizer_root, "nucleotide_fast_tokenizer_256"),
    recursive = TRUE
  )
  dir.create(
    file.path(tokenizer_root, "nucleotide_fast_tokenizer_512"),
    recursive = TRUE
  )

  write_r_executable(
    file.path(bin, "evo2_convert_savanna_to_mbridge"),
    c(
      "args <- commandArgs(TRUE)",
      "log <- Sys.getenv('BIONEMOR_FAKE_LOG')",
      "writeLines(c('convert', args), log)",
      "output <- args[[match('--mbridge-ckpt-dir', args) + 1L]]",
      "dir.create(output, recursive = TRUE)",
      "writeLines(c('model_size: evo2_7b', 'kind: dense'), file.path(output, 'run_config.yaml'))",
      "writeLines('metadata', file.path(output, '.metadata'))",
      "writeLines('weights', file.path(output, '__0_0.distcp'))",
      "writeLines('common', file.path(output, 'common.pt'))",
      "writeLines('{}', file.path(output, 'metadata.json'))",
      "writeLines('complete', file.path(output, '.bionemor-complete'))"
    )
  )

  write_r_executable(
    file.path(bin, "download_bionemo_data"),
    c(
      "source <- Sys.getenv('BIONEMOR_FAKE_NGC_SOURCE')",
      "stopifnot(nzchar(source))",
      "dir.create(source, recursive = TRUE, showWarnings = FALSE)",
      "cat(source)"
    )
  )

  write_r_executable(
    file.path(bin, "evo2_convert_nemo2_to_mbridge"),
    c(
      "args <- commandArgs(TRUE)",
      "if (identical(Sys.getenv('BIONEMOR_FAKE_ECHO_CREDENTIAL'), 'true')) cat(Sys.getenv('NGC_CLI_API_KEY'), '\\n')",
      "output <- args[[match('--mbridge-ckpt-dir', args) + 1L]]",
      "dir.create(output, recursive = TRUE)",
      "writeLines(c('model_size: evo2_7b', 'kind: dense'), file.path(output, 'run_config.yaml'))",
      "writeLines('metadata', file.path(output, '.metadata'))",
      "writeLines('weights', file.path(output, '__0_0.distcp'))",
      "writeLines('common', file.path(output, 'common.pt'))",
      "writeLines('{}', file.path(output, 'metadata.json'))",
      "writeLines('complete', file.path(output, '.bionemor-complete'))"
    )
  )

  write_r_executable(
    file.path(bin, "evo2_remove_optimizer"),
    c(
      "args <- commandArgs(TRUE)",
      "value <- function(flag) args[[match(flag, args) + 1L]]",
      "output <- value('--dst-ckpt-dir')",
      "dir.create(output, recursive = TRUE, showWarnings = FALSE)",
      "writeLines(c('model_size: evo2_7b', 'kind: weights_only'), file.path(output, 'run_config.yaml'))",
      "writeLines('metadata', file.path(output, '.metadata'))",
      "writeLines('weights', file.path(output, '__0_0.distcp'))"
    )
  )

  write_r_executable(
    file.path(bin, "evo2_export_mbridge_to_vortex"),
    c(
      "args <- commandArgs(TRUE)",
      "value <- function(flag) args[[match(flag, args) + 1L]]",
      "output <- value('--output-path')",
      "dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)",
      "writeLines('vortex checkpoint', output)",
      "jsonlite::write_json(list(model_size = value('--model-size')), file.path(dirname(output), 'config.json'), auto_unbox = TRUE)"
    )
  )

  write_r_executable(
    file.path(bin, "preprocess_evo2"),
    c(
      "args <- commandArgs(TRUE)",
      "config <- yaml12::read_yaml(args[[match('--config', args) + 1L]], simplify = FALSE)",
      "for (record in config) {",
      "  dir.create(record$output_dir, recursive = TRUE, showWarnings = FALSE)",
      "  tokenizer <- tolower(gsub(' ', '', basename(record$hf_tokenizer_model_path), fixed = TRUE))",
      "  for (split in c('train', 'val', 'test')) {",
      "    prefix <- file.path(record$output_dir, paste(record$output_prefix, tokenizer, split, sep = '_'))",
      "    writeLines('bin', paste0(prefix, '.bin'))",
      "    writeLines('idx', paste0(prefix, '.idx'))",
      "  }",
      "}"
    )
  )

  write_r_executable(
    file.path(bin, "torchrun"),
    c(
      "args <- commandArgs(TRUE)",
      "log <- Sys.getenv('BIONEMOR_FAKE_LOG')",
      "write(c('torchrun', args), file = log, ncolumns = 1L, append = TRUE)",
      "if (identical(Sys.getenv('BIONEMOR_REJECT_CREDENTIALS'), 'true') && any(nzchar(Sys.getenv(c('NGC_API_KEY', 'NGC_CLI_API_KEY', 'HF_TOKEN', 'HUGGING_FACE_HUB_TOKEN'))))) stop('credential leaked into recipe process')",
      "pid_file <- Sys.getenv('BIONEMOR_FAKE_PID_FILE')",
      "if (nzchar(pid_file)) writeLines(as.character(Sys.getpid()), pid_file)",
      "delay <- as.numeric(Sys.getenv('BIONEMOR_FAKE_DELAY', '0'))",
      "if (delay > 0) Sys.sleep(delay)",
      "if (identical(Sys.getenv('BIONEMOR_FAKE_STALE_OOM'), 'true')) cat('torch.OutOfMemoryError: CUDA out of memory\\n', file = stderr())",
      "operation <- args[[match('--no-python', args) + 1L]]",
      "operation_args <- args[(match('--no-python', args) + 2L):length(args)]",
      "value <- function(flag) operation_args[[match(flag, operation_args) + 1L]]",
      "if (operation == 'infer_evo2') {",
      "  input <- jsonlite::stream_in(file(value('--prompt-file')), verbose = FALSE)",
      "  completion <- Sys.getenv('BIONEMOR_FAKE_COMPLETION', 'ACGT')",
      "  generated_tokens <- as.integer(Sys.getenv('BIONEMOR_FAKE_GENERATED_TOKENS', '4'))",
      "  log_probability <- as.numeric(Sys.getenv('BIONEMOR_FAKE_LOG_PROBABILITY', as.character(log(0.25))))",
      "  input$completion <- rep(completion, nrow(input))",
      "  input$finish_reason <- rep(Sys.getenv('BIONEMOR_FAKE_FINISH_REASON', 'length'), nrow(input))",
      "  input$usage <- I(lapply(nchar(input$prompt), function(n) list(prompt_tokens = jsonlite::unbox(n), completion_tokens = jsonlite::unbox(generated_tokens), total_tokens = jsonlite::unbox(n + generated_tokens))))",
      "  input$logprobs <- I(replicate(nrow(input), list(completion_logprobs = rep(log_probability, generated_tokens)), simplify = FALSE))",
      "  input$input_id <- NULL",
      "  input$sample <- NULL",
      "  dir.create(dirname(value('--output-file')), recursive = TRUE, showWarnings = FALSE)",
      "  jsonlite::stream_out(input, file(value('--output-file')), verbose = FALSE)",
      "} else if (operation == 'predict_evo2') {",
      "  output <- value('--output-dir')",
      "  dir.create(output, recursive = TRUE, showWarnings = FALSE)",
      "  writeLines('trusted test tensor', file.path(output, 'predictions__0_0.pt'))",
      "} else if (operation == 'train_evo2') {",
      "  root <- file.path(value('--result-dir'), value('--experiment-name'))",
      "  checkpoint <- file.path(root, 'checkpoints', 'iter_0000001')",
      "  dir.create(checkpoint, recursive = TRUE, showWarnings = FALSE)",
      "  kind <- if ('--lora-finetune' %in% operation_args) 'lora' else 'training'",
      "  config <- list(model_size = value('--model-size'), kind = kind, model = list(vortex_style_fp8 = FALSE), checkpoint = list(pretrained_checkpoint = value('--finetune-ckpt-dir')))",
      "  yaml12::write_yaml(config, file.path(checkpoint, 'run_config.yaml'))",
      "  writeLines('metadata', file.path(checkpoint, '.metadata'))",
      "  writeLines('weights', file.path(checkpoint, '__0_0.distcp'))",
      "  writeLines('common', file.path(checkpoint, 'common.pt'))",
      "  writeLines('{}', file.path(checkpoint, 'metadata.json'))",
      "  writeLines('1', file.path(root, 'checkpoints', 'latest_checkpointed_iteration.txt'))",
      "} else stop('unsupported fake torchrun operation')"
    )
  )

  fake_helper <- write_r_executable(
    file.path(bin, "bionemor-evo2-helper-fake"),
    c(
      "args <- commandArgs(TRUE)",
      "command <- args[[1L]]",
      "value <- function(flag) args[[match(flag, args) + 1L]]",
      fake_pooled_embedding_writer,
      "if (identical(args, c('describe', '--json'))) {",
      "  result <- list(",
      "    protocol_version = 2L,",
      "    driver = 'evo2-megatron',",
      "    execution_schema_version = 1L,",
      "    semantic_operations = list('generate'),",
      "    helper_version = '0.2.0',",
      paste0("    helper_sha256 = '", strrep("d", 64L), "',"),
      "    recipe_version = '2.4',",
      "    recipe_revision = 'e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e',",
      paste0(
        "    tokenizers = list(",
        "nucleotide_fast_tokenizer_256 = ",
        deparse(file.path(tokenizer_root, "nucleotide_fast_tokenizer_256")),
        ", nucleotide_fast_tokenizer_512 = ",
        deparse(file.path(tokenizer_root, "nucleotide_fast_tokenizer_512")),
        "),"
      ),
      "    commands = list(infer_evo2 = TRUE, predict_evo2 = TRUE, train_evo2 = TRUE, preprocess_evo2 = TRUE, savanna_to_mbridge = TRUE, nemo2_to_mbridge = TRUE, mbridge_to_vortex = TRUE, remove_optimizer = TRUE),",
      "    features = list(generation_jsonl = TRUE, generation_log_probs = TRUE, score_sum = TRUE, score_mean = TRUE, score_per_token = TRUE, embedding_layer = TRUE, pooled_embeddings_f32_gzip = TRUE, lora = TRUE),",
      "    runtime = list(",
      "      python = '3.12.0', pytorch = '2.8.0', cuda = '12.9',",
      "      cuda_available = TRUE, gpu_count = 1L, driver = '575.51',",
      "      transformer_engine = '2.5.0', megatron_bridge = '0.4.1', megatron_core = '0.13.0',",
      "      imports = list(torch = TRUE, bionemo = TRUE, megatron_bridge = TRUE, transformer_engine = TRUE),",
      "      gpus = list(list(index = 0L, name = 'H100', total_memory_bytes = 85899345920, compute_capability_major = 9L, compute_capability_minor = 0L))",
      "    )",
      "  )",
      "  cat(jsonlite::toJSON(result, auto_unbox = TRUE))",
      "} else if (command == 'inspect-checkpoint') {",
      "  path <- value('--path')",
      "  resolved <- path",
      "  if (!file.exists(file.path(resolved, 'run_config.yaml'))) {",
      "    candidates <- list.dirs(resolved, recursive = FALSE, full.names = TRUE)",
      "    candidates <- candidates[grepl('iter_[0-9]+$', candidates)]",
      "    stopifnot(length(candidates) > 0L)",
      "    resolved <- sort(candidates)[[length(candidates)]]",
      "  }",
      "  stopifnot(file.exists(file.path(resolved, '.metadata')))",
      "  shards <- list.files(resolved, pattern = '[.]distcp$', full.names = TRUE)",
      "  stopifnot(length(shards) > 0L, all(file.info(shards)$size > 0))",
      "  config <- yaml12::read_yaml(file.path(resolved, 'run_config.yaml'), simplify = FALSE)",
      "  transformer_engine <- config[['model']][['transformer_engine']]",
      "  result <- list(path = normalizePath(path), resolved_path = normalizePath(resolved), transformer_engine = transformer_engine, distributed_checkpoint = list(metadata = file.path(resolved, '.metadata'), weight_shards = as.list(shards)))",
      "  jsonlite::write_json(result, value('--output'), auto_unbox = TRUE, null = 'null', pretty = TRUE)",
      "} else if (command == 'materialize-predictions') {",
      "  map <- jsonlite::read_json(value('--sequence-map'), simplifyVector = TRUE)",
      "  mode <- value('--mode')",
      "  if (mode == 'score') {",
      "    rows <- data.frame(",
      "      derived_id = map$derived_id,",
      "      score = -seq_along(map$derived_id),",
      "      tokens_scored = pmax(map$sequence_length - 1L, 0L),",
      "      stringsAsFactors = FALSE",
      "    )",
      "  } else if (mode == 'embedding-pooled') {",
      "    rows <- data.frame(id = map$id, stringsAsFactors = FALSE)",
      "    rows$embedding <- I(lapply(seq_len(nrow(rows)), function(i) as.double(c(i, i + 1L, i + 2L))))",
      "  } else if (mode == 'profile') {",
      "    rows <- data.frame(id = map$id, value = 0, stringsAsFactors = FALSE)",
      "  } else if (mode == 'embedding-unpooled') {",
      "    rows <- do.call(rbind, lapply(seq_len(nrow(map)), function(i) {",
      "      n <- map$sequence_length[[i]]",
      "      row <- data.frame(id = rep(map$id[[i]], n), position = seq_len(n), strand = rep(map$strand[[i]], n), stringsAsFactors = FALSE)",
      "      row$embedding <- I(rep(list(c(0, 1, 2)), n))",
      "      row[c('id', 'position', 'embedding', 'strand')]",
      "    }))",
      "  } else stop('unsupported fake materialization mode')",
      "  output <- value('--output')",
      "  if (mode == 'embedding-pooled') {",
      "    write_pooled_embeddings(output, rows$id, do.call(rbind, rows$embedding), 'bfloat16')",
      "  } else {",
      "    jsonlite::stream_out(rows, file(output), verbose = FALSE)",
      "    summary <- list(rows = nrow(rows), mode = mode)",
      "    if (mode == 'embedding-unpooled') {",
      "      summary$shape <- c(nrow(rows), 4L)",
      "      summary$schema <- list(id = 'string', position = 'int64', embedding = 'list<double>', strand = 'string')",
      "    }",
      "    jsonlite::write_json(summary, paste0(output, '.summary.json'), auto_unbox = TRUE, pretty = TRUE)",
      "  }",
      "} else if (command == 'write-manifest-fragment') {",
      "  result <- list(path = normalizePath(value('--path')), kind = 'dense')",
      "  jsonlite::write_json(result, value('--output'), auto_unbox = TRUE, pretty = TRUE)",
      "} else stop('unsupported fake helper command')"
    )
  )

  write_executable(
    file.path(bin, "bionemor-evo2-helper"),
    c(
      "if [[ \"${1:-}\" == \"validate-generation\" ]]; then",
      paste0("  export PYTHONPATH=", shQuote(python_modules)),
      paste("  exec", shQuote(python), shQuote(materializer), "\"$@\""),
      "fi",
      paste("exec", shQuote(fake_helper), "\"$@\"")
    )
  )

  invisible(bin)
}

fake_esm2_runtime <- function(
  bin,
  compute_capability_major = 8L,
  compute_capability_minor = 9L
) {
  stopifnot(
    is_scalar_integerish(compute_capability_major, min = 0L),
    is_scalar_integerish(compute_capability_minor, min = 0L)
  )
  capability <- paste(
    compute_capability_major,
    compute_capability_minor,
    sep = "."
  )
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  write_executable(
    file.path(bin, "nvidia-smi"),
    paste0("printf '", capability, "\\n'")
  )
  write_r_executable(
    file.path(bin, "bionemor-esm2-helper"),
    c(
      "args <- commandArgs(TRUE)",
      "if (identical(args[[1L]], '--help')) quit(save = 'no', status = 0L)",
      "value <- function(flag) args[[match(flag, args) + 1L]]",
      fake_pooled_embedding_writer,
      "if (identical(args[[1L]], 'describe')) {",
      paste0(
        "  report <- list(protocol_version = 2L, helper_version = '0.3.0', helper_sha256 = paste(rep('e', 64L), collapse = ''), recipe_version = 'transformers-5.14.1', recipe_revision = 'e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e', driver = 'esm2-transformers', execution_schema_version = 1L, semantic_operations = list('embed'), commands = list(embed = TRUE), features = list(pooled_embeddings_f32_gzip = TRUE), runtime = list(python = '3.12.0', pytorch = '2.8.0', cuda = '12.9', cuda_available = TRUE, gpu_count = 1L, driver = '575.51', transformers = '5.14.1', transformer_engine = '2.16.0', imports = list(torch = TRUE, transformers = TRUE, transformer_engine = TRUE), gpus = list(list(index = 0L, name = 'L40S', total_memory_bytes = 51539607552, compute_capability_major = ",
        as.integer(compute_capability_major),
        "L, compute_capability_minor = ",
        as.integer(compute_capability_minor),
        "L))))"
      ),
      "  cat(jsonlite::toJSON(report, auto_unbox = TRUE))",
      "} else if (identical(args[[1L]], 'embed')) {",
      "  log <- Sys.getenv('BIONEMOR_FAKE_LOG')",
      "  if (nzchar(log)) writeLines(args, log)",
      "  lines <- readLines(value('--input'), warn = FALSE)",
      "  headers <- which(startsWith(lines, '>'))",
      "  ids <- substring(lines[headers], 2L)",
      "  embeddings <- matrix(rep(as.double(seq_along(ids)), each = 320L), nrow = length(ids), byrow = TRUE)",
      "  write_pooled_embeddings(value('--output'), ids, embeddings, 'float32')",
      "} else stop('unsupported fake ESM-2 helper command')"
    )
  )
  invisible(bin)
}

make_mbridge_checkpoint <- function(
  workspace,
  name = "checkpoint",
  model_size = "evo2_7b",
  kind = "dense",
  base_checkpoint = NULL,
  vortex_style_fp8 = FALSE,
  transformer_engine = TRUE
) {
  path <- file.path(workspace, name)
  write_fake_dcp_weights(path)
  yaml12::write_yaml(
    list(
      model_size = model_size,
      kind = kind,
      model = list(
        vortex_style_fp8 = vortex_style_fp8,
        transformer_engine = transformer_engine
      ),
      checkpoint = list(pretrained_checkpoint = base_checkpoint)
    ),
    file.path(path, "run_config.yaml")
  )
  writeLines("complete", file.path(path, ".bionemor-complete"))
  path
}

fake_slurm_runtime <- function(bin) {
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  write_executable(
    file.path(bin, "sbatch"),
    c(
      "if [[ \"${1:-}\" == \"--parsable\" ]]; then",
      "  printf '123\\n'",
      "else",
      "  printf '123\\n'",
      "fi"
    )
  )
  write_executable(
    file.path(bin, "sacct"),
    c(
      "state=\"${BIONEMOR_FAKE_STATE:-COMPLETED}\"",
      "if [[ -n \"${BIONEMOR_FAKE_STATE_FILE:-}\" ]]; then",
      "  state=$(<\"$BIONEMOR_FAKE_STATE_FILE\")",
      "fi",
      "printf '123|%s|%s\\n' \"$state\" \"${BIONEMOR_FAKE_EXIT:-0:0}\""
    )
  )
  write_executable(
    file.path(bin, "scancel"),
    c(
      "printf '%s\\n' \"$@\" > \"${BIONEMOR_CANCEL_ARGS:-/dev/null}\"",
      "if [[ -n \"${BIONEMOR_FAKE_STATE_FILE:-}\" ]]; then",
      "  printf 'CANCELLED\\n' > \"$BIONEMOR_FAKE_STATE_FILE\"",
      "fi"
    )
  )
  invisible(bin)
}

fake_container_runtime <- function(bin, engine = "docker") {
  helper <- system.file(
    "scripts",
    "materialize-evo2.py",
    package = "bionemor"
  )
  if (!nzchar(helper)) {
    helper <- testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      "materialize-evo2.py"
    )
  }
  helper_revision <- trimws(
    processx::run(
      "git",
      c("hash-object", helper)
    )$stdout
  )
  recipe <- evo2_recipe()
  labels <- jsonlite::toJSON(
    as.list(c(
      "org.opencontainers.image.source" = recipe@repository,
      "org.opencontainers.image.revision" = recipe@revision,
      "org.opencontainers.image.version" = paste0(
        "evo2-recipe-",
        recipe@recipe_version
      ),
      "io.bionemor.helper.revision" = helper_revision,
      "io.bionemor.base.image" = recipe@base_image,
      "io.bionemor.base.digest" = recipe@base_image_digest,
      "io.bionemor.bridge.protocol" = as.character(recipe@bridge_protocol)
    )),
    auto_unbox = TRUE
  )
  write_executable(
    file.path(bin, engine),
    c(
      paste0(
        "printf '%s\\n' '",
        engine,
        "' \"$@\" > \"$BIONEMOR_CONTAINER_LOG\""
      ),
      "if [[ \"${1:-}\" == \"image\" && \"${2:-}\" == \"inspect\" ]]; then",
      "  if [[ \"${3:-}\" != \"--format\" ]]; then printf '{}\\n'; exit 0; fi",
      "  case \"${4:-}\" in",
      paste0(
        "    \"{{.Id}}\") printf '%s\\n' 'sha256:",
        strrep("c", 64L),
        "' ;;"
      ),
      paste0(
        "    \"{{json .Config.Labels}}\") printf '%s\\n' ",
        shQuote(labels),
        " ;;"
      ),
      paste0(
        "    \"{{json .RepoDigests}}\") printf '%s\\n' ",
        shQuote(jsonlite::toJSON(
          paste0(recipe@base_image, "@", recipe@base_image_digest),
          auto_unbox = TRUE
        )),
        " ;;"
      ),
      "    *) exit 2 ;;",
      "  esac",
      "  exit 0",
      "fi",
      "if [[ \"$1\" != \"run\" ]]; then exit 2; fi",
      "shift",
      "entrypoint=\"\"",
      "while [[ $# -gt 0 ]]; do",
      "  case \"$1\" in",
      "    --rm|--ipc=host) shift ;;",
      "    --gpus|--user|--name|-v|-w|-e) shift 2 ;;",
      "    --entrypoint) entrypoint=\"$2\"; shift 2 ;;",
      "    *) shift",
      "       if [[ -n \"$entrypoint\" && \"${1:-}\" == \"--help\" ]]; then printf 'help\\n'; exit 0; fi",
      "       if [[ -n \"$entrypoint\" ]]; then exec \"$entrypoint\" \"$@\"; fi",
      "       if [[ \"${2:-}\" == \"--help\" ]]; then printf 'help\\n'; exit 0; fi",
      "       exec \"$@\" ;;",
      "  esac",
      "done",
      "exit 2"
    )
  )
  invisible(bin)
}

fake_bionemo_runtime <- function(bin) {
  dir.create(bin, recursive = TRUE)
  write_executable(
    file.path(bin, "preprocess_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'preprocess help\\n'; exit 0; fi",
      "exit 2"
    )
  )
  write_executable(
    file.path(bin, "train_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'train help\\n'; exit 0; fi",
      "exit 2"
    )
  )
  write_executable(
    file.path(bin, "predict_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'predict help\\n'; exit 0; fi",
      "exit 2"
    )
  )
  write_executable(
    file.path(bin, "infer_evo2"),
    c(
      "if [[ \"${1:-}\" == \"--help\" ]]; then printf 'infer help\\n'; exit 0; fi",
      "exit 2"
    )
  )
  write_executable(
    file.path(bin, "python"),
    c(
      "if [[ \"${1:-}\" == \"--version\" ]]; then",
      "  printf 'Python 3.12.0\\n'",
      "  exit 0",
      "fi",
      "output=\"${@: -1}\"",
      "printf '{\"sequence_indices\":[0,1],\"scores\":[-1.25,-2.5]}\\n' > \"$output\""
    )
  )
  write_executable(
    file.path(bin, "nvidia-smi"),
    "printf 'Fake GPU, 49152 MiB, 555.1\\n'"
  )
  invisible(bin)
}
