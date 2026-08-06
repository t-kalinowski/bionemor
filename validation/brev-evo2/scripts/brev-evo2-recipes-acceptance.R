library(bionemor)

format_utc <- function(x) {
  base::format(x, tz = "UTC", usetz = TRUE)
}

timed_value <- function(value) {
  started <- Sys.time()
  value <- force(value)
  ended <- Sys.time()
  list(
    value = value,
    timing = list(
      started_at = format_utc(started),
      ended_at = format_utc(ended),
      duration_seconds = as.double(difftime(ended, started, units = "secs"))
    )
  )
}

timed_job <- function(job, timeout = 3600) {
  started <- Sys.time()
  job <- force(job)
  value <- job_wait(job, poll = 2, timeout = timeout)
  ended <- Sys.time()
  list(
    value = value,
    run_path = job_path(job),
    timing = list(
      started_at = format_utc(started),
      ended_at = format_utc(ended),
      duration_seconds = as.double(difftime(ended, started, units = "secs"))
    )
  )
}

read_terminal_manifest <- function(run_path) {
  path <- file.path(run_path, "manifest.json")
  stopifnot("terminal run manifest is missing" = file.exists(path))
  manifest <- jsonlite::read_json(path, simplifyVector = FALSE)
  stopifnot(
    "run manifest is not successful" = identical(manifest$state, "succeeded"),
    "run manifest has a nonzero exit status" = as.integer(
      manifest$exit_status
    ) ==
      0L
  )
  manifest
}

portable_character <- function(x, workspace, package_source) {
  replacements <- c(
    workspace = "$WORKSPACE",
    package_source = "$PACKAGE_SOURCE",
    remote_home = "$REMOTE_HOME",
    runtime = "$RUNTIME",
    container_root = "$CONTAINER_ROOT"
  )
  names(replacements) <- c(
    workspace,
    package_source,
    "/home/ubuntu",
    "/workspace/bionemo",
    "/root"
  )
  order <- order(nchar(names(replacements)), decreasing = TRUE)
  replacements <- replacements[order]
  for (source in names(replacements)) {
    x <- gsub(source, replacements[[source]], x, fixed = TRUE)
  }
  vapply(
    x,
    function(value) {
      if (startsWith(value, "/")) {
        paste0("$ABSOLUTE_PATH/", basename(value))
      } else {
        value
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

sensitive_field <- function(name) {
  grepl(
    "(^|_)(api_?key|credential|secret|password|authorization|auth_token)(_|$)",
    tolower(name)
  )
}

portable_record <- function(x, workspace, package_source, field = "") {
  if (nzchar(field) && sensitive_field(field)) {
    return("<redacted>")
  }
  if (is.list(x)) {
    fields <- names(x)
    if (is.null(fields)) {
      return(lapply(
        x,
        portable_record,
        workspace = workspace,
        package_source = package_source
      ))
    }
    return(
      Map(
        function(value, name) {
          portable_record(
            value,
            workspace = workspace,
            package_source = package_source,
            field = name
          )
        },
        x,
        fields
      ) |>
        stats::setNames(fields)
    )
  }
  if (is.character(x)) {
    return(portable_character(x, workspace, package_source))
  }
  x
}

write_json <- function(x, path) {
  jsonlite::write_json(
    x,
    path,
    auto_unbox = TRUE,
    dataframe = "rows",
    matrix = "rowmajor",
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE
  )
}

embedding_summary <- function(x) {
  stopifnot(
    "embedding result must be a matrix" = is.matrix(x),
    "embedding result must be finite" = all(is.finite(x))
  )
  retained <- seq_len(min(8L, ncol(x)))
  list(
    shape = unname(dim(x)),
    row_names = rownames(x),
    retained_columns = colnames(x)[retained],
    retained_values = unname(x[, retained, drop = FALSE]),
    row_l2_norm = unname(sqrt(rowSums(x * x)))
  )
}

gpu_names <- function(gpus) {
  if (is.data.frame(gpus)) {
    return(as.character(gpus$name))
  }
  vapply(gpus, function(gpu) as.character(gpu$name), character(1))
}

capture_evidence <- function(context) {
  staging <- paste0(context$evidence_path, ".tmp-", Sys.getpid())
  stopifnot(
    "evidence destination already exists" = !file.exists(
      context$evidence_path
    ),
    "evidence staging destination already exists" = !file.exists(staging)
  )
  dir.create(
    file.path(staging, "outputs"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(staging, "manifests"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

  dense_output <- list(
    scores = as.data.frame(context$dense_score),
    generation = as.data.frame(context$dense_generation),
    embedding = embedding_summary(context$dense_embedding)
  )
  fitted_output <- list(
    scores = as.data.frame(context$fitted_score),
    generation = as.data.frame(context$fitted_generation)
  )
  write_json(dense_output, file.path(staging, "outputs/dense.json"))
  write_json(fitted_output, file.path(staging, "outputs/fitted.json"))

  manifest_files <- c(
    dense_score = "manifests/dense-score.json",
    dense_generation = "manifests/dense-generation.json",
    dense_embedding = "manifests/dense-embedding.json",
    preprocess = "manifests/preprocess.json",
    fine_tune = "manifests/fine-tune.json",
    fitted_score = "manifests/fitted-score.json",
    fitted_generation = "manifests/fitted-generation.json"
  )
  for (name in names(manifest_files)) {
    manifest <- portable_record(
      context$run_manifests[[name]],
      workspace = context$workspace,
      package_source = context$package_source
    )
    write_json(manifest, file.path(staging, manifest_files[[name]]))
  }
  write_json(
    portable_record(
      context$lora_manifest,
      workspace = context$workspace,
      package_source = context$package_source
    ),
    file.path(staging, "lora-inspection.json")
  )

  artifact_files <- c(
    "outputs/dense.json",
    "outputs/fitted.json",
    unname(manifest_files),
    "lora-inspection.json"
  )
  evidence <- list(
    schema_version = 1L,
    kind = "brev-evo2-acceptance",
    status = "succeeded",
    captured_at = format_utc(Sys.time()),
    capture_date = context$capture_date,
    package = list(
      name = "bionemor",
      version = as.character(
        utils::packageDescription("bionemor", fields = "Version")
      ),
      source_revision = context$package_revision,
      source_dirty = context$package_dirty
    ),
    recipe = context$recipe,
    runtime = context$runtime,
    workload = list(
      model = "7b",
      checkpoint = "uploaded MBridge checkpoint",
      training_sequences = list(
        train = 4L,
        validation = 1L,
        test = 1L,
        bases_per_sequence = 127L,
        tokens_per_sample = 128L
      ),
      fine_tuning = list(
        method = "lora",
        rank = 4L,
        steps = 2L,
        precision = "bf16"
      ),
      generation_tokens = 8L
    ),
    operations = context$timings,
    assertions = list(
      terminal_manifests_succeeded = TRUE,
      scores_finite = TRUE,
      result_schemas_stable = TRUE,
      generation_returned_eight_tokens = TRUE,
      lora_checkpoint_retained = TRUE,
      lora_checkpoint_links_dense_base = TRUE
    ),
    artifacts = artifact_files,
    limitations = c(
      "One mechanical end-to-end acceptance run on synthetic DNA sequences.",
      "Not a performance benchmark or a biological-quality evaluation.",
      "Two optimizer steps are not expected to improve model quality.",
      "No Slurm cluster was exercised."
    )
  )
  write_json(evidence, file.path(staging, "evidence.json"))

  runtime_gpus <- gpu_names(context$runtime$gpus)
  timing_lines <- vapply(
    names(context$timings),
    function(name) {
      sprintf(
        "- `%s`: %.3f seconds",
        gsub("_", "-", name, fixed = TRUE),
        context$timings[[name]]$duration_seconds
      )
    },
    character(1)
  )
  writeLines(
    c(
      paste("# Brev Evo 2 acceptance:", context$capture_date),
      "",
      "This directory records one mechanical end-to-end acceptance run on",
      "deterministic synthetic sequences. It is not a benchmark or a",
      "biological-quality evaluation.",
      "",
      paste("- Package revision:", context$package_revision),
      paste("- Package source dirty:", context$package_dirty),
      paste("- Recipe revision:", context$recipe$revision),
      paste("- Derived image ID:", context$runtime$image_id),
      paste("- GPU:", paste(runtime_gpus, collapse = ", ")),
      paste("- NVIDIA driver:", context$runtime$driver),
      "",
      "## Elapsed time",
      "",
      timing_lines,
      "",
      "## Contents",
      "",
      "- `evidence.json` contains the full-precision evidence record.",
      "- `outputs/` contains compact portable dense and fitted results.",
      "- `manifests/` contains redacted terminal run manifests.",
      "- `lora-inspection.json` records the retained two-step LoRA checkpoint.",
      "",
      "The run used an uploaded MBridge checkpoint. It did not test checkpoint",
      "conversion. Absolute remote paths and credential-shaped fields were",
      "removed from the committed records."
    ),
    file.path(staging, "README.md")
  )

  persisted <- list.files(staging, recursive = TRUE, full.names = TRUE)
  persisted <- persisted[!dir.exists(persisted)]
  contents <- vapply(
    persisted,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  secrets <- Sys.getenv(
    c(
      "NGC_API_KEY",
      "NGC_CLI_API_KEY",
      "HF_TOKEN",
      "HUGGING_FACE_HUB_TOKEN"
    ),
    unset = ""
  )
  secrets <- unique(secrets[nzchar(secrets)])
  stopifnot(
    "evidence contains a remote workspace path" = !any(grepl(
      context$workspace,
      contents,
      fixed = TRUE
    )),
    "evidence contains a remote package path" = !any(grepl(
      context$package_source,
      contents,
      fixed = TRUE
    )),
    "evidence contains a credential" = !any(vapply(
      secrets,
      function(secret) any(grepl(secret, contents, fixed = TRUE)),
      logical(1)
    ))
  )

  stopifnot(
    "failed to publish the evidence directory" = file.rename(
      staging,
      context$evidence_path
    )
  )
  cat("Brev Evo 2 acceptance evidence:", context$evidence_path, "\n")
}

workspace <- Sys.getenv(
  "BIONEMOR_EVO2_WORKSPACE",
  "/home/ubuntu/workspace/bionemor"
)
image <- Sys.getenv("BIONEMOR_EVO2_IMAGE")
checkpoint_path <- Sys.getenv("BIONEMOR_EVO2_CHECKPOINT")
evidence_path <- Sys.getenv("BIONEMOR_EVO2_EVIDENCE")
capture_date <- Sys.getenv("BIONEMOR_EVO2_CAPTURE_DATE")
package_revision <- Sys.getenv("BIONEMOR_PACKAGE_REVISION")
package_dirty <- Sys.getenv("BIONEMOR_PACKAGE_DIRTY")
package_source <- Sys.getenv("BIONEMOR_PACKAGE_SOURCE")
stopifnot(
  "BIONEMOR_EVO2_IMAGE is required" = nzchar(image),
  "BIONEMOR_EVO2_IMAGE must be an immutable image ID" = grepl(
    "^sha256:[0-9a-f]{64}$",
    image
  ),
  "BIONEMOR_EVO2_CHECKPOINT is required" = nzchar(checkpoint_path),
  "BIONEMOR_EVO2_WORKSPACE must exist" = dir.exists(workspace),
  "BIONEMOR_EVO2_CHECKPOINT must be an existing directory" = dir.exists(
    checkpoint_path
  ),
  "BIONEMOR_EVO2_EVIDENCE is required" = nzchar(evidence_path),
  "BIONEMOR_EVO2_CAPTURE_DATE must use YYYY-MM-DD" = grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
    capture_date
  ),
  "BIONEMOR_PACKAGE_REVISION must be a full commit SHA" = grepl(
    "^[0-9a-f]{40}$",
    package_revision
  ),
  "BIONEMOR_PACKAGE_DIRTY must be true or false" = package_dirty %in%
    c(
      "true",
      "false"
    ),
  "BIONEMOR_PACKAGE_SOURCE must be an existing directory" = dir.exists(
    package_source
  )
)

workspace <- normalizePath(workspace, mustWork = TRUE)
checkpoint_path <- normalizePath(checkpoint_path, mustWork = TRUE)
package_source <- normalizePath(package_source, mustWork = TRUE)
evidence_path <- normalizePath(evidence_path, mustWork = FALSE)
stopifnot(
  "BIONEMOR_EVO2_CHECKPOINT must be inside BIONEMOR_EVO2_WORKSPACE" = startsWith(
    checkpoint_path,
    paste0(workspace, .Platform$file.sep)
  ),
  "BIONEMOR_EVO2_EVIDENCE must be inside BIONEMOR_EVO2_WORKSPACE" = startsWith(
    evidence_path,
    paste0(workspace, .Platform$file.sep)
  ),
  "BIONEMOR_EVO2_EVIDENCE must end in the capture date" = identical(
    basename(evidence_path),
    capture_date
  ),
  "BIONEMOR_EVO2_EVIDENCE already exists" = !file.exists(evidence_path)
)

compute <- bionemo_compute(
  recipe = evo2_recipe(),
  backend = "local",
  engine = "container",
  image = image,
  workspace = workspace
)
compute <- bionemo_install(compute, pull = FALSE)
capabilities <- bionemo_capabilities(compute, refresh = TRUE)
model_step <- timed_value(evo2("7b", checkpoint = checkpoint_path))
model <- model_step$value

sequence_127 <- function(pattern) {
  stopifnot(
    is.character(pattern),
    length(pattern) == 1L,
    !is.na(pattern),
    nzchar(pattern)
  )
  substr(
    strrep(pattern, ceiling(127 / nchar(pattern))),
    1L,
    127L
  )
}

train <- c(
  train_1 = sequence_127("ACGT"),
  train_2 = sequence_127("TGCA"),
  train_3 = sequence_127("GATTACA"),
  train_4 = sequence_127("CCGTA")
)
validation <- c(validation_1 = sequence_127("AGCT"))
test <- c(test_1 = sequence_127("TGCAT"))
inference_sequences <- c(
  first = sequence_127("ACGTGCAA"),
  second = sequence_127("GCTATGCA")
)
stopifnot(all(nchar(c(train, validation, test, inference_sequences)) == 127L))

dense_score_step <- timed_job(evo2_score(
  model,
  inference_sequences,
  compute,
  reduction = "mean",
  strand = "forward",
  name = "brev-acceptance-dense-score",
  async = TRUE
))
dense_generation_step <- timed_job(evo2_generate(
  model,
  inference_sequences,
  compute,
  num_tokens = 8L,
  seed = 17L,
  return_probabilities = TRUE,
  name = "brev-acceptance-dense-generation",
  async = TRUE
))
dense_embedding_step <- timed_job(evo2_embed(
  model,
  inference_sequences,
  compute,
  pool = "mean",
  strand = "forward",
  name = "brev-acceptance-dense-embedding",
  async = TRUE
))

data <- evo2_dataset(
  train = train,
  validation = validation,
  test = test
)
preprocess_step <- timed_job(evo2_preprocess(
  data,
  model,
  compute,
  path = "datasets/brev-evo2-acceptance-128",
  control = evo2_preprocess_control(
    append_eod = TRUE,
    sample_length = 128L,
    workers = 1L,
    seed = 17L
  ),
  async = TRUE
))
fine_tune_step <- timed_job(
  evo2_finetune(
    model,
    preprocess_step$value,
    compute,
    steps = 2L,
    method = evo2_lora(
      rank = 4L,
      alpha = 8,
      dropout = 0
    ),
    control = evo2_fit_control(
      sequence_length = 128L,
      global_batch_size = 1L,
      micro_batch_size = 1L,
      learning_rate = 1e-4,
      minimum_learning_rate = 0,
      warmup_steps = 0L,
      decay_steps = 2L,
      constant_steps = 0L,
      eval_interval = 1L,
      eval_iters = 1L,
      log_interval = 1L,
      precision = "bf16",
      keep_checkpoints = 1L,
      workers = 1L,
      seed = 17L,
      dataset_seed = 17L
    ),
    path = "artifacts/brev-evo2-acceptance",
    name = "brev-evo2-lora",
    async = TRUE,
    timeout = 3600
  ),
  timeout = 3600
)
fitted <- fine_tune_step$value
fitted_score_step <- timed_job(evo2_score(
  fitted,
  inference_sequences["first"],
  compute,
  reduction = "mean",
  strand = "forward",
  name = "brev-acceptance-fitted-score",
  async = TRUE
))
fitted_generation_step <- timed_job(evo2_generate(
  fitted,
  inference_sequences["first"],
  compute,
  num_tokens = 8L,
  seed = 17L,
  return_probabilities = TRUE,
  name = "brev-acceptance-fitted-generation",
  async = TRUE
))

dense_score <- dense_score_step$value
dense_generation <- dense_generation_step$value
dense_embedding <- dense_embedding_step$value
fitted_score <- fitted_score_step$value
fitted_generation <- fitted_generation_step$value
score_schema <- c(
  "id",
  "sequence_length",
  "tokens_scored",
  "score",
  "forward_score",
  "reverse_score",
  "reduction",
  "strand"
)
generation_schema <- c(
  "id",
  "input_id",
  "sample",
  "prompt",
  "completion",
  "sequence",
  "finish_reason",
  "prompt_tokens",
  "generated_tokens",
  "total_tokens",
  "log_probabilities",
  "probabilities",
  "generated_bases",
  "gc_fraction",
  "ambiguous_fraction",
  "longest_homopolymer",
  "validation_warnings"
)
stopifnot(
  "dense score schema changed" = identical(names(dense_score), score_schema),
  "fitted score schema changed" = identical(names(fitted_score), score_schema),
  "dense generation schema changed" = identical(
    names(dense_generation),
    generation_schema
  ),
  "fitted generation schema changed" = identical(
    names(fitted_generation),
    generation_schema
  ),
  "dense scoring returned the wrong row count" = nrow(dense_score) ==
    length(inference_sequences),
  "fitted scoring returned the wrong row count" = nrow(fitted_score) == 1L,
  "dense scoring returned non-finite values" = all(is.finite(
    dense_score$score
  )),
  "fitted scoring returned non-finite values" = all(is.finite(
    fitted_score$score
  )),
  "dense generation returned the wrong row count" = nrow(dense_generation) ==
    length(inference_sequences),
  "fitted generation returned the wrong row count" = nrow(
    fitted_generation
  ) ==
    1L,
  "dense generation did not return eight tokens" = all(
    dense_generation$generated_tokens == 8L
  ),
  "fitted generation did not return eight tokens" = all(
    fitted_generation$generated_tokens == 8L
  ),
  "dense generation returned empty completions" = all(nzchar(
    dense_generation$completion
  )),
  "fitted generation returned an empty completion" = all(nzchar(
    fitted_generation$completion
  )),
  "dense embedding returned the wrong row count" = nrow(dense_embedding) ==
    length(inference_sequences),
  "dense embedding row names changed" = identical(
    rownames(dense_embedding),
    names(inference_sequences)
  ),
  "dense embedding returned non-finite values" = all(is.finite(
    dense_embedding
  ))
)

lora_manifest <- checkpoint_manifest(fitted)
dense_checkpoint_root <- normalizePath(
  checkpoint_path(model),
  mustWork = TRUE
)
lora_base_path <- normalizePath(
  lora_manifest$base_checkpoint_path,
  mustWork = TRUE
)
stopifnot(
  "fitted checkpoint is not LoRA" = identical(lora_manifest$kind, "lora"),
  "fitted checkpoint did not retain two steps" = as.integer(
    lora_manifest$provenance$steps
  ) ==
    2L,
  "fitted checkpoint is missing its dense base" = dir.exists(
    lora_manifest$base_checkpoint_path
  ),
  "fitted checkpoint does not link the dense base" = identical(
    lora_base_path,
    dense_checkpoint_root
  ) ||
    startsWith(
      lora_base_path,
      paste0(dense_checkpoint_root, .Platform$file.sep)
    )
)

run_steps <- list(
  dense_score = dense_score_step,
  dense_generation = dense_generation_step,
  dense_embedding = dense_embedding_step,
  preprocess = preprocess_step,
  fine_tune = fine_tune_step,
  fitted_score = fitted_score_step,
  fitted_generation = fitted_generation_step
)
run_manifests <- lapply(run_steps, function(step) {
  read_terminal_manifest(step$run_path)
})
timings <- lapply(run_steps, `[[`, "timing")
timings$model_attach <- model_step$timing

lock <- jsonlite::read_json(
  system.file("recipes", "evo2.json", package = "bionemor", mustWork = TRUE),
  simplifyVector = FALSE
)
runtime <- capabilities$runtime
stopifnot(
  "runtime did not report GPU details" = length(runtime$gpus) > 0L,
  "runtime did not report the NVIDIA driver" = is.character(runtime$driver) &&
    length(runtime$driver) == 1L &&
    nzchar(runtime$driver)
)
capture_evidence(list(
  evidence_path = evidence_path,
  capture_date = capture_date,
  workspace = workspace,
  package_source = package_source,
  package_revision = package_revision,
  package_dirty = identical(package_dirty, "true"),
  recipe = list(
    repository = lock$repository,
    revision = lock$revision,
    recipe_version = lock$recipe_version,
    dockerfile_blob = lock$dockerfile_blob,
    base_image = lock$base_image,
    base_image_digest = lock$base_image_digest
  ),
  runtime = list(
    image_id = image,
    driver = runtime$driver,
    cuda = runtime$cuda,
    pytorch = runtime$pytorch,
    megatron_bridge = runtime$megatron_bridge,
    megatron_core = runtime$megatron_core,
    transformer_engine = runtime$transformer_engine,
    gpus = runtime$gpus
  ),
  dense_score = dense_score,
  dense_generation = dense_generation,
  dense_embedding = dense_embedding,
  fitted_score = fitted_score,
  fitted_generation = fitted_generation,
  run_manifests = run_manifests,
  lora_manifest = lora_manifest,
  timings = timings
))
