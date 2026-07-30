test_that("the recipe lock drives compute and installation planning", {
  workspace <- tempfile("bionemor-recipe-")
  recipe <- evo2_recipe()

  expect_s3_class(recipe, "bionemor::BioNeMoRecipe")
  expect_equal(
    recipe@repository,
    "https://github.com/NVIDIA-BioNeMo/bionemo-recipes"
  )
  expect_equal(
    recipe@revision,
    "e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e"
  )
  expect_equal(recipe@recipe_version, "2.4")
  expect_equal(recipe@subdirectory, "recipes/evo2_megatron")
  expect_equal(recipe@base_image, "nvcr.io/nvidia/pytorch:26.06-py3")
  expect_equal(
    recipe@base_image_digest,
    "sha256:abd110b23600e877173dafc3078385b7c13ddacd7e0c6a6acb0a864586d59622"
  )
  expect_true(recipe@verified)

  compute <- bionemo_compute(workspace = workspace)
  expect_true(dir.exists(workspace))
  expect_equal(compute@engine, "container")
  expect_identical(compute@recipe, recipe)
  expect_error(compute@profile)

  expect_equal(
    bionemo_compute(engine = "external", workspace = tempfile())@engine,
    "external"
  )
  without_container_runtime <- withr::with_envvar(
    c(PATH = ""),
    bionemo_compute(
      workspace = tempfile(),
      config = list(container_engine = "missing-container-runtime")
    )
  )
  expect_equal(without_container_runtime@engine, "container")
  expect_error(
    bionemo_compute(engine = "embedded", workspace = tempfile()),
    "external"
  )

  plan <- bionemo_install_plan(compute)
  expect_s3_class(plan, "bionemor::BioNeMoSetupPlan")
  serialized <- jsonlite::toJSON(plan@steps, auto_unbox = TRUE)
  expect_match(serialized, recipe@revision, fixed = TRUE)
  expect_match(serialized, "recipes/evo2_megatron/Dockerfile", fixed = TRUE)
  expect_match(serialized, recipe@base_image, fixed = TRUE)
  expect_match(serialized, recipe@base_image_digest, fixed = TRUE)
  expect_false(grepl("<resolved-at-install>", serialized, fixed = TRUE))
  expect_match(
    serialized,
    "BIONEMOR_HELPER_REVISION=[0-9a-f]{40}"
  )
  expect_match(serialized, "\"run\"", fixed = TRUE)
  expect_match(serialized, "\"--gpus\"", fixed = TRUE)
  expect_match(serialized, "bionemor-evo2-helper", fixed = TRUE)
  for (command in c(
    "infer_evo2",
    "predict_evo2",
    "preprocess_evo2",
    "train_evo2",
    "evo2_convert_savanna_to_mbridge",
    "evo2_convert_nemo2_to_mbridge",
    "evo2_export_mbridge_to_vortex",
    "evo2_remove_optimizer"
  )) {
    expect_match(serialized, command, fixed = TRUE)
  }
})

test_that("a Savanna checkpoint is converted once and registered as MBridge", {
  workspace <- tempfile("bionemor-checkpoint-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace
  )

  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    source = "hf://arcinstitute/savanna_evo2_7b",
    format = "savanna",
    path = "checkpoints/evo2-7b",
    compute = compute,
    revision = "9e69aeeaacf4d11fdbabfa73da65a770e5031f02"
  )

  expect_s3_class(checkpoint, "bionemor::BioNeMoCheckpoint")
  expect_equal(checkpoint@format, "mbridge")
  expect_true(file.exists(file.path(checkpoint_path(checkpoint), "run_config.yaml")))
  args <- readLines(log)
  expect_equal(sum(args == "convert"), 1L)
  expect_true(all(c(
    "--savanna-ckpt-path",
    "arcinstitute/savanna_evo2_7b",
    "--mbridge-ckpt-dir",
    checkpoint_path(checkpoint),
    "--model-size",
    "evo2_7b",
    "--seq-length",
    "1048576",
    "--revision",
    "9e69aeeaacf4d11fdbabfa73da65a770e5031f02"
  ) %in% args))

  manifest <- checkpoint_manifest(checkpoint)
  expect_equal(manifest$source, "hf://arcinstitute/savanna_evo2_7b")
  expect_equal(
    manifest$source_revision,
    "9e69aeeaacf4d11fdbabfa73da65a770e5031f02"
  )
  expect_equal(
    manifest$recipe_revision,
    "e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e"
  )
})

test_that("custom remote pickle-based checkpoints require explicit trust", {
  workspace <- tempfile("bionemor-custom-remote-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  revision <- "0123456789abcdef0123456789abcdef01234567"
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)

  expect_error(
    evo2_checkpoint(
      evo2("7b"),
      source = "hf://example/custom-evo2",
      format = "savanna",
      revision = revision,
      path = "checkpoints/untrusted",
      compute = compute
    ),
    "trust = TRUE is required for an unknown local or remote pickle-based checkpoint"
  )
  expect_false(file.exists(log))

  expect_error(
    evo2_checkpoint(
      evo2("7b"),
      source = "ngc://example/custom-evo2:1.0",
      format = "nemo2",
      revision = "1.0",
      path = "checkpoints/untrusted-nemo2",
      compute = compute
    ),
    "trust = TRUE is required for an unknown local or remote pickle-based checkpoint"
  )
  expect_false(file.exists(log))

  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    source = "hf://example/custom-evo2",
    format = "savanna",
    revision = revision,
    path = "checkpoints/trusted",
    compute = compute,
    trust = TRUE
  )
  expect_equal(checkpoint_manifest(checkpoint)$source_revision, revision)
})

test_that("model revision selects the recommended checkpoint revision", {
  workspace <- tempfile("bionemor-model-revision-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  revision <- "0123456789abcdef0123456789abcdef01234567"
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", revision = revision)

  expect_error(
    evo2_checkpoint(
      model,
      source = "recommended",
      revision = "main",
      path = "checkpoints/mutable-revision",
      compute = compute
    ),
    "Hugging Face revision must be a full commit SHA"
  )
  checkpoint <- evo2_checkpoint(
    model,
    source = "recommended",
    path = "checkpoints/custom-revision",
    compute = compute,
    trust = TRUE
  )

  expect_equal(checkpoint_manifest(checkpoint)$source_revision, revision)
  args <- readLines(log)
  revision_flag <- which(args == "--revision")
  expect_length(revision_flag, 1L)
  expect_equal(args[[revision_flag + 1L]], revision)
})

test_that("path registration inspects current nested MBridge configurations", {
  workspace <- tempfile("bionemor-nested-checkpoint-")
  dir.create(workspace)
  base <- make_mbridge_checkpoint(workspace, "base-checkpoint")
  tokenizer <- "/workspace/bionemo/tokenizers/nucleotide_fast_tokenizer_512"
  provider <- paste0(
    "bionemo.evo2.models.evo2_provider.",
    "Hyena7bARCLongContextModelProvider"
  )
  precision <- paste0(
    "megatron.bridge.training.mixed_precision.",
    "MixedPrecisionConfig"
  )
  config <- list(
    model = list(
      `_target_` = provider,
      num_layers = 32L,
      hidden_size = 4096L,
      seq_length = 1048576L
    ),
    tokenizer = list(tokenizer_model = tokenizer),
    mixed_precision = list(`_target_` = precision),
    peft = list(
      `_target_` = "bionemo.evo2.models.evo2_provider.Evo2LoRA"
    ),
    checkpoint = list(pretrained_checkpoint = base)
  )

  direct <- file.path(workspace, "direct-lora")
  write_fake_dcp_weights(direct)
  yaml::write_yaml(config, file.path(direct, "run_config.yaml"))
  model <- evo2("7b", checkpoint = direct)
  manifest <- checkpoint_manifest(model@checkpoint)

  expect_equal(model@checkpoint@kind, "lora")
  expect_equal(model@checkpoint@base_checkpoint, normalizePath(base))
  expect_equal(manifest$model_size, "evo2_7b")
  expect_equal(manifest$tokenizer, tokenizer)
  expect_equal(manifest$mixed_precision_recipe, precision)
  expect_equal(manifest$inspection$model_provider, provider)
  expect_equal(
    manifest$base_checkpoint_digest,
    bionemor:::path_digest(normalizePath(base))
  )

  root <- file.path(workspace, "training-root")
  iteration <- file.path(root, "iter_0000003")
  write_fake_dcp_weights(iteration)
  config$peft <- NULL
  config$checkpoint$pretrained_checkpoint <- NULL
  yaml::write_yaml(config, file.path(iteration, "run_config.yaml"))
  writeLines("3", file.path(root, "latest_checkpointed_iteration.txt"))
  reopened <- evo2("7b", checkpoint = root)
  reopened_manifest <- checkpoint_manifest(reopened@checkpoint)

  expect_equal(reopened@checkpoint@kind, "dense")
  expect_equal(
    reopened_manifest$inspection$resolved_path,
    normalizePath(iteration)
  )
})

test_that("inference uses the checkpoint iteration selected by its manifest", {
  workspace <- tempfile("bionemor-checkpoint-iteration-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  root <- file.path(workspace, "training-root")
  selected <- make_mbridge_checkpoint(
    workspace,
    "training-root/iter_0000003",
    vortex_style_fp8 = TRUE
  )
  make_mbridge_checkpoint(workspace, "training-root/iter_0000004")
  writeLines("3", file.path(root, "latest_checkpointed_iteration.txt"))
  model <- evo2("7b", checkpoint = root)
  registered <- checkpoint_manifest(model)
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )

  generated <- evo2_generate(model, "ACGT", compute, num_tokens = 4L)

  invocation <- readLines(log)
  checkpoint_flag <- which(invocation == "--ckpt-dir")
  expect_length(checkpoint_flag, 1L)
  expect_equal(
    invocation[[checkpoint_flag + 1L]],
    normalizePath(selected)
  )
  precision_flag <- which(invocation == "--mixed-precision-recipe")
  expect_length(precision_flag, 1L)
  expect_equal(
    invocation[[precision_flag + 1L]],
    registered$mixed_precision_recipe
  )
  expect_true("--vortex-style-fp8" %in% invocation)

  manifest <- jsonlite::read_json(
    file.path(attr(generated, "provenance")$run_path, "manifest.json"),
    simplifyVector = FALSE
  )
  expect_equal(manifest$checkpoint$path, normalizePath(root))
  expect_equal(manifest$checkpoint$source, registered$source)
  expect_equal(manifest$checkpoint$revision, registered$source_revision)
  expect_equal(
    manifest$precision$resolved_recipe,
    registered$mixed_precision_recipe
  )
})

test_that("external LoRA checkpoints require an explicit base iteration", {
  workspace <- tempfile("bionemor-lora-base-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  base <- file.path(workspace, "base-training-root")
  make_mbridge_checkpoint(workspace, "base-training-root/iter_0000003")
  make_mbridge_checkpoint(workspace, "base-training-root/iter_0000004")
  writeLines("3", file.path(base, "latest_checkpointed_iteration.txt"))
  lora <- make_mbridge_checkpoint(
    workspace,
    "external-lora",
    kind = "lora",
    base_checkpoint = base
  )
  model <- evo2("7b", checkpoint = lora)
  compute <- bionemo_compute(engine = "external", workspace = workspace)

  expect_error(
    evo2_generate(model, "ACGT", compute, num_tokens = 4L),
    "external LoRA base checkpoint must be an explicit checkpoint directory",
    class = "BN_BASE_CHECKPOINT_MISSING"
  )
  expect_false(file.exists(log))
})

test_that("generation batches prompts and returns portable R results", {
  workspace <- tempfile("bionemor-generation-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  generated <- evo2_generate(
    model,
    c(first = "ACGT", second = "TGCA"),
    compute,
    num_tokens = 4L,
    seed = 1L,
    name = "batched-generation"
  )

  expect_s3_class(generated, "evo2_generation")
  expect_s3_class(generated, "data.frame")
  expect_false(inherits(generated, "bionemor::BioNeMoPrediction"))
  expect_equal(generated$id, c("first::1", "second::1"))
  expect_named(generated, c(
    "id", "input_id", "sample", "prompt", "completion", "sequence",
    "finish_reason", "prompt_tokens", "generated_tokens", "total_tokens",
    "log_probabilities", "probabilities", "generated_bases", "gc_fraction",
    "ambiguous_fraction", "longest_homopolymer", "validation_warnings"
  ))

  invocations <- readLines(log)
  expect_equal(sum(invocations == "infer_evo2"), 1L)
  expect_true("--prompt-file" %in% invocations)
  expect_false("--prompt" %in% invocations)
  run_path <- attr(generated, "provenance")$run_path
  prompts <- jsonlite::stream_in(
    file(file.path(run_path, "inputs", "prompts.jsonl")),
    verbose = FALSE
  )
  expect_equal(prompts$id, generated$id)
  expect_true(file.exists(file.path(run_path, "outputs", "generation.jsonl")))
  expect_true(file.exists(file.path(run_path, "outputs", "generated.fasta")))

  before <- length(readLines(log))
  expect_error(
    evo2_generate(
      model,
      "ACGT",
      compute,
      top_k = 3L,
      top_p = 0.5
    ),
    "at most one"
  )
  expect_length(readLines(log), before)
  expect_error(
    evo2_generate(model, "ACGT", compute, n = 2L),
    "n must equal 1"
  )
  expect_length(readLines(log), before)
  expect_error(
    evo2_generate(model, "ACGT", compute, seed = 0L),
    "seed must be NULL or a positive integer"
  )
  expect_length(readLines(log), before)
  expect_error(
    evo2_generate(
      model,
      "ACGT",
      compute,
      control = evo2_inference_control(micro_batch_size = 2L)
    ),
    "use max_batch_size for generation"
  )
  expect_length(readLines(log), before)
})

test_that("inference rejects checkpoints prepared by another recipe", {
  workspace <- tempfile("bionemor-recipe-mismatch-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(workspace)
  model <- evo2("7b", checkpoint = checkpoint)
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    recipe = evo2_recipe(
      revision = "0123456789abcdef0123456789abcdef01234567"
    )
  )

  expect_error(
    evo2_generate(model, "ACGT", compute, num_tokens = 4L),
    "checkpoint recipe revision does not match the compute recipe",
    class = "BN_RECIPE_MISMATCH"
  )
  expect_false(file.exists(log))
})

test_that("explicit missing or empty sequence IDs are rejected", {
  workspace <- tempfile("bionemor-sequence-ids-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  expect_error(
    evo2_generate(
      model,
      stats::setNames("ACGT", ""),
      compute,
      num_tokens = 4L
    ),
    "sequence IDs must not be missing or empty",
    class = "BN_INVALID_SEQUENCE"
  )
  expect_error(
    evo2_score(
      model,
      data.frame(id = NA_character_, sequence = "ACGT"),
      compute
    ),
    "sequence IDs must not be missing or empty",
    class = "BN_INVALID_SEQUENCE"
  )
  expect_false(file.exists(log))
})

test_that("generation validates declared length and reports low complexity", {
  workspace <- tempfile("bionemor-generation-validation-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  withr::local_envvar(
    BIONEMOR_FAKE_GENERATED_TOKENS = "5"
  )
  expect_error(
    evo2_generate(model, "ACGT", compute, num_tokens = 4L),
    "generated token count exceeds the request",
    class = "BN_OUTPUT_SCHEMA"
  )

  withr::local_envvar(
    BIONEMOR_FAKE_GENERATED_TOKENS = "4",
    BIONEMOR_FAKE_LOG_PROBABILITY = "0.1"
  )
  for (validate in c("basic", "strict", "none")) {
    expect_error(
      evo2_generate(
        model,
        "ACGT",
        compute,
        num_tokens = 4L,
        validate = validate
      ),
      "log probabilities must be finite and non-positive",
      class = "BN_NONFINITE_OUTPUT"
    )
  }

  withr::local_envvar(BIONEMOR_FAKE_LOG_PROBABILITY = "Inf")
  for (validate in c("basic", "strict", "none")) {
    expect_error(
      evo2_generate(
        model,
        "ACGT",
        compute,
        num_tokens = 4L,
        validate = validate
      ),
      "log probabilities must be finite and non-positive",
      class = "BN_NONFINITE_OUTPUT"
    )
  }

  withr::local_envvar(
    BIONEMOR_FAKE_COMPLETION = "ACNT",
    BIONEMOR_FAKE_LOG_PROBABILITY = as.character(log(0.25))
  )
  expect_error(
    evo2_generate(
      model,
      "ACGT",
      compute,
      num_tokens = 4L,
      validate = "strict"
    ),
    "non-ACGT",
    class = "BN_INVALID_SEQUENCE"
  )

  withr::local_envvar(
    BIONEMOR_FAKE_COMPLETION = "ACACACAC",
    BIONEMOR_FAKE_GENERATED_TOKENS = "8"
  )
  generated <- evo2_generate(model, "ACGT", compute, num_tokens = 8L)
  expect_true(
    "completion has low complexity" %in% generated$validation_warnings[[1L]]
  )
})

test_that("scoring returns ordered portable results and predict delegates", {
  workspace <- tempfile("bionemor-score-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  sequences <- c(first = "ACGT", second = "TGCA")

  scores <- evo2_score(model, sequences, compute, reduction = "mean")

  expect_s3_class(scores, "evo2_scores")
  expect_equal(scores$id, names(sequences))
  expect_named(scores, c(
    "id", "sequence_length", "tokens_scored", "score", "forward_score",
    "reverse_score", "reduction", "strand"
  ))
  invocations <- readLines(log)
  expect_equal(sum(invocations == "predict_evo2"), 1L)
  expect_true(all(c(
    "--output-log-prob-seqs",
    "--log-prob-collapse-option",
    "per_token"
  ) %in% invocations))
  plan <- jsonlite::read_json(
    file.path(attr(scores, "provenance")$run_path, "plan.json"),
    simplifyVector = TRUE
  )
  tokens <- unlist(plan$steps, use.names = FALSE)
  expect_true(all(c(
    "materialize-predictions",
    "--input",
    "--reduction",
    "mean"
  ) %in% tokens))
  expect_equal(sum(tokens == "materialize-predictions", na.rm = TRUE), 1L)
  expect_false(any(endsWith(as.character(unlist(scores)), ".pt"), na.rm = TRUE))
  run_path <- attr(scores, "provenance")$run_path
  manifest <- jsonlite::read_json(
    file.path(run_path, "manifest.json"),
    simplifyVector = FALSE
  )
  expect_true(any(vapply(
    manifest$upstream,
    function(file) endsWith(file$path, ".pt"),
    logical(1)
  )))
  expect_false(any(endsWith(
    list.files(
      file.path(run_path, "upstream"),
      recursive = TRUE,
      full.names = TRUE
    ),
    ".pt"
  )))
  expect_true(file.exists(
    file.path(run_path, "outputs", "scores.jsonl.summary.json")
  ))
  expect_s3_class(job_result(bionemo_job(run_path)), "evo2_scores")
  reopened_manifest <- jsonlite::read_json(
    file.path(run_path, "manifest.json"),
    simplifyVector = FALSE
  )
  expect_true(any(vapply(
    reopened_manifest$upstream,
    function(file) endsWith(file$path, ".pt"),
    logical(1)
  )))

  delegated <- predict(model, sequences, type = "score", compute = compute)
  expect_s3_class(delegated, "evo2_scores")
  expect_error(
    predict(model, sequences, type = "raw", compute = compute),
    "portable raw-forward output is not yet supported",
    class = "BN_PROTOCOL"
  )
  expect_warning(
    generated <- predict(
      model,
      sequences,
      type = "response",
      compute = compute,
      num_tokens = 4L
    ),
    "deprecated"
  )
  expect_s3_class(generated, "evo2_generation")
})

test_that("asynchronous jobs can be reopened from their run directory", {
  workspace <- tempfile("bionemor-job-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    c(first = "ACGT", second = "TGCA"),
    compute,
    num_tokens = 4L,
    name = "persistent-generation",
    async = TRUE
  )
  path <- job_path(job)
  reopened <- bionemo_job(path)

  expect_s3_class(reopened, "bionemor::BioNeMoJob")
  expect_true(all(file.exists(file.path(path, c(
    "request.json", "plan.json", "state.json", "stdout.log", "stderr.log"
  )))))
  expect_true(job_status(reopened) %in% c("submitted", "starting", "running", "succeeded"))

  waited <- job_wait(reopened, poll = 0.01, timeout = 10)
  expect_s3_class(waited, "evo2_generation")
  expect_equal(waited, job_result(bionemo_job(path)))
  expect_true(file.exists(file.path(path, "manifest.json")))
})

test_that("inference requests retain original FASTA provenance", {
  workspace <- tempfile("bionemor-input-provenance-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  source <- file.path(workspace, "source.fasta")
  writeLines(c(">source", "acgt"), source)
  expected <- list(
    source = "fasta",
    path = normalizePath(source, mustWork = TRUE),
    digest = unname(tools::md5sum(source))
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  jobs <- list(
    generation = evo2_generate(
      model,
      source,
      compute,
      num_tokens = 4L,
      async = TRUE
    ),
    score = evo2_score(model, source, compute, async = TRUE),
    profile = evo2_profile(
      model,
      source,
      compute,
      output = "profiles/input-provenance.parquet",
      async = TRUE
    ),
    embedding = evo2_embed(model, source, compute, async = TRUE)
  )

  for (job in jobs) {
    request <- jsonlite::read_json(
      file.path(job_path(job), "request.json"),
      simplifyVector = FALSE
    )
    expect_equal(request$request$input_source, expected)
    expect_equal(request$expected_result$input_source, expected)
    job_wait(job, poll = 0.01, timeout = 10)
    manifest <- jsonlite::read_json(
      file.path(job_path(job), "manifest.json"),
      simplifyVector = FALSE
    )
    expect_equal(manifest$request$input_source, expected)
    expect_identical(
      manifest$value_origins$request$input_source,
      "auto_resolved"
    )
  }
})
