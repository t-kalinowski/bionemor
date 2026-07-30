test_that("recommended tokenizers resolve in the selected runtime", {
  workspace <- tempfile("bionemor-tokenizer-")
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

  prepared <- evo2_prepare(
    evo2_dataset(c(first = "ACGT", second = "TGCA")),
    model,
    compute,
    path = "datasets/tokenizer"
  )

  expected <- normalizePath(
    file.path(bin, "tokenizers", "nucleotide_fast_tokenizer_512"),
    mustWork = TRUE
  )
  expect_equal(prepared@manifest$tokenizer, expected)
  config <- yaml12::read_yaml(
    prepared@manifest$preprocess_config,
    simplify = FALSE
  )
  expect_equal(config[[1L]]$hf_tokenizer_model_path, expected)
})

test_that("fine-tuning rejects sequence lengths beyond the model context", {
  workspace <- tempfile("bionemor-context-limit-fit-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(
    workspace,
    model_size = "evo2_7b_base"
  )
  model <- evo2("7b-base", checkpoint = checkpoint)
  compute <- bionemo_compute(engine = "external", workspace = workspace)

  error <- expect_error(
    evo2_finetune(
      model,
      evo2_dataset(c(first = "ACGT", second = "TGCA")),
      compute,
      steps = 1L,
      control = evo2_fit_control(
        sequence_length = model@context_length + 1L
      ),
      async = FALSE
    ),
    class = "BN_CONTEXT_LIMIT"
  )

  expect_s3_class(error, "bionemor_error")
  expect_identical(error$code, "BN_CONTEXT_LIMIT")
  expect_identical(error$operation, "fine-tune")
  expect_identical(error$model, "7b-base")
  expect_identical(error$context_length, 8192L)
  expect_identical(error$sequence_length, 8193L)
  expect_identical(error$additional_tokens, 0L)
  expect_identical(error$required_length, 8193L)
  expect_false(dir.exists(file.path(workspace, ".bionemor", "runs")))
  expect_false(file.exists(log))
})

test_that("Slurm tokenizer paths must be inside the shared workspace", {
  workspace <- tempfile("bionemor-slurm-tokenizer-")
  tokenizer <- tempfile("bionemor-tokenizer-outside-workspace-")
  dir.create(workspace)
  dir.create(tokenizer)
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace
  )

  expect_error(
    evo2_checkpoint(
      evo2("7b"),
      path = "checkpoints/evo2-7b",
      compute = compute,
      tokenizer = tokenizer
    ),
    "Slurm tokenizer path must be inside"
  )
})

test_that("MBridge registration requires distributed checkpoint weights", {
  workspace <- tempfile("bionemor-metadata-only-")
  checkpoint <- file.path(workspace, "checkpoint")
  dir.create(checkpoint, recursive = TRUE)
  writeLines(
    c("model_size: evo2_7b", "kind: dense"),
    file.path(checkpoint, "run_config.yaml")
  )
  writeLines("metadata", file.path(checkpoint, ".metadata"))

  expect_error(
    evo2("7b", checkpoint = checkpoint),
    "distributed checkpoint weight shard"
  )
})

test_that("documented 1B BF16 fine-tuning is independent of inference policy", {
  workspace <- tempfile("bionemor-1b-fit-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(
    workspace,
    "checkpoint-1b",
    model_size = "evo2_1b_base"
  )
  model <- evo2("1b", checkpoint = checkpoint)
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    config = list(
      capabilities = list(
        runtime = list(
          gpu_count = 1L,
          gpus = data.frame(compute_capability_major = 8L)
        )
      )
    )
  )

  fitted <- evo2_finetune(
    model,
    evo2_dataset(c(first = "ACGT", second = "TGCA")),
    compute,
    steps = 1L,
    method = evo2_lora(),
    control = evo2_fit_control(
      global_batch_size = 1L,
      warmup_steps = 0L,
      constant_steps = 0L
    ),
    async = FALSE
  )

  expect_s3_class(fitted, "bionemor::Evo2Model")
  plan <- checkpoint_manifest(fitted)$provenance$plan
  tokens <- unlist(plan$steps, use.names = FALSE)
  precision <- which(tokens == "--mixed-precision-recipe")
  expect_length(precision, 1L)
  expect_equal(tokens[[precision + 1L]], "bf16_mixed")
})

test_that("full fine-tuning cannot use a LoRA checkpoint as its base", {
  workspace <- tempfile("bionemor-full-from-lora-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  base <- make_mbridge_checkpoint(workspace, "base")
  lora <- make_mbridge_checkpoint(
    workspace,
    "lora",
    kind = "lora",
    base_checkpoint = base
  )
  model <- evo2("7b", checkpoint = lora)
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    config = list(
      capabilities = list(
        runtime = list(
          gpu_count = 1L,
          gpus = data.frame(compute_capability_major = 9L)
        )
      )
    )
  )

  expect_error(
    evo2_finetune(
      model,
      evo2_dataset(c(first = "ACGT", second = "TGCA")),
      compute,
      steps = 1L,
      method = evo2_full(),
      async = FALSE
    ),
    "full fine-tuning from a LoRA checkpoint"
  )
  expect_false(file.exists(log))
})

test_that("unsupported training precision paths fail at construction", {
  expect_error(
    evo2_fit_control(precision = "fp8-delayed"),
    "arg"
  )
  expect_error(
    evo2_fit_control(
      mixed_precision_recipe = "bf16_with_fp8_delayed_scaling_mixed"
    ),
    "delayed"
  )
})

test_that("Vortex-style MBridge training fails before recipe execution", {
  workspace <- tempfile("bionemor-vortex-fit-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(
    workspace,
    "checkpoint-1b-vortex",
    model_size = "evo2_1b_base",
    vortex_style_fp8 = TRUE
  )
  model <- evo2("1b", checkpoint = checkpoint)
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    config = list(
      capabilities = list(
        runtime = list(
          gpu_count = 1L,
          gpus = data.frame(compute_capability_major = 9L)
        )
      )
    )
  )

  expect_error(
    evo2_finetune(
      model,
      evo2_dataset(c(first = "ACGT", second = "TGCA")),
      compute,
      steps = 1L,
      async = FALSE
    ),
    "Vortex-style"
  )
  expect_false(file.exists(log))
})

test_that("Vortex-sensitive NeMo2 conversion requires an explicit upstream path", {
  workspace <- tempfile("bionemor-vortex-nemo2-")
  source <- file.path(workspace, "nemo2")
  dir.create(source, recursive = TRUE)
  compute <- bionemo_compute(engine = "external", workspace = workspace)

  expect_error(
    evo2_checkpoint(
      evo2("1b"),
      source = source,
      format = "nemo2",
      path = "checkpoints/evo2-1b",
      compute = compute,
      trust = TRUE
    ),
    "Vortex-sensitive NeMo2"
  )
})

test_that("Vortex export resolves the checkpoint Transformer Engine layout", {
  workspace <- tempfile("bionemor-vortex-layout-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(
    workspace,
    transformer_engine = FALSE
  )
  model <- evo2("7b", checkpoint = checkpoint)
  compute <- bionemo_compute(engine = "external", workspace = workspace)

  exported <- evo2_export(
    model,
    path = "exports/no-te/model.pt",
    compute = compute
  )

  plan <- checkpoint_manifest(exported)$provenance$plan
  tokens <- unlist(plan$steps, use.names = FALSE)
  expect_true("--no-te" %in% tokens)
})

test_that("Vortex export directory owns one checkpoint and config", {
  workspace <- tempfile("bionemor-vortex-layout-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  compute <- bionemo_compute(engine = "external", workspace = workspace)

  evo2_export(
    model,
    path = "exports/shared/first.pt",
    compute = compute
  )

  expect_error(
    evo2_export(
      model,
      path = "exports/shared/second.pt",
      compute = compute
    ),
    "one Vortex checkpoint"
  )
  expect_false(file.exists(file.path(workspace, "exports/shared/second.pt")))
})

test_that("Vortex export reuse requires the requested checkpoint identity", {
  workspace <- tempfile("bionemor-vortex-reuse-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  first <- evo2(
    "7b",
    checkpoint = make_mbridge_checkpoint(workspace, "checkpoint-first")
  )
  second <- evo2(
    "7b",
    checkpoint = make_mbridge_checkpoint(workspace, "checkpoint-second")
  )
  destination <- "exports/reused/model.pt"

  evo2_export(first, path = destination, compute = compute)

  expect_error(
    evo2_export(second, path = destination, compute = compute),
    "checkpoint manifest source does not match the requested source"
  )
})
