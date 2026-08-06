test_that("the shipped registry defines the supported recipe models", {
  models <- evo2_models()
  expect_false("prepared" %in% names(models))
  expect_equal(
    models$name,
    c("1b-base", "7b-base", "7b", "20b", "40b-base", "40b")
  )
  expect_equal(evo2()@size, "7b")
  expect_equal(evo2("1b")@size, "1b-base")
  expect_equal(evo2("7b")@model_size, "evo2_7b")
  expect_equal(evo2("7b")@context_length, 1048576L)
  expect_error(evo2("7b", pretrained = FALSE), "unused argument")
  expect_error(evo2("7b-262k"), "supported")
})

test_that("model compatibility follows advertised GPU and precision policy", {
  gpu_capabilities <- function(major, count) {
    list(
      runtime = list(
        gpu_count = count,
        gpus = data.frame(
          compute_capability_major = rep(major, count)
        )
      )
    )
  }
  compute <- function(major, requested = 2L, advertised = requested) {
    bionemo_compute(
      recipe = evo2_recipe(),
      engine = "external",
      workspace = tempfile("bionemor-models-"),
      gpus = requested,
      config = list(
        capabilities = gpu_capabilities(major, advertised)
      )
    )
  }

  unknown <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = tempfile("bionemor-models-")
  )
  expect_false(any(evo2_models(unknown)$compatible))

  insufficient <- evo2_models(compute(9L, requested = 2L, advertised = 1L))
  expect_false(any(insufficient$compatible))
  expect_match(insufficient$compatibility_note[[1L]], "advertises 1 GPU")

  ampere <- evo2_models(compute(8L), compatible = TRUE)
  expect_equal(ampere$name, c("7b-base", "7b"))

  hopper <- evo2_models(compute(9L))
  expect_equal(
    hopper$name[hopper$compatible],
    c("1b-base", "7b-base", "7b", "20b", "40b")
  )
  unverified <- hopper[hopper$name == "40b-base", ]
  expect_false(unverified$compatible)
  expect_equal(unverified$precision_policy, "unverified")
  expect_match(unverified$compatibility_note, "not verified")
})

test_that("inference rejects models without a verified execution policy", {
  workspace <- tempfile("bionemor-unverified-inference-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(workspace, "40b-base")
  writeLines(
    c("model_size: evo2_40b_base", "kind: dense"),
    file.path(checkpoint, "run_config.yaml")
  )
  model <- evo2("40b-base", checkpoint = checkpoint)
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
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
    evo2_generate(model, "ACGT", compute, num_tokens = 4L),
    "no verified precision"
  )
  expect_false(file.exists(log))
})

test_that("inference validates requested precision against the actual GPUs", {
  workspace <- tempfile("bionemor-inference-preflight-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  compute <- function(major) {
    bionemo_compute(
      recipe = evo2_recipe(),
      engine = "external",
      workspace = workspace,
      config = list(
        capabilities = list(
          runtime = list(
            gpu_count = 1L,
            gpus = data.frame(compute_capability_major = major)
          )
        )
      )
    )
  }

  expect_error(
    evo2_generate(
      model,
      "ACGT",
      compute(8L),
      num_tokens = 4L,
      control = evo2_inference_control(
        precision = "fp8",
        vortex_style_fp8 = "yes"
      )
    ),
    "verified Hopper"
  )
  expect_error(
    evo2_generate(model, "ACGT", compute(7L), num_tokens = 4L),
    "compute capability 8.0"
  )
  expect_false(file.exists(log))
})

test_that("fine-tuning rejects models without a verified execution policy", {
  workspace <- tempfile("bionemor-unverified-fit-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  checkpoint <- make_mbridge_checkpoint(workspace, "40b-base")
  writeLines(
    c("model_size: evo2_40b_base", "kind: dense"),
    file.path(checkpoint, "run_config.yaml")
  )
  model <- evo2("40b-base", checkpoint = checkpoint)
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
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
    "no verified precision"
  )
  expect_false(file.exists(log))
})

test_that("current inference and fine-tuning controls use semantic fields", {
  inference <- evo2_inference_control()
  expect_equal(inference@precision, "auto")
  expect_equal(inference@tensor_parallel_size, 1L)
  expect_equal(inference@context_parallel_size, 1L)

  fit_control <- evo2_fit_control(
    global_batch_size = 8L,
    micro_batch_size = 2L,
    precision = "fp8-current"
  )
  expect_equal(fit_control@global_batch_size, 8L)
  expect_equal(fit_control@eval_interval, 100L)
  expect_equal(fit_control@precision, "fp8-current")
  expect_error(
    evo2_fit_control(gradient_accumulation = 2L),
    "unused argument"
  )

  method <- evo2_lora(
    rank = 8L,
    alpha = 16,
    targets = c("hyena", "attention")
  )
  expect_s3_class(method, "bionemor::Evo2LoRA")
  expect_equal(method@targets, c("hyena", "attention"))
  expect_s3_class(evo2_full(), "bionemor::Evo2FullFineTune")
})

test_that("in-memory dataset splitting is stable under input reordering", {
  sequences <- stats::setNames(
    rep("ACGT", 100L),
    sprintf("sequence_%03d", seq_len(100L))
  )
  first <- evo2_dataset(sequences, seed = 42L)
  second <- evo2_dataset(sequences[100:1], seed = 42L)

  expect_setequal(names(first@train), names(second@train))
  expect_setequal(names(first@validation), names(second@validation))
  expect_setequal(names(first@test), names(second@test))
  expect_length(intersect(names(first@train), names(first@validation)), 0L)
  expect_length(intersect(names(first@train), names(first@test)), 0L)
})

test_that("FASTA dataset splitting materializes deterministic partitions", {
  sequences <- c(
    California = "AAAA",
    August = "CCCC",
    Jan = "GGGG",
    April = "TTTT",
    Virginia = "ACGT",
    Kentucky = "TGCA",
    Alaska = "AATT",
    Michigan = "CCGG",
    Tennessee = "GTAC"
  )
  source <- tempfile("bionemor-split-", fileext = ".fasta")
  reordered <- tempfile("bionemor-split-reordered-", fileext = ".fasta")
  write_fixture <- function(path, records) {
    lines <- unlist(
      Map(
        function(id, sequence) c(paste0(">", id), sequence),
        names(records),
        unname(records)
      ),
      use.names = FALSE
    )
    writeLines(lines, path, useBytes = TRUE)
  }
  write_fixture(source, sequences)
  write_fixture(reordered, rev(sequences))
  split <- c(train = 0.5, validation = 0.25, test = 0.25)

  first <- evo2_dataset(source, split = split, seed = 42L)
  second <- evo2_dataset(reordered, split = split, seed = 42L)

  expect_setequal(
    names(first@train),
    c("California", "August", "Jan")
  )
  expect_setequal(
    names(first@validation),
    c("April", "Virginia", "Kentucky")
  )
  expect_setequal(
    names(first@test),
    c("Alaska", "Michigan", "Tennessee")
  )
  expect_setequal(names(first@train), names(second@train))
  expect_setequal(names(first@validation), names(second@validation))
  expect_setequal(names(first@test), names(second@test))
  expect_identical(first@provenance$partition_method, "stable-hash")
})

test_that("preprocessing materializes compressed FASTA as plain FASTA", {
  workspace <- tempfile("bionemor-compressed-fasta-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  source <- tempfile("bionemor-train-", fileext = ".fasta.gz")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  connection <- gzfile(source, open = "wt")
  writeLines(c(">train_a", "ACGT", ">train_b", "TGCA"), connection)
  close(connection)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  prepared <- evo2_preprocess(
    evo2_dataset(
      source,
      split = c(train = 1, validation = 0, test = 0)
    ),
    model,
    compute,
    path = "datasets/compressed-fasta"
  )

  materialized <- file.path(
    prepared@provenance$run_path,
    "inputs",
    "train.fasta"
  )
  expect_equal(
    readBin(
      materialized,
      what = "raw",
      n = unname(file.info(materialized)$size)
    ),
    charToRaw(">train_a\nACGT\n>train_b\nTGCA\n")
  )
  expect_equal(
    readLines(materialized, warn = FALSE),
    c(">train_a", "ACGT", ">train_b", "TGCA")
  )
  expect_equal(prepared@manifest$inputs$train$records, 2L)
})

test_that("preprocessing and LoRA fine-tuning use current recipe commands", {
  workspace <- tempfile("bionemor-fit-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  container_log <- tempfile("bionemor-container-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_container_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_CONTAINER_LOG = container_log
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    workspace = workspace,
    image = paste0("example/evo2@sha256:", strrep("c", 64L))
  )
  base_checkpoint <- file.path(workspace, "base-training-root")
  selected_base_checkpoint <- make_mbridge_checkpoint(
    workspace,
    "base-training-root/iter_0000003"
  )
  make_mbridge_checkpoint(workspace, "base-training-root/iter_0000004")
  writeLines(
    "3",
    file.path(base_checkpoint, "latest_checkpointed_iteration.txt")
  )
  model <- evo2("7b", checkpoint = base_checkpoint)
  data <- evo2_dataset(
    train = c(train_a = "ACGT", train_b = "TGCA"),
    validation = c(validation_a = "AAAA"),
    test = c(test_a = "CCCC")
  )
  preprocess_control <- evo2_preprocess_control(
    taxonomy = data.frame(
      id = c("train_a", "train_b", "validation_a", "test_a"),
      domain = rep("Bacteria", 4L),
      class = rep("Gammaproteobacteria", 4L),
      stringsAsFactors = FALSE
    )
  )

  prepared <- evo2_preprocess(
    data,
    model,
    compute,
    path = "datasets/example",
    control = preprocess_control
  )
  expect_s3_class(prepared, "bionemor::Evo2Dataset")
  expect_true(prepared@prepared)
  config <- readLines(prepared@manifest$preprocess_config, warn = FALSE)
  expect_match(
    paste(config, collapse = "\n"),
    "hf_tokenizer_model_path",
    fixed = TRUE
  )
  expect_false(any(grepl("tokenizer_type", config, fixed = TRUE)))
  parsed_config <- yaml12::read_yaml(
    prepared@manifest$preprocess_config,
    simplify = FALSE
  )
  tokenizer <- "/workspace/bionemo/tokenizers/nucleotide_fast_tokenizer_512"
  expect_equal(
    parsed_config[[1L]]$hf_tokenizer_model_path,
    tokenizer
  )
  expect_equal(prepared@manifest$tokenizer, tokenizer)
  expect_equal(
    parsed_config[[1L]]$taxonomy_data$train_a$clazz,
    "Gammaproteobacteria"
  )
  expect_equal(prepared@manifest$model_size, "evo2_7b")
  expect_match(
    prepared@manifest$tokenizer_revision,
    "^[0-9a-f]{40}$"
  )
  expect_named(
    prepared@manifest$inputs$train,
    c(
      "path",
      "digest",
      "records",
      "minimum_length",
      "maximum_length",
      "mean_length"
    )
  )
  expect_true(all(vapply(
    prepared@manifest$outputs,
    function(output) nzchar(output$digest),
    logical(1)
  )))
  expect_true(file.exists(prepared@manifest$manifest_path))

  job <- evo2_finetune(
    model,
    prepared,
    compute,
    steps = 2L,
    method = evo2_lora(rank = 8L, alpha = 16),
    control = evo2_fit_control(
      global_batch_size = 2L,
      micro_batch_size = 1L,
      warmup_steps = 0L,
      constant_steps = 0L,
      precision = "bf16",
      extra = list(
        sequence_parallel = TRUE,
        fp32_residual_connection = FALSE,
        adam_epsilon = 1e-7
      )
    ),
    name = "lora-fit",
    async = TRUE,
    timeout = 10L
  )
  expect_s3_class(bionemo_job(job_path(job)), "bionemor::BioNeMoJob")
  plan <- jsonlite::read_json(
    file.path(job_path(job), "plan.json"),
    simplifyVector = TRUE
  )
  tokens <- unlist(plan$steps, use.names = FALSE)
  expect_true(all(
    c(
      "torchrun",
      "--no-python",
      "train_evo2",
      "--global-batch-size",
      "--finetune-ckpt-dir",
      "--eval-interval",
      "--eval-iters",
      "--sequence-parallel",
      "--no-fp32-residual-connection",
      "--adam-eps",
      "--hf-tokenizer-model-path",
      tokenizer,
      "--disable-tensorboard-logger",
      "--lora-finetune",
      "--lora-dim"
    ) %in%
      tokens
  ))
  base_flag <- which(tokens == "--finetune-ckpt-dir")
  expect_length(base_flag, 1L)
  expect_equal(
    tokens[[base_flag + 1L]],
    normalizePath(selected_base_checkpoint)
  )
  expect_false(any(
    c(
      "--devices",
      "--grad-acc-batches",
      "--ckpt-dir",
      "--val-check-interval",
      "--limit-val-batches"
    ) %in%
      tokens
  ))

  fitted <- job_wait(
    bionemo_job(job_path(job)),
    poll = 0.01,
    timeout = 10
  )
  expect_s3_class(fitted, "bionemor::Evo2Model")
  expect_equal(fitted@checkpoint@format, "mbridge")
  expect_equal(fitted@checkpoint@kind, "lora")
  expect_true(dir.exists(checkpoint_path(fitted)))
  expect_equal(
    fitted@checkpoint@base_checkpoint,
    normalizePath(selected_base_checkpoint)
  )
  fitted_score <- evo2_score(fitted, c(example = "ACGT"))
  expect_equal(fitted_score$id, "example")
  run_manifest <- jsonlite::read_json(
    file.path(job_path(job), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_null(run_manifest$value_origins)
  expect_identical(
    run_manifest$request$control$global_batch_size,
    2L
  )
  expect_identical(
    run_manifest$request$control$eval_interval,
    100L
  )
  expect_identical(
    run_manifest$execution$resolved_control$data_parallel_size,
    1L
  )
  expect_identical(run_manifest$precision$semantic, "bf16")
  expect_identical(run_manifest$precision$resolved_recipe, "bf16_mixed")
  expect_named(run_manifest$precision, c("semantic", "resolved_recipe"))
  fitted_manifest <- checkpoint_manifest(fitted@checkpoint)
  expect_identical(fitted_manifest$source_trust, "not-required")
  expect_false(fitted_manifest$source_verified)
  expect_identical(
    fitted_manifest$base_checkpoint_source_trust,
    "not-required"
  )

  moved_base <- paste0(base_checkpoint, "-missing")
  expect_true(file.rename(base_checkpoint, moved_base))
  before <- length(readLines(log))
  expect_error(
    evo2_generate(fitted, "ACGT", compute, num_tokens = 4L),
    "LoRA base checkpoint is missing",
    class = "BN_BASE_CHECKPOINT_MISSING"
  )
  expect_length(readLines(log), before)
})

test_that("dense MBridge checkpoints export explicitly to Vortex", {
  workspace <- tempfile("bionemor-export-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  exported <- evo2_export(
    model,
    path = "exports/evo2-7b-vortex.pt",
    compute = compute
  )

  expect_s3_class(exported, "bionemor::BioNeMoCheckpoint")
  expect_equal(exported@format, "vortex")
  expect_equal(exported@kind, "dense")
  expect_true(file.exists(checkpoint_path(exported)))
  plan <- jsonlite::read_json(
    file.path(exported@provenance$run_path, "plan.json"),
    simplifyVector = TRUE
  )
  tokens <- unlist(plan$steps, use.names = FALSE)
  expect_true(all(
    c(
      "evo2_remove_optimizer",
      "evo2_export_mbridge_to_vortex",
      "--mbridge-ckpt-dir",
      "--output-path",
      "--model-size",
      "evo2_7b"
    ) %in%
      tokens
  ))
  expect_false("--no-te" %in% tokens)
})

test_that("pooled embeddings return an ordered numeric matrix", {
  workspace <- tempfile("bionemor-embed-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = workspace
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  output <- file.path(workspace, "portable", "evo2-pooled")

  embeddings <- evo2_embed(
    model,
    c(first = "ACGT", second = "TGCA"),
    compute,
    layer = "last",
    pool = "mean",
    output = output
  )

  expect_s3_class(embeddings, "evo2_embeddings")
  expect_true(is.matrix(embeddings))
  expect_type(embeddings, "double")
  expect_equal(rownames(embeddings), c("first", "second"))
  expect_equal(colnames(embeddings), c("dim_1", "dim_2", "dim_3"))
  expect_true(all(is.finite(embeddings)))
  expect_equal(
    unclass(embeddings),
    matrix(
      c(1, 2, 3, 2, 3, 4),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(
        c("first", "second"),
        c("dim_1", "dim_2", "dim_3")
      )
    ),
    ignore_attr = "provenance"
  )
  expect_pooled_embedding_output(
    output,
    c(2L, 3L),
    c("first", "second"),
    "bfloat16"
  )
})
