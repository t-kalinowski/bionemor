test_that("fit maps typed controls to the BioNeMo 2.6.3 command", {
  workspace <- tempfile("bionemor-fit-command-")
  bin <- tempfile("bionemor-slurm-")
  dir.create(workspace)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "python",
    workspace = workspace,
    gpus = 2L
  )
  control <- evo2_fit_control(
    sequence_length = 1024L,
    learning_rate = 1.5e-5,
    minimum_learning_rate = 1e-6,
    warmup_steps = 3L,
    micro_batch_size = 2L,
    gradient_accumulation = 4L,
    precision = "fp8",
    clip_gradient = 250,
    weight_decay = 0.01,
    attention_dropout = 0.1,
    hidden_dropout = 0.2,
    validation_interval = 5L,
    validation_batches = 2L,
    activation_checkpoint_layers = 4L,
    workers = 3L,
    seed = 41L,
    asynchronous_checkpoint = TRUE,
    extra_args = "--sequence-parallel"
  )

  job <- withr::with_options(
    list(OutDec = ","),
    generics::fit(
      evo2("7b", checkpoint = checkpoint),
      data = c(first = "ACGT", second = "TGCA"),
      compute = compute,
      steps = 10L,
      control = control,
      name = "mapped-fit",
      output = "training",
      async = TRUE
    )
  )

  expect_s3_class(job, "bionemor::BioNeMoJob")
  expect_equal(job@kind, "fit")
  expect_equal(job@state, "submitted")
  expect_equal(job@timeout, Inf)
  expect_match(job@command, "preprocess_evo2 --config")
  expect_match(job@command, "train_evo2")
  expect_match(job@command, "--model-size '7b_arc_longcontext'", fixed = TRUE)
  expect_match(job@command, "--ckpt-dir")
  expect_match(job@command, "--max-steps 10")
  expect_match(job@command, "--seq-length 1024")
  expect_match(job@command, "--lr 0.000015")
  expect_match(job@command, "--min-lr 0.000001")
  expect_match(job@command, "--warmup-steps 3")
  expect_match(job@command, "--micro-batch-size 2")
  expect_match(job@command, "--grad-acc-batches 4")
  expect_match(job@command, "--fp8")
  expect_match(job@command, "--clip-grad 250")
  expect_match(job@command, "--wd 0.01")
  expect_match(job@command, "--attention-dropout 0.1")
  expect_match(job@command, "--hidden-dropout 0.2")
  expect_match(job@command, "--val-check-interval 5")
  expect_match(job@command, "--limit-val-batches 2")
  expect_match(job@command, "--activation-checkpoint-recompute-num-layers 4")
  expect_match(job@command, "--workers 3")
  expect_match(job@command, "--seed 41")
  expect_match(job@command, "--ckpt-async-save")
  expect_match(job@command, "--sequence-parallel")
  expect_false(grepl(",", job@command, fixed = TRUE))
  expect_false(grepl("download_bionemo_data", job@command, fixed = TRUE))

  scratch <- generics::fit(
    evo2("1b", pretrained = FALSE),
    data = "ACGT",
    compute = compute,
    steps = 1L,
    name = "scratch-fit",
    async = TRUE
  )
  expect_false(grepl("--ckpt-dir", scratch@command, fixed = TRUE))
  expect_null(scratch@expected_result@provenance$parent_checkpoint)

  expect_error(
    generics::fit(
      evo2("7b", checkpoint = checkpoint),
      data = "ACGT",
      compute = compute,
      steps = 1L,
      name = "reserved-fit-argument",
      async = TRUE,
      unsupported = TRUE
    ),
    "reserved"
  )
  expect_error(
    generics::fit(
      evo2("7b", checkpoint = checkpoint),
      data = "ACGT",
      compute = compute,
      steps = 1L,
      name = "..",
      async = TRUE
    ),
    "safe job name"
  )

  fasta <- readLines(job@metadata$input, warn = FALSE)
  expect_equal(fasta, c(">first", "ACGT", ">second", "TGCA"))
  dataset <- yaml::read_yaml(job@metadata$dataset)
  expect_equal(
    vapply(dataset, `[[`, character(1), "dataset_split"),
    c("train", "validation", "test")
  )
})

test_that("fit accepts every documented R input form", {
  workspace <- tempfile("bionemor-fit-inputs-")
  bin <- tempfile("bionemor-slurm-")
  dir.create(workspace)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)
  compute <- bionemo_compute(
    backend = "slurm",
    workspace = workspace
  )
  model <- evo2("1b", checkpoint = checkpoint)
  fasta <- file.path(workspace, "input.fasta")
  writeLines(c(">existing", "ACGT"), fasta)

  from_path <- generics::fit(
    model,
    fasta,
    compute = compute,
    steps = 1L,
    name = "path-input",
    async = TRUE
  )
  expect_equal(from_path@metadata$input, normalizePath(fasta))

  from_frame <- generics::fit(
    model,
    data.frame(id = c("a", "b"), sequence = c("AC", "GT")),
    compute = compute,
    steps = 1L,
    name = "frame-input",
    async = TRUE
  )
  expect_equal(
    readLines(from_frame@metadata$input),
    c(">a", "AC", ">b", "GT")
  )

  if (requireNamespace("Biostrings", quietly = TRUE)) {
    dna <- Biostrings::DNAStringSet(c(one = "AC", two = "GT"))
    from_dna <- generics::fit(
      model,
      dna,
      compute = compute,
      steps = 1L,
      name = "dna-input",
      async = TRUE
    )
    expect_equal(
      readLines(from_dna@metadata$input),
      c(">one", "AC", ">two", "GT")
    )
  }
})

test_that("fit validates its public contract before submission", {
  workspace <- tempfile("bionemor-fit-validation-")
  dir.create(workspace)
  compute <- bionemo_compute(workspace = workspace)
  checkpoint <- make_checkpoint_dir(workspace)
  empty_fasta <- file.path(workspace, "empty.fasta")
  writeLines(">empty", empty_fasta)

  expect_error(
    generics::fit(evo2("7b"), "ACGT", compute = compute, steps = 1L, async = TRUE),
    "explicit checkpoint"
  )
  expect_error(
    generics::fit(
      evo2("7b", pretrained = FALSE),
      "ACGT",
      compute = compute,
      steps = 0L,
      async = TRUE
    ),
    "steps"
  )
  expect_error(
    generics::fit(
      evo2("7b", checkpoint = checkpoint),
      file.path(workspace, "missing.fasta"),
      compute = compute,
      steps = 1L,
      async = TRUE
    ),
    "data path"
  )
  expect_error(
    generics::fit(
      evo2("7b", checkpoint = checkpoint),
      "ACGT",
      compute = compute,
      steps = 1L,
      control = list(),
      async = TRUE
    ),
    "Evo2FitControl"
  )
  expect_error(
    generics::fit(
      evo2("7b", checkpoint = checkpoint),
      empty_fasta,
      compute = compute,
      steps = 1L,
      async = TRUE
    ),
    "empty"
  )
})

test_that("synchronous fit returns a fitted model with provenance", {
  workspace <- tempfile("bionemor-fit-result-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)

  fitted <- generics::fit(
    evo2("1b", checkpoint = checkpoint),
    c(first = "ACGT", second = "TGCA"),
    compute = bionemo_compute(workspace = workspace),
    steps = 3L,
    name = "sync-fit",
    output = "training",
    timeout = 5,
    async = FALSE
  )

  expect_s3_class(fitted, "bionemor::Evo2Model")
  expect_s3_class(fitted@checkpoint, "bionemor::BioNeMoCheckpoint")
  expect_true(dir.exists(checkpoint_path(fitted)))
  expect_equal(fitted@provenance$parent_checkpoint, normalizePath(checkpoint))
  expect_equal(fitted@provenance$profile, "bionemo-2.6.3")
  expect_equal(fitted@provenance$precision, "bf16")
  expect_equal(fitted@provenance$steps, 3L)
  expect_equal(fitted@provenance$data$ids, c("first", "second"))
})

test_that("relative operation paths resolve under the compute workspace", {
  workspace <- tempfile("bionemor-relative-workspace-")
  caller <- tempfile("bionemor-relative-caller-")
  bin <- tempfile("bionemor-relative-slurm-")
  dir.create(workspace)
  dir.create(caller)
  fake_slurm_runtime(bin)
  checkpoint <- make_checkpoint_dir(workspace)
  fasta <- file.path(workspace, "input.fasta")
  writeLines(c(">relative", "ACGT"), fasta)
  withr::local_dir(caller)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    backend = "slurm",
    workspace = workspace
  )
  model <- evo2("1b", checkpoint = basename(checkpoint))

  fit_job <- generics::fit(
    model,
    data = basename(fasta),
    compute = compute,
    steps = 1L,
    name = "relative-fit",
    output = "fit-output",
    async = TRUE
  )
  predict_job <- predict(
    model,
    basename(fasta),
    type = "raw",
    compute = compute,
    name = "relative-predict",
    output = "predict-output",
    async = TRUE
  )

  expect_equal(fit_job@metadata$input, normalizePath(fasta))
  expect_match(fit_job@command, normalizePath(checkpoint), fixed = TRUE)
  expect_equal(predict_job@metadata$input, normalizePath(fasta))
  expect_match(predict_job@command, normalizePath(checkpoint), fixed = TRUE)
})
