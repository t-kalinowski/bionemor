test_that("evo2 creates a compute-independent model specification", {
  expect_error(evo2(), "size")

  model <- evo2("7b")
  expect_s3_class(model, "bionemor::Evo2Model")
  expect_equal(model@family, "evo2")
  expect_equal(model@size, "7b")
  expect_true(model@pretrained)
  expect_null(model@checkpoint)
  expect_false("compute" %in% names(S7::props(model)))

  expect_equal(evo2("1b-8k")@size, "1b")
  expect_equal(evo2("7b-1m")@size, "7b")
  expect_equal(evo2("40b-1m")@size, "40b")
  expect_error(evo2("20b"), "size")
  expect_error(evo2("7b", checkpoint = "/models/evo2", pretrained = FALSE), "checkpoint")
})

test_that("evo2 accepts one checkpoint path", {
  model <- evo2("7b", checkpoint = "/models/evo2")

  expect_equal(model@checkpoint, "/models/evo2")
  expect_error(evo2("7b", checkpoint = character()), "checkpoint")
  expect_error(evo2("7b", checkpoint = c("one", "two")), "checkpoint")
})

test_that("compute construction maps supported execution engines", {
  workspace <- tempfile("bionemor-compute-")
  dir.create(workspace)

  local_python <- bionemo_compute(workspace = workspace)
  expect_s3_class(local_python, "bionemor::BioNeMoCompute")
  expect_equal(local_python@backend, "local")
  expect_equal(local_python@engine, "python")
  expect_equal(local_python@profile, "bionemo-2.6.3")
  expect_equal(local_python@workspace, normalizePath(workspace))

  local_container <- bionemo_compute(
    backend = "local",
    engine = "container",
    image = "bionemo:2.6.3",
    workspace = workspace,
    gpus = 2L
  )
  expect_equal(local_container@gpus, 2L)

  slurm_container <- bionemo_compute(
    backend = "slurm",
    engine = "container",
    image = "/images/bionemo.sif",
    workspace = workspace,
    queue = "gpu",
    account = "science",
    walltime = "00:30:00"
  )
  expect_equal(slurm_container@queue, "gpu")

  expect_error(bionemo_compute(workspace = workspace, nodes = 2L), "single node")
  expect_error(bionemo_compute(workspace = workspace, gpus = 1.5), "gpus")
  expect_error(
    bionemo_compute(workspace = workspace, profile = "latest"),
    "profile"
  )
  expect_error(
    bionemo_compute(workspace = .Platform$file.sep),
    "filesystem root"
  )
})

test_that("capabilities describe only implemented Evo 2 operations", {
  model <- evo2("1b")
  compute <- bionemo_compute()

  model_capabilities <- bionemo_capabilities(model)
  expect_setequal(
    model_capabilities$operation,
    c("checkpoint", "fit", "response", "score", "raw")
  )
  expect_true(all(model_capabilities$supported))

  compute_capabilities <- bionemo_capabilities(compute)
  expect_setequal(
    compute_capabilities$operation,
    c("checkpoint", "fit", "response", "score", "raw")
  )
  expect_false(any(compute_capabilities$operation == "representation"))
})

test_that("evo2_fit_control validates and types every fitting control", {
  control <- evo2_fit_control()

  expect_s3_class(control, "bionemor::Evo2FitControl")
  expect_equal(control@sequence_length, 8192L)
  expect_equal(control@learning_rate, 1e-5)
  expect_equal(control@micro_batch_size, 1L)
  expect_equal(control@gradient_accumulation, 1L)
  expect_equal(control@precision, "bf16")
  expect_equal(
    control@split,
    c(train = 0.9, validation = 0.05, test = 0.05)
  )
  expect_false(control@asynchronous_checkpoint)

  configured <- evo2_fit_control(
    minimum_learning_rate = 1e-6,
    warmup_steps = 10L,
    clip_gradient = 1,
    weight_decay = 0.1,
    attention_dropout = 0.1,
    hidden_dropout = 0.2,
    validation_interval = 5L,
    validation_batches = 2L,
    activation_checkpoint_layers = 4L,
    precision = "fp8"
  )
  expect_equal(configured@minimum_learning_rate, 1e-6)
  expect_equal(configured@warmup_steps, 10L)
  expect_equal(configured@precision, "fp8")

  reordered <- evo2_fit_control(
    split = c(test = 0.05, train = 0.9, validation = 0.05)
  )
  expect_equal(
    reordered@split,
    c(train = 0.9, validation = 0.05, test = 0.05)
  )

  expect_error(evo2_fit_control(sequence_length = 1.5), "sequence_length")
  expect_error(evo2_fit_control(learning_rate = 0), "learning_rate")
  expect_error(evo2_fit_control(split = c(train = 1, validation = 0)), "split")
  expect_error(
    evo2_fit_control(extra_args = c("--lr", "0.2")),
    "extra_args.*learning_rate"
  )
  expect_error(
    evo2_fit_control(extra_args = "--micro-batch-size=2"),
    "extra_args.*micro_batch_size"
  )
  expect_error(
    evo2_fit_control(extra_args = "--wd=0.2"),
    "extra_args.*weight_decay"
  )
  expect_error(
    evo2_fit_control(extra_args = "--fp8"),
    "extra_args.*precision"
  )
  expect_error(
    evo2_fit_control(extra_args = "--max-steps=20"),
    "extra_args.*steps"
  )
  expect_error(
    evo2_fit_control(extra_args = c("-d", "another-dataset.yaml")),
    "extra_args.*dataset"
  )
  expect_error(
    evo2_fit_control(extra_args = "--ckpt-dir /another/checkpoint"),
    "extra_args.*checkpoint"
  )
})
