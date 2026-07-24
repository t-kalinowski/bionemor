test_that("setup writes exact secret-free training and inference probes", {
  workspace <- tempfile("bionemor-setup-")
  dir.create(workspace)
  compute <- bionemo_compute(workspace = workspace)
  withr::local_envvar(
    NGC_API_KEY = "never-write-this",
    NGC_CLI_API_KEY = "nor-this"
  )

  training <- bionemo_setup(
    compute,
    model = evo2("1b", pretrained = FALSE),
    target = "training",
    path = file.path(workspace, "training-setup")
  )
  expect_s3_class(training, "bionemor::BioNeMoSetupPlan")
  expect_false(training@executed)
  expect_true(all(file.exists(training@files)))
  training_text <- paste(
    unlist(lapply(training@files, readLines, warn = FALSE)),
    collapse = "\n"
  )
  expect_match(training_text, "nvidia-smi")
  expect_match(training_text, "python --version")
  expect_match(training_text, "preprocess_evo2 --help")
  expect_match(training_text, "train_evo2 --help")
  expect_false(grepl("never-write-this", training_text, fixed = TRUE))
  expect_false(grepl("nor-this", training_text, fixed = TRUE))
  expect_false(grepl("NGC_API_KEY", training_text, fixed = TRUE))

  inference <- bionemo_setup(
    compute,
    target = "inference",
    path = file.path(workspace, "inference-setup")
  )
  inference_text <- paste(readLines(inference@files), collapse = "\n")
  expect_match(inference_text, "predict_evo2 --help")
  expect_match(inference_text, "infer_evo2 --help")
  expect_false(grepl("train_evo2", inference_text, fixed = TRUE))
})

test_that("setup maps Docker and Slurm Apptainer without executing", {
  workspace <- tempfile("bionemor-container-setup-")
  dir.create(workspace)

  docker <- bionemo_setup(
    bionemo_compute(
      backend = "local",
      engine = "container",
      image = "bionemo:2.6.3",
      workspace = workspace
    ),
    target = "training",
    path = file.path(workspace, "docker")
  )
  docker_text <- paste(readLines(docker@files), collapse = "\n")
  expect_match(docker_text, "docker run --rm --gpus all")
  expect_match(docker_text, "'bionemo:2.6.3'", fixed = TRUE)
  expect_match(docker_text, "train_evo2 --help")

  apptainer <- bionemo_setup(
    bionemo_compute(
      backend = "slurm",
      engine = "container",
      image = "/images/bionemo.sif",
      workspace = workspace,
      queue = "gpu"
    ),
    target = "inference",
    path = file.path(workspace, "apptainer")
  )
  apptainer_text <- paste(readLines(apptainer@files), collapse = "\n")
  expect_match(apptainer_text, "#SBATCH --partition 'gpu'", fixed = TRUE)
  expect_match(apptainer_text, "apptainer exec --nv")
  expect_match(apptainer_text, "predict_evo2 --help")
  expect_match(
    apptainer@commands,
    "sbatch --parsable --wait",
    fixed = TRUE
  )
})

test_that("setup can execute a local Python probe", {
  workspace <- tempfile("bionemor-execute-setup-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )

  plan <- bionemo_setup(
    bionemo_compute(workspace = workspace),
    target = "training",
    path = file.path(workspace, "setup"),
    execute = TRUE
  )

  expect_true(plan@executed)
})

test_that("doctor reports local Python checks as structured data", {
  workspace <- tempfile("bionemor-python-doctor-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  fake_bionemo_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  checkpoint <- make_checkpoint_dir(workspace)

  doctor <- bionemo_doctor(
    bionemo_compute(workspace = workspace),
    model = evo2("1b", checkpoint = checkpoint),
    target = "training",
    verbose = FALSE
  )

  expect_s3_class(doctor, "bionemor::BioNeMoDoctor")
  expect_true(doctor@ok)
  checks <- as.data.frame(doctor)
  expect_setequal(
    checks$check,
    c(
      "model checkpoint",
      "nvidia-smi",
      "python",
      "preprocess_evo2",
      "train_evo2"
    )
  )
  expect_true(all(checks$status == "pass"))
  expect_true("output" %in% names(checks))
})

test_that("doctor resolves relative checkpoints under the compute workspace", {
  workspace <- tempfile("bionemor-relative-doctor-")
  caller <- tempfile("bionemor-relative-caller-")
  bin <- tempfile("bionemor-runtime-")
  dir.create(workspace)
  dir.create(caller)
  fake_bionemo_runtime(bin)
  checkpoint <- make_checkpoint_dir(workspace)
  withr::local_dir(caller)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )

  doctor <- bionemo_doctor(
    bionemo_compute(workspace = workspace),
    model = evo2("1b", checkpoint = basename(checkpoint)),
    target = "training",
    verbose = FALSE
  )

  expect_true(doctor@ok)
  checks <- as.data.frame(doctor)
  model_check <- checks[checks$check == "model checkpoint", ]
  expect_equal(model_check$detail, normalizePath(checkpoint))
})

test_that("doctor retains full output without printing it in concise mode", {
  workspace <- tempfile("bionemor-doctor-output-")
  bin <- tempfile("bionemor-doctor-bin-")
  dir.create(workspace)
  dir.create(bin)
  for (command in c("nvidia-smi", "python", "predict_evo2", "infer_evo2")) {
    write_executable(
      file.path(bin, command),
      "printf 'FULL HELP OUTPUT FOR TEST\\n'"
    )
  }
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )

  concise <- bionemo_doctor(
    bionemo_compute(workspace = workspace),
    target = "inference",
    verbose = FALSE
  )
  expect_true(any(grepl(
    "FULL HELP OUTPUT FOR TEST",
    as.data.frame(concise)$output,
    fixed = TRUE
  )))
  printed <- paste(capture.output(print(concise)), collapse = "\n")
  expect_false(grepl("FULL HELP OUTPUT FOR TEST", printed, fixed = TRUE))

  verbose <- bionemo_doctor(
    bionemo_compute(workspace = workspace),
    target = "inference",
    verbose = TRUE
  )
  printed <- paste(capture.output(print(verbose)), collapse = "\n")
  expect_match(printed, "FULL HELP OUTPUT FOR TEST", fixed = TRUE)
})

test_that("doctor reports missing commands instead of requiring Python at load", {
  workspace <- tempfile("bionemor-missing-doctor-")
  empty_bin <- tempfile("bionemor-empty-bin-")
  dir.create(workspace)
  dir.create(empty_bin)
  withr::local_envvar(PATH = empty_bin)

  doctor <- bionemo_doctor(
    bionemo_compute(workspace = workspace),
    target = "training",
    verbose = FALSE
  )

  expect_false(doctor@ok)
  expect_true(all(as.data.frame(doctor)$status == "fail"))
  expect_match(
    paste(as.data.frame(doctor)$detail, collapse = "\n"),
    "not available"
  )
})

test_that("doctor covers local Docker and both Slurm engines", {
  workspace <- tempfile("bionemor-backend-doctor-")
  bin <- tempfile("bionemor-backend-bin-")
  dir.create(workspace)
  dir.create(bin)
  write_executable(file.path(bin, "docker"), "printf 'container ok\\n'")
  write_executable(file.path(bin, "sbatch"), "printf '789\\n'")
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )

  docker <- bionemo_doctor(
    bionemo_compute(
      workspace = workspace,
      engine = "container",
      image = "bionemo:2.6.3"
    ),
    target = "training",
    verbose = FALSE
  )
  expect_true(docker@ok)
  expect_true("container training probe" %in% as.data.frame(docker)$check)

  slurm_python <- bionemo_doctor(
    bionemo_compute(
      backend = "slurm",
      workspace = workspace,
      engine = "python"
    ),
    target = "inference",
    verbose = FALSE
  )
  expect_true(slurm_python@ok)
  expect_true("Slurm inference probe" %in% as.data.frame(slurm_python)$check)

  slurm_container <- bionemo_doctor(
    bionemo_compute(
      backend = "slurm",
      workspace = workspace,
      engine = "container",
      image = "/images/bionemo.sif"
    ),
    target = "training",
    verbose = FALSE
  )
  expect_true(slurm_container@ok)
  script <- as.data.frame(slurm_container)$artifact[
    as.data.frame(slurm_container)$check == "Slurm training probe"
  ]
  contents <- paste(readLines(script), collapse = "\n")
  expect_match(contents, "apptainer exec --nv")
  expect_match(contents, "train_evo2 --help")
})

test_that("Slurm doctor retains output redirected by the scheduler", {
  workspace <- tempfile("bionemor-slurm-doctor-output-")
  bin <- tempfile("bionemor-slurm-doctor-bin-")
  dir.create(workspace)
  dir.create(bin)
  write_executable(
    file.path(bin, "sbatch"),
    c(
      "script=\"${@: -1}\"",
      "log=\"$(sed -n \"s/^#SBATCH --output '\\\\(.*\\\\)'$/\\\\1/p\" \"$script\")\"",
      "printf 'FULL SLURM HELP OUTPUT\\n' > \"$log\"",
      "printf '789\\n'"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )

  doctor <- bionemo_doctor(
    bionemo_compute(
      backend = "slurm",
      workspace = workspace,
      engine = "python"
    ),
    target = "inference",
    verbose = FALSE
  )

  expect_true(doctor@ok)
  expect_match(
    as.data.frame(doctor)$output,
    "FULL SLURM HELP OUTPUT",
    fixed = TRUE
  )
  printed <- paste(capture.output(print(doctor)), collapse = "\n")
  expect_false(grepl("FULL SLURM HELP OUTPUT", printed, fixed = TRUE))
})
