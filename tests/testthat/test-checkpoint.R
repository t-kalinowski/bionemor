fake_converter <- function(bin, args_path) {
  converter <- file.path(bin, "evo2_convert_to_nemo2")
  writeLines(
    c(
      "#!/usr/bin/env bash",
      "set -euo pipefail",
      "printf '%s\\n' \"$@\" > \"$BIONEMOR_TEST_ARGS\"",
      "output=",
      "while [[ $# -gt 0 ]]; do",
      "  if [[ \"$1\" == \"--output-dir\" ]]; then",
      "    shift",
      "    output=\"$1\"",
      "  fi",
      "  shift",
      "done",
      "mkdir -p \"$output/context\" \"$output/weights\"",
      "printf 'model\\n' > \"$output/context/model.yaml\"",
      "printf '{}\\n' > \"$output/weights/metadata.json\""
    ),
    converter
  )
  Sys.chmod(converter, "0755")
  converter
}

test_that("public Hugging Face checkpoints are converted and manifested", {
  workspace <- tempfile("bionemor-checkpoint-")
  bin <- tempfile("bionemor-bin-")
  args_path <- tempfile("bionemor-args-")
  dir.create(workspace)
  dir.create(bin)
  fake_converter(bin, args_path)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_TEST_ARGS = args_path,
    NGC_API_KEY = NA,
    NGC_CLI_API_KEY = NA
  )

  checkpoint <- evo2_checkpoint(
    evo2("1b"),
    source = "hf://arcinstitute/savanna_evo2_1b_base",
    path = "checkpoints/evo2-1b-8k",
    compute = bionemo_compute(workspace = workspace)
  )

  expect_s3_class(checkpoint, "bionemor::BioNeMoCheckpoint")
  expect_equal(
    checkpoint_path(checkpoint),
    file.path(normalizePath(workspace), "checkpoints", "evo2-1b-8k")
  )
  expect_true(file.exists(file.path(checkpoint_path(checkpoint), "context", "model.yaml")))
  expect_true(file.exists(file.path(checkpoint_path(checkpoint), "weights", "metadata.json")))

  args <- readLines(args_path)
  expect_equal(
    args,
    c(
      "--model-path",
      "hf://arcinstitute/savanna_evo2_1b_base",
      "--model-size",
      "1b",
      "--output-dir",
      checkpoint_path(checkpoint)
    )
  )

  manifest <- checkpoint_manifest(checkpoint)
  expect_equal(manifest$family, "evo2")
  expect_equal(manifest$variant, "1b")
  expect_equal(manifest$source, "hf://arcinstitute/savanna_evo2_1b_base")
  expect_equal(manifest$profile, "bionemo-2.6.3")
  expect_equal(manifest$format, "nemo2")
  expect_match(checkpoint@provenance$command, "evo2_convert_to_nemo2")
  expect_false(grepl("NGC", checkpoint@provenance$command, fixed = TRUE))

  model <- evo2("1b", checkpoint = checkpoint)
  expect_identical(model@checkpoint, checkpoint)
  expect_error(evo2("7b", checkpoint = checkpoint), "checkpoint.*size")
})

test_that("an existing checkpoint is reused only when its manifest matches", {
  workspace <- tempfile("bionemor-existing-")
  bin <- tempfile("bionemor-bin-")
  args_path <- tempfile("bionemor-args-")
  dir.create(workspace)
  dir.create(bin)
  fake_converter(bin, args_path)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_TEST_ARGS = args_path
  )
  compute <- bionemo_compute(workspace = workspace)
  source <- "hf://arcinstitute/savanna_evo2_7b"

  first <- evo2_checkpoint(
    evo2("7b"),
    source,
    "checkpoint",
    compute
  )
  unlink(args_path)
  second <- evo2_checkpoint(
    evo2("7b"),
    source,
    "checkpoint",
    compute
  )

  expect_equal(checkpoint_manifest(second), checkpoint_manifest(first))
  expect_false(file.exists(args_path))
  expect_error(
    evo2_checkpoint(
      evo2("7b"),
      "hf://another/source",
      "checkpoint",
      compute
    ),
    "manifest.*source"
  )
})

test_that("checkpoint preparation enforces source and workspace boundaries", {
  workspace <- tempfile("bionemor-boundary-")
  outside <- tempfile("bionemor-outside-")
  dir.create(workspace)
  compute <- bionemo_compute(
    workspace = workspace,
    engine = "container",
    image = "bionemo:2.6.3"
  )

  expect_error(
    evo2_checkpoint(evo2("1b"), "https://example.test/model", "checkpoint", compute),
    "source"
  )
  expect_error(
    evo2_checkpoint(evo2("1b"), "hf://example/model", outside, compute),
    "workspace"
  )
  expect_error(
    evo2_checkpoint(
      evo2("1b"),
      "hf://example/model",
      workspace,
      bionemo_compute(workspace = workspace)
    ),
    "workspace itself"
  )
  expect_error(
    evo2_checkpoint(
      evo2("1b"),
      "hf://example/model",
      .Platform$file.sep,
      bionemo_compute(workspace = workspace)
    ),
    "filesystem root"
  )
  expect_true(dir.exists(workspace))

  withr::local_envvar(
    NGC_API_KEY = NA,
    NGC_CLI_API_KEY = NA
  )
  expect_error(
    evo2_checkpoint(
      evo2("1b"),
      "ngc://org/team/model:1",
      "checkpoint",
      bionemo_compute(workspace = workspace)
    ),
    "NGC_CLI_API_KEY or NGC_API_KEY"
  )

  source <- make_checkpoint_dir(workspace, "source")
  marker <- file.path(source, "original-source")
  writeLines("preserve", marker)
  expect_error(
    evo2_checkpoint(
      evo2("1b"),
      source,
      source,
      bionemo_compute(workspace = workspace),
      overwrite = TRUE
    ),
    "source and destination"
  )
  expect_true(file.exists(marker))
})

test_that("Slurm checkpoint preparation runs one bounded allocation script", {
  workspace <- tempfile("bionemor-slurm-checkpoint-")
  bin <- tempfile("bionemor-slurm-bin-")
  args_path <- tempfile("bionemor-sbatch-args-")
  converter_args <- tempfile("bionemor-converter-args-")
  dir.create(workspace)
  dir.create(bin)
  fake_converter(bin, converter_args)
  write_executable(
    file.path(bin, "sbatch"),
    c(
      "printf '%s\\n' \"$@\" > \"$BIONEMOR_SBATCH_ARGS\"",
      "script=\"${@: -1}\"",
      "bash \"$script\"",
      "printf '456\\n'"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_TEST_ARGS = converter_args,
    BIONEMOR_SBATCH_ARGS = args_path
  )

  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    "hf://arcinstitute/savanna_evo2_7b",
    "checkpoints/evo2-7b-1m",
    bionemo_compute(
      backend = "slurm",
      engine = "python",
      workspace = workspace,
      queue = "gpu",
      account = "science",
      walltime = "00:20:00"
    )
  )

  expect_true(dir.exists(checkpoint_path(checkpoint)))
  sbatch_args <- readLines(args_path)
  expect_true("--parsable" %in% sbatch_args)
  expect_true("--wait" %in% sbatch_args)
  script <- sbatch_args[[length(sbatch_args)]]
  contents <- paste(readLines(script), collapse = "\n")
  expect_match(contents, "#SBATCH --partition 'gpu'", fixed = TRUE)
  expect_match(contents, "#SBATCH --account 'science'", fixed = TRUE)
  expect_match(contents, "#SBATCH --time '00:20:00'", fixed = TRUE)
  expect_match(contents, "evo2_convert_to_nemo2")
})

test_that("relative local checkpoint sources resolve under the compute workspace", {
  workspace <- tempfile("bionemor-relative-source-")
  caller <- tempfile("bionemor-caller-")
  bin <- tempfile("bionemor-bin-")
  args_path <- tempfile("bionemor-args-")
  dir.create(workspace)
  dir.create(caller)
  dir.create(file.path(workspace, "source"))
  dir.create(bin)
  fake_converter(bin, args_path)
  withr::local_dir(caller)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_TEST_ARGS = args_path
  )

  checkpoint <- evo2_checkpoint(
    evo2("1b"),
    source = "source",
    path = "converted",
    compute = bionemo_compute(workspace = workspace)
  )

  expect_equal(
    checkpoint@source,
    normalizePath(file.path(workspace, "source"))
  )
  expect_equal(
    readLines(args_path)[[2L]],
    normalizePath(file.path(workspace, "source"))
  )
})

test_that("NGC conversion forwards only the required credential without storing it", {
  workspace <- tempfile("bionemor-ngc-container-")
  bin <- tempfile("bionemor-docker-bin-")
  args_path <- tempfile("bionemor-docker-args-")
  secret <- "bionemor-test-secret"
  cli_secret <- "bionemor-cli-test-secret"
  dir.create(workspace)
  dir.create(bin)
  write_executable(
    file.path(bin, "docker"),
    c(
      "printf '%s\\n' \"$@\" > \"$BIONEMOR_DOCKER_ARGS\"",
      "mkdir -p \"$BIONEMOR_EXPECTED_OUTPUT/context\" \"$BIONEMOR_EXPECTED_OUTPUT/weights\"",
      "printf 'model\\n' > \"$BIONEMOR_EXPECTED_OUTPUT/context/model.yaml\"",
      "printf '{}\\n' > \"$BIONEMOR_EXPECTED_OUTPUT/weights/metadata.json\""
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_DOCKER_ARGS = args_path,
    BIONEMOR_EXPECTED_OUTPUT = file.path(workspace, "checkpoint"),
    NGC_API_KEY = secret,
    NGC_CLI_API_KEY = cli_secret
  )

  checkpoint <- evo2_checkpoint(
    evo2("1b"),
    source = "ngc://org/team/model:1",
    path = "checkpoint",
    compute = bionemo_compute(
      workspace = workspace,
      engine = "container",
      image = "bionemo:2.6.3"
    )
  )

  args <- readLines(args_path)
  expect_true(any(args == "-e"))
  expect_true(any(args == "NGC_CLI_API_KEY"))
  expect_true(any(grepl(
    "download_bionemo_data.*org/team/model:1",
    args
  )))
  expect_false(any(grepl("evo2_convert_to_nemo2", args, fixed = TRUE)))
  expect_false(any(grepl(secret, args, fixed = TRUE)))
  expect_false(any(grepl(cli_secret, args, fixed = TRUE)))
  expect_false(grepl(
    secret,
    paste(capture.output(str(checkpoint)), collapse = "\n"),
    fixed = TRUE
  ))
  expect_false(grepl(
    cli_secret,
    paste(capture.output(str(checkpoint)), collapse = "\n"),
    fixed = TRUE
  ))
  expect_false(grepl(
    secret,
    paste(readLines(
      file.path(checkpoint_path(checkpoint), "bionemor-checkpoint.json")
    ), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("NGC resources are downloaded as existing NeMo2 checkpoints", {
  workspace <- tempfile("bionemor-ngc-download-")
  bin <- tempfile("bionemor-ngc-download-bin-")
  downloaded <- tempfile("bionemor-ngc-downloaded-")
  args_path <- tempfile("bionemor-ngc-download-args-")
  dir.create(workspace)
  dir.create(bin)
  make_checkpoint_dir(dirname(downloaded), basename(downloaded))
  write_executable(
    file.path(bin, "download_bionemo_data"),
    c(
      "test \"$NGC_CLI_API_KEY\" = \"$BIONEMOR_EXPECTED_NGC_KEY\"",
      "test -z \"${NGC_API_KEY:-}\"",
      "printf '%s\\n' \"$@\" > \"$BIONEMOR_NGC_ARGS\"",
      "printf '%s\\n' \"$BIONEMOR_NGC_SOURCE\""
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_NGC_ARGS = args_path,
    BIONEMOR_NGC_SOURCE = downloaded,
    BIONEMOR_EXPECTED_NGC_KEY = "bionemor-legacy-key",
    NGC_CLI_API_KEY = NA,
    NGC_API_KEY = "bionemor-legacy-key"
  )

  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    source = "ngc://evo2/7b-1m:1.0",
    path = "checkpoint",
    compute = bionemo_compute(workspace = workspace)
  )

  expect_equal(readLines(args_path), "evo2/7b-1m:1.0")
  expect_true(file.exists(file.path(
    checkpoint_path(checkpoint),
    "context",
    "model.yaml"
  )))
  expect_match(
    checkpoint@provenance$command,
    "download_bionemo_data",
    fixed = TRUE
  )
  expect_false(grepl(
    "evo2_convert_to_nemo2",
    checkpoint@provenance$command,
    fixed = TRUE
  ))
})

test_that("checkpoint conversion errors redact credential values", {
  workspace <- tempfile("bionemor-ngc-error-")
  bin <- tempfile("bionemor-ngc-error-bin-")
  secret <- "bionemor-error-secret"
  dir.create(workspace)
  dir.create(bin)
  write_executable(
    file.path(bin, "download_bionemo_data"),
    c(
      "printf 'downloader leaked %s\\n' \"$NGC_CLI_API_KEY\" >&2",
      "exit 1"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    NGC_API_KEY = NA,
    NGC_CLI_API_KEY = secret
  )

  error <- tryCatch(
    evo2_checkpoint(
      evo2("1b"),
      source = "ngc://org/team/model:1",
      path = "checkpoint",
      compute = bionemo_compute(workspace = workspace)
    ),
    error = identity
  )

  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "checkpoint conversion failed")
  expect_false(grepl(secret, conditionMessage(error), fixed = TRUE))
})
