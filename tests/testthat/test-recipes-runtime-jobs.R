guard_slurm_recipe_runtime <- function(bin, runtime) {
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  commands <- c(
    "bionemor-evo2-helper",
    "infer_evo2",
    "predict_evo2",
    "preprocess_evo2",
    "train_evo2",
    "evo2_convert_savanna_to_mbridge",
    "evo2_convert_nemo2_to_mbridge",
    "evo2_export_mbridge_to_vortex",
    "evo2_remove_optimizer"
  )
  for (command in commands) {
    write_executable(
      file.path(bin, command),
      c(
        "if [[ \"${BIONEMOR_IN_SLURM:-}\" != \"1\" ]]; then",
        "  printf 'runtime command executed outside Slurm\\n' >&2",
        "  exit 97",
        "fi",
        "if [[ \"${1:-}\" == \"--help\" ]]; then",
        "  printf 'help\\n'",
        "  exit 0",
        "fi",
        paste("exec", shQuote(file.path(runtime, command)), "\"$@\"")
      )
    )
  }
  write_executable(
    file.path(bin, "apptainer"),
    c(
      "if [[ \"${BIONEMOR_IN_SLURM:-}\" != \"1\" ]]; then",
      "  printf 'apptainer executed outside Slurm\\n' >&2",
      "  exit 97",
      "fi",
      "printf '%s\\n' \"$@\" >> \"$BIONEMOR_APPTAINER_LOG\"",
      "[[ \"$1\" == \"exec\" ]]",
      "shift",
      "while [[ $# -gt 0 ]]; do",
      "  case \"$1\" in",
      "    --nv) shift ;;",
      "    --bind|--pwd) shift 2 ;;",
      "    *) shift; exec \"$@\" ;;",
      "  esac",
      "done",
      "exit 2"
    )
  )
  invisible(bin)
}

fake_synchronous_slurm_runtime <- function(bin) {
  dir.create(bin, recursive = TRUE, showWarnings = FALSE)
  write_executable(
    file.path(bin, "sbatch"),
    c(
      "[[ \"$1\" == \"--parsable\" ]]",
      "script=\"${@: -1}\"",
      "printf 'sbatch|%s\\n' \"$script\" >> \"$BIONEMOR_SLURM_LOG\"",
      "if [[ -n \"${BIONEMOR_MUTATE_SIF:-}\" && ! -f \"${BIONEMOR_SLURM_LOG}.mutated\" ]]; then",
      "  printf 'mutated\\n' >> \"$BIONEMOR_MUTATE_SIF\"",
      "  : > \"${BIONEMOR_SLURM_LOG}.mutated\"",
      "fi",
      "if BIONEMOR_IN_SLURM=1 bash \"$script\"; then",
      "  status=0",
      "else",
      "  status=$?",
      "fi",
      "printf '%s\\n' \"$status\" > \"${BIONEMOR_SLURM_LOG}.state\"",
      "printf '123\\n'"
    )
  )
  write_executable(
    file.path(bin, "sacct"),
    c(
      "printf 'sacct|%s\\n' \"$*\" >> \"$BIONEMOR_SLURM_LOG\"",
      "status=$(<\"${BIONEMOR_SLURM_LOG}.state\")",
      "if [[ \"$status\" == \"0\" ]]; then",
      "  printf '123|COMPLETED|0:0\\n'",
      "else",
      "  printf '123|FAILED|%s:0\\n' \"$status\"",
      "fi"
    )
  )
  write_executable(
    file.path(bin, "scancel"),
    "printf 'scancel|%s\\n' \"$*\" >> \"$BIONEMOR_SLURM_LOG\""
  )
  invisible(bin)
}

test_that("capabilities and doctor describe the external recipe runtime", {
  workspace <- tempfile("bionemor-doctor-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  capabilities <- bionemo_capabilities(compute, refresh = TRUE)
  expect_equal(capabilities$protocol_version, 1L)
  expect_equal(capabilities$recipe_version, "2.4")
  expect_equal(capabilities$recipe_revision, evo2_recipe()@revision)
  expect_true(capabilities$commands$infer_evo2)
  expect_true(capabilities$commands$train_evo2)

  doctor <- bionemo_doctor(
    compute,
    model,
    target = "all",
    verbose = FALSE
  )
  checks <- as.data.frame(doctor)
  expect_true(doctor@ok)
  expect_true(all(checks$status == "pass"))
  expect_true(all(c(
    "backend",
    "host tools",
    "workspace",
    "helper protocol",
    "recipe",
    "runtime Python",
    "runtime PyTorch",
    "runtime CUDA",
    "runtime Transformer Engine",
    "runtime Megatron Bridge",
    "runtime BioNeMo",
    "GPU",
    "base image",
    "checkpoint storage",
    "model compatibility",
    "model checkpoint"
  ) %in% checks$check))
})

test_that("Slurm doctor requires the scheduler command set", {
  workspace <- tempfile("bionemor-slurm-doctor-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-slurm-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_synchronous_slurm_runtime(bin)
  withr::local_envvar(c(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_SLURM_LOG = log
  ))
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace
  )

  doctor <- bionemo_doctor(compute, target = "inference")
  backend <- as.data.frame(doctor)
  backend <- backend[backend$check == "backend", , drop = FALSE]
  expect_true(doctor@ok)
  expect_match(backend$detail, "sbatch, sacct, scancel", fixed = TRUE)

  unlink(file.path(bin, "sacct"))
  doctor <- bionemo_doctor(compute, target = "inference")
  backend <- as.data.frame(doctor)
  backend <- backend[backend$check == "backend", , drop = FALSE]
  expect_false(doctor@ok)
  expect_match(backend$detail, "sacct is not available", fixed = TRUE)
})

test_that("Slurm installation probes the runtime inside allocations", {
  workspace <- tempfile("bionemor-slurm-install-")
  runtime <- tempfile("bionemor-runtime-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-slurm-log-")
  dir.create(workspace)
  fake_recipes_runtime(runtime)
  suppressWarnings(fake_bionemo_runtime(runtime))
  guard_slurm_recipe_runtime(bin, runtime)
  fake_synchronous_slurm_runtime(bin)
  withr::local_envvar(c(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_SLURM_LOG = log
  ))
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace
  )

  installed <- bionemo_install(compute)

  expect_equal(installed@config$capabilities$recipe_version, "2.4")
  invocations <- readLines(log, warn = FALSE)
  expect_equal(sum(startsWith(invocations, "sbatch|")), 9L)
  expect_equal(sum(startsWith(invocations, "sacct|")), 9L)
  scripts <- sub("^sbatch\\|", "", invocations[startsWith(
    invocations,
    "sbatch|"
  )])
  expect_true(all(startsWith(scripts, compute@workspace)))
})

test_that("Slurm installation records a local SIF SHA-256 digest", {
  workspace <- tempfile("bionemor-slurm-container-")
  runtime <- tempfile("bionemor-runtime-")
  bin <- tempfile("bionemor-bin-")
  slurm_log <- tempfile("bionemor-slurm-log-")
  apptainer_log <- tempfile("bionemor-apptainer-log-")
  image <- file.path(workspace, "evo2.sif")
  dir.create(workspace)
  writeLines("fake sif", image, useBytes = TRUE)
  fake_recipes_runtime(runtime)
  suppressWarnings(fake_bionemo_runtime(runtime))
  guard_slurm_recipe_runtime(bin, runtime)
  fake_synchronous_slurm_runtime(bin)
  withr::local_envvar(c(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_SLURM_LOG = slurm_log,
    BIONEMOR_APPTAINER_LOG = apptainer_log
  ))
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "container",
    image = image,
    workspace = workspace
  )

  installed <- bionemo_install(compute)

  expected <- paste0(
    "sha256:",
    "a0c4adae0162e3e9d6fc382a0b00eb8ad4589f175c96422f2c43c3be8b4267f4"
  )
  expect_equal(installed@image_digest, expected)
  expect_equal(installed@config$capabilities$image_digest, expected)
  expect_equal(
    sum(startsWith(readLines(slurm_log, warn = FALSE), "sbatch|")),
    9L
  )
  expect_true(image %in% readLines(apptainer_log, warn = FALSE))
  invocations <- readLines(slurm_log, warn = FALSE)
  scripts <- sub("^sbatch\\|", "", invocations[startsWith(
    invocations,
    "sbatch|"
  )])
  contents <- vapply(
    scripts,
    function(path) paste(readLines(path, warn = FALSE), collapse = "\n"),
    character(1)
  )
  expect_true(all(grepl("sha256sum", contents, fixed = TRUE)))
  expect_true(all(grepl(sub("^sha256:", "", expected), contents, fixed = TRUE)))
})

test_that("Slurm probes reject a SIF changed after digest resolution", {
  workspace <- tempfile("bionemor-slurm-mutable-sif-")
  runtime <- tempfile("bionemor-runtime-")
  bin <- tempfile("bionemor-bin-")
  slurm_log <- tempfile("bionemor-slurm-log-")
  apptainer_log <- tempfile("bionemor-apptainer-log-")
  image <- file.path(workspace, "evo2.sif")
  dir.create(workspace)
  writeLines("fake sif", image, useBytes = TRUE)
  fake_recipes_runtime(runtime)
  guard_slurm_recipe_runtime(bin, runtime)
  fake_synchronous_slurm_runtime(bin)
  withr::local_envvar(c(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_SLURM_LOG = slurm_log,
    BIONEMOR_APPTAINER_LOG = apptainer_log,
    BIONEMOR_MUTATE_SIF = image
  ))
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "container",
    image = image,
    workspace = workspace
  )

  expect_error(
    bionemo_capabilities(compute, refresh = TRUE),
    "Slurm SIF SHA-256 does not match installed digest"
  )
  expect_false(file.exists(apptainer_log))
})

test_that("container probes use GPUs and mount the workspace", {
  workspace <- tempfile("bionemor-container-probe-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-container-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_container_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_CONTAINER_LOG = log
  )
  compute <- bionemo_compute(
    workspace = workspace,
    image = paste0("example/evo2@sha256:", strrep("a", 64L))
  )
  capabilities <- bionemo_capabilities(compute, refresh = TRUE)
  expect_equal(capabilities$runtime$gpu_count, 1L)
  invocation <- readLines(log, warn = FALSE)
  expect_true(all(c(
    "docker",
    "run",
    "--rm",
    "--gpus",
    "all",
    "-v",
    paste0(compute@workspace, ":", compute@workspace),
    "-w",
    compute@workspace,
    compute@image,
    "bionemor-evo2-helper",
    "capabilities",
    "--json"
  ) %in% invocation))

  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  doctor <- bionemo_doctor(compute, model, target = "inference")
  expect_true(doctor@ok)

  fake_container_runtime(bin, "podman")
  podman_compute <- bionemo_compute(
    workspace = workspace,
    image = paste0("example/evo2@sha256:", strrep("b", 64L)),
    config = list(container_engine = "podman")
  )
  generated <- evo2_generate(
    model,
    "ACGT",
    podman_compute,
    num_tokens = 4L
  )
  expect_s3_class(generated, "evo2_generation")
  expect_equal(readLines(log, warn = FALSE)[[1L]], "podman")
})

test_that("installation verifies an existing recipe image", {
  workspace <- tempfile("bionemor-container-install-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-container-log-")
  recipe_log <- tempfile("bionemor-recipe-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_container_runtime(bin, "podman")
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_CONTAINER_LOG = log,
    BIONEMOR_FAKE_LOG = recipe_log
  )
  compute <- bionemo_compute(
    workspace = workspace,
    image = "example/evo2:verified",
    config = list(container_engine = "podman")
  )

  installed <- bionemo_install(compute, pull = FALSE)

  expect_equal(installed@image_digest, paste0("sha256:", strrep("c", 64L)))
  install_invocation <- readLines(log, warn = FALSE)
  expect_true(installed@image_digest %in% install_invocation)
  expect_false(installed@image %in% install_invocation)
  expect_equal(installed@config$capabilities$recipe_version, "2.4")
  expect_equal(
    installed@config$capabilities$recipe_revision,
    evo2_recipe()@revision
  )

  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_generate(
    model,
    "ACGT",
    installed,
    num_tokens = 4L,
    async = TRUE
  )
  reopened <- bionemo_job(job_path(job))
  expect_equal(reopened@compute@image_digest, installed@image_digest)
  expect_s3_class(
    job_wait(reopened, poll = 0.01, timeout = 10),
    "evo2_generation"
  )
  invocation <- readLines(log, warn = FALSE)
  expect_true(installed@image_digest %in% invocation)
  expect_false(installed@image %in% invocation)
})

test_that("sequence contracts map strands, layers, and portable artifacts", {
  workspace <- tempfile("bionemor-sequences-")
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

  expect_equal(
    evo2_phylo_tag(domain = "Bacteria", species = "E. coli"),
    paste0(
      "|D__BACTERIA;P__NONE;C__NONE;O__NONE;",
      "F__NONE;G__NONE;S__E. COLI|"
    )
  )
  expect_error(
    evo2_phylo_tag(species = "E. coli;K-12"),
    "must not contain separators"
  )

  scores <- evo2_score(
    model,
    c(example = "aagc"),
    compute,
    strand = "both"
  )
  expect_equal(scores$strand, "both")
  expect_true(is.finite(scores$forward_score))
  expect_true(is.finite(scores$reverse_score))
  score_run <- attr(scores, "provenance")$run_path
  fasta <- readLines(
    file.path(score_run, "inputs", "sequences.fasta"),
    warn = FALSE
  )
  expect_true("AAGC" %in% fasta)
  expect_true("GCTT" %in% fasta)

  embeddings <- evo2_embed(
    model,
    c(example = "ACGT"),
    compute,
    layer = 1L
  )
  expect_s3_class(embeddings, "evo2_embeddings")
  invocation <- readLines(log)
  layer_flag <- which(invocation == "--embedding-layer")
  expect_true(length(layer_flag) >= 1L)
  expect_equal(invocation[layer_flag[[length(layer_flag)]] + 1L], "0")

  unpooled <- evo2_embed(
    model,
    c(example = "ACGT"),
    compute,
    pool = "none",
    output = "embeddings/unpooled.parquet"
  )
  expect_s3_class(unpooled, "bionemor::BioNeMoArtifact")
  expect_equal(unpooled@shape, c(4L, 4L))
  expect_identical(
    unpooled@schema,
    list(
      id = "string",
      position = "int64",
      embedding = "list<double>",
      strand = "string"
    )
  )

  profile <- evo2_profile(
    model,
    c(example = "ACGT"),
    compute,
    output = "profiles/example.parquet"
  )
  expect_s3_class(profile, "bionemor::BioNeMoArtifact")
  expect_equal(profile@format, "parquet")
  expect_true(file.exists(profile@path))
  expect_false(endsWith(profile@path, ".pt"))
  invocation <- readLines(log)
  profile_call <- tail(which(invocation == "predict_evo2"), 1L)
  expect_true("--prepend-bos" %in% invocation[profile_call:length(invocation)])

  before <- length(invocation)
  expect_error(
    evo2_embed(
      model,
      "ACGT",
      bionemo_compute(
        engine = "external",
        workspace = workspace,
        gpus = 2L
      ),
      control = evo2_inference_control(context_parallel_size = 2L)
    ),
    "context parallelism is not supported for embeddings"
  )
  expect_length(readLines(log), before)

  for (strand in c("reverse", "both")) {
    expect_error(
      evo2_score(
        model,
        "AC?T",
        compute,
        strand = strand,
        normalize = "none"
      ),
      "reverse-complement input may contain only uppercase IUPAC DNA symbols"
    )
    expect_length(readLines(log), before)
  }
})

test_that("semantic inference controls map to exact supported recipe flags", {
  workspace <- tempfile("bionemor-controls-")
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

  generation_control <- evo2_inference_control(
    max_sequence_length = 1024L,
    max_batch_size = 2L,
    precision = "fp8",
    vortex_style_fp8 = "no",
    cuda_graphs = "none",
    subquadratic_ops = TRUE,
    chunked_prefill = TRUE,
    dynamic_max_tokens = 512L,
    dynamic_block_size = 128L
  )
  evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    control = generation_control
  )
  invocation <- readLines(log)
  expect_true(all(c(
    "--max-seq-length", "1024",
    "--max-batch-size", "2",
    "--mixed-precision-recipe",
    "bf16_with_fp8_current_scaling_mixed",
    "--cuda-graph-impl", "none",
    "--use-subquadratic-ops",
    "--enable-chunked-prefill",
    "--inference-dynamic-batching-max-tokens", "512",
    "--inference-dynamic-batching-block-size", "128"
  ) %in% invocation))
  expect_false("--vortex-style-fp8" %in% invocation)

  writeLines(character(), log)
  vortex_control <- evo2_inference_control(
    precision = "fp8",
    vortex_style_fp8 = "yes"
  )
  evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    control = vortex_control
  )
  invocation <- readLines(log)
  vortex <- which(invocation == "--vortex-style-fp8")
  expect_length(vortex, 1L)
  expect_false(
    vortex[[1L]] < length(invocation) &&
      identical(invocation[[vortex[[1L]] + 1L]], "True")
  )
  expect_true(all(c(
    "--mixed-precision-recipe",
    "bf16_mixed"
  ) %in% invocation))

  writeLines(character(), log)
  prediction_control <- evo2_inference_control(extra = list(
    no_sequence_parallel = TRUE,
    min_length = 4L
  ))
  evo2_score(
    model,
    "ACGT",
    compute,
    batch_size = 2L,
    control = prediction_control
  )
  invocation <- readLines(log)
  expect_true(all(c(
    "--no-sequence-parallel",
    "--min-length", "4",
    "--micro-batch-size", "2"
  ) %in% invocation))

  before <- length(invocation)
  for (setting in c(
    "eden_tokenizer",
    "hybrid_override_pattern",
    "num_layers",
    "seq_len_interpolation_factor"
  )) {
    value <- switch(
      setting,
      eden_tokenizer = TRUE,
      hybrid_override_pattern = "SDH*",
      num_layers = 2L,
      seq_len_interpolation_factor = 2
    )
    control <- evo2_inference_control(
      extra = stats::setNames(list(value), setting)
    )
    expect_error(
      evo2_score(model, "ACGT", compute, control = control),
      "not applied by the pinned prediction entry point",
      class = "BN_PROTOCOL"
    )
    expect_length(readLines(log), before)
  }
  expect_error(
    evo2_score(
      model,
      "ACGT",
      compute,
      mask_phylogenetic_tags = TRUE
    ),
    "mask_phylogenetic_tags must be FALSE"
  )
  expect_length(readLines(log), before)
  expect_error(
    evo2_score(
      model,
      "ACGT",
      compute,
      control = evo2_inference_control(micro_batch_size = 2L)
    ),
    "use the task-specific batch_size argument"
  )
  expect_length(readLines(log), before)
  expect_error(
    evo2_generate(
      model,
      "ACGT",
      compute,
      control = prediction_control
    ),
    "not supported for generation"
  )
})

test_that("local jobs preserve wait and cancellation boundaries", {
  workspace <- tempfile("bionemor-cancel-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  secret <- "credential-that-must-not-persist"
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_REJECT_CREDENTIALS = "true",
    NGC_API_KEY = secret,
    NGC_CLI_API_KEY = paste0(secret, "-cli"),
    HF_TOKEN = paste0(secret, "-hf")
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  generated <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L
  )
  expect_s3_class(generated, "evo2_generation")
  run_path <- attr(generated, "provenance")$run_path
  files <- list.files(run_path, recursive = TRUE, full.names = TRUE)
  files <- files[!dir.exists(files)]
  contents <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  expect_false(any(grepl(secret, contents, fixed = TRUE)))

  pid_file <- tempfile("bionemor-child-pid-")
  withr::local_envvar(
    BIONEMOR_FAKE_DELAY = "60",
    BIONEMOR_FAKE_PID_FILE = pid_file
  )
  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "cancel-me",
    async = TRUE
  )
  expect_error(
    job_wait(job, poll = 0.001, timeout = 0.01),
    "timed out waiting"
  )
  expect_true(job_status(job) %in% c("starting", "running"))
  deadline <- Sys.time() + 2
  while (!file.exists(pid_file) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(file.exists(pid_file))
  child_pid <- as.integer(readLines(pid_file, warn = FALSE))

  reopened <- bionemo_job(job_path(job))
  job_cancel(reopened, force = TRUE)
  alive <- function(pid) {
    isTRUE(tools::pskill(pid, signal = 0L))
  }
  deadline <- Sys.time() + 2
  while (alive(child_pid) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_false(alive(child_pid))
  expect_equal(job_status(reopened), "cancelled")
  expect_true(file.exists(file.path(job_path(job), "cancel.request")))
})

test_that("default local cancellation terminates a TERM-resistant process tree", {
  workspace <- tempfile("bionemor-cancel-term-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  pid_file <- tempfile("bionemor-cancel-term-pid-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  write_executable(
    file.path(bin, "torchrun"),
    c(
      "trap '' TERM",
      "printf '%s\\n' \"$$\" > \"$BIONEMOR_FAKE_PID_FILE\"",
      "while true; do sleep 60; done"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_FAKE_PID_FILE = pid_file
  )
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(
      recipe_version = evo2_recipe()@recipe_version,
      recipe_revision = evo2_recipe()@revision,
      commands = list(infer_evo2 = TRUE),
      features = list(generation_jsonl = TRUE),
      runtime = list(
        gpu_count = 1L,
        gpus = data.frame(compute_capability_major = 9L)
      )
    ))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "cancel-term-resistant",
    async = TRUE
  )
  withr::defer({
    if (file.exists(pid_file)) {
      tools::pskill(
        as.integer(readLines(pid_file, warn = FALSE)),
        signal = 9L
      )
    }
  })
  deadline <- Sys.time() + 2
  while (!file.exists(pid_file) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_true(file.exists(pid_file))
  pid <- as.integer(readLines(pid_file, warn = FALSE))

  job_cancel(bionemo_job(job_path(job)))

  expect_false(isTRUE(tools::pskill(pid, signal = 0L)))
  expect_equal(job_status(bionemo_job(job_path(job))), "cancelled")
  expect_true(file.exists(file.path(job_path(job), "manifest.json")))
})

test_that("reopened jobs preserve custom recipe provenance", {
  workspace <- tempfile("bionemor-custom-recipe-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  revision <- strrep("a", 40L)
  recipe <- evo2_recipe(
    revision = revision,
    repository = "https://example.com/custom/bionemo-recipes",
    base_image = "example.com/custom/pytorch:26.06"
  )
  capabilities <- bionemo_capabilities(
    bionemo_compute(engine = "external", workspace = workspace),
    refresh = TRUE
  )
  capabilities$recipe_revision <- revision
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    recipe = recipe,
    config = list(capabilities = capabilities)
  )
  source <- make_mbridge_checkpoint(workspace, name = "custom-source")
  checkpoint <- evo2_checkpoint(
    evo2("7b"),
    source = source,
    format = "mbridge",
    path = "checkpoints/custom-recipe",
    compute = compute
  )
  model <- evo2("7b", checkpoint = checkpoint)

  job <- evo2_generate(
    model,
    "ACGT",
    compute,
    num_tokens = 4L,
    name = "custom-recipe-provenance",
    async = TRUE
  )
  job_wait(job, poll = 0.01, timeout = 10)
  reopened <- bionemo_job(job_path(job))

  expect_equal(reopened@compute@recipe@repository, recipe@repository)
  expect_equal(reopened@compute@recipe@revision, revision)
  expect_equal(reopened@compute@recipe@base_image, recipe@base_image)
  expect_false(reopened@compute@recipe@verified)
  job_result(reopened)
  manifest <- jsonlite::read_json(
    file.path(job_path(reopened), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_equal(manifest$recipe$repository, recipe@repository)
  expect_equal(manifest$recipe$revision, revision)
  expect_equal(manifest$recipe$base_image, recipe@base_image)
  expect_false(manifest$recipe$verified)
})

test_that("Slurm failure manifests retain the scheduler exit status", {
  workspace <- tempfile("bionemor-slurm-failure-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE = "FAILED",
    BIONEMOR_FAKE_EXIT = "42:0"
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-failure-status",
    async = TRUE
  )

  expect_equal(job_status(job), "failed")
  manifest <- jsonlite::read_json(
    file.path(job_path(job), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_equal(manifest$exit_status, 42L)
  expect_error(job_result(job), class = "BN_UPSTREAM")
})

test_that("Slurm accounting lag preserves the submitted state", {
  workspace <- tempfile("bionemor-slurm-accounting-lag-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  write_executable(file.path(bin, "sacct"), "exit 0")
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-accounting-lag",
    async = TRUE
  )

  expect_equal(job_status(job), "submitted")
  expect_error(
    job_wait(job, poll = 0.01, timeout = 0.05),
    "timed out waiting",
    fixed = TRUE
  )
})

test_that("Slurm jobs use one quoted script and scheduler cancellation", {
  workspace <- file.path(tempdir(), "bionemor slurm workspace")
  bin <- tempfile("bionemor-bin-")
  cancel_args <- tempfile("bionemor-cancel-")
  state_file <- tempfile("bionemor-slurm-state-")
  dir.create(workspace, recursive = TRUE, showWarnings = FALSE)
  writeLines("RUNNING", state_file)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE_FILE = state_file,
    BIONEMOR_CANCEL_ARGS = cancel_args
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    gpus = 2L,
    queue = "gpu",
    config = list(capabilities = list(runtime = list(
      gpu_count = 2L,
      gpus = data.frame(compute_capability_major = c(9L, 9L))
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_score(model, "ACGT", compute, async = TRUE)
  script <- file.path(job_path(job), "slurm.sh")
  contents <- readLines(script, warn = FALSE)
  expect_true(file.exists(script))
  expect_match(paste(contents, collapse = "\n"), "#SBATCH --gpus 2", fixed = TRUE)
  expect_match(
    paste(contents, collapse = "\n"),
    shQuote(compute@workspace),
    fixed = TRUE
  )
  expect_equal(job_status(job), "running")

  job_cancel(job)
  expect_equal(job_status(job), "cancelled")
  expect_equal(readLines(cancel_args, warn = FALSE), "123")
})

test_that("Slurm submission cannot overwrite a job that already finished", {
  workspace <- tempfile("bionemor-slurm-fast-finish-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  slurm_log <- tempfile("bionemor-slurm-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_synchronous_slurm_runtime(bin)
  write_executable(
    file.path(bin, "sbatch"),
    c(
      "[[ \"$1\" == \"--parsable\" ]]",
      "script=\"${@: -1}\"",
      "SLURM_JOB_ID=123 BIONEMOR_IN_SLURM=1 bash \"$script\"",
      "printf '123\\n'"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log,
    BIONEMOR_SLURM_LOG = slurm_log
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-fast-finish",
    async = TRUE
  )

  expect_equal(job_status(job), "succeeded")
  state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
  expect_equal(state$state, "succeeded")
  expect_equal(state$backend_id, "123")
  expect_equal(job_status(bionemo_job(job_path(job))), "succeeded")
})

test_that("a stale Slurm query cannot overwrite a terminal state", {
  workspace <- tempfile("bionemor-slurm-stale-query-")
  bin <- tempfile("bionemor-bin-")
  state_replacement <- tempfile("bionemor-terminal-state-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- withr::with_envvar(
    c(BIONEMOR_FAKE_STATE = "RUNNING"),
    evo2_score(
      model,
      "ACGT",
      compute,
      name = "slurm-stale-query",
      async = TRUE
    )
  )
  state <- jsonlite::read_json(
    file.path(job_path(job), "state.json"),
    simplifyVector = FALSE
  )
  state$state <- "failed"
  state$exit_status <- 9L
  jsonlite::write_json(state, state_replacement, auto_unbox = TRUE)
  write_executable(
    file.path(bin, "sacct"),
    c(
      "cp \"$BIONEMOR_TERMINAL_STATE\" \"$BIONEMOR_STATE_PATH\"",
      "printf '123|RUNNING|0:0\\n'"
    )
  )

  observed <- withr::with_envvar(
    c(
      BIONEMOR_TERMINAL_STATE = state_replacement,
      BIONEMOR_STATE_PATH = file.path(job_path(job), "state.json")
    ),
    job_status(job)
  )

  expect_equal(observed, "failed")
  expect_equal(
    jsonlite::read_json(file.path(job_path(job), "state.json"))$state,
    "failed"
  )
})

test_that("Slurm infrastructure terminal states map to failed jobs", {
  workspace <- tempfile("bionemor-slurm-terminal-states-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  for (scheduler_state in c("PREEMPTED", "BOOT_FAIL", "DEADLINE")) {
    job <- withr::with_envvar(
      c(
        BIONEMOR_FAKE_STATE = scheduler_state,
        BIONEMOR_FAKE_EXIT = "9:0"
      ),
      evo2_score(
        model,
        "ACGT",
        compute,
        name = paste0("slurm-", tolower(scheduler_state)),
        async = TRUE
      )
    )
    expect_equal(
      withr::with_envvar(
        c(
          BIONEMOR_FAKE_STATE = scheduler_state,
          BIONEMOR_FAKE_EXIT = "9:0"
        ),
        job_status(job)
      ),
      "failed",
      info = scheduler_state
    )
    state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
    expect_equal(state$exit_status, 9L, info = scheduler_state)
    expect_true(
      file.exists(file.path(job_path(job), "manifest.json")),
      info = scheduler_state
    )
  }
})

test_that("failed scheduler cancellation removes its request marker", {
  workspace <- tempfile("bionemor-slurm-cancel-failure-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  write_executable(
    file.path(bin, "scancel"),
    c(
      "printf 'scheduler refused cancellation\\n' >&2",
      "exit 2"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE = "RUNNING"
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-cancel-failure",
    async = TRUE
  )

  expect_error(job_cancel(job), "scheduler refused cancellation")
  expect_false(file.exists(file.path(job_path(job), "cancel.request")))
})

test_that("Slurm submission persists the submitted state", {
  workspace <- tempfile("bionemor-slurm-submitted-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE = "PENDING"
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-submitted",
    async = TRUE
  )

  state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
  expect_equal(state$state, "submitted")
  expect_equal(job_status(job, refresh = FALSE), "submitted")
  expect_equal(
    job_status(bionemo_job(job_path(job)), refresh = FALSE),
    "submitted"
  )
})

test_that("failed Slurm submission becomes a durable failed job", {
  workspace <- tempfile("bionemor-slurm-submit-failed-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  write_executable(
    file.path(bin, "sbatch"),
    c(
      "printf 'scheduler refused submission\\n' >&2",
      "exit 17"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  run_path <- file.path(
    workspace,
    ".bionemor",
    "runs",
    "slurm-submit-failed"
  )

  error <- expect_error(
    evo2_score(
      model,
      "ACGT",
      compute,
      name = "slurm-submit-failed",
      async = TRUE
    ),
    class = "BN_UPSTREAM"
  )

  expect_identical(error$upstream_exit_status, 17L)
  state <- jsonlite::read_json(
    file.path(run_path, "state.json"),
    simplifyVector = FALSE
  )
  expect_identical(state$state, "failed")
  expect_identical(state$exit_status, 17L)
  expect_identical(state$failure_reason, "SBATCH_FAILED")
  expect_equal(
    job_status(bionemo_job(run_path), refresh = FALSE),
    "failed"
  )
  expect_false(file.exists(file.path(run_path, "slurm-job.id")))
})

test_that("invalid sbatch output becomes a durable failed job", {
  workspace <- tempfile("bionemor-slurm-submit-invalid-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  write_executable(
    file.path(bin, "sbatch"),
    "printf 'not-a-job-id\\n'"
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  run_path <- file.path(
    workspace,
    ".bionemor",
    "runs",
    "slurm-submit-invalid"
  )

  error <- expect_error(
    evo2_score(
      model,
      "ACGT",
      compute,
      name = "slurm-submit-invalid",
      async = TRUE
    ),
    class = "BN_PROTOCOL"
  )

  expect_identical(error$upstream_exit_status, 0L)
  state <- jsonlite::read_json(
    file.path(run_path, "state.json"),
    simplifyVector = FALSE
  )
  expect_identical(state$state, "failed")
  expect_identical(state$exit_status, 1L)
  expect_identical(state$failure_reason, "SBATCH_INVALID_OUTPUT")
  expect_equal(
    job_status(bionemo_job(run_path), refresh = FALSE),
    "failed"
  )
  expect_false(file.exists(file.path(run_path, "slurm-job.id")))
})

test_that("Slurm running progress survives reopen and stale accounting", {
  workspace <- tempfile("bionemor-slurm-monotonic-")
  bin <- tempfile("bionemor-bin-")
  scheduler_state <- tempfile("bionemor-scheduler-state-")
  dir.create(workspace)
  writeLines("RUNNING", scheduler_state)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE_FILE = scheduler_state
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-monotonic",
    async = TRUE
  )

  expect_equal(job_status(job), "running")
  expect_equal(
    jsonlite::read_json(file.path(job_path(job), "state.json"))$state,
    "running"
  )
  writeLines("PENDING", scheduler_state)
  reopened <- bionemo_job(job_path(job))

  expect_equal(job_status(reopened), "running")
  expect_equal(job_status(reopened, refresh = FALSE), "running")
  expect_equal(
    jsonlite::read_json(file.path(job_path(job), "state.json"))$state,
    "running"
  )
})

test_that("a terminal scheduler state clears a stale Slurm finalizer", {
  workspace <- tempfile("bionemor-slurm-stale-finalizer-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE = "NODE_FAIL",
    BIONEMOR_FAKE_EXIT = "9:0"
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-stale-finalizer",
    async = TRUE
  )
  finalizing <- file.path(job_path(job), "finalizing")
  file.create(finalizing)

  expect_equal(job_status(job), "failed")
  expect_false(file.exists(finalizing))
  state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
  expect_equal(state$state, "failed")
  expect_equal(state$exit_status, 9L)
})

test_that("accepted Slurm cancellation retains its request marker", {
  workspace <- tempfile("bionemor-slurm-cancel-accepted-")
  bin <- tempfile("bionemor-bin-")
  accepted <- tempfile("bionemor-cancel-accepted-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  write_executable(
    file.path(bin, "sacct"),
    c(
      "if [[ -f \"$BIONEMOR_CANCEL_ACCEPTED\" ]]; then",
      "  printf 'accounting unavailable\\n' >&2",
      "  exit 3",
      "fi",
      "printf '123|RUNNING|0:0\\n'"
    )
  )
  write_executable(
    file.path(bin, "scancel"),
    ": > \"$BIONEMOR_CANCEL_ACCEPTED\""
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_CANCEL_ACCEPTED = accepted
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_score(
    model,
    "ACGT",
    compute,
    name = "slurm-cancel-accepted",
    async = TRUE
  )

  expect_error(job_cancel(job), "accounting unavailable")
  expect_true(file.exists(file.path(job_path(job), "cancel.request")))
})

test_that("local CUDA OOM exposes the GPU memory error context", {
  workspace <- tempfile("bionemor-local-oom-")
  bin <- tempfile("bionemor-bin-")
  log <- tempfile("bionemor-log-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  write_executable(
    file.path(bin, "torchrun"),
    c(
      "printf 'torch.OutOfMemoryError: CUDA out of memory\\n' >&2",
      "exit 124"
    )
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )
  memory <- 80 * 1024^3
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(
        compute_capability_major = 9L,
        total_memory_bytes = memory
      )
    )))
  )
  checkpoint <- make_mbridge_checkpoint(workspace)
  model <- evo2("7b", checkpoint = checkpoint)
  control <- evo2_inference_control(
    max_batch_size = 2L,
    precision = "bf16"
  )
  job <- evo2_generate(
    model,
    c(first = "ACGT", second = "ACGTAC"),
    compute,
    num_tokens = 4L,
    control = control,
    name = "local-cuda-oom",
    async = TRUE
  )

  error <- expect_error(
    job_wait(job, poll = 0.01, timeout = 10),
    class = "BN_GPU_MEMORY"
  )
  expect_identical(error$failure_reason, "CUDA_OUT_OF_MEMORY")
  expect_identical(error$model, "7b")
  expect_identical(error$checkpoint, normalizePath(checkpoint))
  expect_identical(
    error$sequence_summary,
    list(count = 2L, min_length = 4L, max_length = 6L, total_length = 10L)
  )
  expect_identical(error$micro_batch_size, 1L)
  expect_identical(error$tensor_parallel_size, 1L)
  expect_identical(error$pipeline_parallel_size, 1L)
  expect_identical(error$context_parallel_size, 1L)
  expect_identical(error$data_parallel_size, 1L)
  expect_identical(error$precision, "bf16")
  expect_identical(error$mixed_precision_recipe, "bf16_mixed")
  expect_identical(error$gpu_count, 1L)
  expect_equal(error$gpu_memory_bytes, memory)
  expect_match(error$hint, "Reduce max_batch_size", fixed = TRUE)
  expect_match(
    error$hint,
    "without changing precision or sequence lengths",
    fixed = TRUE
  )
  expect_error(job_result(job), class = "BN_GPU_MEMORY")
})

test_that("Slurm OUT_OF_MEMORY exposes and persists GPU memory context", {
  workspace <- tempfile("bionemor-slurm-oom-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE = "OUT_OF_MEMORY",
    BIONEMOR_FAKE_EXIT = "124:0"
  )
  memory <- 80 * 1024^3
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(
        compute_capability_major = 9L,
        total_memory_bytes = memory
      )
    )))
  )
  checkpoint <- make_mbridge_checkpoint(workspace)
  model <- evo2("7b", checkpoint = checkpoint)
  job <- evo2_generate(
    model,
    c(first = "ACGT", second = "ACGTAC"),
    compute,
    num_tokens = 4L,
    control = evo2_inference_control(
      max_batch_size = 2L,
      precision = "bf16"
    ),
    name = "slurm-oom",
    async = TRUE
  )
  job <- bionemo_job(job_path(job))

  error <- expect_error(
    job_wait(job, poll = 0.01, timeout = 10),
    class = "BN_GPU_MEMORY"
  )
  expect_identical(error$failure_reason, "OUT_OF_MEMORY")
  expect_identical(error$model, "7b")
  expect_identical(error$checkpoint, normalizePath(checkpoint))
  expect_identical(error$sequence_summary$count, 2L)
  expect_identical(error$tensor_parallel_size, 1L)
  expect_identical(error$pipeline_parallel_size, 1L)
  expect_identical(error$context_parallel_size, 1L)
  expect_identical(error$data_parallel_size, 1L)
  expect_identical(error$precision, "bf16")
  expect_identical(error$gpu_count, 1L)
  expect_equal(error$gpu_memory_bytes, memory)
  state <- jsonlite::read_json(file.path(job_path(job), "state.json"))
  expect_identical(state$failure_reason, "OUT_OF_MEMORY")
  manifest <- jsonlite::read_json(file.path(job_path(job), "manifest.json"))
  expect_identical(manifest$failure_reason, "OUT_OF_MEMORY")
  expect_error(job_result(job), class = "BN_GPU_MEMORY")
})

test_that("unattributed generation exit statuses remain upstream failures", {
  workspace <- tempfile("bionemor-generation-exit-codes-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  fake_slurm_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STATE = "FAILED"
  )
  compute <- bionemo_compute(
    backend = "slurm",
    engine = "external",
    workspace = workspace,
    config = list(capabilities = list(runtime = list(
      gpu_count = 1L,
      gpus = data.frame(compute_capability_major = 9L)
    )))
  )
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  for (status in c("65", "66", "67")) {
    job <- withr::with_envvar(
      c(BIONEMOR_FAKE_EXIT = paste0(status, ":0")),
      evo2_generate(
        model,
        "ACGT",
        compute,
        num_tokens = 4L,
        name = paste0("generation-exit-", status),
        async = TRUE
      )
    )
    expect_error(
      withr::with_envvar(
        c(BIONEMOR_FAKE_EXIT = paste0(status, ":0")),
        job_result(job)
      ),
      class = "BN_UPSTREAM",
      info = status
    )
  }
})

test_that("generation validation errors ignore stale upstream OOM text", {
  workspace <- tempfile("bionemor-generation-stale-oom-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_STALE_OOM = "true",
    BIONEMOR_FAKE_GENERATED_TOKENS = "5"
  )
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))

  error <- expect_error(
    evo2_generate(
      model,
      "ACGT",
      compute,
      num_tokens = 4L,
      name = "generation-stale-oom"
    ),
    class = "BN_OUTPUT_SCHEMA"
  )
  expect_identical(error$upstream_exit_status, 65L)
  state <- jsonlite::read_json(
    file.path(
      workspace,
      ".bionemor",
      "runs",
      "generation-stale-oom",
      "state.json"
    ),
    simplifyVector = FALSE
  )
  expect_null(state$failure_reason)
})
