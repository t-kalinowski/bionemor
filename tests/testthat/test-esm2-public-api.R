test_that("compute requires an explicit recipe", {
  expect_identical(names(formals(bionemo_compute))[[1L]], "recipe")
  expect_error(
    bionemo_compute(
      engine = "external",
      workspace = tempfile("bionemor-explicit-recipe-")
    ),
    "argument.*recipe.*missing"
  )

  compute <- bionemo_compute(
    recipe = evo2_recipe(),
    engine = "external",
    workspace = tempfile("bionemor-explicit-evo2-recipe-")
  )
  expect_identical(compute@recipe, evo2_recipe())
})

test_that("the public API exposes the ESM-2 embedding workflow", {
  functions <- c(
    "esm2_recipe",
    "esm2_models",
    "esm2",
    "esm2_model",
    "esm2_embed"
  )
  expect_true(all(functions %in% getNamespaceExports("bionemor")))

  workflows <- bionemo_workflows()
  record <- workflows[workflows$id == "esm2/embed", , drop = FALSE]
  expect_equal(nrow(record), 1L)
  expect_equal(record$family, "esm2")
  expect_equal(record$task, "embed")
  expect_equal(record$input_schema, "sequence/protein-v1")
  expect_equal(record$result_schema, "matrix/esm2-embeddings-v1")

  workflow <- bionemo_workflow("esm2/embed")
  expect_equal(workflow@id, "esm2/embed")
  expect_equal(workflow@family, "esm2")
  expect_equal(workflow@task, "embed")
  expect_equal(workflow@input_schema, "sequence/protein-v1")
  expect_equal(workflow@result_schema, "matrix/esm2-embeddings-v1")
  expect_equal(bionemo_workflows("esm2"), record)
})

test_that("ESM-2 recipes and model descriptors are available offline", {
  recipe <- esm2_recipe()
  workflow <- bionemo_workflow("esm2/embed")
  expect_s3_class(recipe, "bionemor::BioNeMoRecipe")
  expect_equal(recipe@adapter, workflow@adapter)
  expect_equal(recipe@recipe_version, "vllm-0.15.1")
  expect_equal(
    recipe@revision,
    "e8e7f597363c3b6dcc26f9b51fe683dd7f282f9e"
  )
  expect_true(recipe@verified)

  models <- esm2_models()
  expect_s3_class(models, "data.frame")
  expect_true(all(
    c(
      "name",
      "parameters",
      "source",
      "source_revision",
      "source_format"
    ) %in%
      names(models)
  ))
  expect_equal(models$name, c("8m", "35m", "150m", "650m", "3b", "15b"))
  expect_true(all(grepl("^[0-9a-f]{40}$", models$source_revision)))
  expect_true(all(models$source_format == "huggingface"))
  expect_equal(models$embedding_size[models$name == "8m"], 320L)

  model <- esm2()
  expect_s3_class(model, "bionemor::Esm2Model")
  expect_equal(model@family, "esm2")
  expect_equal(model@size, "8m")
  expect_false("task" %in% names(S7::props(model)))
  expect_null(model@checkpoint)
  expect_null(model@compute)

  expect_named(formals(esm2_model), c("size", "compute", "path"))
  expect_identical(
    head(names(formals(esm2_embed)), 3L),
    c("object", "newdata", "compute")
  )
})

test_that("ESM-2 runtime capabilities reject an unsupported GPU build", {
  workspace <- tempfile("bionemor-esm2-architecture-")
  bin <- tempfile("bionemor-esm2-bin-")
  dir.create(workspace)
  fake_esm2_runtime(
    bin,
    compute_capability_major = 7L,
    compute_capability_minor = 5L
  )
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = esm2_recipe(),
    engine = "external",
    workspace = workspace
  )

  doctor <- bionemo_doctor(compute, target = "inference", verbose = FALSE)
  checks <- as.data.frame(doctor)
  architecture <- checks[checks$check == "runtime GPU architecture", ]
  expect_false(doctor@ok)
  expect_identical(architecture$status, "fail")
  expect_match(architecture$detail, "7.5", fixed = TRUE)
  expect_error(
    bionemo_install(compute),
    "recipe runtime does not advertise required command",
    fixed = TRUE
  )
})

test_that("an external ESM-2 build may omit a managed architecture list", {
  workspace <- tempfile("bionemor-esm2-external-architecture-")
  bin <- tempfile("bionemor-esm2-bin-")
  dir.create(workspace)
  fake_esm2_runtime(bin, supported_compute_capabilities = character())
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = esm2_recipe(),
    engine = "external",
    workspace = workspace
  )

  doctor <- bionemo_doctor(compute, target = "inference", verbose = FALSE)
  checks <- as.data.frame(doctor)
  expect_true(doctor@ok)
  expect_false("runtime GPU architecture" %in% checks$check)
})

test_that("bionemo_run accepts a family-qualified workflow ID", {
  workspace <- tempfile("bionemor-workflow-id-")
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
  result <- bionemo_run(
    "evo2/score",
    model = model,
    input = c(reference = "ACGT", variant = "TGCA"),
    compute = compute,
    parameters = list(
      reduction = "mean",
      strand = "forward",
      batch_size = 1L
    ),
    name = "workflow-id-score"
  )

  expect_s3_class(result, "evo2_scores")
  expect_equal(result$id, c("reference", "variant"))
  expect_equal(result$score, c(-1, -2))
})

test_that("predict dispatches ESM-2 embedding through the family API", {
  model <- esm2("8m")
  proteins <- c(reference = "m k t", variant = "Mnt")

  direct <- tryCatch(
    esm2_embed(model, proteins),
    error = identity
  )
  generic <- tryCatch(
    predict(model, proteins, type = "embedding"),
    error = identity
  )

  expect_s3_class(direct, "error")
  expect_s3_class(generic, "error")
  expect_match(conditionMessage(direct), "compute is required")
  expect_identical(conditionMessage(generic), conditionMessage(direct))
})

test_that("ESM-2 embeddings use the public durable workflow", {
  workspace <- tempfile("bionemor-esm2-embedding-")
  bin <- tempfile("bionemor-esm2-bin-")
  log <- tempfile("bionemor-esm2-log-")
  dir.create(workspace)
  fake_esm2_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    BIONEMOR_FAKE_LOG = log
  )

  compute <- bionemo_compute(
    recipe = esm2_recipe(),
    engine = "external",
    workspace = workspace
  )
  compute <- bionemo_install(compute)
  doctor <- bionemo_doctor(compute, target = "inference", verbose = FALSE)
  checks <- as.data.frame(doctor)
  expect_true(doctor@ok)
  expect_true("runtime vLLM" %in% checks$check)
  expect_identical(
    checks$detail[checks$check == "runtime vLLM"],
    "0.15.1+cu133"
  )
  expect_false(any(
    c(
      "runtime Transformer Engine",
      "runtime Megatron Bridge",
      "runtime BioNeMo"
    ) %in%
      checks$check
  ))
  large_doctor <- bionemo_doctor(
    compute,
    esm2("15b"),
    target = "inference",
    verbose = FALSE
  )
  large_checks <- as.data.frame(large_doctor)
  memory <- large_checks[large_checks$check == "model memory floor", ]
  expect_false(large_doctor@ok)
  expect_identical(memory$status, "fail")
  expect_match(memory$detail, "before runtime overhead", fixed = TRUE)
  expect_error(
    bionemo_doctor(compute, target = "training"),
    "does not support the 'training' operation group",
    fixed = TRUE
  )
  model <- esm2_model("8m", compute)
  proteins <- c(reference = "m k t", variant = "Mnt")
  protein_file <- file.path(workspace, "proteins.faa")
  writeLines(
    c(">reference", proteins[[1L]], ">variant", proteins[[2L]]),
    protein_file
  )

  embeddings <- bionemo_run(
    "esm2/embed",
    model = model,
    input = protein_file,
    parameters = list()
  )
  expect_s3_class(embeddings, "esm2_embeddings")
  expect_equal(dim(embeddings), c(2L, 320L))
  expect_equal(rownames(embeddings), names(proteins))
  expect_equal(embeddings[, 1L], c(reference = 1, variant = 2))
  predicted <- predict(model, proteins, type = "embedding")
  expect_equal(predicted, embeddings, ignore_attr = TRUE)

  arguments <- readLines(log, warn = FALSE)
  expect_true(all(
    c(
      "--model",
      "nvidia/esm2_t6_8M_UR50D",
      "--revision",
      "3674a6acb6c217bbeff709d182a11b196125dfc3"
    ) %in%
      arguments
  ))
  input_path <- arguments[[match("--input", arguments) + 1L]]
  expect_equal(
    readLines(input_path, warn = FALSE),
    c(">reference", "MKT", ">variant", "MNT")
  )

  expect_error(
    esm2_embed(model, c(invalid = "M*T")),
    "protein sequences may contain only amino-acid symbols"
  )

  job <- esm2_embed(model, proteins, async = TRUE, name = "esm2-async")
  expect_s3_class(job, "bionemor::BioNeMoJob")
  reopened <- bionemo_job(job_path(job))
  result <- job_wait(reopened, timeout = 30)
  expect_s3_class(result, "esm2_embeddings")
  expect_equal(dim(result), dim(embeddings))
  expect_equal(rownames(result), rownames(embeddings))
  expect_equal(as.double(result), as.double(embeddings))

  manifest <- jsonlite::read_json(
    file.path(job_path(reopened), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_identical(manifest$precision$semantic, "float32")
  expect_identical(manifest$precision$resolved_recipe, "float32")

  checkpoint <- file.path(workspace, "local-esm2-8m")
  dir.create(checkpoint)
  writeLines("{}", file.path(checkpoint, "config.json"))
  writeLines("weights", file.path(checkpoint, "model.safetensors"))
  local_model <- esm2_model("8m", compute, path = checkpoint)
  local_job <- esm2_embed(
    local_model,
    proteins[[1L]],
    async = TRUE,
    name = "esm2-local-checkpoint"
  )
  job_wait(local_job, timeout = 30)
  local_manifest <- jsonlite::read_json(
    file.path(job_path(local_job), "manifest.json"),
    simplifyVector = FALSE
  )
  expect_identical(local_manifest$checkpoint$path, normalizePath(checkpoint))
  expect_identical(local_manifest$checkpoint$digest$algorithm, "md5")
  expect_match(local_manifest$checkpoint$digest$value, "^[0-9a-f]{32}$")
})
