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

test_that("the public API exposes ESM-2 protein embeddings", {
  functions <- c(
    "esm2_recipe",
    "esm2_models",
    "esm2",
    "esm2_model",
    "esm2_embed"
  )
  expect_true(all(functions %in% getNamespaceExports("bionemor")))
})

test_that("ESM-2 recipes and model descriptors are available offline", {
  recipe <- esm2_recipe()
  expect_s3_class(recipe, "bionemor::BioNeMoRecipe")
  expect_equal(recipe@adapter, "esm2-transformers")
  expect_equal(recipe@recipe_version, "transformers-5.14.1")
  expect_identical(recipe@bridge_protocol, 2L)
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

test_that("ESM-2 rejects multi-GPU inference before submission", {
  workspace <- tempfile("bionemor-esm2-multi-gpu-")
  bin <- tempfile("bionemor-esm2-bin-")
  dir.create(workspace)
  fake_esm2_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = esm2_recipe(),
    engine = "external",
    workspace = workspace,
    gpus = 2L
  )
  model <- esm2("8m")

  doctor <- bionemo_doctor(
    compute,
    model = model,
    target = "inference",
    verbose = FALSE
  )
  checks <- as.data.frame(doctor)
  gpu_count <- checks[
    checks$check == "model GPU count",
    ,
    drop = FALSE
  ]
  expect_identical(gpu_count$status, "fail")
  expect_match(gpu_count$detail, "gpus = 1", fixed = TRUE)
  expect_error(
    esm2("8m", compute = compute),
    "currently requires gpus = 1",
    fixed = TRUE
  )
  expect_error(
    esm2_embed(model, c(protein = "MKT"), compute = compute),
    "currently requires gpus = 1",
    fixed = TRUE
  )
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

test_that("ESM-2 embeddings use durable jobs", {
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
  expect_true("runtime Transformers" %in% checks$check)
  expect_identical(
    checks$detail[checks$check == "runtime Transformers"],
    "5.14.1"
  )
  expect_true("runtime Transformer Engine" %in% checks$check)
  expect_false("runtime vLLM" %in% checks$check)
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
  output <- file.path(workspace, "portable", "esm2-pooled")
  collision <- file.path(workspace, "portable", "collision")
  dir.create(dirname(collision), recursive = TRUE)
  writeLines("{}", paste0(collision, ".json"))
  expect_error(
    esm2_embed(model, proteins, output = collision),
    "output path already exists"
  )
  protein_file <- file.path(workspace, "proteins.faa")
  writeLines(
    c(">reference", proteins[[1L]], ">variant", proteins[[2L]]),
    protein_file
  )

  embeddings <- esm2_embed(model, protein_file, output = output)
  expect_s3_class(embeddings, "esm2_embeddings")
  expect_true(is.matrix(embeddings))
  expect_equal(dim(embeddings), c(2L, 320L))
  expect_equal(rownames(embeddings), names(proteins))
  expect_equal(embeddings[, 1L], c(reference = 1, variant = 2))
  expect_pooled_embedding_output(
    output,
    c(2L, 320L),
    names(proteins),
    "float32"
  )

  provenance <- attr(embeddings, "provenance", exact = TRUE)
  expect_identical(class(provenance), "list")
  expect_identical(
    names(provenance),
    c(
      "run_path",
      "model",
      "source",
      "source_revision",
      "pooling",
      "recipe_revision"
    )
  )
  expect_true(dir.exists(provenance$run_path))
  expect_identical(provenance$model, "8m")
  expect_identical(provenance$source, "nvidia/esm2_t6_8M_UR50D")
  expect_identical(
    provenance$source_revision,
    "3674a6acb6c217bbeff709d182a11b196125dfc3"
  )
  expect_identical(provenance$pooling, "last-token-l2")
  expect_identical(provenance$recipe_revision, esm2_recipe()@revision)

  rematerialized <- job_result(bionemo_job(provenance$run_path))
  expect_s3_class(rematerialized, "esm2_embeddings")
  expect_identical(as.double(rematerialized), as.double(embeddings))

  predicted <- predict(model, proteins, type = "embedding")
  expect_true(is.matrix(predicted))
  expect_identical(dim(predicted), dim(embeddings))
  expect_identical(dimnames(predicted), dimnames(embeddings))
  expect_identical(as.double(predicted), as.double(embeddings))
  predicted_provenance <- attr(predicted, "provenance", exact = TRUE)
  expect_identical(class(predicted_provenance), "list")
  expect_identical(names(predicted_provenance), names(provenance))
  expect_identical(
    predicted_provenance[setdiff(names(provenance), "run_path")],
    provenance[setdiff(names(provenance), "run_path")]
  )
  expect_false(identical(predicted_provenance$run_path, provenance$run_path))

  arguments <- readLines(log, warn = FALSE)
  expect_true(all(
    c(
      c("--model", "nvidia/esm2_t6_8M_UR50D"),
      c("--revision", "3674a6acb6c217bbeff709d182a11b196125dfc3")
    ) %in%
      arguments
  ))
  expect_false(any(c(
    "--max-num-seqs",
    "--disable-prefix-caching",
    "--tensor-parallel-size",
    "--max-num-batched-tokens"
  ) %in% arguments))
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

test_that("ESM-2 durable outputs reject corrupted float data", {
  workspace <- tempfile("bionemor-esm2-corrupt-")
  bin <- tempfile("bionemor-esm2-bin-")
  dir.create(workspace)
  fake_esm2_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )
  compute <- bionemo_compute(
    recipe = esm2_recipe(),
    engine = "external",
    workspace = workspace
  )
  model <- esm2_model("8m", compute)
  job <- esm2_embed(model, c(protein = "MKT"), async = TRUE)
  job_wait(job, timeout = 30)

  data <- file.path(job_path(job), "outputs", "embeddings.f32.gz")
  connection <- file(data, "ab")
  writeBin(as.raw(0L), connection)
  close(connection)

  expect_error(
    job_result(bionemo_job(job_path(job))),
    "data checksum does not match"
  )
})
