test_that("BioNeMo workflows are discovered independently of models and compute", {
  workflows <- bionemo_workflows()

  expect_s3_class(workflows, "data.frame")
  expect_named(
    workflows,
    c(
      "id",
      "adapter",
      "adapter_version",
      "family",
      "task",
      "protocol_version",
      "input_schema",
      "result_schema"
    )
  )
  expect_true(all(
    c(
      "evo2/generate",
      "evo2/score",
      "evo2/embed",
      "evo2/fine-tune"
    ) %in%
      workflows$id
  ))
  expect_equal(unique(workflows$family), "evo2")

  workflow <- bionemo_workflow("evo2/score")
  expect_s3_class(workflow, "bionemor::BioNeMoWorkflow")
  expect_equal(workflow@id, "evo2/score")
  expect_equal(workflow@adapter, "evo2-megatron")
  expect_equal(workflow@adapter_version, 1L)
  expect_equal(workflow@family, "evo2")
  expect_equal(workflow@task, "score")
  expect_equal(workflow@protocol_version, 1L)
  expect_equal(workflow@input_schema, "sequence/dna-v1")
  expect_equal(workflow@result_schema, "table/evo2-scores-v1")
  expect_equal(
    bionemo_workflow("evo2/fine-tune")@result_schema,
    "model/evo2-v1"
  )
  expect_equal(
    bionemo_workflow("evo2/export")@input_schema,
    "path/destination-v1"
  )
  expect_false(any(c("model", "compute") %in% S7::prop_names(workflow)))

  expect_equal(
    bionemo_workflows("evo2")$id,
    workflows$id
  )
  expect_error(bionemo_workflow("missing/score"), "unsupported")
  expect_error(bionemo_workflows("missing"), "unsupported")
})

test_that("recipes identify the adapter that installs and runs them", {
  recipe <- evo2_recipe()
  expect_equal(recipe@adapter, "evo2-megatron")

  workspace <- tempfile("bionemor-adapter-recipe-")
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace,
    recipe = recipe
  )
  expect_equal(compute@recipe@adapter, "evo2-megatron")
})

test_that("generic workflow dispatch uses the Evo 2 adapter contract", {
  workspace <- tempfile("bionemor-workflow-")
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
  scores <- bionemo_run(
    bionemo_workflow("evo2/score"),
    model = model,
    input = c(reference = "ACGT", variant = "TGCA"),
    compute = compute,
    parameters = list(
      reduction = "mean",
      strand = "forward",
      batch_size = 1L
    ),
    name = "adapter-score"
  )

  expect_s3_class(scores, "evo2_scores")
  expect_equal(scores$id, c("reference", "variant"))
  expect_equal(scores$score, c(-1, -2))

  run_path <- attr(scores, "provenance")$run_path
  request <- jsonlite::read_json(
    file.path(run_path, "request.json"),
    simplifyVector = FALSE
  )
  expect_equal(request$workflow$id, "evo2/score")
  expect_equal(request$workflow$adapter, "evo2-megatron")
  expect_equal(request$workflow$adapter_version, 1L)
  expect_equal(request$workflow$protocol_version, 1L)
  expect_equal(request$workflow$input_schema, "sequence/dna-v1")
  expect_equal(request$workflow$result_schema, "table/evo2-scores-v1")
  expect_equal(
    request$expected_result$result_schema,
    "table/evo2-scores-v1"
  )

  plan <- jsonlite::read_json(
    file.path(run_path, "plan.json"),
    simplifyVector = FALSE
  )
  expect_equal(plan$metadata$workflow$id, "evo2/score")
  expect_equal(plan$metadata$workflow$adapter, "evo2-megatron")
})

test_that("workflow dispatch rejects incompatible models before execution", {
  workflow <- bionemo_workflow("evo2/score")
  compute <- bionemo_compute(
    engine = "external",
    workspace = tempfile("bionemor-workflow-")
  )

  expect_error(
    bionemo_run(
      workflow,
      model = list(family = "not-evo2"),
      input = "ACGT",
      compute = compute
    ),
    "Evo 2 model"
  )
})

test_that("every installed Evo 2 workflow reaches its public operation", {
  workspace <- tempfile("bionemor-workflow-routes-")
  dir.create(workspace)
  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  cases <- list(
    generate = list("ACGT", list(num_tokens = 0L), "num_tokens must"),
    score = list("ACGT", list(batch_size = 0L), "batch_size must"),
    profile = list("ACGT", list(), "profile output is required"),
    embed = list("ACGT", list(pool = "none"), "require output"),
    checkpoint = list(
      "recommended",
      list(format = "unsupported", path = "checkpoint"),
      "should be one of"
    ),
    export = list(
      "export.pt",
      list(format = "unsupported"),
      "format must be 'vortex'"
    ),
    prepare = list("ACGT", list(path = 1), "path must"),
    `fine-tune` = list("ACGT", list(steps = 0L), "steps must")
  )

  for (task in names(cases)) {
    case <- cases[[task]]
    expect_error(
      bionemo_run(
        bionemo_workflow(paste0("evo2/", task)),
        model,
        case[[1L]],
        compute,
        parameters = case[[2L]]
      ),
      case[[3L]],
      info = task
    )
  }
})

test_that("generic asynchronous score workflows reopen and materialize", {
  workspace <- tempfile("bionemor-workflow-async-")
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
  input <- c(reference = "ACGT", variant = "TGCA")
  workflow <- bionemo_workflow("evo2/score")
  parameters <- list(reduction = "mean", strand = "forward")

  expected <- bionemo_run(
    workflow,
    model = model,
    input = input,
    compute = compute,
    parameters = parameters,
    name = "workflow-score-sync"
  )
  job <- bionemo_run(
    workflow,
    model = model,
    input = input,
    compute = compute,
    parameters = parameters,
    async = TRUE,
    name = "workflow-score-async"
  )

  reopened <- bionemo_job(job_path(job))
  actual <- job_wait(reopened, poll = 0.01, timeout = 10)

  expect_s3_class(actual, "evo2_scores")
  expect_equal(actual, expected, ignore_attr = TRUE)
  expect_equal(attr(actual, "provenance")$run_path, job_path(job))
})

test_that("family wrappers and generic workflows share one public contract", {
  workspace <- tempfile("bionemor-workflow-wrapper-equivalence-")
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
  input <- c(reference = "ACGT", variant = "TGCA")
  parameters <- list(
    reduction = "sum",
    strand = "forward",
    batch_size = 2L
  )

  wrapped <- evo2_score(
    model,
    input,
    compute,
    reduction = parameters$reduction,
    strand = parameters$strand,
    batch_size = parameters$batch_size,
    name = "workflow-wrapper-score"
  )
  generic <- bionemo_run(
    bionemo_workflow("evo2/score"),
    model = model,
    input = input,
    compute = compute,
    parameters = parameters,
    name = "workflow-generic-score"
  )

  expect_equal(wrapped, generic, ignore_attr = TRUE)
  requests <- lapply(
    list(wrapped, generic),
    function(result) {
      jsonlite::read_json(
        file.path(attr(result, "provenance")$run_path, "request.json"),
        simplifyVector = FALSE
      )
    }
  )
  expect_identical(requests[[1L]]$workflow, requests[[2L]]$workflow)
  expect_identical(requests[[1L]]$request, requests[[2L]]$request)
  expect_identical(
    requests[[1L]]$request_origins,
    requests[[2L]]$request_origins
  )
})

test_that("reopening rejects a changed persisted workflow identity", {
  workspace <- tempfile("bionemor-workflow-protocol-")
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
  job <- bionemo_run(
    bionemo_workflow("evo2/score"),
    model = model,
    input = c(reference = "ACGT"),
    compute = compute,
    async = TRUE,
    name = "workflow-protocol-mismatch"
  )
  deadline <- Sys.time() + 10
  while (job_status(job) != "succeeded" && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_identical(job_status(job), "succeeded")

  run_path <- job_path(job)
  request_path <- file.path(run_path, "request.json")
  request <- jsonlite::read_json(request_path, simplifyVector = FALSE)
  request$workflow$protocol_version <- 999L
  jsonlite::write_json(
    request,
    request_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  runtime_log <- readLines(log, warn = FALSE)

  error <- expect_error(
    bionemo_job(run_path),
    class = "BN_PROTOCOL"
  )

  expect_identical(error$operation, "workflow-resolution")
  expect_identical(error$workflow, "evo2/score")
  expect_identical(error$fields, "protocol_version")
  expect_identical(readLines(log, warn = FALSE), runtime_log)
})

test_that("reopening rejects inconsistent persisted adapter contracts", {
  workspace <- tempfile("bionemor-workflow-reopen-")
  bin <- tempfile("bionemor-bin-")
  dir.create(workspace)
  fake_recipes_runtime(bin)
  withr::local_envvar(
    PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)
  )

  compute <- bionemo_compute(engine = "external", workspace = workspace)
  model <- evo2("7b", checkpoint = make_mbridge_checkpoint(workspace))
  job <- evo2_score(
    model,
    c(reference = "ACGT"),
    compute,
    async = TRUE,
    name = "workflow-reopen-mismatch"
  )
  job_wait(job, poll = 0.01, timeout = 10)

  run_path <- job_path(job)
  plan_path <- file.path(run_path, "plan.json")
  plan <- jsonlite::read_json(plan_path, simplifyVector = FALSE)
  plan$metadata$workflow$adapter_version <- 999L
  jsonlite::write_json(
    plan,
    plan_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  expect_error(bionemo_job(run_path), class = "BN_PROTOCOL")

  plan$metadata$workflow$adapter_version <- 1L
  jsonlite::write_json(
    plan,
    plan_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  request_path <- file.path(run_path, "request.json")
  request <- jsonlite::read_json(request_path, simplifyVector = FALSE)
  request$compute$recipe$adapter <- "other-adapter"
  jsonlite::write_json(
    request,
    request_path,
    auto_unbox = TRUE,
    null = "null",
    pretty = TRUE
  )
  expect_error(bionemo_job(run_path), class = "BN_PROTOCOL")
})

test_that("direct Evo 2 scoring persists its exact workflow identity", {
  workspace <- tempfile("bionemor-workflow-wrapper-")
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
  scores <- evo2_score(
    model,
    c(reference = "ACGT"),
    compute,
    name = "direct-evo2-score"
  )
  manifest <- jsonlite::read_json(
    file.path(attr(scores, "provenance")$run_path, "manifest.json"),
    simplifyVector = FALSE
  )
  workflow <- bionemo_workflow("evo2/score")

  expect_identical(
    manifest$workflow,
    list(
      id = workflow@id,
      adapter = workflow@adapter,
      adapter_version = workflow@adapter_version,
      family = workflow@family,
      task = workflow@task,
      protocol_version = workflow@protocol_version,
      input_schema = workflow@input_schema,
      result_schema = workflow@result_schema
    )
  )
})
