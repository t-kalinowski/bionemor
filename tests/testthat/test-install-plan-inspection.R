test_that("installation plans expose ordered public step details", {
  workspace <- file.path(
    tempfile("bionemor-plan-inspection-"),
    "workspace with spaces"
  )
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace
  )
  workspace <- normalizePath(workspace)

  plan <- bionemo_install_plan(compute)
  steps <- as.data.frame(plan)

  expect_identical(
    names(steps),
    c("step", "id", "purpose", "command", "cwd", "expected")
  )
  expect_identical(steps$step, seq_len(nrow(steps)))
  expect_identical(
    steps$id,
    c(
      "runtime-capabilities",
      "probe-infer_evo2",
      "probe-predict_evo2",
      "probe-preprocess_evo2",
      "probe-train_evo2",
      "probe-evo2_convert_savanna_to_mbridge",
      "probe-evo2_convert_nemo2_to_mbridge",
      "probe-evo2_export_mbridge_to_vortex",
      "probe-evo2_remove_optimizer"
    )
  )
  expect_identical(
    steps$purpose[[1L]],
    "verify the helper protocol and current recipe commands"
  )
  expect_true(all(nzchar(steps$purpose)))
  expect_true(all(nzchar(steps$command)))
  expect_true(all(steps$cwd == workspace))
  expect_match(steps$command[[1L]], shQuote(workspace), fixed = TRUE)
  expect_true(is.list(steps$expected))
  expect_named(steps$expected[[1L]], "protocol_version")
  expect_length(steps$expected[[1L]]$protocol_version, 1L)
  expect_identical(steps$expected[[2L]], list())

  output <- capture.output(print(plan))
  expect_true(any(grepl("Steps:  9", output, fixed = TRUE)))
  for (i in seq_len(nrow(steps))) {
    line <- paste0(i, ". ", steps$id[[i]], ": ", steps$purpose[[i]])
    expect_true(any(grepl(line, output, fixed = TRUE)))
  }
})

test_that("installation plan inspection redacts credentials", {
  secret <- "installation-plan-secret"
  withr::local_envvar(NGC_API_KEY = secret)
  workspace <- file.path(
    tempfile("bionemor-plan-redaction-"),
    secret,
    "workspace"
  )
  compute <- bionemo_compute(
    engine = "external",
    workspace = workspace
  )

  steps <- as.data.frame(bionemo_install_plan(compute))

  expect_false(any(grepl(secret, steps$command, fixed = TRUE)))
  expect_false(any(grepl(secret, steps$cwd, fixed = TRUE)))
  expect_true(all(grepl("[REDACTED]", steps$cwd, fixed = TRUE)))
})
