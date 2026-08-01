test_that("public functions expose only supported choices", {
  expect_false("n" %in% names(formals(evo2_generate)))
  expect_false("mask_phylogenetic_tags" %in% names(formals(evo2_score)))
  expect_false("metric" %in% names(formals(evo2_profile)))
  expect_false("format" %in% names(formals(evo2_export)))

  inference_arguments <- names(formals(evo2_inference_control))
  expect_false("pipeline_parallel_size" %in% inference_arguments)
  expect_false("micro_batch_size" %in% inference_arguments)

  expect_false("nodes" %in% names(formals(bionemo_compute)))
  expect_false("bionemo_install_plan" %in% getNamespaceExports("bionemor"))
  expect_false("bionemo_setup" %in% getNamespaceExports("bionemor"))
})

test_that("predict supports the three implemented inference operations", {
  model <- evo2("7b")

  expect_error(
    predict(model, "ACGT", type = "response"),
    "should be one of"
  )
  expect_error(
    predict(model, "ACGT", type = "raw"),
    "should be one of"
  )
})

test_that("inference extras reject settings that no operation applies", {
  unsupported <- c(
    "eden_tokenizer",
    "hybrid_override_pattern",
    "num_layers",
    "seq_len_interpolation_factor"
  )

  for (setting in unsupported) {
    value <- stats::setNames(list(TRUE), setting)
    expect_error(
      evo2_inference_control(extra = value),
      "unsupported setting",
      info = setting
    )
  }
})
