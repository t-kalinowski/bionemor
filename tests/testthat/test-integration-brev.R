test_that("the opt-in Brev acceptance workflow completes and stops its instance", {
  skip_if(
    Sys.getenv("BIONEMOR_RUN_BREV") != "true",
    "Set BIONEMOR_RUN_BREV=true to provision the acceptance instance."
  )
  script <- system.file(
    "scripts",
    "brev-evo2-run.sh",
    package = "bionemor"
  )
  result <- processx::run(
    "bash",
    c(script, "--run"),
    error_on_status = FALSE,
    echo = TRUE,
    timeout = 7200
  )
  expect_equal(result$status, 0L, info = result$stderr)
})
