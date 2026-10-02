test_that("export_ecoglmm writes a DHARMa diagnostic PDF", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("DHARMa")

  set.seed(42)
  example_data <- expand.grid(
    site = paste0("s", seq_len(6)),
    period = c("early", "late"),
    replicate = seq_len(3),
    stringsAsFactors = FALSE
  )
  example_data$forest <- stats::runif(nrow(example_data))
  site_effect <- setNames(stats::rnorm(6, sd = 0.3), paste0("s", seq_len(6)))
  example_data$index <- 1 + 0.8 * example_data$forest +
    0.4 * (example_data$period == "late") +
    site_effect[example_data$site] +
    stats::rnorm(nrow(example_data), sd = 0.15)

  config <- data.frame(response = "index", family = "gaussian")
  fit <- run_ecoglmm(
    data = example_data,
    config = config,
    predictors = "forest",
    period = "period",
    group = "site",
    include_interactions = FALSE
  )
  diagnostics <- diagnose_models(fit, nsim = 20, seed = 42)
  output <- file.path(tempdir(), "ecoGLMM-export-pdf-test")

  files <- export_ecoglmm(
    object = fit,
    path = output,
    diagnostics = diagnostics,
    figures = FALSE,
    diagnostic_pdf = TRUE
  )

  pdf_file <- file.path(output, "DHARMa_diagnostics.pdf")
  expect_true(pdf_file %in% unname(files))
  expect_true(file.exists(pdf_file))
  expect_gt(file.info(pdf_file)$size, 0)
})
