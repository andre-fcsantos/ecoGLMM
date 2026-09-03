test_that("a small Gaussian pipeline returns a structured result", {
  skip_if_not_installed("glmmTMB")
  set.seed(42)
  sites <- factor(rep(seq_len(10), each = 10))
  period <- factor(rep(c("T-0", "T-1"), 50))
  forest <- stats::rnorm(100)
  random_site <- stats::rnorm(10, sd = 0.5)[sites]
  response <- 1 + 0.4 * forest + 0.3 * (period == "T-1") +
    random_site + stats::rnorm(100, sd = 0.5)
  dat <- data.frame(response, forest, period, sites)
  cfg <- data.frame(response = "response", family = "gaussian")

  fit <- run_ecoglmm(dat, cfg, predictors = "forest", period = "period",
                     group = "sites")
  expect_s3_class(fit, "ecoglmm_fit")
  expect_equal(nrow(fit$selection$response), 2)
  expect_equal(fit$interaction_tests$response$df, 1)
  expect_true(all(c("overview", "coefficients") %in% names(fit)))
})
