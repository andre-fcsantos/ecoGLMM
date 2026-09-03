test_that("adjust_beta respects the open unit interval", {
  expect_equal(adjust_beta(c(0, 0.5, 1)), c(0.001, 0.5, 0.999))
  expect_error(adjust_beta(c(0, 1), epsilon = 0.8))
})

test_that("prepare_ecodata uses common complete cases and scales predictors", {
  x <- data.frame(
    response = c(1, 2, 3, 4), predictor = c(1, 2, NA, 4),
    period = c("a", "b", "a", "b"), site = c("x", "x", "y", "y")
  )
  cfg <- data.frame(response = "response", family = "gaussian")
  out <- prepare_ecodata(x, cfg, "predictor", "period", "site")
  expect_equal(nrow(out), 3)
  expect_equal(round(mean(out$predictor), 10), 0)
  expect_true(is.factor(out$period))
  expect_true(is.factor(out$site))
})

