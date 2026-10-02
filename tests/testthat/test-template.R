test_that("installed generic template is present and syntactically valid", {
  path <- system.file(
    "examples",
    "ecoGLMM_template.R",
    package = "ecoGLMM"
  )

  expect_true(nzchar(path))
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
  expect_error(parse(file = path), NA)

  template_text <- readLines(path, warn = FALSE)
  expect_true(any(grepl(
    "CHECK_PREDICTOR_CORRELATIONS <- TRUE",
    template_text,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "predictor_correlations.csv",
    template_text,
    fixed = TRUE
  )))
})
