#' Export ecoGLMM results to an output directory
#'
#' @param object An `ecoglmm_fit` object.
#' @param path Output directory. Must be explicitly supplied by the caller.
#' @param diagnostics Optional result returned by [diagnose_models()].
#' @param figures Save effect and selection figures.
#' @param diagnostic_pdf Save a multi-page PDF containing the DHARMa plot and
#'   a diagnostic summary for each selected response model. The PDF is created
#'   only when `diagnostics` is supplied.
#' @return Invisibly returns generated file paths.
#' @examples
#' set.seed(1)
#' example_data <- expand.grid(
#'   site = paste0("s", 1:6),
#'   period = c("early", "late"),
#'   replicate = 1:3,
#'   stringsAsFactors = FALSE
#' )
#' example_data$forest <- runif(nrow(example_data))
#' site_effect <- setNames(rnorm(6, sd = 0.3), paste0("s", 1:6))
#' example_data$index <- 1 + 0.8 * example_data$forest +
#'   0.4 * (example_data$period == "late") +
#'   site_effect[example_data$site] + rnorm(nrow(example_data), sd = 0.15)
#' config <- data.frame(response = "index", family = "gaussian")
#' fit <- run_ecoglmm(
#'   data = example_data,
#'   config = config,
#'   predictors = "forest",
#'   period = "period",
#'   group = "site",
#'   include_interactions = FALSE
#' )
#' output <- file.path(tempdir(), "ecoGLMM-example")
#' files <- export_ecoglmm(fit, path = output, figures = FALSE)
#' basename(files)
#' @export
export_ecoglmm <- function(object, path,
                           diagnostics = NULL, figures = TRUE,
                           diagnostic_pdf = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Supply a non-empty output directory in `path`.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  selection <- do.call(rbind, lapply(names(object$selection), function(x) {
    cbind(Response = x, object$selection[[x]])
  }))
  lrt <- do.call(rbind, lapply(names(object$interaction_tests), function(x) {
    tab <- object$interaction_tests[[x]]
    cbind(Response = rep(x, nrow(tab)), tab)
  }))
  sheets <- list(Overview = object$overview, Coefficients = object$coefficients,
                 Model_selection = selection, Interaction_LRT = lrt)
  if (!is.null(diagnostics)) sheets$Diagnostics <- diagnostics$summary
  workbook <- file.path(path, "ecoGLMM_results.xlsx")
  writexl::write_xlsx(sheets, workbook)
  files <- workbook

  if (!is.null(diagnostics) && isTRUE(diagnostic_pdf)) {
    diagnostic_file <- file.path(path, "DHARMa_diagnostics.pdf")
    save_dharma_diagnostics_pdf(diagnostics, diagnostic_file)
    files <- c(files, DHARMa_diagnostics = diagnostic_file)
  }

  if (figures) {
    figure_dir <- file.path(path, "figures")
    dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
    for (response in names(object$best_models)) {
      effect_file <- file.path(figure_dir, paste0(response, "_effect.png"))
      selection_file <- file.path(figure_dir, paste0(response, "_selection.png"))
      ggplot2::ggsave(effect_file, plot_effect(object, response),
                      width = 7, height = 5, dpi = 400)
      ggplot2::ggsave(selection_file, plot_selection(object, response),
                      width = 7, height = 5, dpi = 400)
      files <- c(files, effect_file, selection_file)
    }
    coefficient_file <- file.path(figure_dir, "coefficients.png")
    ggplot2::ggsave(coefficient_file, plot_coefficients(object),
                    width = 9, height = 6, dpi = 400)
    files <- c(files, coefficient_file)
  }
  invisible(files)
}

save_dharma_diagnostics_pdf <- function(diagnostics, file) {
  if (
    !is.list(diagnostics) ||
    is.null(diagnostics$summary) ||
    is.null(diagnostics$residuals) ||
    !length(diagnostics$residuals)
  ) {
    stop(
      "`diagnostics` must be a result returned by `diagnose_models()`.",
      call. = FALSE
    )
  }

  grDevices::pdf(file, width = 11, height = 8, onefile = TRUE)
  device_open <- TRUE
  on.exit({
    if (device_open) grDevices::dev.off()
  }, add = TRUE)

  for (response in names(diagnostics$residuals)) {
    simulated_residuals <- diagnostics$residuals[[response]]
    graphics::plot(simulated_residuals)

    summary_row <- diagnostics$summary[
      diagnostics$summary$Response == response,
      ,
      drop = FALSE
    ]

    if (!nrow(summary_row)) next

    old_par <- graphics::par(no.readonly = TRUE)
    graphics::par(mfrow = c(1, 1), mar = c(1, 1, 1, 1), xpd = NA)
    graphics::plot.new()

    graphics::rect(
      xleft = 0.05, ybottom = 0.86,
      xright = 0.95, ytop = 0.96,
      col = "grey95", border = "grey70", lwd = 1.2
    )
    graphics::text(
      0.50, 0.925, "DHARMa DIAGNOSTIC REPORT",
      font = 2, cex = 1.35
    )
    graphics::text(0.50, 0.885, paste("Response:", response), cex = 1.05)

    graphics::text(
      0.08, 0.78, "FORMAL TESTS",
      adj = c(0, 1), font = 2, cex = 1.05
    )
    graphics::segments(0.08, 0.755, 0.92, 0.755, lwd = 1.2)

    test_labels <- c("Uniformity", "Dispersion", "Outliers")
    test_values <- unlist(summary_row[test_labels], use.names = FALSE)
    test_y <- c(0.69, 0.63, 0.57)

    for (index in seq_along(test_labels)) {
      graphics::text(
        0.11, test_y[index],
        paste0(test_labels[index], ": p = ", sprintf("%.4f", test_values[index])),
        adj = c(0, 0.5), cex = 0.98
      )
    }

    passed <- identical(as.character(summary_row$Status[1]), "Passed")
    graphics::text(
      0.08, 0.47, "MODEL STATUS",
      adj = c(0, 1), font = 2, cex = 1.05
    )
    graphics::segments(0.08, 0.445, 0.92, 0.445, lwd = 1.2)
    graphics::rect(
      0.11, 0.31, 0.89, 0.40,
      col = if (passed) "honeydew2" else "mistyrose",
      border = "grey60", lwd = 1.2
    )
    graphics::text(
      0.50, 0.355,
      if (passed) "PASSED" else "REVIEW REQUIRED",
      font = 2, cex = 1.25
    )

    graphics::text(
      0.08, 0.23, "INTERPRETATION",
      adj = c(0, 1), font = 2, cex = 1.05
    )
    graphics::segments(0.08, 0.205, 0.92, 0.205, lwd = 1.2)
    graphics::text(
      0.11, 0.16,
      paste(
        "Formal tests use a 0.05 threshold.",
        "Always inspect the DHARMa residual panels before accepting a model.",
        "Statistical tests complement, but do not replace, graphical inspection.",
        sep = "\n"
      ),
      adj = c(0, 1), cex = 0.92
    )

    graphics::par(old_par)
  }

  grDevices::dev.off()
  device_open <- FALSE
  invisible(file)
}
