#' Export ecoGLMM results to an output directory
#'
#' @param object An `ecoglmm_fit` object.
#' @param path Output directory. Must be explicitly supplied by the caller.
#' @param diagnostics Optional result returned by [diagnose_models()].
#' @param figures Save effect and selection figures.
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
                           diagnostics = NULL, figures = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Supply a non-empty output directory in `path`.", call. = FALSE)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  selection <- do.call(rbind, lapply(names(object$selection), function(x) {
    cbind(Response = x, object$selection[[x]])
  }))
  lrt <- do.call(rbind, lapply(names(object$interaction_tests), function(x) {
    cbind(Response = x, object$interaction_tests[[x]])
  }))
  sheets <- list(Overview = object$overview, Coefficients = object$coefficients,
                 Model_selection = selection, Interaction_LRT = lrt)
  if (!is.null(diagnostics)) sheets$Diagnostics <- diagnostics$summary
  workbook <- file.path(path, "ecoGLMM_results.xlsx")
  writexl::write_xlsx(sheets, workbook)
  files <- workbook

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

