#' Export ecoGLMM results to an output directory
#'
#' @param object An `ecoglmm_fit` object.
#' @param path Output directory.
#' @param diagnostics Optional result returned by [diagnose_models()].
#' @param figures Save effect and selection figures.
#' @return Invisibly returns generated file paths.
#' @export
export_ecoglmm <- function(object, path = "ecoGLMM_results",
                           diagnostics = NULL, figures = TRUE) {
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

