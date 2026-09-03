#' Run DHARMa diagnostics for selected models
#'
#' @param object An `ecoglmm_fit` object.
#' @param nsim Number of simulations.
#' @param seed Optional random seed.
#' @return List containing a summary table and simulated residual objects.
#' @export
diagnose_models <- function(object, nsim = 1000, seed = NULL) {
  if (!inherits(object, "ecoglmm_fit")) {
    stop("`object` must inherit from `ecoglmm_fit`.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)
  residuals <- list()
  rows <- lapply(names(object$best_models), function(response) {
    model <- object$best_models[[response]][[1]]
    sim <- DHARMa::simulateResiduals(model, n = nsim, plot = FALSE)
    residuals[[response]] <<- sim
    uniformity <- DHARMa::testUniformity(sim, plot = FALSE)$p.value
    dispersion <- DHARMa::testDispersion(sim, plot = FALSE)$p.value
    outliers <- DHARMa::testOutliers(sim, plot = FALSE)$p.value
    passed <- all(c(uniformity, dispersion, outliers) > 0.05)
    data.frame(Response = response, Uniformity = uniformity,
               Dispersion = dispersion, Outliers = outliers,
               Status = ifelse(passed, "Passed", "Review"),
               stringsAsFactors = FALSE)
  })
  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL
  list(summary = summary, residuals = residuals, nsim = nsim, seed = seed)
}

