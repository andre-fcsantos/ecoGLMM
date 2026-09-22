#' Run DHARMa diagnostics for selected models
#'
#' @param object An `ecoglmm_fit` object.
#' @param nsim Number of simulations.
#' @param seed Optional random seed.
#' @return List containing a summary table and simulated residual objects.
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
#' diagnostics <- diagnose_models(fit, nsim = 20, seed = 1)
#' diagnostics$summary
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

