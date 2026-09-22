#' Run an ecological GLMM candidate-model pipeline
#'
#' @param data Prepared or raw data frame.
#' @param config Data frame with `response`, `family`, and optional `label`.
#' @param predictors Environmental predictor names.
#' @param period Temporal-factor column name.
#' @param group Random-intercept grouping column name.
#' @param period_levels Optional order of temporal levels.
#' @param standardize Standardize predictors with z-scores.
#' @param complete_cases Use one common complete-case dataset.
#' @param include_additive Include predictor + period models.
#' @param include_interactions Include predictor * period models.
#' @param competitive_delta Delta AICc threshold.
#' @param control Optional [glmmTMB::glmmTMBControl()] object.
#' @return An object of class `ecoglmm_fit`.
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
#' fit
#' summary(fit)
#' @export
run_ecoglmm <- function(data, config, predictors, period, group,
                        period_levels = NULL, standardize = TRUE,
                        complete_cases = TRUE, include_additive = TRUE,
                        include_interactions = TRUE, competitive_delta = 2,
                        control = NULL) {
  config <- validate_config(config)
  prepared <- prepare_ecodata(
    data, config, predictors, period, group, period_levels,
    standardize, complete_cases
  )
  all_models <- list()
  selections <- list()
  interaction_tests <- list()
  best_models <- list()

  for (i in seq_len(nrow(config))) {
    response <- config$response[i]
    candidates <- fit_candidate_set(
      prepared, response, predictors, period, group, config$family[i],
      include_additive, include_interactions, control
    )
    selection <- compare_models(candidates, competitive_delta)
    best_name <- selection$Model[1]
    all_models[[response]] <- candidates
    selections[[response]] <- selection
    interaction_tests[[response]] <- test_interactions(candidates, predictors)
    best_models[[response]] <- candidates[best_name]
  }

  coefficients <- extract_results(best_models, selections)
  overview <- do.call(rbind, lapply(names(selections), function(response) {
    x <- selections[[response]][1, , drop = FALSE]
    data.frame(Response = response, Model = x$Model, Predictor = x$Predictor,
               Type = x$Type, AICc = x$AICc,
               Akaike_weight = x$Akaike_weight, stringsAsFactors = FALSE)
  }))
  rownames(overview) <- NULL

  structure(list(
    call = match.call(), data = prepared, config = config,
    predictors = predictors, period = period, group = group,
    models = all_models, selection = selections,
    interaction_tests = interaction_tests, best_models = best_models,
    overview = overview, coefficients = coefficients,
    preparation = attr(prepared, "ecoglmm_preparation")
  ), class = "ecoglmm_fit")
}

#' @export
print.ecoglmm_fit <- function(x, ...) {
  cat("ecoGLMM analysis\n")
  cat("Responses:", length(x$best_models), "\n")
  cat("Observations:", nrow(x$data), "\n\n")
  print(x$overview, row.names = FALSE)
  invisible(x)
}

#' @export
summary.ecoglmm_fit <- function(object, ...) {
  list(
    overview = object$overview,
    coefficients = object$coefficients,
    interaction_tests = object$interaction_tests,
    preparation = object$preparation
  )
}

