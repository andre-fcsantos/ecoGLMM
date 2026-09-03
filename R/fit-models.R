make_formula <- function(response, predictor, period, group, interaction) {
  operator <- if (interaction) "*" else "+"
  text <- sprintf("%s ~ %s %s %s + (1 | %s)",
                  quote_name(response), quote_name(predictor), operator,
                  quote_name(period), quote_name(group))
  stats::as.formula(text)
}

fit_one_model <- function(data, response, predictor, period, group,
                          family, interaction, control = NULL) {
  args <- list(
    formula = make_formula(response, predictor, period, group, interaction),
    data = data,
    family = resolve_family(family),
    na.action = stats::na.fail
  )
  if (!is.null(control)) args$control <- control
  do.call(glmmTMB::glmmTMB, args)
}

fit_candidate_set <- function(data, response, predictors, period, group,
                              family, include_additive = TRUE,
                              include_interactions = TRUE, control = NULL) {
  if (!include_additive && !include_interactions) {
    stop("At least one candidate-model type must be requested.", call. = FALSE)
  }
  models <- list()
  for (predictor in predictors) {
    if (include_additive) {
      models[[paste0(predictor, "_Add")]] <- fit_one_model(
        data, response, predictor, period, group, family, FALSE, control
      )
    }
    if (include_interactions) {
      models[[paste0(predictor, "_Int")]] <- fit_one_model(
        data, response, predictor, period, group, family, TRUE, control
      )
    }
  }
  models
}

#' Compare a candidate set using corrected Akaike information criterion
#'
#' @param models Named list of fitted models.
#' @param competitive_delta Maximum delta AICc defining competitive models.
#' @return Model-selection data frame.
#' @export
compare_models <- function(models, competitive_delta = 2) {
  if (!is.list(models) || !length(models) || is.null(names(models))) {
    stop("`models` must be a non-empty named list.", call. = FALSE)
  }
  n <- vapply(models, stats::nobs, numeric(1))
  if (length(unique(n)) != 1L) {
    stop("AICc comparison requires the same observations in every model.",
         call. = FALSE)
  }
  k <- vapply(models, function(x) attr(stats::logLik(x), "df"), numeric(1))
  aic <- vapply(models, stats::AIC, numeric(1))
  denominator <- n - k - 1
  if (any(denominator <= 0)) {
    stop("Sample size is too small to calculate AICc for at least one model.",
         call. = FALSE)
  }
  aicc <- aic + (2 * k * (k + 1)) / denominator
  tab <- data.frame(
    Model = names(models),
    Predictor = sub("_(Add|Int)$", "", names(models)),
    Type = ifelse(grepl("_Int$", names(models)), "Interaction", "Additive"),
    n = unname(n), K = unname(k), AICc = unname(aicc),
    stringsAsFactors = FALSE
  )
  tab <- tab[order(tab$AICc), , drop = FALSE]
  tab$Delta_AICc <- tab$AICc - min(tab$AICc)
  relative <- exp(-0.5 * tab$Delta_AICc)
  tab$Akaike_weight <- relative / sum(relative)
  tab$Evidence_ratio <- max(tab$Akaike_weight) / tab$Akaike_weight
  tab$Competitive <- tab$Delta_AICc <= competitive_delta
  rownames(tab) <- NULL
  tab
}

#' Likelihood-ratio tests for additive versus interaction models
#'
#' @param models Named candidate-model list.
#' @param predictors Predictor names.
#' @return Data frame of likelihood-ratio tests.
#' @export
test_interactions <- function(models, predictors) {
  rows <- lapply(predictors, function(x) {
    add_name <- paste0(x, "_Add")
    int_name <- paste0(x, "_Int")
    if (!all(c(add_name, int_name) %in% names(models))) return(NULL)
    test <- stats::anova(models[[add_name]], models[[int_name]])
    model_df <- attr(stats::logLik(models[[int_name]]), "df") -
      attr(stats::logLik(models[[add_name]]), "df")
    data.frame(
      Predictor = x,
      Chisq = test$Chisq[2],
      df = model_df,
      p_value = test$`Pr(>Chisq)`[2],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(Predictor = character(), Chisq = numeric(),
                      df = numeric(), p_value = numeric())
  }
  rownames(out) <- NULL
  out
}
