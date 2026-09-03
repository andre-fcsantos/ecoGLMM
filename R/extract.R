extract_r2 <- function(model) {
  mumin <- suppressWarnings(MuMIn::r.squaredGLMM(model))
  performance <- suppressWarnings(performance::r2_nakagawa(model))

  c(
    marginal = if (is.null(mumin)) NA_real_ else unname(mumin[1]),
    conditional = if (is.null(mumin)) NA_real_ else unname(mumin[2]),
    marginal_performance = if (is.null(performance)) {
      NA_real_
    } else {
      unname(performance$R2_marginal)
    },
    conditional_performance = if (is.null(performance)) {
      NA_real_
    } else {
      unname(performance$R2_conditional)
    }
  )
}

#' Extract coefficients and model-level statistics
#'
#' @param models Named list of selected models.
#' @param selections Named list of model-selection tables.
#' @param conf_level Confidence level for Wald intervals.
#' @return Tidy coefficient data frame.
#' @export
extract_results <- function(models, selections, conf_level = 0.95) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be between zero and one.", call. = FALSE)
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  rows <- lapply(names(models), function(response) {
    model <- models[[response]]
    selection <- selections[[response]]
    chosen <- selection[match(names(model), selection$Model), , drop = FALSE]
    coefs <- as.data.frame(summary(model[[1]])$coefficients$cond)
    coefs$Term <- rownames(coefs)
    rownames(coefs) <- NULL
    names(coefs)[1:4] <- c("Estimate", "SE", "Statistic", "p_value")
    r2 <- extract_r2(model[[1]])
    data.frame(
      Response = response,
      Model = names(model),
      Type = chosen$Type,
      Predictor = chosen$Predictor,
      Term = coefs$Term,
      Estimate = coefs$Estimate,
      SE = coefs$SE,
      Statistic = coefs$Statistic,
      p_value = coefs$p_value,
      CI_low = coefs$Estimate - as.numeric(z) * coefs$SE,
      CI_high = coefs$Estimate + as.numeric(z) * coefs$SE,
      R2_marginal = r2[["marginal"]],
      R2_conditional = r2[["conditional"]],
      R2_marginal_performance = r2[["marginal_performance"]],
      R2_conditional_performance = r2[["conditional_performance"]],
      AICc = chosen$AICc,
      Delta_AICc = chosen$Delta_AICc,
      Akaike_weight = chosen$Akaike_weight,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
