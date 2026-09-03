#' Plot the marginal effect of a selected model
#'
#' @param object An `ecoglmm_fit` object.
#' @param response Response name.
#' @param show_observed Add observed values.
#' @return A ggplot object.
#' @export
plot_effect <- function(object, response, show_observed = TRUE) {
  if (!response %in% names(object$best_models)) {
    stop("Unknown response: ", response, call. = FALSE)
  }
  model <- object$best_models[[response]][[1]]
  predictor <- object$overview$Predictor[object$overview$Response == response]
  interaction <- object$overview$Type[object$overview$Response == response] ==
    "Interaction"
  terms <- if (interaction) c(predictor, object$period) else predictor
  pred <- as.data.frame(ggeffects::ggpredict(model, terms = terms))
  p <- ggplot2::ggplot(pred, ggplot2::aes(x = x, y = predicted))
  if (interaction) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high,
                                       fill = group), alpha = 0.15,
                           colour = NA) +
      ggplot2::geom_line(ggplot2::aes(colour = group), linewidth = 1.05)
  } else {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = conf.low, ymax = conf.high),
                           fill = "grey75", alpha = 0.25) +
      ggplot2::geom_line(linewidth = 1.05)
  }
  if (show_observed) {
    observed <- data.frame(x = object$data[[predictor]],
                           y = object$data[[response]])
    p <- p + ggplot2::geom_point(
      data = observed, ggplot2::aes(x = x, y = y), inherit.aes = FALSE,
      alpha = 0.22, size = 1.5
    )
  }
  label <- object$config$label[object$config$response == response]
  p <- p + ggplot2::theme_classic(base_size = 13) +
    ggplot2::labs(x = predictor, y = label)
  if (interaction) {
    p <- p + ggplot2::labs(colour = "Period", fill = "Period")
  }
  p
}

#' Plot model-selection results
#'
#' @param object An `ecoglmm_fit` object.
#' @param response Response name.
#' @param metric Either `delta` or `weight`.
#' @return A ggplot object.
#' @export
plot_selection <- function(object, response, metric = c("delta", "weight")) {
  metric <- match.arg(metric)
  tab <- object$selection[[response]]
  if (is.null(tab)) stop("Unknown response: ", response, call. = FALSE)
  if (metric == "delta") {
    p <- ggplot2::ggplot(tab, ggplot2::aes(
      x = stats::reorder(Model, Delta_AICc), y = Delta_AICc, fill = Type
    )) + ggplot2::geom_col() +
      ggplot2::geom_hline(yintercept = 2, linetype = 2, colour = "firebrick") +
      ggplot2::labs(y = "Delta AICc")
  } else {
    p <- ggplot2::ggplot(tab, ggplot2::aes(
      x = stats::reorder(Model, Akaike_weight), y = Akaike_weight, fill = Type
    )) + ggplot2::geom_col() + ggplot2::labs(y = "Akaike weight")
  }
  p + ggplot2::coord_flip() + ggplot2::theme_classic(base_size = 13) +
    ggplot2::labs(x = NULL, title = response)
}

#' Plot coefficients and confidence intervals
#'
#' @param object An `ecoglmm_fit` object.
#' @param include_intercept Include intercept terms.
#' @return A faceted ggplot object.
#' @export
plot_coefficients <- function(object, include_intercept = FALSE) {
  tab <- object$coefficients
  if (!include_intercept) tab <- tab[tab$Term != "(Intercept)", , drop = FALSE]
  ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(Term, Estimate),
                                    y = Estimate)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = CI_low, ymax = CI_high),
                           width = 0.15) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~Response, scales = "free_y") +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::labs(x = NULL, y = "Coefficient estimate")
}
