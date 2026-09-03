#' Prepare common acoustic-index transformations
#'
#' @param data A data frame.
#' @param ndsi Name of the NDSI column, or `NULL`.
#' @param beta_responses Names of responses already bounded by zero and one.
#' @param ndsi_output Name assigned to transformed NDSI.
#' @param epsilon Boundary adjustment used by [adjust_beta()].
#' @return A modified data frame.
#' @export
prepare_acoustic_indices <- function(data, ndsi = "NDSI",
                                     beta_responses = c("AEI", "ACT"),
                                     ndsi_output = "NDSI_beta",
                                     epsilon = 0.001) {
  out <- as.data.frame(data)
  if (!is.null(ndsi)) {
    check_columns(out, ndsi)
    out[[ndsi_output]] <- adjust_beta((as.numeric(out[[ndsi]]) + 1) / 2,
                                      epsilon)
  }
  if (length(beta_responses)) {
    check_columns(out, beta_responses)
    for (x in beta_responses) {
      out[[x]] <- adjust_beta(as.numeric(out[[x]]), epsilon)
    }
  }
  out
}

#' Validate and prepare ecological modelling data
#'
#' @param data A data frame containing responses, predictors and grouping data.
#' @param config Response configuration with `response` and `family` columns.
#' @param predictors Environmental predictor names.
#' @param period Temporal-factor column.
#' @param group Random-intercept grouping column.
#' @param period_levels Optional ordered levels for the temporal factor.
#' @param standardize Logical; standardize numeric predictors using z-scores.
#' @param complete_cases Logical; use one shared complete-case dataset for all
#'   candidate models. This should normally remain `TRUE` for valid AICc
#'   comparisons.
#' @return Prepared data with preparation metadata stored as attributes.
#' @export
prepare_ecodata <- function(data, config, predictors, period, group,
                            period_levels = NULL, standardize = TRUE,
                            complete_cases = TRUE) {
  config <- validate_config(config)
  columns <- unique(c(config$response, predictors, period, group))
  check_columns(data, columns)
  out <- as.data.frame(data)

  for (x in unique(c(config$response, predictors))) {
    if (!is.numeric(out[[x]])) {
      converted <- suppressWarnings(as.numeric(gsub(",", ".", out[[x]],
                                                   fixed = TRUE)))
      if (any(is.na(converted) & !is.na(out[[x]]))) {
        stop("Column `", x, "` could not be safely converted to numeric.",
             call. = FALSE)
      }
      out[[x]] <- converted
    }
  }

  out[[group]] <- factor(out[[group]])
  if (is.null(period_levels)) {
    out[[period]] <- factor(out[[period]])
  } else {
    unknown <- setdiff(unique(stats::na.omit(out[[period]])), period_levels)
    if (length(unknown)) {
      stop("Unknown values in `", period, "`: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
    out[[period]] <- factor(out[[period]], levels = period_levels)
  }

  removed <- 0L
  if (complete_cases) {
    keep <- stats::complete.cases(out[, columns, drop = FALSE])
    removed <- sum(!keep)
    out <- out[keep, , drop = FALSE]
  }
  if (!nrow(out)) stop("No observations remain after preparation.", call. = FALSE)

  scaling <- NULL
  if (standardize) {
    constant <- predictors[vapply(out[predictors], function(x) {
      value <- stats::sd(x, na.rm = TRUE)
      !is.finite(value) || value == 0
    }, logical(1))]
    if (length(constant)) {
      stop("Constant predictors cannot be standardized: ",
           paste(constant, collapse = ", "), call. = FALSE)
    }
    scaling <- lapply(out[predictors], function(x) {
      c(center = mean(x, na.rm = TRUE), scale = stats::sd(x, na.rm = TRUE))
    })
    out[predictors] <- lapply(out[predictors], function(x) as.numeric(scale(x)))
  }

  attr(out, "ecoglmm_preparation") <- list(
    removed_rows = removed,
    original_n = nrow(data),
    analysis_n = nrow(out),
    standardize = standardize,
    scaling = scaling
  )
  out
}
