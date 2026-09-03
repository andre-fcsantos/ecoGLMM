#' Keep proportions inside the open interval (0, 1)
#'
#' @param x Numeric vector.
#' @param epsilon Small boundary value.
#' @return A numeric vector with boundary values adjusted.
#' @export
adjust_beta <- function(x, epsilon = 0.001) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  if (epsilon <= 0 || epsilon >= 0.5) {
    stop("`epsilon` must be between 0 and 0.5.", call. = FALSE)
  }
  x[x <= 0 & !is.na(x)] <- epsilon
  x[x >= 1 & !is.na(x)] <- 1 - epsilon
  x
}

quote_name <- function(x) {
  paste0("`", gsub("`", "", x, fixed = TRUE), "`")
}

resolve_family <- function(family) {
  if (!is.character(family) || length(family) != 1L) {
    stop("Each family must be a single character value.", call. = FALSE)
  }
  switch(
    tolower(family),
    beta = glmmTMB::beta_family(link = "logit"),
    gamma = stats::Gamma(link = "log"),
    gaussian = stats::gaussian(),
    poisson = stats::poisson(link = "log"),
    nbinom2 = glmmTMB::nbinom2(link = "log"),
    stop("Unsupported family: ", family, call. = FALSE)
  )
}

validate_config <- function(config) {
  needed <- c("response", "family")
  if (!is.data.frame(config) || !all(needed %in% names(config))) {
    stop("`config` must be a data frame containing `response` and `family`.",
         call. = FALSE)
  }
  if (anyDuplicated(config$response)) {
    stop("Response names in `config` must be unique.", call. = FALSE)
  }
  config$response <- as.character(config$response)
  config$family <- as.character(config$family)
  if (!"label" %in% names(config)) config$label <- config$response
  config$label <- as.character(config$label)
  config
}

check_columns <- function(data, columns) {
  absent <- setdiff(columns, names(data))
  if (length(absent)) {
    stop("Columns not found: ", paste(absent, collapse = ", "), call. = FALSE)
  }
}

