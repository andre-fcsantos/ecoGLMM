\name{run_ecoglmm}
\alias{run_ecoglmm}
\alias{adjust_beta}
\alias{prepare_acoustic_indices}
\alias{prepare_ecodata}
\alias{compare_models}
\alias{test_interactions}
\alias{extract_results}
\alias{diagnose_models}
\alias{plot_effect}
\alias{plot_selection}
\alias{plot_coefficients}
\alias{export_ecoglmm}
\title{Core ecoGLMM functions}
\description{Functions for preparing data, running candidate GLMMs, selecting
models, extracting results, checking diagnostics, producing figures, and
exporting an analysis.}
\usage{
run_ecoglmm(data, config, predictors, period, group,
  period_levels = NULL, standardize = TRUE, complete_cases = TRUE,
  include_additive = TRUE, include_interactions = TRUE,
  competitive_delta = 2, control = NULL)
}
\arguments{
  \item{data}{A data frame.}
  \item{config}{Response configuration data frame.}
  \item{predictors}{Names of environmental predictors.}
  \item{period}{Temporal-factor column name.}
  \item{group}{Random-intercept grouping column name.}
  \item{period_levels}{Optional ordered temporal levels.}
  \item{standardize}{Whether to z-standardize predictors.}
  \item{complete_cases}{Whether to use a shared complete-case dataset.}
  \item{include_additive}{Include additive models.}
  \item{include_interactions}{Include interaction models.}
  \item{competitive_delta}{Delta AICc threshold.}
  \item{control}{Optional glmmTMB control object.}
}
\value{An object of class \code{ecoglmm_fit}, or a function-specific result.}

