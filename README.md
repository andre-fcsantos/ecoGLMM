# ecoGLMM

`ecoGLMM` implements a reproducible candidate-model pipeline for ecological
generalized linear mixed models. It was created from the ecoacoustic analysis
workflow developed by André Felipe Carneiro dos Santos at the Graduate Program in Animal Biology,
Universidade Federal de Pernambuco (UFPE).

## Authors

- André Felipe Carneiro dos Santos — author and maintainer —
  ORCID: 0009-0000-8596-1975
- Bárbara Lins Caldas de Moraes — author — ORCID: 0000-0002-1804-9828
- Bruna Martins Bezerra — author — ORCID: 0000-0003-3039-121X

Version 0.1.3 fits one additive and one temporal-interaction model for each
response–predictor combination, compares models using AICc, performs nested
likelihood-ratio tests, extracts coefficients and Nakagawa R², runs DHARMa
diagnostics, creates figures, and exports results.

## Installation from the source archive

```r
install.packages("ecoGLMM_0.1.3.tar.gz", repos = NULL, type = "source")
```

## Installation from GitHub

```r
install.packages("remotes")
remotes::install_github("andre-fcsantos/ecoGLMM")
```

Development dependencies can also be installed manually:

```r
install.packages(c(
  "glmmTMB", "MuMIn", "performance", "DHARMa", "ggeffects", "ggplot2",
  "writexl"
))
```

## Ecoacoustic example

```r
library(ecoGLMM)

index_config <- data.frame(
  response = c("NDSI_beta", "AEI", "ACT", "BI", "ACI", "EVN", "ADI"),
  family = c("beta", "beta", "beta", "gamma", "gaussian", "gaussian",
             "gaussian"),
  label = c("NDSI", "AEI", "ACT", "BI", "ACI", "EVN", "ADI")
)

analysis_data <- merge(acoustic_data, environmental_data,
                       by = "ponto", all.x = TRUE)
analysis_data$site <- analysis_data$ponto
analysis_data$period <- analysis_data$periodo
analysis_data <- prepare_acoustic_indices(analysis_data)

fit <- run_ecoglmm(
  data = analysis_data,
  config = index_config,
  predictors = c("forest", "urban", "proximity", "watercourses"),
  period = "period",
  group = "site",
  period_levels = c("T-0", "T-1", "T-2", "T-3", "T-4")
)

fit
summary(fit)

diagnostics <- diagnose_models(fit, nsim = 1000, seed = 123)
plot_effect(fit, "NDSI_beta")
plot_selection(fit, "NDSI_beta", metric = "delta")
plot_coefficients(fit)

export_ecoglmm(fit, "results", diagnostics = diagnostics)
```

The columns `R2_marginal` and `R2_conditional` are calculated with
`MuMIn::r.squaredGLMM()` to reproduce the original analytical pipeline.
The alternative results from `performance::r2_nakagawa()` are retained in
columns ending in `_performance`.

## Important statistical safeguards

- Candidate models are fitted to one shared complete-case dataset by default,
  ensuring that AICc values are based on identical observations.
- AICc comparison stops if models have different sample sizes.
- Likelihood-ratio tests are only performed for nested additive and interaction
  models belonging to the same predictor.
- Diagnostic status reflects the actual DHARMa test results; graphical residual
  inspection is still required.
- Only continuous environmental predictors are z-standardized. Period contrasts
  are not described as standardized coefficients.

## Development status

This is an initial research version. Before CRAN submission, add real-data
regression tests, check convergence warnings across supported families, and
complete the user documentation.
