# ecoGLMM

[![CRAN status](https://www.r-pkg.org/badges/version/ecoGLMM)](https://cran.r-project.org/package=ecoGLMM)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/ecoGLMM)](https://cran.r-project.org/package=ecoGLMM)
[![DOI](https://img.shields.io/badge/DOI-10.32614%2FCRAN.package.ecoGLMM-blue)](https://doi.org/10.32614/CRAN.package.ecoGLMM)
[![R-CMD-check](https://github.com/andre-fcsantos/ecoGLMM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andre-fcsantos/ecoGLMM/actions/workflows/R-CMD-check.yaml)

`ecoGLMM` implements a reproducible candidate-model pipeline for ecological
generalized linear mixed models. It was created from the ecoacoustic analysis
workflow developed by André Felipe Carneiro dos Santos at the Graduate Program in Animal Biology,
Universidade Federal de Pernambuco (UFPE).

## Authors

- André Felipe Carneiro dos Santos — author and maintainer —
  ORCID: 0009-0000-8596-1975
- Bruna Martins Bezerra — author — ORCID: 0000-0003-3039-121X
- Bárbara Lins Caldas de Moraes — author — ORCID: 0000-0002-1804-9828

Version 0.1.3 fits one additive and one temporal-interaction model for each
response–predictor combination, compares models using AICc, performs nested
likelihood-ratio tests, extracts coefficients and Nakagawa R², runs DHARMa
diagnostics, creates figures, and exports results.

## Installation

Install the stable release from CRAN:

```r
install.packages("ecoGLMM")
library(ecoGLMM)
```

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("andre-fcsantos/ecoGLMM", upgrade = "never")
```

## Documentation

The package includes two complementary resources:

- A generic editable template installed at `inst/examples/ecoGLMM_template.R`.
- A step-by-step vignette covering data structure, model configuration, diagnostics, figures, and exports.

After installation, open the vignette with:

```r
vignette("getting-started", package = "ecoGLMM")
```

Copy the complete script to a writable directory with:

```r
template_path <- system.file(
  "examples",
  "ecoGLMM_template.R",
  package = "ecoGLMM"
)

if (!nzchar(template_path) || !file.exists(template_path)) {
  stop("The generic analysis template was not found in the installed package.")
}

template_copy <- file.path(tempdir(), "ecoGLMM_template.R")
copied <- file.copy(template_path, template_copy, overwrite = TRUE)

if (!copied || !file.exists(template_copy) || file.size(template_copy) == 0) {
  stop("The generic analysis template could not be copied correctly.")
}

if (interactive()) {
  file.edit(template_copy)
}
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

## Citation

To cite `ecoGLMM` in publications, use `citation("ecoGLMM")`. The permanent
CRAN DOI is [10.32614/CRAN.package.ecoGLMM](https://doi.org/10.32614/CRAN.package.ecoGLMM).

## Development status

Version 0.1.3 is available on
[CRAN](https://cran.r-project.org/package=ecoGLMM). Development continues on
GitHub; please report problems through the issue tracker.
