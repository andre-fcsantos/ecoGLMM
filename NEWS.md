# ecoGLMM 0.1.4

- Added preflight data-quality checks to the generic analysis template,
  including response-family validation, non-finite-value detection, constant
  predictor detection, and summaries of missing and unique values.
- Added an optional predictor-correlation screen to the template. Because the
  current candidate set fits one environmental predictor per model, the screen
  reports potentially redundant ecological hypotheses without applying VIF or
  removing variables automatically.
- Added exports for the data-quality summary, predictor correlations, and the
  complete preflight-check object.
- Added an automated test ensuring that the installed generic template exists,
  is non-empty, and can be parsed by R.
- Restored automatic export of a multi-page `DHARMa_diagnostics.pdf`, with the
  residual panels and a formal-test summary for every selected response model.

# ecoGLMM 0.1.3

- Published on CRAN on 30 September 2026.
- Assigned the permanent CRAN DOI
  [10.32614/CRAN.package.ecoGLMM](https://doi.org/10.32614/CRAN.package.ecoGLMM).

- Standardized the complete real-data workflow and documentation in English.
- Added complete author metadata and ORCID identifiers for André Felipe
  Carneiro dos Santos, Bruna Martins Bezerra, and Bárbara Lins Caldas de
  Moraes.
- Restored `MuMIn::r.squaredGLMM()` as the primary R-squared method to match the
  original dissertation pipeline.
- Added parallel Nakagawa R-squared results from `performance::r2_nakagawa()`
  in explicitly labelled columns.

# ecoGLMM 0.1.2

- Corrected likelihood-ratio-test degrees of freedom to report the difference
  in model parameters rather than the total parameter count.
- Removed unused colour and fill labels from additive effect plots.

# ecoGLMM 0.1.1

- Corrected the `Authors@R` role field in `DESCRIPTION`.

# ecoGLMM 0.1.0

- Initial research release.
- Fits additive and predictor-by-period GLMM candidate sets.
- Supports beta, Gamma, Gaussian, Poisson, and negative-binomial families.
- Calculates AICc, Akaike weights, evidence ratios, and competitive-model flags.
- Compares nested additive and interaction models with likelihood-ratio tests.
- Extracts coefficients, Wald confidence intervals, and Nakagawa R-squared.
- Runs DHARMa uniformity, dispersion, and outlier diagnostics.
- Produces marginal-effect, model-selection, and coefficient figures.
- Exports analysis tables and figures.
