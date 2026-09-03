# ecoGLMM 0.1.3

- Standardized the complete real-data workflow and documentation in English.
- Added complete author metadata and ORCID identifiers for André Felipe
  Carneiro dos Santos, Bárbara Lins Caldas de Moraes, and Bruna Martins
  Bezerra.
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
