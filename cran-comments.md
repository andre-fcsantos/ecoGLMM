## Update from ecoGLMM 0.1.3

This update improves the generic analysis template with preflight data-quality
checks and an optional predictor-correlation screen. It also restores the
multi-page DHARMa diagnostic PDF export and documents these changes in the
vignette.

The correlation screen reports redundancy among environmental predictors.
It does not remove variables automatically or apply VIF to the current
single-environmental-predictor candidate models.

Writing functions require an explicit output path. Packaged examples and
the template use temporary output directories by default.

## Release verification

This file is a preparation draft for ecoGLMM 0.1.4.
Record the final local R CMD check --as-cran and win-builder results for
this version before submitting it to CRAN. Results from older versions
must not be reported as checks of this release.

The GitHub Actions workflow checks Windows, macOS and Ubuntu with R release,
and Ubuntu with R-devel. Confirm that all jobs pass for the final release
commit before submission.

## Notes

"AICc" is the standard abbreviation for the small-sample corrected Akaike
information criterion. Burnham, Nakagawa and Schielzeth are surnames in
the methodological references included in DESCRIPTION.

## Downstream dependencies

Verify the current reverse-dependency status before submission.
