## Test environments

* Local: Windows 10 x64, R 4.6.0
* GitHub Actions:
  * Windows, R release
  * macOS, R release
  * Ubuntu, R release
  * Ubuntu, R-devel
* win-builder: Windows Server 2022 x64,
  R-devel (2026-08-31 r90457 ucrt)

## R CMD check results

Local and GitHub Actions checks:

0 errors | 0 warnings | 0 notes

Win-builder:

0 errors | 0 warnings | 1 note

## Notes

This is a new submission.

CRAN incoming feasibility identified "AICc" and "Nakagawa" as
possibly misspelled words. AICc is the standard abbreviation for the
small-sample corrected Akaike information criterion. Nakagawa is a
surname used in the established name of the marginal and conditional
R-squared method implemented by the package.

## Downstream dependencies

There are currently no downstream dependencies.

