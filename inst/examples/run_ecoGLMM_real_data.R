###############################################################
## ecoGLMM 0.1.3 — COMPLETE REAL-DATA WORKFLOW
## Authors:
## André Felipe Carneiro dos Santos
## Bárbara Lins Caldas de Moraes
## Bruna Martins Bezerra
##
## This script imports the acoustic and environmental tables,
## validates and merges them, fits the complete GLMM candidate set,
## runs DHARMa diagnostics, and exports tables and figures.
###############################################################

###############################################################
## 01. SETTINGS
###############################################################

options(scipen = 999)

# Change to FALSE for a faster preliminary run without DHARMa.
RUN_DIAGNOSTICS <- TRUE
DHARMA_SIMULATIONS <- 1000
OUTPUT_DIRECTORY <- file.path(getwd(), "ecoGLMM_results")

# Original column names in the input workbooks. Edit these values when needed.
SITE_COLUMN <- "ponto"
PERIOD_COLUMN <- "periodo"

###############################################################
## 02. PACKAGE CHECK
###############################################################

required_packages <- c(
  "ecoGLMM", "readxl", "glmmTMB", "MuMIn", "performance",
  "DHARMa", "ggeffects", "ggplot2", "writexl"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before continuing: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(ecoGLMM)

if (packageVersion("ecoGLMM") < "0.1.3") {
  stop("This script requires ecoGLMM 0.1.3 or later.", call. = FALSE)
}

cat(
  "\necoGLMM loaded — version ",
  as.character(packageVersion("ecoGLMM")),
  "\n",
  sep = ""
)

###############################################################
## 03. DATA IMPORT
###############################################################

message("Select the ACOUSTIC DATA workbook.")
acoustic_path <- file.choose()

message("Select the 1-km ENVIRONMENTAL DATA workbook.")
environmental_path <- file.choose()

acoustic_data <- readxl::read_excel(acoustic_path)
environmental_data <- readxl::read_excel(environmental_path)

cat(
  "\nAcoustic table: ", nrow(acoustic_data), " rows and ",
  ncol(acoustic_data), " columns.\n",
  sep = ""
)
cat(
  "Environmental table: ", nrow(environmental_data), " rows and ",
  ncol(environmental_data), " columns.\n",
  sep = ""
)

###############################################################
## 04. VALIDATION AND MERGE
###############################################################

if (!SITE_COLUMN %in% names(acoustic_data) ||
    !SITE_COLUMN %in% names(environmental_data)) {
  stop(
    "Both workbooks must contain the configured site column: `",
    SITE_COLUMN,
    "`.",
    call. = FALSE
  )
}

if (anyDuplicated(environmental_data[[SITE_COLUMN]])) {
  duplicated_sites <- unique(
    environmental_data[[SITE_COLUMN]][
      duplicated(environmental_data[[SITE_COLUMN]])
    ]
  )
  stop(
    "The environmental table contains duplicated sites: ",
    paste(duplicated_sites, collapse = ", "),
    ". Correct them before merging.",
    call. = FALSE
  )
}

unmatched_sites <- setdiff(
  unique(acoustic_data[[SITE_COLUMN]]),
  unique(environmental_data[[SITE_COLUMN]])
)

if (length(unmatched_sites) > 0) {
  warning(
    "Environmental data were not found for: ",
    paste(unmatched_sites, collapse = ", "),
    call. = FALSE
  )
}

analysis_data <- merge(
  acoustic_data,
  environmental_data,
  by = SITE_COLUMN,
  all.x = TRUE,
  sort = FALSE
)

if (nrow(analysis_data) != nrow(acoustic_data)) {
  stop(
    "The merge unexpectedly changed the number of observations. ",
    "Check the configured site column for duplicated values.",
    call. = FALSE
  )
}

if ("-" %in% names(analysis_data)) {
  analysis_data[["-"]] <- NULL
}

empty_columns <- vapply(
  analysis_data,
  function(x) all(is.na(x)),
  logical(1)
)
analysis_data <- analysis_data[, !empty_columns, drop = FALSE]

required_columns <- c(
  SITE_COLUMN, PERIOD_COLUMN,
  "NDSI", "AEI", "ACT", "BI", "ACI", "EVN", "ADI",
  "forest", "urban", "proximity", "watercourses"
)

absent_columns <- setdiff(required_columns, names(analysis_data))

if (length(absent_columns) > 0) {
  stop(
    "Columns missing after the merge: ",
    paste(absent_columns, collapse = ", "),
    call. = FALSE
  )
}

# English aliases ensure that formula terms and exported coefficients
# use `period` and `site` rather than the original Portuguese names.
analysis_data$site <- analysis_data[[SITE_COLUMN]]
analysis_data$period <- analysis_data[[PERIOD_COLUMN]]

cat("Merged data: ", nrow(analysis_data), " observations.\n", sep = "")

###############################################################
## 05. ACOUSTIC-INDEX PREPARATION
###############################################################

analysis_data <- prepare_acoustic_indices(
  data = analysis_data,
  ndsi = "NDSI",
  beta_responses = c("AEI", "ACT"),
  ndsi_output = "NDSI_beta",
  epsilon = 0.001
)

if (any(analysis_data$BI <= 0, na.rm = TRUE)) {
  stop(
    "BI contains values less than or equal to zero and therefore cannot be ",
    "modelled with a Gamma distribution. Review these values.",
    call. = FALSE
  )
}

###############################################################
## 06. MODEL CONFIGURATION
###############################################################

index_config <- data.frame(
  response = c("NDSI_beta", "AEI", "ACT", "BI", "ACI", "EVN", "ADI"),
  family = c(
    "beta", "beta", "beta", "gamma", "gaussian", "gaussian", "gaussian"
  ),
  label = c("NDSI", "AEI", "ACT", "BI", "ACI", "EVN", "ADI"),
  stringsAsFactors = FALSE
)

environmental_predictors <- c(
  "forest", "urban", "proximity", "watercourses"
)

period_levels <- c("T-0", "T-1", "T-2", "T-3", "T-4")

###############################################################
## 07. GLMM FITTING AND MODEL SELECTION
###############################################################

set.seed(123)

results <- run_ecoglmm(
  data = analysis_data,
  config = index_config,
  predictors = environmental_predictors,
  period = "period",
  group = "site",
  period_levels = period_levels,
  standardize = TRUE,
  complete_cases = TRUE,
  include_additive = TRUE,
  include_interactions = TRUE,
  competitive_delta = 2
)

cat("\n=========================================\n")
cat("BEST-MODEL SUMMARY\n")
cat("=========================================\n\n")
print(results)

cat(
  "\nRows removed because of missing values: ",
  results$preparation$removed_rows,
  "\n",
  sep = ""
)

full_summary <- summary(results)

for (response in names(results$selection)) {
  cat("\n=========================================\n")
  cat("AICc MODEL SELECTION — ", response, "\n", sep = "")
  cat("=========================================\n")
  print(results$selection[[response]])

  cat("\nINTERACTION LRT — ", response, "\n", sep = "")
  print(results$interaction_tests[[response]])
}

###############################################################
## 08. DHARMa DIAGNOSTICS
###############################################################

diagnostics <- NULL

if (RUN_DIAGNOSTICS) {
  cat("\nRunning DHARMa diagnostics...\n")
  diagnostics <- diagnose_models(
    object = results,
    nsim = DHARMA_SIMULATIONS,
    seed = 123
  )
  print(diagnostics$summary)
}

###############################################################
## 09. EXPORT
###############################################################

dir.create(OUTPUT_DIRECTORY, recursive = TRUE, showWarnings = FALSE)

generated_files <- export_ecoglmm(
  object = results,
  path = OUTPUT_DIRECTORY,
  diagnostics = diagnostics,
  figures = TRUE
)

saveRDS(results, file.path(OUTPUT_DIRECTORY, "ecoGLMM_results.rds"))

if (!is.null(diagnostics)) {
  saveRDS(
    diagnostics,
    file.path(OUTPUT_DIRECTORY, "DHARMa_diagnostics.rds")
  )
}

capture.output(
  sessionInfo(),
  file = file.path(OUTPUT_DIRECTORY, "sessionInfo.txt")
)

###############################################################
## 10. OPTIONAL RSTUDIO PLOTS
###############################################################

# Remove the leading # to display a plot again.
# plot_effect(results, "NDSI_beta")
# plot_selection(results, "NDSI_beta", metric = "delta")
# plot_selection(results, "NDSI_beta", metric = "weight")
# plot_coefficients(results)

###############################################################
## 11. COMPLETION MESSAGE
###############################################################

cat("\n=========================================\n")
cat("PIPELINE COMPLETED\n")
cat("=========================================\n")
cat("Results saved to:\n", OUTPUT_DIRECTORY, "\n", sep = "")
cat("\nMain R objects:\n")
cat("- results\n")
cat("- diagnostics\n")
cat("- full_summary\n")
cat("- generated_files\n")
