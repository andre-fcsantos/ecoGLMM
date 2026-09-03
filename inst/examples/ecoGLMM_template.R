###############################################################
## ecoGLMM — GENERIC ANALYSIS TEMPLATE
##
## Edit only Section 01 before running the complete script.
## Input: one analysis-ready CSV or Excel table.
## Each row must represent one observation.
###############################################################

###############################################################
## 01. USER SETTINGS — EDIT THIS SECTION
###############################################################

DATA_FILE <- NULL
EXCEL_SHEET <- 1
GROUP_COLUMN <- "site"
PERIOD_COLUMN <- "period"
PERIOD_LEVELS <- c("T-0", "T-1", "T-2", "T-3", "T-4")
PREDICTORS <- c("forest", "urban")

# One row per response. Supported families:
# "beta", "gamma", "gaussian", "poisson", and "nbinom2".
RESPONSE_CONFIG <- data.frame(
  response = c("acoustic_index"),
  family = c("beta"),
  label = c("Acoustic index"),
  stringsAsFactors = FALSE
)

STANDARDIZE_PREDICTORS <- TRUE
COMPLETE_CASES <- TRUE
COMPETITIVE_DELTA <- 2
RUN_DIAGNOSTICS <- TRUE
DHARMA_SIMULATIONS <- 1000
RANDOM_SEED <- 123
OUTPUT_DIRECTORY <- file.path(getwd(), "ecoGLMM_results")

###############################################################
## 02. PACKAGE AND DATA IMPORT
###############################################################

if (!requireNamespace("ecoGLMM", quietly = TRUE)) {
  stop(
    "Install ecoGLMM before running this script. ",
    "See https://github.com/andre-fcsantos/ecoGLMM",
    call. = FALSE
  )
}

if (is.null(DATA_FILE)) {
  message("Select the analysis-ready data file.")
  DATA_FILE <- file.choose()
}

extension <- tolower(tools::file_ext(DATA_FILE))

if (extension %in% c("xlsx", "xls")) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Install the readxl package to import Excel files.", call. = FALSE)
  }
  analysis_data <- readxl::read_excel(DATA_FILE, sheet = EXCEL_SHEET)
} else if (extension == "csv") {
  analysis_data <- utils::read.csv(
    DATA_FILE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
} else {
  stop("Use an .xlsx, .xls, or .csv input file.", call. = FALSE)
}

analysis_data <- as.data.frame(analysis_data)

required_columns <- unique(c(
  RESPONSE_CONFIG$response,
  PREDICTORS,
  PERIOD_COLUMN,
  GROUP_COLUMN
))

missing_columns <- setdiff(required_columns, names(analysis_data))

if (length(missing_columns) > 0) {
  stop(
    "Columns not found in the input table: ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

cat(
  "Imported ", nrow(analysis_data), " rows and ",
  ncol(analysis_data), " columns.\n",
  sep = ""
)

###############################################################
## 03. OPTIONAL RESPONSE TRANSFORMATIONS
###############################################################

# For an NDSI column ranging from -1 to 1 and beta-distributed indices,
# uncomment and adapt the following block. Remember to use the resulting
# response names in RESPONSE_CONFIG.
#
# analysis_data <- ecoGLMM::prepare_acoustic_indices(
#   data = analysis_data,
#   ndsi = "NDSI",
#   beta_responses = c("AEI", "ACT"),
#   ndsi_output = "NDSI_beta",
#   epsilon = 0.001
# )

###############################################################
## 04. FIT AND COMPARE CANDIDATE MODELS
###############################################################

set.seed(RANDOM_SEED)

results <- ecoGLMM::run_ecoglmm(
  data = analysis_data,
  config = RESPONSE_CONFIG,
  predictors = PREDICTORS,
  period = PERIOD_COLUMN,
  group = GROUP_COLUMN,
  period_levels = PERIOD_LEVELS,
  standardize = STANDARDIZE_PREDICTORS,
  complete_cases = COMPLETE_CASES,
  include_additive = TRUE,
  include_interactions = TRUE,
  competitive_delta = COMPETITIVE_DELTA
)

print(results)
analysis_summary <- summary(results)

for (response in names(results$selection)) {
  cat("\nMODEL SELECTION — ", response, "\n", sep = "")
  print(results$selection[[response]])

  cat("\nINTERACTION TEST — ", response, "\n", sep = "")
  print(results$interaction_tests[[response]])
}

###############################################################
## 05. DIAGNOSTICS
###############################################################

diagnostics <- NULL

if (RUN_DIAGNOSTICS) {
  diagnostics <- ecoGLMM::diagnose_models(
    object = results,
    nsim = DHARMA_SIMULATIONS,
    seed = RANDOM_SEED
  )
  print(diagnostics$summary)

  # Example for graphical inspection:
  # plot(diagnostics$residuals[[RESPONSE_CONFIG$response[1]]])
}

###############################################################
## 06. FIGURES AND EXPORT
###############################################################

# Examples for the RStudio Plots pane:
# ecoGLMM::plot_effect(results, RESPONSE_CONFIG$response[1])
# ecoGLMM::plot_selection(
#   results,
#   RESPONSE_CONFIG$response[1],
#   metric = "delta"
# )
# ecoGLMM::plot_coefficients(results)

generated_files <- ecoGLMM::export_ecoglmm(
  object = results,
  path = OUTPUT_DIRECTORY,
  diagnostics = diagnostics,
  figures = TRUE
)

saveRDS(
  results,
  file.path(OUTPUT_DIRECTORY, "ecoGLMM_results.rds")
)

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

cat(
  "\nAnalysis completed. Results were saved to:\n",
  normalizePath(OUTPUT_DIRECTORY, winslash = "/", mustWork = FALSE),
  "\n",
  sep = ""
)
