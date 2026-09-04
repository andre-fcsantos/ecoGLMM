###############################################################
## ecoGLMM — GENERIC ANALYSIS TEMPLATE
##
## Edit only Section 01 before running the complete script.
## Input: either one analysis-ready table or separate acoustic
## and environmental tables in CSV or Excel format.
## Each row of the final data set must represent one observation.
###############################################################

###############################################################
## 01. USER SETTINGS — EDIT THIS SECTION
###############################################################

# Choose "single" for one analysis-ready table or "separate" to
# join acoustic and environmental tables using the configured keys.
INPUT_MODE <- "single"

SINGLE_DATA_FILE <- NULL
ACOUSTIC_DATA_FILE <- NULL
ENVIRONMENTAL_DATA_FILE <- NULL

SINGLE_EXCEL_SHEET <- 1
ACOUSTIC_EXCEL_SHEET <- 1
ENVIRONMENTAL_EXCEL_SHEET <- 1

# These may be identical. Separate names are supported when the
# site identifier differs between the two input tables.
ACOUSTIC_JOIN_COLUMN <- "site"
ENVIRONMENTAL_JOIN_COLUMN <- "site"

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

read_input_table <- function(path, excel_sheet = 1) {
  extension <- tolower(tools::file_ext(path))

  if (extension %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Install the readxl package to import Excel files.", call. = FALSE)
    }
    output <- readxl::read_excel(path, sheet = excel_sheet)
  } else if (extension == "csv") {
    output <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    stop("Use an .xlsx, .xls, or .csv input file.", call. = FALSE)
  }

  as.data.frame(output)
}

INPUT_MODE <- match.arg(tolower(INPUT_MODE), c("single", "separate"))

if (INPUT_MODE == "single") {
  if (is.null(SINGLE_DATA_FILE)) {
    message("Select the analysis-ready data file.")
    SINGLE_DATA_FILE <- file.choose()
  }

  analysis_data <- read_input_table(
    SINGLE_DATA_FILE,
    excel_sheet = SINGLE_EXCEL_SHEET
  )
} else {
  if (is.null(ACOUSTIC_DATA_FILE)) {
    message("Select the acoustic data file.")
    ACOUSTIC_DATA_FILE <- file.choose()
  }
  if (is.null(ENVIRONMENTAL_DATA_FILE)) {
    message("Select the environmental data file.")
    ENVIRONMENTAL_DATA_FILE <- file.choose()
  }

  acoustic_data <- read_input_table(
    ACOUSTIC_DATA_FILE,
    excel_sheet = ACOUSTIC_EXCEL_SHEET
  )
  environmental_data <- read_input_table(
    ENVIRONMENTAL_DATA_FILE,
    excel_sheet = ENVIRONMENTAL_EXCEL_SHEET
  )

  if (!ACOUSTIC_JOIN_COLUMN %in% names(acoustic_data)) {
    stop(
      "The acoustic join column was not found: ",
      ACOUSTIC_JOIN_COLUMN,
      call. = FALSE
    )
  }
  if (!ENVIRONMENTAL_JOIN_COLUMN %in% names(environmental_data)) {
    stop(
      "The environmental join column was not found: ",
      ENVIRONMENTAL_JOIN_COLUMN,
      call. = FALSE
    )
  }
  if (anyNA(environmental_data[[ENVIRONMENTAL_JOIN_COLUMN]])) {
    stop("The environmental join column contains missing values.", call. = FALSE)
  }
  if (anyDuplicated(environmental_data[[ENVIRONMENTAL_JOIN_COLUMN]])) {
    stop(
      "Each join value must occur only once in the environmental table.",
      call. = FALSE
    )
  }

  overlapping_columns <- intersect(
    setdiff(names(acoustic_data), ACOUSTIC_JOIN_COLUMN),
    setdiff(names(environmental_data), ENVIRONMENTAL_JOIN_COLUMN)
  )

  if (length(overlapping_columns) > 0) {
    stop(
      "Non-key columns occur in both input tables: ",
      paste(overlapping_columns, collapse = ", "),
      ". Rename or remove them before joining.",
      call. = FALSE
    )
  }

  unmatched_values <- setdiff(
    unique(acoustic_data[[ACOUSTIC_JOIN_COLUMN]]),
    unique(environmental_data[[ENVIRONMENTAL_JOIN_COLUMN]])
  )

  if (length(unmatched_values) > 0) {
    warning(
      "Environmental data were not found for: ",
      paste(unmatched_values, collapse = ", "),
      call. = FALSE
    )
  }

  acoustic_n <- nrow(acoustic_data)
  analysis_data <- merge(
    acoustic_data,
    environmental_data,
    by.x = ACOUSTIC_JOIN_COLUMN,
    by.y = ENVIRONMENTAL_JOIN_COLUMN,
    all.x = TRUE,
    sort = FALSE
  )

  if (nrow(analysis_data) != acoustic_n) {
    stop(
      "The join changed the number of acoustic observations. ",
      "Check the configured join columns.",
      call. = FALSE
    )
  }
}

structure_columns <- unique(c(
  PREDICTORS,
  PERIOD_COLUMN,
  GROUP_COLUMN
))

missing_structure_columns <- setdiff(
  structure_columns,
  names(analysis_data)
)

if (length(missing_structure_columns) > 0) {
  stop(
    "Columns not found in the imported data: ",
    paste(missing_structure_columns, collapse = ", "),
    call. = FALSE
  )
}

cat(
  "Prepared ", nrow(analysis_data), " rows and ",
  ncol(analysis_data), " columns.\n",
  sep = ""
)

###############################################################
## 03. OPTIONAL RESPONSE TRANSFORMATIONS
###############################################################

# For an NDSI column ranging from -1 to 1 and beta-distributed indices,
# uncomment and adapt the following block. Use the resulting response
# names, such as "NDSI_beta", in RESPONSE_CONFIG.
#
# analysis_data <- ecoGLMM::prepare_acoustic_indices(
#   data = analysis_data,
#   ndsi = "NDSI",
#   beta_responses = c("AEI", "ACT"),
#   ndsi_output = "NDSI_beta",
#   epsilon = 0.001
# )

# Response validation occurs after optional transformations so newly
# created columns such as NDSI_beta are recognized correctly.
missing_responses <- setdiff(
  RESPONSE_CONFIG$response,
  names(analysis_data)
)

if (length(missing_responses) > 0) {
  stop(
    "Response columns not found after data preparation: ",
    paste(missing_responses, collapse = ", "),
    call. = FALSE
  )
}

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

dir.create(OUTPUT_DIRECTORY, recursive = TRUE, showWarnings = FALSE)

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
