################################################################################
# Title = Extract and split Day 1 medication responses
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table
# Creation Date = 23 June 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Reads the Day 1 free-text medication response, splits listed
# medications at approved delimiters, and returns one medication name per row.
# Inputs = One or more primary PsychoPy Day 1 wide session CSV files
# Outputs = medication_list_extracted.csv through the master pipeline
# Tags = PsychoPy, demographics, medication, text parsing, Day 1, R
################################################################################
# Public release: private paths, study attribution, and participant-specific
# records have been removed. Execution has not been validated for this release.

# Standalone mode accepts explicit files; sourcing exposes extract_one_file() to
# the master without executing any file writes.
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop(
      paste(
        "Provide: output.csv input1.csv [input2.csv ...].",
        "Use run_all_extractions.R for the full participant extraction."
      )
    )
  }
  output_file <- args[[1]]
  input_files <- args[-1]
}

# Resolve the shared metadata helper beside this module for portable direct runs.
module_file_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)
module_script_dir <- if (length(module_file_argument) > 0) {
  dirname(normalizePath(sub("^--file=", "", module_file_argument[[1]])))
} else {
  normalizePath(getwd())
}

# Early sessions may store medication in the main column or its companion alias.
medication_columns <- c(
  "O_Medication.text",
  "Demographics_day1_only.O_Medication.text"
)

# Scalar and alias helpers select the first recorded day-1 response.
first_nonmissing <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  x[[1]]
}

first_medication_response <- function(df) {
  matching_columns <- medication_columns[medication_columns %in% names(df)]

  for (column_name in matching_columns) {
    response <- first_nonmissing(df[[column_name]])
    if (!is.na(response)) return(response)
  }

  NA_character_
}

# Normalize restart suffixes for direct module runs.
normalize_participant <- function(x, input_file) {
  value <- as.character(first_nonmissing(x))
  if (is.na(value) || !grepl("^[0-9]+", value)) {
    stop(sprintf("%s has an invalid participant value.", basename(input_file)))
  }
  sub("^([0-9]+).*$", "\\1", value)
}

normalize_day <- function(x, input_file) {
  value <- as.character(first_nonmissing(x))
  if (is.na(value) || !grepl("^[12]", value)) {
    stop(sprintf("%s has an invalid day value.", basename(input_file)))
  }
  sub("^([12]).*$", "\\1", value)
}

# Medication entries are separated by the delimiters used in the free-text form;
# duplicate names within a response are collapsed for that participant.
split_medications <- function(response) {
  if (is.na(response) || !nzchar(trimws(response))) {
    return(character(0))
  }

  medications <- unlist(
    strsplit(response, "[,;|\\r\\n]+", perl = TRUE),
    use.names = FALSE
  )
  medications <- trimws(medications)
  medications <- medications[nzchar(medications)]
  unique(medications)
}

# Return one row per medication. Day 2 is intentionally excluded by study design.
extract_one_file <- function(input_file, df = NULL) {
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }

  if (is.null(df)) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
      stop("The data.table package is required to read these PsychoPy CSV files.")
    }

    df <- data.table::fread(
      input_file,
      data.table = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA")
    )
  }

  required_columns <- c("participant", "day")
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Missing required columns in %s: %s",
        basename(input_file),
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  participant <- normalize_participant(df[["participant"]], input_file)
  day <- normalize_day(df[["day"]], input_file)
  if (day != "1") return(NULL)

  response <- first_medication_response(df)
  medications <- split_medications(response)

  if (length(medications) == 0) {
    return(NULL)
  }

  data.frame(
    ID = rep(sprintf("P%sD%s", participant, day), length(medications)),
    medication_name = medications,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Direct execution binds nonempty day-1 responses, enriches, and writes one CSV.
if (sys.nframe() == 0) {
  extracted_list <- lapply(input_files, extract_one_file)
  extracted_list <- Filter(Negate(is.null), extracted_list)

  if (length(extracted_list) == 0) {
    extracted <- data.frame(
      ID = character(0),
      medication_name = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    extracted <- do.call(rbind, extracted_list)
    rownames(extracted) <- NULL
  }
  metadata_helper <- new.env(parent = globalenv())
  sys.source(
    file.path(module_script_dir, "extraction_metadata_helpers.R"),
    envir = metadata_helper
  )
  extracted <- metadata_helper$metadata_enrich_from_files(
    extracted,
    input_files
  )

  write.csv(
    extracted,
    output_file,
    row.names = FALSE,
    na = ""
  )
  message(sprintf("Wrote %d medication rows to %s", nrow(extracted), output_file))
}
