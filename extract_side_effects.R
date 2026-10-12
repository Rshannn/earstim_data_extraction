################################################################################
# Title = Extract parameter-phase and N-back side-effect ratings
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table
# Creation Date = 14 June 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Extracts ten side-effect ratings after the parameter and N-back
# phases and separates the two phases into analysis-ready files.
# Inputs = One or more primary PsychoPy wide session CSV files
# Outputs = Sideeffects_Parameter_extracted.csv and
# Sideeffects_N_back_extracted.csv through the master pipeline
# Tags = PsychoPy, taVNS, side effects, parameter phase, N-back, R
################################################################################
# Public release: private paths, study attribution, and participant-specific
# records have been removed. Execution has not been validated for this release.

# Standalone mode accepts two output paths followed by one or more source files.
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 3) {
    stop(
      paste(
        "Provide: parameter_output.csv nback_output.csv",
        "input1.csv [input2.csv ...].",
        "Use run_all_extractions.R for the full participant extraction."
      )
    )
  }
  parameter_output_file <- args[[1]]
  nback_output_file <- args[[2]]
  input_files <- args[-c(1, 2)]
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

# Map source names to the concise analysis-facing output names.
side_effect_columns <- c(
  pain = "O_side_effects_pain.response",
  tingling = "O_side_effects_Tingling.response",
  itching = "O_side_effects_Itching.response",
  burning = "O_side_effects_burning.response",
  headache = "O_side_effects_headache.response",
  neck_pain = "O_side_effects_neckpain.response",
  numbness = "O_side_effects_numbness.response",
  nausea = "O_side_effects_nausea.response",
  drowsiness = "O_side_effects_drowsiness.response",
  uncomfortable_feeling = "O_side_effects_uncomfortable_feeling.response"
)

required_columns <- c(
  "participant",
  "day",
  unname(side_effect_columns)
)

read_sideeffect_source <- function(input_file, df = NULL) {
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

  df
}

# Helpers preserve the source response order: first parameter, then N-back.
first_nonmissing <- function(x) {
  x <- x[!is.na(x) & as.character(x) != ""]
  if (length(x) == 0) return(NA)
  x[[1]]
}

nonmissing_values <- function(df, column_name) {
  x <- df[[column_name]]
  x[!is.na(x) & as.character(x) != ""]
}

# Restart suffixes are normalized for direct runs; the master has already applied
# any explicit participant/day exception before calling this module.
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

take_phase_responses <- function(df, column_name, input_file) {
  values <- nonmissing_values(df, column_name)
  if (length(values) < 2) {
    stop(
      sprintf(
        "%s has only %d non-missing values in %s; need 2 values for parameter and N-back phases.",
        basename(input_file),
        length(values),
        column_name
      )
    )
  }
  if (length(values) > 2) {
    warning(
      sprintf(
        "%s has %d non-missing values in %s; using the first 2 values for parameter and N-back phases.",
        basename(input_file),
        length(values),
        column_name
      ),
      call. = FALSE
    )
  }
  values[1:2]
}

# Produce two rows per session; the caller separates them into phase files.
extract_one_file <- function(input_file, df = NULL) {
  df <- read_sideeffect_source(input_file, df)

  phase_types <- c("parameter", "n_back")
  result <- data.frame(
    participant = normalize_participant(df[["participant"]], input_file),
    day = normalize_day(df[["day"]], input_file),
    phase_type = phase_types,
    stringsAsFactors = FALSE
  )

  for (output_name in names(side_effect_columns)) {
    result[[output_name]] <-
      take_phase_responses(df, side_effect_columns[[output_name]], input_file)
  }

  result
}

# Explicit partial-session rules may recover one known phase without treating the
# unavailable phase as complete. Normal extraction still requires both responses.
extract_one_phase <- function(input_file, phase_type, df = NULL) {
  valid_phase_types <- c("parameter", "n_back")
  if (!phase_type %in% valid_phase_types) {
    stop(sprintf("Unsupported side-effect phase: %s", phase_type))
  }

  df <- read_sideeffect_source(input_file, df)
  response_index <- match(phase_type, valid_phase_types)
  result <- data.frame(
    participant = normalize_participant(df[["participant"]], input_file),
    day = normalize_day(df[["day"]], input_file),
    phase_type = phase_type,
    stringsAsFactors = FALSE
  )

  for (output_name in names(side_effect_columns)) {
    column_name <- side_effect_columns[[output_name]]
    values <- nonmissing_values(df, column_name)
    if (length(values) < response_index) {
      stop(
        sprintf(
          "%s has only %d non-missing values in %s; cannot extract the %s phase.",
          basename(input_file),
          length(values),
          column_name,
          phase_type
        )
      )
    }
    result[[output_name]] <- values[[response_index]]
  }

  result
}

# Direct execution enriches once, splits by phase, and writes two CSV outputs.
if (sys.nframe() == 0) {
  extracted <- do.call(rbind, lapply(input_files, extract_one_file))
  metadata_helper <- new.env(parent = globalenv())
  sys.source(
    file.path(module_script_dir, "extraction_metadata_helpers.R"),
    envir = metadata_helper
  )
  extracted <- metadata_helper$metadata_enrich_from_files(
    extracted,
    input_files
  )

  output_columns <- setdiff(names(extracted), "phase_type")
  parameter_extracted <- extracted[
    extracted$phase_type == "parameter",
    output_columns,
    drop = FALSE
  ]
  nback_extracted <- extracted[
    extracted$phase_type == "n_back",
    output_columns,
    drop = FALSE
  ]

  write.csv(
    parameter_extracted,
    parameter_output_file,
    row.names = FALSE,
    na = ""
  )
  write.csv(
    nback_extracted,
    nback_output_file,
    row.names = FALSE,
    na = ""
  )

  message(
    sprintf(
      "Wrote %d parameter-phase rows to %s",
      nrow(parameter_extracted),
      parameter_output_file
    )
  )
  message(
    sprintf(
      "Wrote %d N-back rows to %s",
      nrow(nback_extracted),
      nback_output_file
    )
  )
}
