################################################################################
# Title = Extract parameter-phase block ratings from PsychoPy session data
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table through the shared metadata helper
# Creation Date = 13 June 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Extracts six parameter-phase block rows containing arousal,
# alertness, tolerability, intensity, and rounded inter-block break duration.
# Inputs = One or more primary PsychoPy wide session CSV files
# Outputs = parameter_phase_extracted.csv when run through the master pipeline
# Tags = PsychoPy, taVNS, parameter phase, ratings, block data, R
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

# Required source columns define the minimum contract for a complete phase.
required_columns <- c(
  "participant",
  "day",
  "O_SelfAssessmentManikinArousal.response",
  "O_Drowsy_Alert.response",
  "O_Tollerabillity_StimulationParametersPhase.response",
  "O_IntensityRating_StimulationParametersPhase.response",
  "O_Duration_Break_Parameter_phase_Seconds_without10sfixed"
)

# Helpers preserve PsychoPy response order while rejecting incomplete positions.
first_nonmissing <- function(x) {
  x <- x[!is.na(x) & as.character(x) != ""]
  if (length(x) == 0) return(NA)
  x[[1]]
}

nonmissing_values <- function(df, column_name) {
  x <- df[[column_name]]
  x[!is.na(x) & as.character(x) != ""]
}

take_positions <- function(x, positions, column_name, input_file,
                           expected_max = NULL) {
  if (length(x) < max(positions)) {
    stop(
      sprintf(
        "%s has only %d non-missing values in %s; need positions %s.",
        basename(input_file),
        length(x),
        column_name,
        paste(range(positions), collapse = " to ")
      )
    )
  }
  if (!is.null(expected_max) && length(x) > expected_max) {
    warning(
      sprintf(
        "%s has %d non-missing values in %s; expected at most %d. Positional extraction may include restart overlap.",
        basename(input_file),
        length(x),
        column_name,
        expected_max
      ),
      call. = FALSE
    )
  }
  x[positions]
}

take_first_n_pad <- function(x, n, column_name, input_file) {
  if (length(x) < n) {
    warning(
      sprintf(
        "%s has only %d non-missing values in %s; padding to %d rows with NA.",
        basename(input_file),
        length(x),
        column_name,
        n
      ),
      call. = FALSE
    )
    return(c(x, rep(NA, n - length(x))))
  }
  if (length(x) > n) {
    warning(
      sprintf(
        "%s has %d non-missing values in %s; using the first %d values.",
        basename(input_file),
        length(x),
        column_name,
        n
      ),
      call. = FALSE
    )
  }
  x[seq_len(n)]
}

# Restart suffixes (a participant restart letter or a day suffix) belong to the same
# canonical participant-day when a module is run outside the master.
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

# Extract one six-row block-level table. The master may supply an already-read
# data frame so each large source CSV is loaded only once.
extract_one_file <- function(input_file, df = NULL) {
  if (!file.exists(input_file)) {
    stop(sprintf("Input file not found: %s", input_file))
  }

  if (is.null(df)) {
    df <- read.csv(
      input_file,
      check.names = FALSE,
      stringsAsFactors = FALSE,
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

  n_blocks <- 6

  sam_all <- nonmissing_values(df, "O_SelfAssessmentManikinArousal.response")
  drowsy_all <- nonmissing_values(df, "O_Drowsy_Alert.response")
  tolerability_all <- nonmissing_values(df, "O_Tollerabillity_StimulationParametersPhase.response")
  intensity_all <- nonmissing_values(df, "O_IntensityRating_StimulationParametersPhase.response")
  break_duration_all <- nonmissing_values(df, "O_Duration_Break_Parameter_phase_Seconds_without10sfixed")

  data.frame(
    participant = normalize_participant(df[["participant"]], input_file),
    day = normalize_day(df[["day"]], input_file),
    block_number = seq_len(n_blocks),
    phase_type = "parametric",
    arousal = take_positions(
      sam_all,
      3:8,
      "O_SelfAssessmentManikinArousal.response",
      input_file,
      expected_max = 15
    ),
    alertness = take_positions(
      drowsy_all,
      3:8,
      "O_Drowsy_Alert.response",
      input_file,
      expected_max = 15
    ),
    tolerability = take_first_n_pad(
      tolerability_all,
      n_blocks,
      "O_Tollerabillity_StimulationParametersPhase.response",
      input_file
    ),
    intensity = take_first_n_pad(
      intensity_all,
      n_blocks,
      "O_IntensityRating_StimulationParametersPhase.response",
      input_file
    ),
    break_duration_without10sfixed_ins = round(
      take_first_n_pad(
        break_duration_all,
        n_blocks,
        "O_Duration_Break_Parameter_phase_Seconds_without10sfixed",
        input_file
      ),
      digits = 0
    ),
    stringsAsFactors = FALSE
  )
}

# Direct execution enriches and writes only the files provided on the command line.
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

  write.csv(
    extracted,
    output_file,
    row.names = FALSE,
    na = ""
  )
  message(sprintf("Wrote %d rows to %s", nrow(extracted), output_file))
}
