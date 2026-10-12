################################################################################
# Title = Extract N-back phase ratings and block-level stimulation status
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table through the shared metadata helper
# Creation Date = 14 June 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Extracts six N-back block rows containing ratings, stimulation
# status, stimulation sequence, and rounded inter-block break duration.
# Inputs = One or more primary PsychoPy wide session CSV files
# Outputs = nback_phase_extracted.csv when run through the master pipeline
# Tags = PsychoPy, taVNS, N-back, stimulation, ratings, block data, R
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
  "O_TollerabilityNBack.response",
  "O_IntensityRatingNBack.response",
  "O_StimulationBeforeN_back",
  "O_Duration_Break_N_back_phase_Seconds_without10sfixed",
  "N_back_Block_order.thisN"
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

# Restart suffixes are normalized for direct module runs; the master already
# supplies these canonical values after applying documented exceptions.
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

# Derive the six stimulation labels from their associated PsychoPy block index.
extract_stimulation_before_by_block <- function(df, n_blocks, input_file) {
  block_column <- "N_back_Block_order.thisN"
  response_column <- "O_StimulationBeforeN_back"
  responses_by_block <- rep(NA_character_, n_blocks)

  for (block_index in seq_len(n_blocks)) {
    block_value <- as.character(block_index - 1)
    in_block <- as.character(df[[block_column]]) == block_value
    values <- df[[response_column]][in_block]
    values <- values[!is.na(values) & as.character(values) != ""]

    if (length(values) == 0) next

    unique_values <- unique(as.character(values))
    if (length(unique_values) > 1) {
      warning(
        sprintf(
          "%s has multiple %s values in N-back block %d: %s. Taking the first value.",
          basename(input_file),
          response_column,
          block_index,
          paste(unique_values, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    responses_by_block[[block_index]] <- unique_values[[1]]
  }

  missing_blocks <- which(is.na(responses_by_block))
  if (length(missing_blocks) > 0) {
    stop(
      sprintf(
        "%s is missing %s values for N-back block(s): %s.",
        basename(input_file),
        response_column,
        paste(missing_blocks, collapse = ", ")
      )
    )
  }

  responses_by_block
}

# Tolerability/intensity were asked only after stimulation-on blocks. Place those
# sequential ratings back onto the corresponding six-row block table.
place_stimulation_ratings <- function(rating_values, stimulation_before_blocks,
                                      column_name, input_file) {
  n_blocks <- length(stimulation_before_blocks)
  output <- rep(NA, n_blocks)
  stimulation_blocks <- which(stimulation_before_blocks == "StimulationBeforeN_back")

  if (length(rating_values) != length(stimulation_blocks)) {
    warning(
      sprintf(
        "%s has %d non-missing values in %s but %d stimulation N-back blocks. Filling by order and padding/truncating as needed.",
        basename(input_file),
        length(rating_values),
        column_name,
        length(stimulation_blocks)
      ),
      call. = FALSE
    )
  }

  n_to_place <- min(length(rating_values), length(stimulation_blocks))
  if (n_to_place > 0) {
    output[stimulation_blocks[seq_len(n_to_place)]] <- rating_values[seq_len(n_to_place)]
  }

  output
}

# Extract one six-row block-level table from a complete participant-day session.
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
  tolerability_all <- nonmissing_values(df, "O_TollerabilityNBack.response")
  intensity_all <- nonmissing_values(df, "O_IntensityRatingNBack.response")
  break_duration_all <- nonmissing_values(
    df,
    "O_Duration_Break_N_back_phase_Seconds_without10sfixed"
  )

  stimulation_before_blocks <- extract_stimulation_before_by_block(
    df,
    n_blocks,
    input_file
  )

  stimulation_on <- stimulation_before_blocks == "StimulationBeforeN_back"
  block_stimulation_on <- rep(NA_integer_, n_blocks)
  block_stimulation_on[stimulation_on] <- seq_len(sum(stimulation_on))

  data.frame(
    participant = normalize_participant(df[["participant"]], input_file),
    day = normalize_day(df[["day"]], input_file),
    block_number_n_back = seq_len(n_blocks),
    block_stimulation_on = block_stimulation_on,
    phase_type = "n_back",
    arousal = take_positions(
      sam_all,
      10:15,
      "O_SelfAssessmentManikinArousal.response",
      input_file,
      expected_max = 15
    ),
    alertness = take_positions(
      drowsy_all,
      10:15,
      "O_Drowsy_Alert.response",
      input_file,
      expected_max = 15
    ),
    stimulation_before_nback_response = stimulation_before_blocks,
    tolerability = place_stimulation_ratings(
      tolerability_all,
      stimulation_before_blocks,
      "O_TollerabilityNBack.response",
      input_file
    ),
    intensity = place_stimulation_ratings(
      intensity_all,
      stimulation_before_blocks,
      "O_IntensityRatingNBack.response",
      input_file
    ),
    break_duration_without10sfixed_ins = round(
      take_first_n_pad(
        break_duration_all,
        n_blocks,
        "O_Duration_Break_N_back_phase_Seconds_without10sfixed",
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
