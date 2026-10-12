################################################################################
# Title = Extract trial-level N-back task outcomes
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table
# Creation Date = 23 June 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Extracts 30 N-back trials for each of six blocks and attaches
# block stimulation, arousal, alertness, and rounded break-duration information.
# Inputs = One or more primary PsychoPy wide session CSV files
# Outputs = nback_task_trials_extracted.csv through the master pipeline
# Tags = PsychoPy, taVNS, N-back, reaction time, accuracy, trial data, R
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

# A valid N-back source must contain trial indices, outcomes, and the block-level
# fields repeated onto each of the 30 trial rows.
required_columns <- c(
  "participant",
  "day",
  "N_back_Block_order.thisN",
  "Trials_Within_Block_N_Back_task.thisN",
  "O_StimulationBeforeN_back",
  "O_SelfAssessmentManikinArousal.response",
  "O_Drowsy_Alert.response",
  "O_Duration_Break_N_back_phase_Seconds_without10sfixed",
  "O_Accuracy_N_back_coded",
  "O_Performance_N_back.rt",
  "targettask"
)

# Helpers preserve sparse PsychoPy response order and make truncation observable.
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
        "%s has only %d non-missing values in %s; padding to %d blocks with NA.",
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

# Restart suffixes are normalized for direct runs; the master supplies canonical
# IDs after applying its explicit exception registry.
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

# Resolve one stimulation label for each block before expanding it to trial rows.
extract_stimulation_by_block <- function(df, input_file, n_blocks = 6) {
  block_values <- as.character(df[["N_back_Block_order.thisN"]])
  stimulation_values <- as.character(df[["O_StimulationBeforeN_back"]])
  stimulation_by_block <- rep(NA_character_, n_blocks)

  for (block_number in seq_len(n_blocks)) {
    block_index <- as.character(block_number - 1)
    in_block <- block_values == block_index
    values <- stimulation_values[in_block]
    values <- values[!is.na(values) & values != ""]

    if (length(values) == 0) {
      stop(
        sprintf(
          "%s is missing O_StimulationBeforeN_back for N-back block %d.",
          basename(input_file),
          block_number
        )
      )
    }

    unique_values <- unique(values)
    if (length(unique_values) > 1) {
      warning(
        sprintf(
          "%s has multiple O_StimulationBeforeN_back values in block %d: %s. Taking the first value.",
          basename(input_file),
          block_number,
          paste(unique_values, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    stimulation_by_block[[block_number]] <- unique_values[[1]]
  }

  stimulation_by_block
}

# Translate PsychoPy routine labels to the analysis-facing on/off values.
map_stimulation_type <- function(stimulation_response, input_file) {
  output <- rep(NA_character_, length(stimulation_response))
  output[stimulation_response == "StimulationBeforeN_back"] <- "on"
  output[stimulation_response == "NoStimulationBeforeN_back"] <- "off"

  unknown_values <- unique(stimulation_response[is.na(output)])
  unknown_values <- unknown_values[!is.na(unknown_values) & unknown_values != ""]
  if (length(unknown_values) > 0) {
    warning(
      sprintf(
        "%s has unrecognized stimulation values: %s.",
        basename(input_file),
        paste(unknown_values, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  output
}

# Extract and validate one complete 6 x 30 N-back trial table.
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
  trials_per_block <- 30
  expected_trials <- n_blocks * trials_per_block

  task_rows <- !is.na(df[["Trials_Within_Block_N_Back_task.thisN"]]) &
    as.character(df[["Trials_Within_Block_N_Back_task.thisN"]]) != ""

  task_df <- df[task_rows, , drop = FALSE]

  if (nrow(task_df) != expected_trials) {
    stop(
      sprintf(
        "%s has %d N-back task rows; expected %d rows (%d blocks x %d trials).",
        basename(input_file),
        nrow(task_df),
        expected_trials,
        n_blocks,
        trials_per_block
      )
    )
  }

  block_number <- suppressWarnings(
    as.integer(task_df[["N_back_Block_order.thisN"]])
  ) + 1L
  trial_number <- suppressWarnings(
    as.integer(task_df[["Trials_Within_Block_N_Back_task.thisN"]])
  ) + 1L

  if (anyNA(block_number) || anyNA(trial_number)) {
    stop(sprintf("%s has non-integer N-back block or trial indices.", basename(input_file)))
  }
  if (!identical(sort(unique(block_number)), seq_len(n_blocks))) {
    stop(
      sprintf(
        "%s does not contain exactly N-back blocks 1 through %d.",
        basename(input_file),
        n_blocks
      )
    )
  }

  trial_key <- paste(block_number, trial_number, sep = "|")
  if (anyDuplicated(trial_key)) {
    stop(sprintf("%s has duplicate N-back block/trial indices.", basename(input_file)))
  }

  invalid_trial_blocks <- vapply(
    seq_len(n_blocks),
    function(block) {
      !identical(
        sort(trial_number[block_number == block]),
        seq_len(trials_per_block)
      )
    },
    logical(1)
  )
  if (any(invalid_trial_blocks)) {
    stop(
      sprintf(
        "%s does not contain trials 1 through %d exactly once in block(s): %s.",
        basename(input_file),
        trials_per_block,
        paste(which(invalid_trial_blocks), collapse = ", ")
      )
    )
  }

  trial_counts <- table(block_number)
  bad_blocks <- names(trial_counts)[trial_counts != trials_per_block]
  if (length(bad_blocks) > 0) {
    stop(
      sprintf(
        "%s has unexpected N-back trial counts for block(s) %s.",
        basename(input_file),
        paste(bad_blocks, collapse = ", ")
      )
    )
  }

  stimulation_by_block <- extract_stimulation_by_block(df, input_file, n_blocks)
  stimulation_response <- stimulation_by_block[block_number]
  arousal_by_block <- take_positions(
    nonmissing_values(df, "O_SelfAssessmentManikinArousal.response"),
    10:15,
    "O_SelfAssessmentManikinArousal.response",
    input_file,
    expected_max = 15
  )
  alertness_by_block <- take_positions(
    nonmissing_values(df, "O_Drowsy_Alert.response"),
    10:15,
    "O_Drowsy_Alert.response",
    input_file,
    expected_max = 15
  )
  break_duration_by_block <- round(
    take_first_n_pad(
      nonmissing_values(
        df,
        "O_Duration_Break_N_back_phase_Seconds_without10sfixed"
      ),
      n_blocks,
      "O_Duration_Break_N_back_phase_Seconds_without10sfixed",
      input_file
    ),
    digits = 0
  )

  data.frame(
    participant = normalize_participant(df[["participant"]], input_file),
    day = normalize_day(df[["day"]], input_file),
    block_number = block_number,
    trial_number = trial_number,
    stimulation_type = map_stimulation_type(stimulation_response, input_file),
    arousal = arousal_by_block[block_number],
    alertness = alertness_by_block[block_number],
    break_duration_without10sfixed_ins = break_duration_by_block[block_number],
    accuracy_nback_coded = task_df[["O_Accuracy_N_back_coded"]],
    performance_nback_rt = task_df[["O_Performance_N_back.rt"]],
    targettask = task_df[["targettask"]],
    stringsAsFactors = FALSE
  )
}

# Direct execution enriches and writes only the explicitly supplied sessions.
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
