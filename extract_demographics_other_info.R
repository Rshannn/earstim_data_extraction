################################################################################
# Title = Extract demographics and session-level study information
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table
# Creation Date = 14 June 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Extracts session timing, baseline and pre-phase ratings, Day 1
# demographics, and the interval between the two participant sessions.
# Inputs = One or more primary PsychoPy wide session CSV files and optional Day 1
# demographics companion columns
# Outputs = demographics_other_info_extracted.csv through the master pipeline
# Tags = PsychoPy, demographics, session timing, ratings, taVNS, R
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

# Session-level fields are required on both days; demographics are optional and
# intentionally populated only from day 1 below.
required_columns <- c(
  "participant",
  "day",
  "date",
  "O_StartExperiment_Timing",
  "O_Duration_Break_BeforeNbackTask_Seconds",
  "O_SelfAssessmentManikinArousal.response",
  "O_Drowsy_Alert.response"
)

# Alternate column names cover the companion demographics file and a historical
# misspelling present in early PsychoPy exports.
day1_demographic_columns <- list(
  sex = c("O_Sex.response", "Demographics_day1_only.O_Sex.response"),
  weight = c("O_Weight.text", "Demographics_day1_only.O_Weight.text"),
  years_education = c(
    "O_YearsEducation.text",
    "Demographics_day1_only.O_YearsEducation.text"
  ),
  height = c("O_Height.text", "Demographics_day1_only.O_Height.text"),
  pre_or_postmenopause = c(
    "O_Pre_or_Postmenopause.response",
    "O_Pre_or_Postmenopasue.response",
    "Demographics_day1_only.O_Pre_or_Postmenopause.response",
    "Demographics_day1_only.O_Pre_or_Postmenopasue.response"
  ),
  premenopause_last_period = c(
    "O_PreMenopause_Last_Period.text",
    "Demographics_day1_only.O_PreMenopause_Last_Period.text"
  ),
  post_menopause_last_period = c(
    "O_Post_Menopause_Last_Period.text",
    "Demographics_day1_only.O_Post_Menopause_Last_Period.text"
  ),
  dominant_hand = c(
    "O_dominanthand.response",
    "Demographics_day1_only.O_dominanthand.response"
  )
)

# Scalar parsing helpers convert blank PsychoPy cells to missing values and fail
# clearly when fields required for dates or time classification are malformed.
first_nonmissing <- function(x) {
  x <- x[!is.na(x) & as.character(x) != ""]
  if (length(x) == 0) return(NA)
  x[[1]]
}

parse_session_date <- function(x, input_file) {
  date_text <- as.character(first_nonmissing(x))
  session_date <- as.Date(substr(date_text, 1, 10), format = "%Y-%m-%d")

  if (is.na(session_date)) {
    stop(
      sprintf(
        "%s has an invalid or missing date value: %s.",
        basename(input_file),
        date_text
      )
    )
  }

  session_date
}

classify_time_of_day <- function(start_time, input_file) {
  start_time <- trimws(as.character(start_time))
  time_match <- regmatches(
    start_time,
    regexec("^([0-9]{1,2})(?::[0-9]{2}){0,2}", start_time)
  )[[1]]
  hour <- if (length(time_match) >= 2) {
    suppressWarnings(as.integer(time_match[[2]]))
  } else {
    NA_integer_
  }

  if (is.na(hour) || hour < 0 || hour > 23) {
    stop(
      sprintf(
        "%s has an invalid or missing O_StartExperiment_Timing value: %s.",
        basename(input_file),
        start_time
      )
    )
  }

  if (hour < 12) "morning" else "afternoon"
}

# Restart suffixes are normalized in direct runs. The master supplies canonical
# values after applying participant-specific filename exceptions.
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

nonmissing_values <- function(df, column_name) {
  x <- df[[column_name]]
  x[!is.na(x) & as.character(x) != ""]
}

take_position <- function(x, position, column_name, input_file) {
  if (length(x) < position) {
    stop(
      sprintf(
        "%s has only %d non-missing values in %s; need position %d.",
        basename(input_file),
        length(x),
        column_name,
        position
      )
    )
  }
  x[[position]]
}

warn_if_excess_responses <- function(x, expected_max, column_name, input_file) {
  if (length(x) > expected_max) {
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
}

# Search aliases in priority order, returning blank only for optional demographics.
first_from_candidate_columns <- function(df, candidate_columns, input_file,
                                         required = FALSE) {
  matching_columns <- candidate_columns[candidate_columns %in% names(df)]

  for (column_name in matching_columns) {
    value <- first_nonmissing(df[[column_name]])
    if (!is.na(value)) return(value)
  }

  if (required) {
    stop(
      sprintf(
        "%s has no non-missing value in candidate columns: %s.",
        basename(input_file),
        paste(candidate_columns, collapse = ", ")
      )
    )
  }

  NA
}

extract_day1_demographics <- function(df, input_file, is_day1) {
  if (!is_day1) {
    empty_values <- as.list(rep(NA_character_, length(day1_demographic_columns)))
    names(empty_values) <- names(day1_demographic_columns)
    return(as.data.frame(empty_values, stringsAsFactors = FALSE))
  }

  values <- lapply(
    day1_demographic_columns,
    function(candidate_columns) {
      first_from_candidate_columns(
        df,
        candidate_columns,
        input_file,
        required = FALSE
      )
    }
  )

  as.data.frame(values, stringsAsFactors = FALSE)
}

# Extract one session-level row. Ratings are positional because PsychoPy stores
# repeated routine responses in a wide, sparsely populated column.
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

  participant <- normalize_participant(df[["participant"]], input_file)
  day <- normalize_day(df[["day"]], input_file)
  is_day1 <- day == "1"

  sam_all <- nonmissing_values(df, "O_SelfAssessmentManikinArousal.response")
  alert_all <- nonmissing_values(df, "O_Drowsy_Alert.response")
  warn_if_excess_responses(
    sam_all,
    15,
    "O_SelfAssessmentManikinArousal.response",
    input_file
  )
  warn_if_excess_responses(
    alert_all,
    15,
    "O_Drowsy_Alert.response",
    input_file
  )
  start_experiment_timing <- first_nonmissing(df[["O_StartExperiment_Timing"]])

  base_row <- data.frame(
    participant = participant,
    day = day,
    .session_date = parse_session_date(df[["date"]], input_file),
    start_experiment_timing = start_experiment_timing,
    time_of_day = classify_time_of_day(start_experiment_timing, input_file),
    duration_break_before_nback_task_seconds = round(
      first_nonmissing(df[["O_Duration_Break_BeforeNbackTask_Seconds"]]),
      digits = 0
    ),
    baseline_arousal = take_position(
      sam_all,
      1,
      "O_SelfAssessmentManikinArousal.response",
      input_file
    ),
    pre_parameter_phase_arousal = take_position(
      sam_all,
      2,
      "O_SelfAssessmentManikinArousal.response",
      input_file
    ),
    pre_nback_phase_arousal = take_position(
      sam_all,
      9,
      "O_SelfAssessmentManikinArousal.response",
      input_file
    ),
    baseline_alert = take_position(
      alert_all,
      1,
      "O_Drowsy_Alert.response",
      input_file
    ),
    pre_parameter_phase_alert = take_position(
      alert_all,
      2,
      "O_Drowsy_Alert.response",
      input_file
    ),
    pre_nback_phase_alert = take_position(
      alert_all,
      9,
      "O_Drowsy_Alert.response",
      input_file
    ),
    stringsAsFactors = FALSE
  )

  cbind(
    base_row,
    extract_day1_demographics(df, input_file, is_day1),
    stringsAsFactors = FALSE
  )
}

# Direct execution calculates the paired-day interval, attaches standardized
# metadata, and writes only the explicitly supplied sessions.
if (sys.nframe() == 0) {
  extracted <- do.call(rbind, lapply(input_files, extract_one_file))

  extracted$days_between_day1_day2 <- NA_integer_

  for (participant_id in unique(extracted$participant)) {
    participant_rows <- extracted$participant == participant_id
    day1_dates <- extracted$.session_date[
      participant_rows & as.character(extracted$day) == "1"
    ]
    day2_dates <- extracted$.session_date[
      participant_rows & as.character(extracted$day) == "2"
    ]

    if (length(day1_dates) > 0 && length(day2_dates) > 0) {
      extracted$days_between_day1_day2[participant_rows] <- as.integer(
        day2_dates[[1]] - day1_dates[[1]]
      )
    }
  }

  extracted$.session_date <- NULL
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
