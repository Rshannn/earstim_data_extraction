################################################################################
# Title = Build standardized participant and session metadata
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table for file-based helper calls
# Creation Date = 18 July 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Builds and attaches age, age group, sex, condition, time of day,
# days between sessions, and session start metadata shared by every output.
# Inputs = Selected PsychoPy session data and the taVNS/sham order CSV
# Outputs = In-memory metadata tables used by the extraction modules and master
# Tags = PsychoPy, metadata, demographics, counterbalancing, sessions, R
################################################################################
# Public release: private paths, study attribution, and participant-specific
# records have been removed. Execution has not been validated for this release.

# Resolve project resources relative to the running R script. This keeps direct
# module runs portable while still working when this file is sourced by the master.
metadata_file_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)
metadata_script_dir <- if (length(metadata_file_argument) > 0) {
  dirname(normalizePath(sub("^--file=", "", metadata_file_argument[[1]])))
} else {
  normalizePath(getwd())
}
metadata_default_order_file <- file.path(
  metadata_script_dir,
  "taVNS_sham_&_stim_no_stim_order.csv"
)

metadata_output_columns <- c(
  "age",
  "age_group",
  "sex",
  "condition",
  "time_of_day",
  "days_between_sessions",
  "session_start"
)

# Scalar helpers consistently treat blank strings as missing PsychoPy values.
metadata_first_nonmissing <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  x[[1]]
}

metadata_first_from_columns <- function(df, candidate_columns) {
  for (column_name in candidate_columns[candidate_columns %in% names(df)]) {
    value <- metadata_first_nonmissing(df[[column_name]])
    if (!is.na(value)) return(value)
  }
  NA_character_
}

metadata_parse_file_identity <- function(input_file, df) {
  file_name <- basename(input_file)
  parts <- regmatches(
    file_name,
    regexec("^P_([0-9]+)[[:alpha:]]*_D__([12])", file_name)
  )[[1]]

  if (length(parts) >= 3) {
    return(c(participant = parts[[2]], day = parts[[3]]))
  }

  participant <- metadata_first_from_columns(df, "participant")
  participant <- sub("^([0-9]+).*$", "\\1", participant)
  day <- metadata_first_from_columns(df, "day")
  day <- sub("^([12]).*$", "\\1", day)
  c(participant = participant, day = day)
}

# Filename timestamps provide a stable ordering when several restart files exist.
metadata_parse_file_timestamp <- function(input_file, source_index) {
  file_name <- basename(input_file)
  timestamp_text <- sub(
    ".*_date__([0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}h[0-9]{2}[.][0-9]{2}[.][0-9]+)[.]csv$",
    "\\1",
    file_name
  )

  timestamp <- as.POSIXct(
    timestamp_text,
    format = "%Y-%m-%d_%Hh%M.%OS",
    tz = "America/Los_Angeles"
  )
  if (is.na(timestamp)) {
    as.POSIXct(source_index, origin = "1970-01-01", tz = "UTC")
  } else {
    timestamp
  }
}

metadata_parse_session_date <- function(df) {
  date_text <- metadata_first_from_columns(df, "date")
  if (is.na(date_text)) return(as.Date(NA))
  as.Date(substr(date_text, 1, 10), format = "%Y-%m-%d")
}

metadata_classify_time <- function(start_time) {
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

  if (is.na(hour) || hour < 0 || hour > 23) return(NA_character_)
  if (hour < 12) "morning" else "afternoon"
}

metadata_extract_record <- function(df, input_file, source_index,
                                    participant_override = NA_character_,
                                    day_override = NA_character_) {
  has_identity_override <- !is.na(participant_override) &&
    nzchar(participant_override) &&
    !is.na(day_override) &&
    nzchar(day_override)
  identity <- if (has_identity_override) {
    c(participant = participant_override, day = day_override)
  } else {
    metadata_parse_file_identity(input_file, df)
  }
  if (any(is.na(identity)) || any(!nzchar(identity))) return(NULL)

  data.frame(
    participant = unname(identity[["participant"]]),
    day = unname(identity[["day"]]),
    .file_timestamp = metadata_parse_file_timestamp(input_file, source_index),
    .source_index = source_index,
    .session_date = metadata_parse_session_date(df),
    session_start = metadata_first_from_columns(df, "O_StartExperiment_Timing"),
    age_text = metadata_first_from_columns(
      df,
      c("O_Age.text", "Demographics_day1_only.O_Age.text")
    ),
    sex = metadata_first_from_columns(
      df,
      c("O_Sex.response", "Demographics_day1_only.O_Sex.response")
    ),
    stringsAsFactors = FALSE
  )
}

# Validate the external counterbalancing table before deriving session condition.
metadata_read_order <- function(order_file) {
  if (!file.exists(order_file)) {
    stop(sprintf("Condition-order file not found: %s", order_file))
  }

  order_data <- read.csv(
    order_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  required <- c("ID", "taVNS_sham_order")
  missing_columns <- setdiff(required, names(order_data))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Condition-order file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  order_data$ID <- as.character(order_data$ID)
  if (anyDuplicated(order_data$ID)) {
    stop("Condition-order file contains duplicate participant IDs.")
  }

  valid_orders <- c("t-s", "s-t")
  invalid_orders <- setdiff(unique(order_data$taVNS_sham_order), valid_orders)
  if (length(invalid_orders) > 0) {
    stop(
      sprintf(
        "Condition-order file contains invalid taVNS_sham_order values: %s",
        paste(invalid_orders, collapse = ", ")
      )
    )
  }

  order_data
}

# Collapse one or more source fragments into one metadata row per participant-day.
# The caller is responsible for passing only fragments from the selected visit.
metadata_build <- function(data_frames, input_files,
                           order_file = metadata_default_order_file,
                           participant_overrides = NULL,
                           day_overrides = NULL) {
  if (length(data_frames) != length(input_files)) {
    stop("data_frames and input_files must have the same length.")
  }

  if (is.null(participant_overrides)) {
    participant_overrides <- rep(NA_character_, length(input_files))
  }
  if (is.null(day_overrides)) {
    day_overrides <- rep(NA_character_, length(input_files))
  }
  if (
    length(participant_overrides) != length(input_files) ||
      length(day_overrides) != length(input_files)
  ) {
    stop("Metadata identity overrides must align with input_files.")
  }

  records <- lapply(seq_along(data_frames), function(index) {
    df <- data_frames[[index]]
    if (is.null(df)) return(NULL)
    metadata_extract_record(
      df,
      input_files[[index]],
      index,
      as.character(participant_overrides[[index]]),
      as.character(day_overrides[[index]])
    )
  })
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0) {
    stop("No readable participant/session records were available for metadata.")
  }

  records <- do.call(rbind, records)
  records <- records[
    order(records$participant, records$day, records$.file_timestamp),
    ,
    drop = FALSE
  ]
  record_key <- paste(records$participant, records$day, sep = "|")

  session_rows <- lapply(unique(record_key), function(key) {
    rows <- records[record_key == key, , drop = FALSE]
    valid_dates <- rows$.session_date[!is.na(rows$.session_date)]
    age_text <- metadata_first_nonmissing(rows$age_text)
    age <- suppressWarnings(as.numeric(age_text))

    data.frame(
      participant = rows$participant[[1]],
      day = rows$day[[1]],
      .session_date = if (length(valid_dates) == 0) {
        as.Date(NA)
      } else {
        min(valid_dates)
      },
      age = age,
      sex = metadata_first_nonmissing(rows$sex),
      session_start = metadata_first_nonmissing(rows$session_start),
      stringsAsFactors = FALSE
    )
  })
  metadata <- do.call(rbind, session_rows)
  rownames(metadata) <- NULL

  # Age and sex are participant attributes. Propagate a recorded value to both
  # days, but warn if source fragments disagree instead of hiding the conflict.
  for (participant_id in unique(metadata$participant)) {
    participant_rows <- metadata$participant == participant_id
    participant_age <- metadata$age[participant_rows & !is.na(metadata$age)]
    participant_sex <- metadata$sex[participant_rows & !is.na(metadata$sex)]

    if (length(unique(participant_age)) > 1) {
      warning(
        sprintf("Participant %s has conflicting recorded ages.", participant_id),
        call. = FALSE
      )
    }
    if (length(unique(participant_sex)) > 1) {
      warning(
        sprintf("Participant %s has conflicting recorded sex values.", participant_id),
        call. = FALSE
      )
    }

    if (length(participant_age) > 0) {
      metadata$age[participant_rows] <- participant_age[[1]]
    }
    if (length(participant_sex) > 0) {
      metadata$sex[participant_rows] <- participant_sex[[1]]
    }
  }

  # The same signed day interval is attached to both sessions for easy joins.
  metadata$days_between_sessions <- NA_integer_
  for (participant_id in unique(metadata$participant)) {
    participant_rows <- metadata$participant == participant_id
    day1_dates <- metadata$.session_date[
      participant_rows & metadata$day == "1" & !is.na(metadata$.session_date)
    ]
    day2_dates <- metadata$.session_date[
      participant_rows & metadata$day == "2" & !is.na(metadata$.session_date)
    ]

    if (length(day1_dates) > 0 && length(day2_dates) > 0) {
      metadata$days_between_sessions[participant_rows] <- as.integer(
        day2_dates[[1]] - day1_dates[[1]]
      )
    }
  }

  order_data <- metadata_read_order(order_file)
  order_index <- match(metadata$participant, order_data$ID)
  session_order <- order_data$taVNS_sham_order[order_index]
  # Age group is derived only from the recorded numeric age, per the study rule.
  metadata$age_group <- ifelse(
    !is.na(metadata$age) & metadata$age < 36,
    "young",
    ifelse(!is.na(metadata$age) & metadata$age > 59, "old", NA_character_)
  )

  # Counterbalancing order maps each study day to the administered condition.
  metadata$condition <- ifelse(
    metadata$day == "1" & session_order == "t-s",
    "taVNS",
    ifelse(
      metadata$day == "2" & session_order == "t-s",
      "sham",
      ifelse(
        metadata$day == "1" & session_order == "s-t",
        "sham",
        ifelse(
          metadata$day == "2" & session_order == "s-t",
          "taVNS",
          NA_character_
        )
      )
    )
  )
  metadata$time_of_day <- vapply(
    metadata$session_start,
    metadata_classify_time,
    character(1)
  )
  metadata$.session_date <- NULL

  metadata[
    ,
    c("participant", "day", metadata_output_columns),
    drop = FALSE
  ]
}

# Convenience entry point used when an individual module is run directly.
metadata_build_from_files <- function(
  input_files,
  order_file = metadata_default_order_file
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The data.table package is required to build extraction metadata.")
  }

  data_frames <- lapply(input_files, function(input_file) {
    data.table::fread(
      input_file,
      data.table = FALSE,
      check.names = FALSE,
      na.strings = c("", "NA"),
      showProgress = FALSE
    )
  })
  metadata_build(data_frames, input_files, order_file)
}

# Join standardized metadata while retaining each extraction's native columns.
metadata_enrich <- function(df, metadata) {
  if (is.null(df) || nrow(df) == 0) return(df)

  if (all(c("participant", "day") %in% names(df))) {
    participant <- as.character(df$participant)
    day <- as.character(df$day)
    leading_columns <- c("participant", "day")
  } else if ("ID" %in% names(df)) {
    participant <- sub("D[12]$", "", sub("^P", "", as.character(df$ID)))
    day <- sub("^.*D([12])$", "\\1", as.character(df$ID))
    leading_columns <- intersect(c("ID", "medication_name"), names(df))
  } else {
    stop("Extraction output must contain participant/day columns or an ID column.")
  }

  output_key <- paste(participant, day, sep = "|")
  metadata_key <- paste(metadata$participant, metadata$day, sep = "|")
  metadata_index <- match(output_key, metadata_key)
  if (anyNA(metadata_index)) {
    missing_keys <- unique(output_key[is.na(metadata_index)])
    stop(
      sprintf(
        "No session metadata was found for participant-day(s): %s",
        paste(missing_keys, collapse = ", ")
      )
    )
  }

  original_columns <- setdiff(names(df), metadata_output_columns)
  remaining_columns <- setdiff(original_columns, leading_columns)
  metadata_values <- metadata[metadata_index, metadata_output_columns, drop = FALSE]
  rownames(metadata_values) <- NULL

  result <- cbind(
    df[leading_columns],
    metadata_values,
    df[remaining_columns],
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL
  result
}

metadata_enrich_from_files <- function(
  df,
  input_files,
  order_file = metadata_default_order_file
) {
  metadata <- metadata_build_from_files(input_files, order_file)
  metadata_enrich(df, metadata)
}
