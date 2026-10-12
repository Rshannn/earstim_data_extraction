################################################################################
# Title = Extract Day 2 blinding and belief-condition responses
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table
# Creation Date = 8 July 2026
# Modification Date = 30 August 2026
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Extracts eligible Day 2 beliefs about helpful, active, and
# control stimulation sites while handling early-study aliases and exclusions.
# Inputs = One or more primary PsychoPy Day 2 wide session CSV files
# Outputs = blinding_extracted.csv through the master pipeline
# Tags = PsychoPy, taVNS, blinding, belief condition, Day 2, R
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

# Current item names and early-study aliases are searched in priority order.
blinding_columns <- list(
  had_belief_about_more_helpful_site = c(
    "O_Blinding1.response",
    "Belief_stimulation_loop_day2_only.O_Blinding1.response"
  ),
  believed_more_helpful_site = c(
    "O_Blinding2.response",
    "Belief_stimulation_loop_day2_only.O_Blinding2.response"
  ),
  believed_active_site = c(
    "O_BeliefCondition_active.response",
    paste0(
      "Belief_stimulation_loop_day2_only.",
      "O_BeliefCondition_active.response"
    )
  ),
  believed_control_site = c(
    "O_BeliefCondition_control.response",
    paste0(
      "Belief_stimulation_loop_day2_only.",
      "O_BeliefCondition_control.response"
    )
  )
)

legacy_belief_columns <- list(
  cymba_concha_assigned_role = c(
    "O_BeliefCondition_cymba_concha.response",
    paste0(
      "Belief_stimulation_loop_day2_only.",
      "O_BeliefCondition_cymba_concha.response"
    )
  ),
  earlobe_assigned_role = c(
    "O_BeliefCondition_earlobe_2.response",
    paste0(
      "Belief_stimulation_loop_day2_only.",
      "O_BeliefCondition_earlobe_2.response"
    )
  )
)

# Private questionnaire introduction date is omitted from the public release.
# Supply an appropriate cutoff before using this module with authorized data.
blinding_start_date <- as.Date(NA_character_)

# Private participant-specific legacy mappings are omitted.
legacy_belief_participants <- character(0)

# Analysis-specific exclusions remove only the listed blinding rows. The same
# participant-day remains eligible for every other extraction module.
excluded_participant_day_keys <- character(0)

# Scalar and alias helpers treat blank cells as missing responses.
first_nonmissing <- function(x) {
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(NA_character_)
  x[[1]]
}

first_from_columns <- function(df, column_names) {
  for (column_name in column_names[column_names %in% names(df)]) {
    value <- first_nonmissing(df[[column_name]])
    if (!is.na(value)) return(value)
  }
  NA_character_
}

# Normalize restart suffixes for direct runs; explicit filename exceptions remain
# the responsibility of the master script.
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

# Standardize site labels while preserving unexpected source text for auditability.
normalize_site <- function(value) {
  if (is.na(value)) return(NA_character_)

  normalized <- tolower(trimws(as.character(value)))
  if (normalized %in% c("concha cymba", "cymba concha")) {
    return("cymba concha")
  }
  if (normalized == "earlobe") return("earlobe")

  trimws(as.character(value))
}

# Participant 1 used one role response per physical site instead of one site
# response per role. Validate and invert that legacy representation.
normalize_legacy_role <- function(value, input_file, site_name) {
  if (is.na(value)) return(NA_character_)
  role <- tolower(trimws(as.character(value)))
  if (!role %in% c("active", "control")) {
    warning(
      sprintf(
        "%s has an unrecognized legacy belief role for %s: %s.",
        basename(input_file),
        site_name,
        value
      ),
      call. = FALSE
    )
    return(NA_character_)
  }
  role
}

# Prefer the recorded session date and fall back to the validated filename date.
parse_session_date <- function(df, input_file) {
  date_text <- first_from_columns(df, "date")
  session_date <- if (is.na(date_text)) {
    as.Date(NA)
  } else {
    as.Date(substr(date_text, 1, 10), format = "%Y-%m-%d")
  }

  if (!is.na(session_date)) return(session_date)

  file_name <- basename(input_file)
  date_match <- regmatches(
    file_name,
    regexec("_date__([0-9]{4}-[0-9]{2}-[0-9]{2})_", file_name)
  )[[1]]
  if (length(date_match) < 2) return(as.Date(NA))

  as.Date(date_match[[2]], format = "%Y-%m-%d")
}

# Produce one day-2 row. The first two belief items are populated only for visits
# after their documented introduction date; earlier sessions remain blank.
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
  if (day != "2") return(NULL)
  if (paste(participant, day, sep = "|") %in% excluded_participant_day_keys) {
    return(NULL)
  }

  values <- setNames(
    as.list(rep(NA_character_, length(blinding_columns))),
    names(blinding_columns)
  )

  if (participant %in% legacy_belief_participants) {
    cymba_concha_role <- normalize_legacy_role(
      first_from_columns(
        df,
        legacy_belief_columns$cymba_concha_assigned_role
      ),
      input_file,
      "cymba concha"
    )
    earlobe_role <- normalize_legacy_role(
      first_from_columns(
        df,
        legacy_belief_columns$earlobe_assigned_role
      ),
      input_file,
      "earlobe"
    )

    if (
      !is.na(cymba_concha_role) &&
        !is.na(earlobe_role) &&
        cymba_concha_role == earlobe_role
    ) {
      stop(
        sprintf(
          "%s assigns the same legacy belief belief role to both sites.",
          basename(input_file)
        )
      )
    }

    if (!is.na(cymba_concha_role) && cymba_concha_role == "active") {
      values$believed_active_site <- "cymba concha"
    }
    if (!is.na(cymba_concha_role) && cymba_concha_role == "control") {
      values$believed_control_site <- "cymba concha"
    }
    if (!is.na(earlobe_role) && earlobe_role == "active") {
      values$believed_active_site <- "earlobe"
    }
    if (!is.na(earlobe_role) && earlobe_role == "control") {
      values$believed_control_site <- "earlobe"
    }
  } else {
    values$believed_active_site <- normalize_site(first_from_columns(
      df,
      blinding_columns$believed_active_site
    ))
    values$believed_control_site <- normalize_site(first_from_columns(
      df,
      blinding_columns$believed_control_site
    ))
  }

  session_date <- parse_session_date(df, input_file)
  if (!is.na(blinding_start_date) &&
      !is.na(session_date) && session_date > blinding_start_date) {
    values$had_belief_about_more_helpful_site <- first_from_columns(
      df,
      blinding_columns$had_belief_about_more_helpful_site
    )
    values$believed_more_helpful_site <- first_from_columns(
      df,
      blinding_columns$believed_more_helpful_site
    )
  }

  data.frame(
    participant = participant,
    day = day,
    date = session_date,
    values,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Direct execution retains only day-2 rows, enriches, and places date after time.
if (sys.nframe() == 0) {
  extracted_list <- lapply(input_files, extract_one_file)
  extracted_list <- Filter(Negate(is.null), extracted_list)

  if (length(extracted_list) == 0) {
    empty_columns <- lapply(
      blinding_columns,
      function(unused_column_name) character(0)
    )
    names(empty_columns) <- names(blinding_columns)
    extracted <- data.frame(
      participant = character(0),
      day = character(0),
      date = as.Date(character(0)),
      empty_columns,
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
  if (nrow(extracted) == 0) {
    empty_metadata <- as.data.frame(
      setNames(
        replicate(
          length(metadata_helper$metadata_output_columns),
          character(0),
          simplify = FALSE
        ),
        metadata_helper$metadata_output_columns
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    remaining_columns <- setdiff(names(extracted), c("participant", "day"))
    extracted <- cbind(
      extracted[c("participant", "day")],
      empty_metadata,
      extracted[remaining_columns],
      stringsAsFactors = FALSE
    )
  }
  columns_without_date <- setdiff(names(extracted), "date")
  extracted <- extracted[
    append(
      columns_without_date,
      "date",
      after = match("time_of_day", columns_without_date)
    )
  ]

  write.csv(
    extracted,
    output_file,
    row.names = FALSE,
    na = ""
  )
  message(sprintf("Wrote %d blinding rows to %s", nrow(extracted), output_file))
}
