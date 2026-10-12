################################################################################
# Title = Master PsychoPy data extraction pipeline
# Author(s) = Yuwen "Richard" Shan
# R version = 4.5.1
# Dependencies = base R; data.table
# Creation Date = 1 July 2026
# Modification Date = 6 September 2026 (original research version)
# Context = PsychoPy behavioral-task data extraction; public code showcase
# Description = Discovers primary sessions, resolves restarts and documented
# exceptions, runs every extraction module, enriches metadata, and writes audits.
# Inputs = PsychoPy raw-data root, extraction modules, and configuration CSVs
# Outputs = Analysis-ready CSVs and reproducibility audits in the output folder
# Tags = PsychoPy, taVNS, N-back, extraction pipeline, restart handling, R
################################################################################
# Public release: private paths, study attribution, and participant-specific
# records have been removed. Execution has not been validated for this release.

run_started_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# Public placeholders replace private workstation and cloud-storage paths.
# This release contains no input data or populated study configuration.
# Both directories can be overridden with positional command-line arguments.
default_input_dir <- file.path(getwd(), "private_data")
default_output_dir <- file.path(getwd(), "outputs")
script_file_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)
script_dir <- if (length(script_file_argument) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_file_argument[[1]])))
} else {
  normalizePath(getwd())
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  input_dir <- default_input_dir
  output_dir <- default_output_dir
} else if (length(args) == 2) {
  input_dir <- args[[1]]
  output_dir <- args[[2]]
} else {
  stop("Provide either no arguments, or: input_directory output_directory")
}

if (!dir.exists(input_dir)) {
  stop(sprintf("Input directory not found: %s", input_dir))
}
if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("The data.table package is required.")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Modules are sourced into isolated environments so identically named helper
# functions cannot overwrite one another.
module_files <- c(
  parameter = "extract_parameter_phase.R",
  nback = "extract_nback_phase.R",
  demographics = "extract_demographics_other_info.R",
  sideeffects = "extract_side_effects.R",
  trials = "extract_nback_task_trials.R",
  medication = "extract_medication_list.R",
  blinding = "extract_blinding.R"
)

load_module <- function(module_file) {
  module_path <- file.path(script_dir, module_file)
  if (!file.exists(module_path)) {
    stop(sprintf("Extraction module not found: %s", module_path))
  }

  module_environment <- new.env(parent = globalenv())
  sys.source(module_path, envir = module_environment)

  if (!exists("extract_one_file", envir = module_environment, inherits = FALSE)) {
    stop(sprintf("%s does not define extract_one_file().", module_file))
  }

  module_environment
}

modules <- lapply(module_files, load_module)

metadata_helper_file <- file.path(script_dir, "extraction_metadata_helpers.R")
metadata_helper <- new.env(parent = globalenv())
sys.source(metadata_helper_file, envir = metadata_helper)
condition_order_file <- file.path(
  script_dir,
  "taVNS_sham_&_stim_no_stim_order.csv"
)
session_exception_file <- file.path(script_dir, "session_exceptions.csv")
session_exclusion_file <- file.path(script_dir, "session_exclusions.csv")
supplemental_session_source_file <- file.path(
  script_dir,
  "supplemental_session_sources.csv"
)
sideeffect_partial_phase_rule_file <- file.path(
  script_dir,
  "sideeffect_partial_phase_rules.csv"
)
session_assembly_rule_file <- file.path(
  script_dir,
  "session_assembly_rules.csv"
)
data_correction_file <- file.path(script_dir, "data_corrections.csv")

# Corrections are applied only after extraction and metadata enrichment. Requiring
# the expected value makes each correction fail if its source or position changes.
read_data_corrections <- function(correction_file) {
  if (!file.exists(correction_file)) {
    stop(sprintf("Data correction file not found: %s", correction_file))
  }

  corrections <- read.csv(
    correction_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = character(0)
  )
  required_columns <- c(
    "output_table",
    "participant",
    "day",
    "block_number",
    "column",
    "expected_value",
    "replacement_value",
    "reason"
  )
  missing_columns <- setdiff(required_columns, names(corrections))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Data correction file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  corrections <- corrections[required_columns]
  corrections[] <- lapply(corrections, trimws)
  if (nrow(corrections) == 0) return(corrections)

  required_values <- corrections[c(
    "output_table",
    "participant",
    "day",
    "column",
    "expected_value",
    "reason"
  )]
  if (anyNA(required_values) || any(!nzchar(as.matrix(required_values)))) {
    stop(
      paste(
        "Data corrections must include a table, identity, column, expected",
        "value, and reason. A blank replacement_value sets the cell to missing."
      )
    )
  }
  if (any(!grepl("^[0-9]+$", corrections$participant))) {
    stop("Data correction participants must be numeric IDs.")
  }
  if (any(!corrections$day %in% c("1", "2"))) {
    stop("Data correction days must be 1 or 2.")
  }

  allowed_columns <- list(
    parameter_phase = c(
      "arousal",
      "alertness",
      "tolerability",
      "intensity",
      "break_duration_without10sfixed_ins"
    ),
    demographics = c(
      "start_experiment_timing",
      "duration_break_before_nback_task_seconds",
      "baseline_arousal",
      "pre_parameter_phase_arousal",
      "pre_nback_phase_arousal",
      "baseline_alert",
      "pre_parameter_phase_alert",
      "pre_nback_phase_alert",
      "weight",
      "years_education",
      "height",
      "pre_or_postmenopause",
      "premenopause_last_period",
      "post_menopause_last_period",
      "dominant_hand"
    )
  )
  if (any(!corrections$output_table %in% names(allowed_columns))) {
    stop("Data correction file contains an unsupported output_table.")
  }
  valid_column <- mapply(
    function(output_table, column) column %in% allowed_columns[[output_table]],
    corrections$output_table,
    corrections$column,
    USE.NAMES = FALSE
  )
  if (any(!valid_column)) {
    stop("Data correction file contains a column unsupported for its table.")
  }

  parameter_rows <- corrections$output_table == "parameter_phase"
  valid_parameter_block <- corrections$block_number %in%
    as.character(seq_len(6))
  if (any(parameter_rows & !valid_parameter_block)) {
    stop("Parameter-phase corrections require block numbers between 1 and 6.")
  }
  if (any(!parameter_rows & nzchar(corrections$block_number))) {
    stop("Demographic corrections must leave block_number blank.")
  }

  correction_key <- paste(
    corrections$output_table,
    corrections$participant,
    corrections$day,
    corrections$block_number,
    corrections$column,
    sep = "|"
  )
  if (anyDuplicated(correction_key)) {
    stop("Data correction file contains duplicate target cells.")
  }

  corrections
}

data_corrections <- read_data_corrections(data_correction_file)

# Day-specific expectations prevent intentional day-1/day-2 absences from being
# reported as extraction failures.
core_modules <- c(
  "parameter",
  "nback",
  "demographics",
  "sideeffects",
  "trials"
)
selection_modules <- c(core_modules, "medication", "blinding")
module_expected_days <- list(
  parameter = c("1", "2"),
  nback = c("1", "2"),
  demographics = c("1", "2"),
  sideeffects = c("1", "2"),
  trials = c("1", "2"),
  medication = "1",
  blinding = "2"
)

# Modules may declare participant-day exclusions that apply only to their own
# output. Keeping them out of the expected set also keeps audits and row counts
# aligned with the scientific tables.
module_excluded_keys <- lapply(modules[selection_modules], function(module_env) {
  if (!exists(
    "excluded_participant_day_keys",
    envir = module_env,
    inherits = FALSE
  )) {
    return(character(0))
  }

  keys <- as.character(module_env$excluded_participant_day_keys)
  if (anyNA(keys) || any(!grepl("^[0-9]+[|][12]$", keys))) {
    stop("Module participant-day exclusions must use participant|day keys.")
  }
  unique(keys)
})

is_module_expected <- function(module_name, participant, day) {
  participant_day_key <- paste(participant, day, sep = "|")
  as.character(day) %in% module_expected_days[[module_name]] &&
    !participant_day_key %in% module_excluded_keys[[module_name]]
}

expected_keys_for_module <- function(module_name, audit_table) {
  expected_rows <- mapply(
    function(participant_value, day_value) {
      is_module_expected(module_name, participant_value, day_value)
    },
    audit_table$participant,
    audit_table$day,
    USE.NAMES = FALSE
  )

  unique(
    audit_table$participant_day_key[expected_rows]
  )
}

# Accept alphabetic restart suffixes in participant/day identifiers while keeping
# a strict timestamped PsychoPy filename contract.
session_pattern <- paste0(
  "^P_([0-9]+)([[:alpha:]]*)",
  "_D__([12])([[:alnum:]-]*)",
  "_date__",
  "([0-9]{4}-[0-9]{2}-[0-9]{2}_",
  "[0-9]{2}h[0-9]{2}[.][0-9]{2}[.][0-9]+)",
  "[.]csv$"
)

timestamp_csv_pattern <- paste0(
  "^P_.*_D_+.*_date__",
  "[0-9]{4}-[0-9]{2}-[0-9]{2}_",
  "[0-9]{2}h[0-9]{2}[.][0-9]{2}[.][0-9]+",
  "[.]csv$"
)

# Only the primary data roots are scanned. Edge-case subfolders enter through the
# explicit exception registry below and are not recursively harvested.
primary_search_dirs <- c("", "sessions")
primary_search_dirs <- primary_search_dirs[
  dir.exists(file.path(input_dir, primary_search_dirs))
]

list_search_directory <- function(relative_dir) {
  directory <- file.path(input_dir, relative_dir)
  file_names <- list.files(
    directory,
    recursive = FALSE,
    full.names = FALSE,
    all.files = FALSE
  )
  relative_paths <- if (nzchar(relative_dir)) {
    file.path(relative_dir, file_names)
  } else {
    file_names
  }

  data.frame(
    file_name = file_names,
    relative_path = relative_paths,
    search_dir = relative_dir,
    stringsAsFactors = FALSE
  )
}

discovered_files <- do.call(
  rbind,
  lapply(primary_search_dirs, list_search_directory)
)
candidate_files <- discovered_files[
  grepl(session_pattern, discovered_files$file_name),
  ,
  drop = FALSE
]

# Duplicate filenames are accepted only when their contents are byte-identical.
choose_primary_copy <- function(rows) {
  if (nrow(rows) == 1) return(rows)

  paths <- file.path(input_dir, rows$relative_path)
  checksums <- unname(tools::md5sum(paths))
  if (anyNA(checksums) || length(unique(checksums)) != 1) {
    stop(
      sprintf(
        "Conflicting primary session copies share the filename %s: %s",
        rows$file_name[[1]],
        paste(rows$relative_path, collapse = ", ")
      )
    )
  }

  session_subfolder_copy <- which(rows$search_dir == "sessions")
  chosen_index <- if (length(session_subfolder_copy) > 0) {
    session_subfolder_copy[[1]]
  } else {
    1L
  }
  rows[chosen_index, , drop = FALSE]
}

candidate_files <- do.call(
  rbind,
  lapply(
    split(candidate_files, candidate_files$file_name),
    choose_primary_copy
  )
)
candidate_files <- candidate_files[
  order(candidate_files$file_name, candidate_files$relative_path),
  ,
  drop = FALSE
]
candidate_names <- candidate_files$file_name

if (length(candidate_names) == 0) {
  stop("No primary session CSVs matched the validated filename pattern.")
}

parse_candidate_name <- function(relative_path) {
  file_name <- basename(relative_path)
  parts <- regmatches(file_name, regexec(session_pattern, file_name))[[1]]
  timestamp_text <- parts[[6]]
  timestamp <- as.POSIXct(
    timestamp_text,
    format = "%Y-%m-%d_%Hh%M.%OS",
    tz = "America/Los_Angeles"
  )

  if (is.na(timestamp)) {
    stop(sprintf("Could not parse timestamp in filename: %s", file_name))
  }

  data.frame(
    file_name = file_name,
    relative_path = relative_path,
    participant = parts[[2]],
    participant_suffix = parts[[3]],
    day = parts[[4]],
    day_suffix = parts[[5]],
    timestamp_text = timestamp_text,
    timestamp = timestamp,
    identity_override = FALSE,
    identity_override_reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

candidate_metadata <- do.call(
  rbind,
  lapply(candidate_files$relative_path, parse_candidate_name)
)

# Exception rows map known naming errors or pilot filenames to canonical IDs.
read_session_exceptions <- function(exception_file) {
  if (!file.exists(exception_file)) {
    stop(sprintf("Session exception file not found: %s", exception_file))
  }

  exceptions <- read.csv(
    exception_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  required_columns <- c(
    "relative_path",
    "canonical_participant",
    "canonical_day",
    "reason"
  )
  missing_columns <- setdiff(required_columns, names(exceptions))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Session exception file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  exceptions <- exceptions[required_columns]
  exceptions[] <- lapply(exceptions, function(values) {
    trimws(as.character(values))
  })
  if (anyNA(exceptions) || any(!nzchar(as.matrix(exceptions)))) {
    stop("Session exception file contains blank required values.")
  }
  if (anyDuplicated(exceptions$relative_path)) {
    stop("Session exception file contains duplicate relative paths.")
  }
  if (any(!grepl("^[0-9]+$", exceptions$canonical_participant))) {
    stop("Session exception participants must be numeric IDs.")
  }
  if (any(!exceptions$canonical_day %in% c("1", "2"))) {
    stop("Session exception days must be 1 or 2.")
  }
  unsafe_path <- grepl("^/", exceptions$relative_path) |
    grepl("(^|/)[.][.](/|$)", exceptions$relative_path)
  if (any(unsafe_path)) {
    stop("Session exception paths must stay relative to the input directory.")
  }

  exception_paths <- file.path(input_dir, exceptions$relative_path)
  missing_files <- exceptions$relative_path[!file.exists(exception_paths)]
  if (length(missing_files) > 0) {
    stop(
      sprintf(
        "Session exception file(s) not found under input directory: %s",
        paste(missing_files, collapse = ", ")
      )
    )
  }
  timestamp_pattern <- paste0(
    "([0-9]{4}-[0-9]{2}-[0-9]{2}_",
    "[0-9]{2}h[0-9]{2}[.][0-9]{2}[.][0-9]+)"
  )
  rows <- lapply(seq_len(nrow(exceptions)), function(index) {
    exception <- exceptions[index, , drop = FALSE]
    file_name <- basename(exception$relative_path[[1]])
    timestamp_match <- regmatches(
      file_name,
      regexec(timestamp_pattern, file_name)
    )[[1]]
    if (length(timestamp_match) < 2) {
      stop(sprintf("Could not parse timestamp in exception: %s", file_name))
    }
    timestamp_text <- timestamp_match[[2]]
    timestamp <- as.POSIXct(
      timestamp_text,
      format = "%Y-%m-%d_%Hh%M.%OS",
      tz = "America/Los_Angeles"
    )
    if (is.na(timestamp)) {
      stop(sprintf("Could not parse timestamp in exception: %s", file_name))
    }

    data.frame(
      file_name = file_name,
      relative_path = exception$relative_path[[1]],
      participant = exception$canonical_participant[[1]],
      participant_suffix = "",
      day = exception$canonical_day[[1]],
      day_suffix = "",
      timestamp_text = timestamp_text,
      timestamp = timestamp,
      identity_override = TRUE,
      identity_override_reason = exception$reason[[1]],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

exception_metadata <- read_session_exceptions(session_exception_file)
overridden_candidate_paths <- intersect(
  candidate_metadata$relative_path,
  exception_metadata$relative_path
)
candidate_metadata <- candidate_metadata[
  !candidate_metadata$relative_path %in% exception_metadata$relative_path,
  ,
  drop = FALSE
]
standard_candidate_count <- nrow(candidate_metadata)
candidate_metadata <- rbind(candidate_metadata, exception_metadata)
candidate_metadata$file_path <- file.path(
  input_dir,
  candidate_metadata$relative_path
)

# Explicitly registered project-local sources supplement the primary input
# directory without making the master harvest arbitrary CSVs beside the scripts.
read_supplemental_session_sources <- function(source_file) {
  if (!file.exists(source_file)) {
    stop(sprintf("Supplemental session source file not found: %s", source_file))
  }

  sources <- read.csv(
    source_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  required_columns <- c("relative_path", "reason")
  missing_columns <- setdiff(required_columns, names(sources))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Supplemental source file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  sources <- sources[required_columns]
  sources[] <- lapply(sources, function(values) trimws(as.character(values)))
  if (nrow(sources) == 0 || anyNA(sources) || any(!nzchar(as.matrix(sources)))) {
    stop("Supplemental session sources must be complete and nonblank.")
  }
  if (anyDuplicated(sources$relative_path)) {
    stop("Supplemental session sources contain duplicate relative paths.")
  }
  unsafe_path <- grepl("^/", sources$relative_path) |
    grepl("(^|/)[.][.](/|$)", sources$relative_path)
  if (any(unsafe_path)) {
    stop("Supplemental session paths must stay within the project directory.")
  }

  local_paths <- file.path(script_dir, sources$relative_path)
  missing_files <- sources$relative_path[!file.exists(local_paths)]
  if (length(missing_files) > 0) {
    stop(
      sprintf(
        "Supplemental session source file(s) not found: %s",
        paste(missing_files, collapse = ", ")
      )
    )
  }
  invalid_names <- sources$relative_path[
    !grepl(session_pattern, basename(sources$relative_path))
  ]
  if (length(invalid_names) > 0) {
    stop(
      sprintf(
        "Supplemental source filename(s) do not match the session pattern: %s",
        paste(invalid_names, collapse = ", ")
      )
    )
  }

  metadata <- do.call(
    rbind,
    lapply(sources$relative_path, parse_candidate_name)
  )
  duplicate_names <- intersect(metadata$file_name, candidate_metadata$file_name)
  if (length(duplicate_names) > 0) {
    stop(
      sprintf(
        "Supplemental source filename(s) already exist in primary inputs: %s",
        paste(duplicate_names, collapse = ", ")
      )
    )
  }
  metadata$file_path <- local_paths

  list(metadata = metadata, registry = sources)
}

supplemental_session_sources <- read_supplemental_session_sources(
  supplemental_session_source_file
)
supplemental_metadata <- supplemental_session_sources$metadata
candidate_metadata <- rbind(candidate_metadata, supplemental_metadata)
candidate_metadata$file_size_bytes <- file.info(
  candidate_metadata$file_path
)$size
candidate_metadata$participant_day_key <- paste(
  candidate_metadata$participant,
  candidate_metadata$day,
  sep = "|"
)

# Session exclusions are applied to canonical participant/day identities before
# files are read, keeping excluded sessions out of every extraction and audit.
read_session_exclusions <- function(exclusion_file) {
  if (!file.exists(exclusion_file)) {
    stop(sprintf("Session exclusion file not found: %s", exclusion_file))
  }

  exclusions <- read.csv(
    exclusion_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  required_columns <- c("participant", "day", "reason")
  missing_columns <- setdiff(required_columns, names(exclusions))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Session exclusion file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  exclusions <- exclusions[required_columns]
  exclusions[] <- lapply(exclusions, function(values) {
    trimws(as.character(values))
  })
  if (nrow(exclusions) == 0 ||
      anyNA(exclusions) ||
      any(!nzchar(as.matrix(exclusions)))) {
    stop("Session exclusion file must contain complete, nonblank rows.")
  }
  if (any(!grepl("^[0-9]+$", exclusions$participant))) {
    stop("Session exclusion participants must be numeric IDs.")
  }
  if (any(!exclusions$day %in% c("1", "2"))) {
    stop("Session exclusion days must be 1 or 2.")
  }

  exclusions$participant_day_key <- paste(
    exclusions$participant,
    exclusions$day,
    sep = "|"
  )
  if (anyDuplicated(exclusions$participant_day_key)) {
    stop("Session exclusion file contains duplicate participant-day rows.")
  }

  exclusions
}

session_exclusions <- read_session_exclusions(session_exclusion_file)
excluded_candidate_metadata <- candidate_metadata[
  candidate_metadata$participant_day_key %in%
    session_exclusions$participant_day_key,
  ,
  drop = FALSE
]
candidate_metadata <- candidate_metadata[
  !candidate_metadata$participant_day_key %in%
    session_exclusions$participant_day_key,
  ,
  drop = FALSE
]
if (nrow(candidate_metadata) == 0) {
  stop("All matched primary sessions were removed by session exclusions.")
}
standard_candidate_count <- sum(!candidate_metadata$identity_override)

# Partial side-effect rules recover a documented phase without declaring the
# participant's missing phase complete in selection and unresolved-session audits.
read_sideeffect_partial_phase_rules <- function(rule_file) {
  if (!file.exists(rule_file)) {
    stop(sprintf("Side-effect partial-phase rule file not found: %s", rule_file))
  }

  rules <- read.csv(
    rule_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  required_columns <- c(
    "relative_path",
    "participant",
    "day",
    "phase_type",
    "reason"
  )
  missing_columns <- setdiff(required_columns, names(rules))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Side-effect partial-phase rule file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  rules <- rules[required_columns]
  rules[] <- lapply(rules, function(values) trimws(as.character(values)))
  if (nrow(rules) == 0 || anyNA(rules) || any(!nzchar(as.matrix(rules)))) {
    stop("Side-effect partial-phase rules must be complete and nonblank.")
  }
  if (any(!grepl("^[0-9]+$", rules$participant))) {
    stop("Side-effect partial-phase participants must be numeric IDs.")
  }
  if (any(!rules$day %in% c("1", "2"))) {
    stop("Side-effect partial-phase days must be 1 or 2.")
  }
  if (any(!rules$phase_type %in% c("parameter", "n_back"))) {
    stop("Side-effect partial-phase values must be parameter or n_back.")
  }
  unsafe_path <- grepl("^/", rules$relative_path) |
    grepl("(^|/)[.][.](/|$)", rules$relative_path)
  if (any(unsafe_path)) {
    stop("Side-effect partial-phase paths must stay within the input directory.")
  }
  rule_key <- paste(rules$relative_path, rules$phase_type, sep = "|")
  if (anyDuplicated(rule_key)) {
    stop("Side-effect partial-phase rules contain duplicate path/phase rows.")
  }

  candidate_index <- match(rules$relative_path, candidate_metadata$relative_path)
  if (anyNA(candidate_index)) {
    stop(
      sprintf(
        "Side-effect partial-phase source is not an included candidate: %s",
        paste(rules$relative_path[is.na(candidate_index)], collapse = ", ")
      )
    )
  }
  identity_mismatch <-
    rules$participant != candidate_metadata$participant[candidate_index] |
    rules$day != candidate_metadata$day[candidate_index]
  if (any(identity_mismatch)) {
    stop("Side-effect partial-phase rule identity does not match its source file.")
  }

  rules$candidate_index <- candidate_index
  rules
}

sideeffect_partial_phase_rules <- read_sideeffect_partial_phase_rules(
  sideeffect_partial_phase_rule_file
)

timestamp_csv_files <- discovered_files[
  grepl(timestamp_csv_pattern, discovered_files$file_name),
  ,
  drop = FALSE
]
excluded_timestamp_csvs <- timestamp_csv_files$relative_path[
  !timestamp_csv_files$file_name %in% candidate_names &
    !timestamp_csv_files$relative_path %in% exception_metadata$relative_path
]
filename_exclusions <- data.frame(
  file_name = excluded_timestamp_csvs,
  reason = rep(
    paste(
      "Excluded by validated participant/day parser",
      "(for example pilot, test, or blank-day file)."
    ),
    length(excluded_timestamp_csvs)
  ),
  stringsAsFactors = FALSE
)

# Assembly rules document participant-specific transformations that cannot be
# inferred safely from generic restart logic.
read_session_assembly_rules <- function(rule_file) {
  if (!file.exists(rule_file)) {
    stop(sprintf("Session assembly rule file not found: %s", rule_file))
  }

  rules <- read.csv(
    rule_file,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("NA")
  )
  required_columns <- c(
    "participant",
    "day",
    "drop_repeated_pre_nback_rating",
    "prefer_assembled_modules",
    "reason"
  )
  missing_columns <- setdiff(required_columns, names(rules))
  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "Session assembly rule file is missing columns: %s",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  rules <- rules[required_columns]
  rules[] <- lapply(rules, function(values) trimws(as.character(values)))
  if (nrow(rules) == 0 || anyNA(rules) || any(!nzchar(as.matrix(rules)))) {
    stop("Session assembly rule file must contain complete, nonblank rows.")
  }
  if (any(!grepl("^[0-9]+$", rules$participant))) {
    stop("Session assembly rule participants must be numeric IDs.")
  }
  if (any(!rules$day %in% c("1", "2"))) {
    stop("Session assembly rule days must be 1 or 2.")
  }

  drop_values <- tolower(rules$drop_repeated_pre_nback_rating)
  if (any(!drop_values %in% c("true", "false"))) {
    stop("drop_repeated_pre_nback_rating must be TRUE or FALSE.")
  }
  rules$drop_repeated_pre_nback_rating <- drop_values == "true"
  rules$participant_day_key <- paste(rules$participant, rules$day, sep = "|")
  if (anyDuplicated(rules$participant_day_key)) {
    stop("Session assembly rule file contains duplicate participant-day rows.")
  }

  preferred_modules <- lapply(
    strsplit(rules$prefer_assembled_modules, ";", fixed = TRUE),
    trimws
  )
  invalid_modules <- setdiff(unique(unlist(preferred_modules)), selection_modules)
  if (length(invalid_modules) > 0) {
    stop(
      sprintf(
        "Session assembly rule file contains unknown modules: %s",
        paste(invalid_modules, collapse = ", ")
      )
    )
  }

  list(rules = rules, preferred_modules = preferred_modules)
}

assembly_rule_config <- read_session_assembly_rules(
  session_assembly_rule_file
)
assembly_rules <- assembly_rule_config$rules
assembly_preferred_modules <- assembly_rule_config$preferred_modules

assembly_rule_index <- function(participant_day_key) {
  match(participant_day_key, assembly_rules$participant_day_key)
}

is_assembled_module_preferred <- function(participant_day_key, module_name) {
  rule_index <- assembly_rule_index(participant_day_key)
  !is.na(rule_index) &&
    module_name %in% assembly_preferred_modules[[rule_index]]
}

drop_repeated_pre_nback_rating <- function(group_data, column_name,
                                           participant_day_key) {
  block_column <- "N_back_Block_order.thisN"
  adjusted_data <- group_data
  preceding_response_count <- 0L
  dropped <- FALSE

  for (component_index in seq_along(adjusted_data)) {
    component <- adjusted_data[[component_index]]
    if (!column_name %in% names(component)) next

    values <- component[[column_name]]
    response_rows <- which(!is.na(values) & as.character(values) != "")
    has_restart_pattern <- FALSE
    if (block_column %in% names(component) && length(response_rows) == 7) {
      response_blocks <- as.character(component[[block_column]][response_rows])
      has_restart_pattern <-
        (is.na(response_blocks[[1]]) || response_blocks[[1]] == "") &&
        identical(
          suppressWarnings(as.integer(response_blocks[-1])),
          0:5
        )
    }

    if (preceding_response_count == 9L && has_restart_pattern) {
      adjusted_data[[component_index]][[column_name]][response_rows[[1]]] <- NA
      response_rows <- response_rows[-1]
      dropped <- TRUE
    }
    preceding_response_count <- preceding_response_count + length(response_rows)
  }

  if (!dropped) {
    stop(
      sprintf(
        "Assembly rule for %s could not find the repeated pre-N-back value in %s.",
        participant_day_key,
        column_name
      )
    )
  }

  adjusted_data
}

prepare_assembly_components <- function(group_data, participant_day_key) {
  rule_index <- assembly_rule_index(participant_day_key)
  if (
    is.na(rule_index) ||
      !assembly_rules$drop_repeated_pre_nback_rating[[rule_index]]
  ) {
    return(list(data = group_data, adjustments = ""))
  }

  response_columns <- c(
    "O_SelfAssessmentManikinArousal.response",
    "O_Drowsy_Alert.response"
  )
  for (column_name in response_columns) {
    group_data <- drop_repeated_pre_nback_rating(
      group_data,
      column_name,
      participant_day_key
    )
  }

  list(
    data = group_data,
    adjustments = "Dropped repeated pre-N-back arousal and alertness ratings"
  )
}

first_nonmissing_text <- function(df, column_name) {
  if (!column_name %in% names(df)) return(NA_character_)
  values <- trimws(as.character(df[[column_name]]))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) NA_character_ else values[[1]]
}

normalize_session_ids <- function(df, participant, day) {
  df[["participant"]] <- participant
  df[["day"]] <- day
  df
}

read_demographics_companion <- function(companion_file) {
  lines <- readLines(companion_file, warn = FALSE)
  if (length(lines) < 2) {
    stop("Companion demographics file has no data lines.")
  }

  headers <- strsplit(lines[[1]], ",", fixed = TRUE)[[1]]
  remaining_lines <- lines[-1]
  blank_positions <- which(!nzchar(trimws(remaining_lines)))
  data_line_count <- if (length(blank_positions) == 0) {
    length(remaining_lines)
  } else {
    blank_positions[[1]] - 1L
  }

  if (data_line_count < 1) {
    stop("Companion demographics file has no response row.")
  }

  response_text <- paste0(
    remaining_lines[seq_len(data_line_count)],
    collapse = ""
  )
  values <- strsplit(response_text, ",", fixed = TRUE)[[1]]

  if (length(values) != length(headers)) {
    stop(
      sprintf(
        "Companion demographics file has %d headers but %d reconstructed values.",
        length(headers),
        length(values)
      )
    )
  }

  names(values) <- sub("_raw$", "", headers)
  values
}

inject_companion_demographics <- function(df, values) {
  for (column_name in names(values)) {
    value <- values[[column_name]]
    if (!nzchar(value)) next

    if (!column_name %in% names(df)) {
      df[[column_name]] <- rep(NA_character_, nrow(df))
    }

    existing_values <- trimws(as.character(df[[column_name]]))
    has_existing_value <- any(
      !is.na(existing_values) & nzchar(existing_values)
    )

    if (!has_existing_value) {
      df[[column_name]][[1]] <- value
    }
  }

  df
}

# Convert module errors and warnings into structured audit fields so one malformed
# candidate cannot terminate the full run before another restart is considered.
safe_extract <- function(module_environment, input_file, df) {
  captured_warnings <- character(0)
  error_message <- NA_character_

  value <- tryCatch(
    withCallingHandlers(
      module_environment$extract_one_file(input_file, df),
      warning = function(warning_condition) {
        captured_warnings <<- c(
          captured_warnings,
          conditionMessage(warning_condition)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) {
      error_message <<- conditionMessage(error_condition)
      NULL
    }
  )

  list(
    success = is.na(error_message),
    value = value,
    error = error_message,
    warnings = paste(unique(captured_warnings), collapse = " | ")
  )
}

failed_result <- function(error_message) {
  list(
    success = FALSE,
    value = NULL,
    error = error_message,
    warnings = ""
  )
}

# Read every candidate once, then reuse the in-memory data frame across modules.
candidate_results <- vector("list", nrow(candidate_metadata))
candidate_data <- vector("list", nrow(candidate_metadata))
audit_rows <- vector("list", nrow(candidate_metadata))

message(
  sprintf(
    "Reading %d matched primary session CSVs (auxiliary files are excluded).",
    nrow(candidate_metadata)
  )
)

for (candidate_index in seq_len(nrow(candidate_metadata))) {
  metadata <- candidate_metadata[candidate_index, , drop = FALSE]
  input_file <- metadata$file_path[[1]]

  message(
    sprintf(
      "[%d/%d] %s",
      candidate_index,
      nrow(candidate_metadata),
      metadata$file_name[[1]]
    )
  )

  read_error <- NA_character_
  read_warnings <- character(0)
  df <- tryCatch(
    withCallingHandlers(
      data.table::fread(
        input_file,
        data.table = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA"),
        showProgress = FALSE
      ),
      warning = function(warning_condition) {
        read_warnings <<- c(
          read_warnings,
          conditionMessage(warning_condition)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) {
      read_error <<- conditionMessage(error_condition)
      NULL
    }
  )

  raw_participant <- NA_character_
  raw_day <- NA_character_
  row_count <- NA_integer_
  companion_file_name <- NA_character_
  companion_used <- FALSE
  companion_error <- NA_character_

  if (is.null(df)) {
    results <- lapply(
      modules,
      function(module_environment) failed_result(read_error)
    )
  } else {
    row_count <- nrow(df)
    raw_participant <- first_nonmissing_text(df, "participant")
    raw_day <- first_nonmissing_text(df, "day")
    df <- normalize_session_ids(
      df,
      metadata$participant[[1]],
      metadata$day[[1]]
    )

    if (length(read_warnings) > 0 && metadata$day[[1]] == "1") {
      companion_base_name <- paste0(
        sub("[.]csv$", "", metadata$file_name[[1]]),
        "Demographics_day1_only.csv"
      )
      relative_directory <- dirname(metadata$relative_path[[1]])
      companion_file_name <- if (relative_directory == ".") {
        companion_base_name
      } else {
        file.path(relative_directory, companion_base_name)
      }
      companion_file <- file.path(dirname(input_file), companion_base_name)

      if (file.exists(companion_file)) {
        companion_values <- tryCatch(
          read_demographics_companion(companion_file),
          error = function(error_condition) {
            companion_error <<- conditionMessage(error_condition)
            NULL
          }
        )

        if (!is.null(companion_values)) {
          df <- inject_companion_demographics(df, companion_values)
          companion_used <- TRUE
        }
      } else {
        companion_error <- "Expected companion demographics file was not found."
      }
    }

    candidate_data[[candidate_index]] <- df
    results <- lapply(
      modules,
      safe_extract,
      input_file = input_file,
      df = df
    )
  }

  candidate_results[[candidate_index]] <- results

  audit_row <- metadata[
    ,
    c(
      "file_name",
      "relative_path",
      "participant",
      "participant_suffix",
      "day",
      "day_suffix",
      "timestamp_text",
      "identity_override",
      "identity_override_reason",
      "file_size_bytes",
      "participant_day_key"
    ),
    drop = FALSE
  ]
  audit_row$raw_participant <- raw_participant
  audit_row$raw_day <- raw_day
  audit_row$participant_id_mismatch <- !is.na(raw_participant) &&
    nzchar(raw_participant) &&
    raw_participant != metadata$participant[[1]]
  audit_row$day_id_mismatch <- !is.na(raw_day) &&
    nzchar(raw_day) &&
    raw_day != metadata$day[[1]]
  audit_row$row_count <- row_count
  audit_row$read_success <- is.na(read_error)
  audit_row$read_error <- read_error
  audit_row$read_warnings <- paste(
    unique(read_warnings),
    collapse = " | "
  )
  audit_row$demographics_companion_file <- companion_file_name
  audit_row$demographics_companion_used <- companion_used
  audit_row$demographics_companion_error <- companion_error

  for (module_name in names(modules)) {
    audit_row[[paste0(module_name, "_success")]] <-
      results[[module_name]]$success
    audit_row[[paste0(module_name, "_error")]] <-
      results[[module_name]]$error
    audit_row[[paste0(module_name, "_warnings")]] <-
      results[[module_name]]$warnings
  }

  audit_rows[[candidate_index]] <- audit_row
}

audit <- do.call(rbind, audit_rows)

for (module_name in selection_modules) {
  audit[[paste0("selected_", module_name)]] <- FALSE
}

# A complete individual file is preferred. Same-visit fragments are also tested
# as an assembly and can be selected when no individual candidate succeeds.
selected_indices <- setNames(
  vector("list", length(selection_modules)),
  selection_modules
)
participant_day_keys <- unique(audit$participant_day_key)
assembly_results <- list()
assembly_component_indices <- list()
assembly_audit_rows <- list()
assembly_audit_index <- 0L

for (participant_day_key in participant_day_keys) {
  group_indices <- which(
    audit$participant_day_key == participant_day_key &
      audit$read_success
  )

  if (length(group_indices) < 2) next

  # Never concatenate attempts from different calendar dates. Those represent
  # repeated visits, not fragments of one interrupted visit.
  group_dates <- as.Date(
    candidate_metadata$timestamp[group_indices],
    tz = "America/Los_Angeles"
  )
  latest_group_date <- max(group_dates)
  group_indices <- group_indices[group_dates == latest_group_date]
  if (length(group_indices) < 2) next

  group_indices <- group_indices[
    order(candidate_metadata$timestamp[group_indices])
  ]
  group_data <- Filter(Negate(is.null), candidate_data[group_indices])
  if (length(group_data) < 2) next

  prepared_assembly <- prepare_assembly_components(
    group_data,
    participant_day_key
  )
  group_data <- prepared_assembly$data

  combined_df <- as.data.frame(
    data.table::rbindlist(
      group_data,
      use.names = TRUE,
      fill = TRUE
    ),
    stringsAsFactors = FALSE
  )
  latest_index <- group_indices[[length(group_indices)]]
  combined_results <- lapply(
    modules,
    safe_extract,
    input_file = candidate_metadata$file_path[[latest_index]],
    df = combined_df
  )
  assembly_results[[participant_day_key]] <- combined_results
  assembly_component_indices[[participant_day_key]] <- group_indices

  assembly_audit_index <- assembly_audit_index + 1L
  assembly_row <- data.frame(
    participant_day_key = participant_day_key,
    participant = audit$participant[[group_indices[[1]]]],
    day = audit$day[[group_indices[[1]]]],
    component_count = length(group_indices),
    component_files = paste(
      audit$relative_path[group_indices],
      collapse = " | "
    ),
    assembled_row_count = nrow(combined_df),
    assembly_adjustments = prepared_assembly$adjustments,
    stringsAsFactors = FALSE
  )

  for (module_name in names(modules)) {
    assembly_row[[paste0(module_name, "_success")]] <-
      combined_results[[module_name]]$success
    assembly_row[[paste0(module_name, "_error")]] <-
      combined_results[[module_name]]$error
    assembly_row[[paste0(module_name, "_warnings")]] <-
      combined_results[[module_name]]$warnings
  }

  for (module_name in selection_modules) {
    assembly_row[[paste0("selected_", module_name)]] <- FALSE
  }

  assembly_audit_rows[[assembly_audit_index]] <- assembly_row
}

if (length(assembly_audit_rows) == 0) {
  assembly_audit <- data.frame()
} else {
  assembly_audit <- do.call(rbind, assembly_audit_rows)
}

for (module_name in selection_modules) {
  success_column <- paste0(module_name, "_success")
  selected_column <- paste0("selected_", module_name)

  for (participant_day_key in participant_day_keys) {
    participant_rows <- audit$participant_day_key == participant_day_key
    participant_value <- audit$participant[participant_rows][[1]]
    day_value <- audit$day[participant_rows][[1]]
    if (!is_module_expected(module_name, participant_value, day_value)) next

    assembly_result <- assembly_results[[participant_day_key]]
    if (
      is_assembled_module_preferred(participant_day_key, module_name) &&
        !is.null(assembly_result) &&
        assembly_result[[module_name]]$success
    ) {
      next
    }

    group_indices <- which(
      audit$participant_day_key == participant_day_key &
        audit[[success_column]]
    )

    if (length(group_indices) == 0) next

    timestamp_values <- candidate_metadata$timestamp[group_indices]
    size_values <- audit$file_size_bytes[group_indices]
    chosen_order <- order(
      timestamp_values,
      size_values,
      decreasing = TRUE,
      na.last = TRUE
    )
    chosen_index <- group_indices[chosen_order[[1]]]
    audit[[selected_column]][chosen_index] <- TRUE
    selected_indices[[module_name]] <- c(
      selected_indices[[module_name]],
      chosen_index
    )
  }
}

selected_assembly_keys <- setNames(
  vector("list", length(selection_modules)),
  selection_modules
)

if (nrow(assembly_audit) > 0) {
  for (module_name in selection_modules) {
    selected_column <- paste0("selected_", module_name)

    for (participant_day_key in assembly_audit$participant_day_key) {
      participant_rows <-
        assembly_audit$participant_day_key == participant_day_key
      participant_value <- assembly_audit$participant[participant_rows][[1]]
      day_value <- assembly_audit$day[participant_rows][[1]]
      if (!is_module_expected(module_name, participant_value, day_value)) next

      has_individual_selection <- any(
        audit$participant_day_key == participant_day_key &
          audit[[selected_column]]
      )
      assembly_result <- assembly_results[[participant_day_key]][[module_name]]

      if (!has_individual_selection && assembly_result$success) {
        selected_assembly_keys[[module_name]] <- c(
          selected_assembly_keys[[module_name]],
          participant_day_key
        )
        assembly_audit[
          assembly_audit$participant_day_key == participant_day_key,
          selected_column
        ] <- TRUE
      }
    }
  }
}

# Build metadata from the selected visit date, not from every historical attempt.
# All readable same-date fragments are retained so demographics split across a
# restart can still supply age, sex, date, and session start.
all_selected_indices <- function(module_names) {
  unique(unlist(selected_indices[module_names], use.names = FALSE))
}

selected_core_indices <- all_selected_indices(core_modules)
selected_any_module_indices <- all_selected_indices(selection_modules)
candidate_visit_dates <- as.Date(
  candidate_metadata$timestamp,
  tz = "America/Los_Angeles"
)
metadata_indices <- unique(unlist(lapply(
  participant_day_keys,
  function(participant_day_key) {
    core_indices <- selected_core_indices
    core_indices <- core_indices[
      candidate_metadata$participant_day_key[core_indices] == participant_day_key
    ]
    uses_core_assembly <- any(vapply(
      core_modules,
      function(module_name) {
        participant_day_key %in% selected_assembly_keys[[module_name]]
      },
      logical(1)
    ))
    if (uses_core_assembly) {
      core_indices <- unique(c(
        core_indices,
        assembly_component_indices[[participant_day_key]]
      ))
    }

    anchor_indices <- core_indices
    if (length(anchor_indices) == 0) {
      anchor_indices <- selected_any_module_indices[
        candidate_metadata$participant_day_key[selected_any_module_indices] ==
          participant_day_key
      ]
    }
    if (length(anchor_indices) == 0) {
      anchor_indices <- which(
        audit$participant_day_key == participant_day_key & audit$read_success
      )
    }
    if (length(anchor_indices) == 0) return(integer(0))

    anchor_date <- max(candidate_visit_dates[anchor_indices])
    which(
      audit$participant_day_key == participant_day_key &
        audit$read_success &
        candidate_visit_dates == anchor_date
    )
  }
), use.names = FALSE))

if (length(metadata_indices) == 0) {
  stop("No selected participant sessions were available for metadata.")
}
metadata_indices <- sort(metadata_indices)
audit$selected_for_metadata <- seq_len(nrow(audit)) %in% metadata_indices

session_metadata <- metadata_helper$metadata_build(
  candidate_data[metadata_indices],
  candidate_metadata$file_path[metadata_indices],
  condition_order_file,
  participant_overrides = candidate_metadata$participant[metadata_indices],
  day_overrides = candidate_metadata$day[metadata_indices]
)

# Summarize selection state after individual and assembly fallbacks are resolved.
audit$complete_for_all_core_modules <- apply(
  audit[paste0(core_modules, "_success")],
  1,
  all
)
audit$selected_modules <- apply(
  audit[paste0("selected_", selection_modules)],
  1,
  function(selected) {
    paste(selection_modules[as.logical(selected)], collapse = ",")
  }
)

bind_selected_results <- function(module_name) {
  indices <- selected_indices[[module_name]]
  individual_values <- lapply(
    indices,
    function(index) {
      candidate_results[[index]][[module_name]]$value
    }
  )
  assembly_values <- lapply(
    selected_assembly_keys[[module_name]],
    function(participant_day_key) {
      assembly_results[[participant_day_key]][[module_name]]$value
    }
  )
  values <- c(individual_values, assembly_values)
  values <- Filter(Negate(is.null), values)
  if (length(values) == 0) return(NULL)

  result <- do.call(rbind, values)
  rownames(result) <- NULL
  result
}

# Apply deterministic numeric ordering so output diffs are stable across runs.
order_by_columns <- function(df, columns) {
  if (is.null(df) || nrow(df) == 0) return(df)

  order_values <- lapply(columns, function(column_name) {
    values <- df[[column_name]]
    numeric_values <- suppressWarnings(as.numeric(as.character(values)))
    if (all(is.na(numeric_values) == is.na(values))) {
      numeric_values
    } else {
      as.character(values)
    }
  })

  df[do.call(order, order_values), , drop = FALSE]
}

parameter_extracted <- bind_selected_results("parameter")
nback_extracted <- bind_selected_results("nback")
demographics_extracted <- bind_selected_results("demographics")
sideeffects_extracted <- bind_selected_results("sideeffects")
trials_extracted <- bind_selected_results("trials")
medication_extracted <- bind_selected_results("medication")
blinding_extracted <- bind_selected_results("blinding")

partial_sideeffect_values <- lapply(
  seq_len(nrow(sideeffect_partial_phase_rules)),
  function(rule_index) {
    rule <- sideeffect_partial_phase_rules[rule_index, , drop = FALSE]
    candidate_index <- rule$candidate_index[[1]]
    df <- candidate_data[[candidate_index]]
    if (is.null(df)) {
      stop(sprintf("Could not read partial side-effect source: %s", rule$relative_path))
    }

    modules$sideeffects$extract_one_phase(
      candidate_metadata$file_path[[candidate_index]],
      rule$phase_type[[1]],
      df
    )
  }
)
partial_sideeffects <- do.call(rbind, partial_sideeffect_values)
if (!is.null(sideeffects_extracted)) {
  existing_key <- paste(
    sideeffects_extracted$participant,
    sideeffects_extracted$day,
    sideeffects_extracted$phase_type,
    sep = "|"
  )
  partial_key <- paste(
    partial_sideeffects$participant,
    partial_sideeffects$day,
    partial_sideeffects$phase_type,
    sep = "|"
  )
  if (any(partial_key %in% existing_key) || anyDuplicated(partial_key)) {
    stop("Partial side-effect rules duplicate an existing participant phase.")
  }
  sideeffects_extracted <- rbind(sideeffects_extracted, partial_sideeffects)
} else {
  sideeffects_extracted <- partial_sideeffects
}

parameter_extracted <- order_by_columns(
  parameter_extracted,
  c("participant", "day", "block_number")
)
nback_extracted <- order_by_columns(
  nback_extracted,
  c("participant", "day", "block_number_n_back")
)
trials_extracted <- order_by_columns(
  trials_extracted,
  c("participant", "day", "block_number", "trial_number")
)

# The selected demographics row is the authoritative source for displayed start
# time; synchronize that value back into metadata before enriching every output.
if (!is.null(demographics_extracted)) {
  demographics_extracted$.session_date <- NULL
  demographics_extracted <- order_by_columns(
    demographics_extracted,
    c("participant", "day")
  )

  demographics_key <- paste(
    demographics_extracted$participant,
    demographics_extracted$day,
    sep = "|"
  )
  metadata_key <- paste(
    session_metadata$participant,
    session_metadata$day,
    sep = "|"
  )
  metadata_index <- match(demographics_key, metadata_key)
  if (anyNA(metadata_index)) {
    stop("Selected demographics rows are missing from session metadata.")
  }

  demographics_extracted$days_between_day1_day2 <-
    session_metadata$days_between_sessions[metadata_index]
  session_metadata$session_start[metadata_index] <-
    demographics_extracted$start_experiment_timing
  session_metadata$time_of_day[metadata_index] <-
    demographics_extracted$time_of_day
}

# Side-effect phases are separated into two analysis-ready files and no longer
# need a redundant phase_type column.
if (is.null(sideeffects_extracted)) {
  sideeffects_parameter <- NULL
  sideeffects_nback <- NULL
} else {
  sideeffect_columns <- setdiff(names(sideeffects_extracted), "phase_type")
  sideeffects_parameter <- sideeffects_extracted[
    sideeffects_extracted$phase_type == "parameter",
    sideeffect_columns,
    drop = FALSE
  ]
  sideeffects_nback <- sideeffects_extracted[
    sideeffects_extracted$phase_type == "n_back",
    sideeffect_columns,
    drop = FALSE
  ]
  sideeffects_parameter <- order_by_columns(
    sideeffects_parameter,
    c("participant", "day")
  )
  sideeffects_nback <- order_by_columns(
    sideeffects_nback,
    c("participant", "day")
  )
}

# Preserve stable schemas when all medication or blinding values are absent.
if (is.null(medication_extracted)) {
  medication_extracted <- data.frame(
    ID = character(0),
    medication_name = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
} else {
  medication_extracted <- order_by_columns(medication_extracted, c("ID"))
}

if (is.null(blinding_extracted)) {
  blinding_extracted <- data.frame(
    participant = character(0),
    day = character(0),
    date = as.Date(character(0)),
    had_belief_about_more_helpful_site = character(0),
    believed_more_helpful_site = character(0),
    believed_active_site = character(0),
    believed_control_site = character(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
} else {
  blinding_extracted <- order_by_columns(
    blinding_extracted,
    c("participant", "day")
  )
}

# Attach the same participant/session metadata contract to every scientific table.
parameter_extracted <- metadata_helper$metadata_enrich(
  parameter_extracted,
  session_metadata
)
nback_extracted <- metadata_helper$metadata_enrich(
  nback_extracted,
  session_metadata
)
demographics_extracted <- metadata_helper$metadata_enrich(
  demographics_extracted,
  session_metadata
)
sideeffects_parameter <- metadata_helper$metadata_enrich(
  sideeffects_parameter,
  session_metadata
)
sideeffects_nback <- metadata_helper$metadata_enrich(
  sideeffects_nback,
  session_metadata
)
trials_extracted <- metadata_helper$metadata_enrich(
  trials_extracted,
  session_metadata
)
medication_extracted <- metadata_helper$metadata_enrich(
  medication_extracted,
  session_metadata
)
blinding_extracted <- metadata_helper$metadata_enrich(
  blinding_extracted,
  session_metadata
)

# Apply approved cell corrections without changing source CSVs or extraction rules.
decode_correction_value <- function(value) {
  value <- gsub("\\r", "\r", value, fixed = TRUE)
  value <- gsub("\\n", "\n", value, fixed = TRUE)
  gsub("\\t", "\t", value, fixed = TRUE)
}

apply_data_corrections <- function(data_tables, corrections) {
  if (nrow(corrections) == 0) return(data_tables)

  for (correction_index in seq_len(nrow(corrections))) {
    correction <- corrections[correction_index, , drop = FALSE]
    table_name <- correction$output_table
    df <- data_tables[[table_name]]
    target_rows <- which(
      as.character(df$participant) == correction$participant &
        as.character(df$day) == correction$day
    )
    if (nzchar(correction$block_number)) {
      target_rows <- target_rows[
        as.character(df$block_number[target_rows]) == correction$block_number
      ]
    }

    if (length(target_rows) != 1) {
      stop(
        sprintf(
          paste(
            "Data correction must match exactly one row in %s:",
            "participant %s day %s block %s matched %d rows."
          ),
          table_name,
          correction$participant,
          correction$day,
          ifelse(nzchar(correction$block_number), correction$block_number, "NA"),
          length(target_rows)
        )
      )
    }

    target_column <- correction$column
    extracted_value <- as.character(df[[target_column]][target_rows])
    expected_missing <- identical(correction$expected_value, "<MISSING>")
    extracted_missing <- is.na(extracted_value) || !nzchar(trimws(extracted_value))
    expected_value <- decode_correction_value(correction$expected_value)
    expected_matches <- if (expected_missing) {
      extracted_missing
    } else {
      !extracted_missing && identical(extracted_value, expected_value)
    }
    if (!expected_matches) {
      stop(
        sprintf(
          paste(
            "Data correction expected %s in %s for participant %s day %s",
            "block %s column %s but found %s."
          ),
          correction$expected_value,
          table_name,
          correction$participant,
          correction$day,
          ifelse(nzchar(correction$block_number), correction$block_number, "NA"),
          target_column,
          ifelse(is.na(extracted_value), "missing", extracted_value)
        )
      )
    }

    replacement_value <- decode_correction_value(correction$replacement_value)
    if (!nzchar(replacement_value)) {
      df[[target_column]][target_rows] <- NA
    } else if (is.numeric(df[[target_column]])) {
      numeric_replacement <- suppressWarnings(as.numeric(replacement_value))
      if (is.na(numeric_replacement)) {
        stop("Numeric data correction has a nonnumeric replacement.")
      }
      df[[target_column]][target_rows] <- numeric_replacement
    } else {
      df[[target_column]][target_rows] <- replacement_value
    }
    data_tables[[table_name]] <- df
  }

  data_tables
}

corrected_data_tables <- apply_data_corrections(
  list(
    parameter_phase = parameter_extracted,
    demographics = demographics_extracted
  ),
  data_corrections
)
parameter_extracted <- corrected_data_tables$parameter_phase
demographics_extracted <- corrected_data_tables$demographics
blinding_columns_without_date <- setdiff(names(blinding_extracted), "date")
blinding_extracted <- blinding_extracted[
  append(
    blinding_columns_without_date,
    "date",
    after = match("time_of_day", blinding_columns_without_date)
  )
]

# Write each CSV through a same-directory temporary file. Renaming only after a
# successful write prevents a truncated file from replacing the previous result.
write_csv <- function(df, file_name) {
  output_path <- file.path(output_dir, file_name)

  if (is.null(df)) {
    stop(
      sprintf(
        "No rows were available for %s; refusing to leave a stale output.",
        file_name
      )
    )
  }

  temporary_path <- tempfile(
    pattern = paste0(".", file_name, "."),
    tmpdir = output_dir
  )
  on.exit(unlink(temporary_path), add = TRUE)
  write.csv(
    df,
    temporary_path,
    row.names = FALSE,
    na = ""
  )
  if (!file.rename(temporary_path, output_path)) {
    stop(sprintf("Could not publish completed output: %s", output_path))
  }
  message(sprintf("Wrote %d rows to %s", nrow(df), output_path))
}

write_csv(parameter_extracted, "parameter_phase_extracted.csv")
write_csv(nback_extracted, "nback_phase_extracted.csv")
write_csv(
  demographics_extracted,
  "demographics_other_info_extracted.csv"
)
write_csv(
  sideeffects_parameter,
  "Sideeffects_Parameter_extracted.csv"
)
write_csv(sideeffects_nback, "Sideeffects_N_back_extracted.csv")
write_csv(trials_extracted, "nback_task_trials_extracted.csv")
write_csv(medication_extracted, "medication_list_extracted.csv")
write_csv(blinding_extracted, "blinding_extracted.csv")
write_csv(audit, "session_candidate_audit.csv")
write_csv(assembly_audit, "session_assembly_audit.csv")
write_csv(filename_exclusions, "filename_exclusions.csv")

# Report participant-days for which no individual file or approved assembly met a
# module's validation contract. Missing optional day-specific modules are excluded.
unresolved_rows <- list()
unresolved_index <- 0L

for (participant_day_key in participant_day_keys) {
  missing_modules <- selection_modules[vapply(
    selection_modules,
    function(module_name) {
      participant_rows <- audit$participant_day_key == participant_day_key
      participant_value <- audit$participant[participant_rows][[1]]
      day_value <- audit$day[participant_rows][[1]]
      if (!is_module_expected(
        module_name,
        participant_value,
        day_value
      )) return(FALSE)

      has_individual <- any(
        audit$participant_day_key == participant_day_key &
          audit[[paste0("selected_", module_name)]]
      )
      has_assembly <- participant_day_key %in%
        selected_assembly_keys[[module_name]]
      !has_individual && !has_assembly
    },
    logical(1)
  )]

  if (length(missing_modules) == 0) next

  candidate_indices <- which(
    audit$participant_day_key == participant_day_key
  )
  unresolved_index <- unresolved_index + 1L
  unresolved_rows[[unresolved_index]] <- data.frame(
    participant_day_key = participant_day_key,
    participant = audit$participant[[candidate_indices[[1]]]],
    day = audit$day[[candidate_indices[[1]]]],
    missing_modules = paste(missing_modules, collapse = ","),
    candidate_files = paste(
      audit$relative_path[candidate_indices],
      collapse = " | "
    ),
    stringsAsFactors = FALSE
  )
}

if (length(unresolved_rows) == 0) {
  unresolved_sessions <- data.frame(
    participant_day_key = character(0),
    participant = character(0),
    day = character(0),
    missing_modules = character(0),
    candidate_files = character(0),
    stringsAsFactors = FALSE
  )
} else {
  unresolved_sessions <- do.call(rbind, unresolved_rows)
}
write_csv(unresolved_sessions, "unresolved_sessions.csv")

# Aggregate selection coverage for a quick completeness check after each run.
selection_summary <- data.frame(
  module = selection_modules,
  expected_participant_days = vapply(
    selection_modules,
    function(module_name) length(expected_keys_for_module(module_name, audit)),
    integer(1)
  ),
  selected_individual_files = vapply(
    selection_modules,
    function(module_name) length(selected_indices[[module_name]]),
    integer(1)
  ),
  selected_assembled_sessions = vapply(
    selection_modules,
    function(module_name) length(selected_assembly_keys[[module_name]]),
    integer(1)
  ),
  selected_participant_days = vapply(
    selection_modules,
    function(module_name) {
      length(selected_indices[[module_name]]) +
        length(selected_assembly_keys[[module_name]])
    },
    integer(1)
  ),
  participant_days_without_valid_candidate = vapply(
    selection_modules,
    function(module_name) {
      length(expected_keys_for_module(module_name, audit)) -
        length(selected_indices[[module_name]]) -
        length(selected_assembly_keys[[module_name]])
    },
    integer(1)
  ),
  stringsAsFactors = FALSE
)
write_csv(selection_summary, "selection_summary.csv")

# Record inputs, configuration checksums, and script checksums for reproducibility.
script_paths <- file.path(
  script_dir,
  c("run_all_extractions.R", module_files, "extraction_metadata_helpers.R")
)
script_checksums <- tools::md5sum(script_paths)
script_manifest <- data.frame(
  key = paste0("script_md5_", basename(script_paths)),
  value = unname(script_checksums),
  stringsAsFactors = FALSE
)
run_manifest <- rbind(
  data.frame(
    key = c(
      "run_started_at",
      "run_finished_at",
      "input_dir",
      "output_dir",
      "r_version",
      "primary_search_dirs",
      "matched_primary_session_csvs",
      "matched_standard_primary_session_csvs",
      "included_session_exceptions",
      "overridden_standard_session_csvs",
      "included_supplemental_session_sources",
      "configured_session_exclusions",
      "excluded_session_candidates",
      "excluded_participant_day_groups",
      "configured_sideeffect_partial_phases",
      "applied_data_corrections",
      "participant_day_groups",
      "excluded_timestamp_csvs"
    ),
    value = c(
      run_started_at,
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      input_dir,
      output_dir,
      R.version.string,
      paste(primary_search_dirs, collapse = " | "),
      as.character(nrow(candidate_metadata)),
      as.character(standard_candidate_count),
      as.character(nrow(exception_metadata)),
      as.character(length(overridden_candidate_paths)),
      as.character(nrow(supplemental_metadata)),
      as.character(nrow(session_exclusions)),
      as.character(nrow(excluded_candidate_metadata)),
      as.character(length(unique(
        excluded_candidate_metadata$participant_day_key
      ))),
      as.character(nrow(sideeffect_partial_phase_rules)),
      as.character(nrow(data_corrections)),
      as.character(length(participant_day_keys)),
      as.character(nrow(filename_exclusions))
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    key = c(
      "condition_order_file",
      "condition_order_file_md5",
      "session_exception_file",
      "session_exception_file_md5",
      "session_exclusion_file",
      "session_exclusion_file_md5",
      "supplemental_session_source_file",
      "supplemental_session_source_file_md5",
      "sideeffect_partial_phase_rule_file",
      "sideeffect_partial_phase_rule_file_md5",
      "session_assembly_rule_file",
      "session_assembly_rule_file_md5",
      "data_correction_file",
      "data_correction_file_md5"
    ),
    value = c(
      condition_order_file,
      unname(tools::md5sum(condition_order_file)),
      session_exception_file,
      unname(tools::md5sum(session_exception_file)),
      session_exclusion_file,
      unname(tools::md5sum(session_exclusion_file)),
      supplemental_session_source_file,
      unname(tools::md5sum(supplemental_session_source_file)),
      sideeffect_partial_phase_rule_file,
      unname(tools::md5sum(sideeffect_partial_phase_rule_file)),
      session_assembly_rule_file,
      unname(tools::md5sum(session_assembly_rule_file)),
      data_correction_file,
      unname(tools::md5sum(data_correction_file))
    ),
    stringsAsFactors = FALSE
  ),
  script_manifest
)
write_csv(run_manifest, "run_manifest.csv")

message("Master extraction completed.")
