# PsychoPy Research Data Extraction in R

**Author:** Yuwen "Richard" Shan

A modular R pipeline for turning wide PsychoPy session exports into structured,
analysis-ready tables. The project demonstrates research data preparation,
validation, restart handling, and auditable correction workflows for a behavioral
task involving N-back trials and stimulation-related questionnaires.

## Public release status

This repository is a **code showcase**, adapted from a research working version.
It contains extraction code and header-only configuration templates. It does not
include research data, participant records, populated study configuration,
extracted results, or internal reports.

**This public version has not been validated as a standalone runnable release.**
Private dependencies and participant-specific rules have been removed or cleared.
There is currently no synthetic dataset or end-to-end demonstration. The code
retains task-specific schema and processing assumptions; it is not a general
extractor for every PsychoPy experiment.

## What the pipeline demonstrates

- **Modular extraction:** separate modules for block ratings, trial outcomes,
  demographics, side effects, medication entries, and blinding responses.
- **Input validation:** required-column checks and checks for expected response
  positions or counts, with errors and warnings recorded for review.
- **Restart handling:** candidate selection and same-day fragment assembly when
  individual session files do not satisfy a module's extraction requirements.
- **Controlled corrections:** cell-level changes require the expected extracted
  value to match before a replacement is applied.
- **Traceability:** candidate and assembly audits, unresolved-session summaries,
  and a run manifest with file checksums.

These are features represented in the source code. No scientific findings or
performance benchmarks are claimed by this public release.

## Repository contents

| File | Purpose |
|---|---|
| `run_all_extractions.R` | Coordinates discovery, validation, selection, assembly, metadata enrichment, corrections, and output writing. |
| `extract_parameter_phase.R` | Extracts parameter-phase block ratings and break durations. |
| `extract_nback_phase.R` | Extracts N-back block ratings and stimulation status. |
| `extract_nback_task_trials.R` | Extracts trial accuracy, reaction time, target status, and associated block fields. |
| `extract_demographics_other_info.R` | Extracts demographics, session timing, and session-level ratings. |
| `extract_side_effects.R` | Extracts side-effect questionnaire responses for each task phase. |
| `extract_medication_list.R` | Converts medication responses into individual medication rows. |
| `extract_blinding.R` | Extracts Day 2 blinding and condition-belief responses. |
| `extraction_metadata_helpers.R` | Provides shared participant, condition, age, and session metadata helpers. |
| `config_templates/` | Header-only schemas for private configuration tables; contains no participant records. |
| `.gitignore` | Allows only the documented source, README, and configuration-template files to be tracked. |

## Processing workflow

```text
Authorized PsychoPy session exports (not included)
                    |
       Filename and identity validation
                    |
       Candidate extraction by module
                    |
       Restart selection / session assembly
                    |
       Metadata enrichment and guarded corrections
                    |
       Structured tables and audit records (not included)
```

The working pipeline organizes outputs at participant-day, participant-day-block,
participant-day-block-trial, and participant-medication levels. The original
schema assumes six blocks and normally 30 N-back trials per block. Such assumptions
must be reviewed before adapting the code to another experiment.

## Configuration templates

Templates contain column names only. Their purpose is to document configuration
interfaces without exposing private records.

| Template | Role |
|---|---|
| `session_exceptions.csv` | Maps filename or identity exceptions to canonical participant/day identities. |
| `session_exclusions.csv` | Records participant-days to exclude. |
| `supplemental_session_sources.csv` | Registers additional project-local source files. |
| `session_assembly_rules.csv` | Specifies adjustments for restart assembly. |
| `sideeffect_partial_phase_rules.csv` | Registers partial phases that can be recovered. |
| `data_corrections.csv` | Specifies target cells, expected values, replacements, and correction reasons. |
| `taVNS_sham_&_stim_no_stim_order.csv` | Defines the participant-level condition-order mapping. |

The master script currently expects populated configuration files beside the R
scripts. The templates remain in `config_templates/` to distinguish them from
working configuration. Some readers reject empty tables, so copying the templates
to the repository root alone will not make the pipeline runnable.

## Software and interface

The original working-version headers identify R 4.5.1. The pipeline uses
`data.table` to read input files and base R for other processing and CSV writing.
No dependency lockfile or public execution validation is included.

The master's command-line interface is:

```bash
Rscript run_all_extractions.R "/path/to/authorized_input" "/path/to/output"
```

This documents the interface, not a working quick start for this repository.
A run requires compatible input exports, authorized private configuration, and
review of the cleared blinding rules. The public no-argument defaults are local
placeholder folders named `private_data/` and `outputs/`. Source discovery refers
to the input root and a generic `sessions/` subfolder.

## Data boundaries and public adaptations

The public copy was prepared separately from the research working directory.
The following content is deliberately excluded:

- Raw session CSVs and all extracted participant tables.
- Populated condition assignments, exceptions, exclusions, corrections, and
  supplemental-source registries.
- Audit outputs and manifests that could reveal participant identities, filenames,
  local paths, or study-specific processing decisions.
- Internal missing-data reports, workstation history, and operating-system files.

Private cloud-storage and workstation paths have been replaced with placeholders.
Real session examples, lab/funding attribution, participant-specific blinding
exclusions and legacy mappings, and the private questionnaire introduction date
have been omitted. Author attribution and the extraction logic are retained.
The blinding cutoff is unset, so date-gated belief items are not populated until
an appropriate cutoff is supplied; private exclusions and legacy mappings are
empty. These changes mean this copy is not equivalent to the working research
pipeline.

The `.gitignore` uses an allowlist for this release. It is a safeguard for Git
tracking, not a substitute for reviewing files before a browser upload. Keep real
research data and configuration outside this public folder.

## Scope of contribution

This project presents the author's R extraction and data-preparation implementation.
It does not claim authorship of the experiment, ownership of the research data,
or a completed statistical analysis of the study. No research findings are
reported here.

## Future work

- Construct fully synthetic session exports and configuration examples.
- Add a reproducible demonstration with expected output tables.
- Validate missing-column, truncated-session, restart, and correction scenarios.
- Separate experiment-specific settings from reusable extraction components.

These items are planned improvements and are not included in this release.
