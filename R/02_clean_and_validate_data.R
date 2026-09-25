# =====================================================================
# 02_clean_and_validate_data.R
# Data Validation & Plausibility Pipeline
# ---------------------------------------------------------------------
# This script loads the synthetic source datasets created in
# 01_generate_synthetic_data.R and performs:
#   - structural validation
#   - source-level validity and plausibility checks
#   - explicit quality-status assignment
#   - controlled imputation of eligible missing observations
#   - preservation of raw source values
#
# The workflow separates:
#   1. raw source evidence,
#   2. quality assessment,
#   3. analytical treatment.
#
# Output:
#   data/clean/firms_clean.csv
#   data/clean/employment_clean.csv
#   data/clean/turnover_clean.csv
#   data/clean/accounting_clean.csv
# =====================================================================

library(dplyr)
library(readr)
library(janitor)
library(lubridate)

source("R/helpers/plausibility.R")
source("R/validation.R")
source("R/config.R")

project_config <- load_project_config()
validation_config <- project_config$processing$validation

foundation_year_min <-
  validation_config$foundation_year_min

employment_spike_multiplier <-
  validation_config$employment_spike_multiplier

# Ensure output directory exists ---------------------------------------
dir.create(
  "data/clean",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Load raw datasets
# ----------------------------------------------------------------------

firms_raw <- read_csv(
  "data/raw/firms.csv",
  show_col_types = FALSE
)

employment_raw <- read_csv(
  "data/raw/employment.csv",
  show_col_types = FALSE
)

turnover_raw <- read_csv(
  "data/raw/turnover.csv",
  show_col_types = FALSE
)

accounting_raw <- read_csv(
  "data/raw/accounting.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------------------
# 2. Structural validation
# ----------------------------------------------------------------------

message("Validating source structures...")

validate_source_structures(
  firms_raw,
  employment_raw,
  turnover_raw,
  accounting_raw
)

message("Structural validation passed.")

# ----------------------------------------------------------------------
# 3. Validate register-style enterprise source
# ----------------------------------------------------------------------

message("Validating register-style enterprise source...")

firms_clean <-
  validate_register_source(
    firms_raw,
    foundation_year_min
  )

# ----------------------------------------------------------------------
# 4. Validate monthly employment source
# ----------------------------------------------------------------------

message("Validating monthly employment source...")

employment_clean <-
  validate_employment_source(
    employment_raw,
    employment_spike_multiplier
  )

# ----------------------------------------------------------------------
# 5. Validate monthly turnover source
# ----------------------------------------------------------------------

message("Validating monthly turnover source...")

turnover_clean <-
  validate_turnover_source(
    turnover_raw
  )

# ----------------------------------------------------------------------
# 6. Validate annual accounting source
# ----------------------------------------------------------------------

message("Validating annual accounting source...")

accounting_clean <-
  validate_accounting_source(
    accounting_raw
  )

# ----------------------------------------------------------------------
# 7. Post-validation assertions
# ----------------------------------------------------------------------

message("Checking analytical values after QA treatment...")

assert_validated_sources(
  firms_clean,
  employment_clean,
  turnover_clean,
  accounting_clean
)

# ----------------------------------------------------------------------
# 8. Report QA status counts
# ----------------------------------------------------------------------

message("Register employment QA statuses:")
print(
  firms_clean %>%
    count(employees_register_status)
)

message("Register revenue QA statuses:")
print(
  firms_clean %>%
    count(revenue_status)
)

message("Monthly employment QA statuses:")
print(
  employment_clean %>%
    count(employment_status)
)

message("Monthly turnover QA statuses:")
print(
  turnover_clean %>%
    count(turnover_status)
)

message("Accounting operating revenue QA statuses:")
print(
  accounting_clean %>%
    count(operating_revenue_status)
)

message("Accounting purchases QA statuses:")
print(
  accounting_clean %>%
    count(purchases_status)
)

message("Accounting personnel expense QA statuses:")
print(
  accounting_clean %>%
    count(personnel_expense_status)
)

# ----------------------------------------------------------------------
# 9. Write validated analytical datasets
# ----------------------------------------------------------------------

write_csv(
  firms_clean,
  "data/clean/firms_clean.csv"
)

write_csv(
  employment_clean,
  "data/clean/employment_clean.csv"
)

write_csv(
  turnover_clean,
  "data/clean/turnover_clean.csv"
)

write_csv(
  accounting_clean,
  "data/clean/accounting_clean.csv"
)

message("Validation and plausibility processing completed successfully.")
