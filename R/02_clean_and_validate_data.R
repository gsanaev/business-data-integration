# =====================================================================
# 02_clean_and_validate_data.R
# Data Cleaning & Validation Pipeline
# ---------------------------------------------------------------------
# This script loads the synthetic datasets created in
# 01_generate_synthetic_data.R and performs:
#   - structural validation of keys
#   - consistency checks across datasets
#   - rule-based cleaning
#   - plausibility checks
#   - creation of clean analytical files
#
# Output:
#   data/clean/firms_clean.csv
#   data/clean/employment_clean.csv
#   data/clean/turnover_clean.csv
# =====================================================================

library(dplyr)
library(readr)
library(janitor)
library(lubridate)

# Ensure output directory exists ---------------------------------------
dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Load raw datasets
# ----------------------------------------------------------------------

firms_raw      <- read_csv("data/raw/firms.csv", show_col_types = FALSE)
employment_raw <- read_csv("data/raw/employment.csv", show_col_types = FALSE)
turnover_raw   <- read_csv("data/raw/turnover.csv", show_col_types = FALSE)

# ----------------------------------------------------------------------
# 2. Basic structural validation
# ----------------------------------------------------------------------

message("Validating structure...")

# unique firm IDs
unique_ids_firms <- nrow(firms_raw) == n_distinct(firms_raw$firm_id)
if (!unique_ids_firms) warning("Duplicate firm IDs detected in firms.csv!")

# check monthly datasets
employment_missing_ids <- setdiff(employment_raw$firm_id, firms_raw$firm_id)
turnover_missing_ids   <- setdiff(turnover_raw$firm_id, firms_raw$firm_id)

if (length(employment_missing_ids) > 0) warning("Employment: IDs not found in firms.csv")
if (length(turnover_missing_ids) > 0)   warning("Turnover: IDs not found in firms.csv")

# ----------------------------------------------------------------------
# 3. Rule-based cleaning for firms dataset
# ----------------------------------------------------------------------

message("Cleaning firms dataset...")

firms_clean <- firms_raw %>%
  clean_names() %>%
  mutate(
    # Replace negative revenue with absolute value (simple correction rule)
    revenue_last_year = ifelse(
      revenue_last_year < 0,
      abs(revenue_last_year),
      revenue_last_year
    ),

    # Replace missing employees with industry/region median
    employees = ifelse(
      is.na(employees),
      ave(employees, nace_code, region_code, FUN = function(x) median(x, na.rm = TRUE)),
      employees
    ),

    # Foundation year plausibility: enforce realistic window
    foundation_year = ifelse(
      foundation_year < 1900 | foundation_year > year(Sys.Date()),
      NA,
      foundation_year
    )
  )

# ----------------------------------------------------------------------
# 4. Cleaning employment data
# ----------------------------------------------------------------------

message("Cleaning employment dataset...")

employment_clean <- employment_raw %>%
  clean_names() %>%
  mutate(
    month = as.Date(month),
    # Keep missing values; they will be imputed by interpolation below
    employees = employees
  ) %>%
  group_by(firm_id) %>%
  arrange(month) %>%
  mutate(
    # Linear interpolation of missing monthly employment per firm
    employees = ifelse(
      is.na(employees),
      round(approx(
        x    = as.numeric(month),
        y    = employees,
        xout = as.numeric(month),
        rule = 2    # use boundary values outside range
      )$y),
      employees
    )
  ) %>%
  ungroup()

# ----------------------------------------------------------------------
# 5. Cleaning turnover data
# ----------------------------------------------------------------------

message("Cleaning turnover dataset...")

turnover_clean <- turnover_raw %>%
  clean_names() %>%
  mutate(
    month = as.Date(month)
  ) %>%
  group_by(firm_id) %>%
  arrange(month) %>%
  mutate(
    # Replace missing turnover using linear interpolation per firm
    turnover = ifelse(
      is.na(turnover),
      approx(
        x    = as.numeric(month),
        y    = turnover,
        xout = as.numeric(month),
        rule = 2
      )$y,
      turnover
    )
  ) %>%
  ungroup()

# ----------------------------------------------------------------------
# 6. Consistency checks across datasets
# ----------------------------------------------------------------------

message("Running consistency checks...")

# Check: monthly employment never below 1
violations_emp <- employment_clean %>% filter(employees < 1)
if (nrow(violations_emp) > 0) warning("Employment < 1 detected after cleaning!")

# Check: turnover should correlate (weakly) positively with employees
corr <- employment_clean %>%
  left_join(turnover_clean, by = c("firm_id", "month")) %>%
  summarise(correlation = cor(employees, turnover, use = "pairwise.complete.obs")) %>%
  pull(correlation)

message("Correlation employees-turnover (expected positive, synthetic data): ",
        round(corr, 3))

# ----------------------------------------------------------------------
# 7. Write cleaned datasets
# ----------------------------------------------------------------------

write_csv(firms_clean,      "data/clean/firms_clean.csv")
write_csv(employment_clean, "data/clean/employment_clean.csv")
write_csv(turnover_clean,   "data/clean/turnover_clean.csv")

message("Cleaning completed successfully.")
