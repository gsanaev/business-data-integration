# =====================================================================
# 01_generate_synthetic_data.R
# Synthetic Business Data Generator
# ---------------------------------------------------------------------
# This script creates realistic synthetic datasets resembling firm-level
# and survey-based data used in structural and short-term business
# statistics (aligned with tasks in E32 & E33).
#
# Output:
#   data/raw/firms.csv
#   data/raw/employment.csv
#   data/raw/turnover.csv
#
# Notes:
#   - No real data is used; all values are simulated.
#   - Includes intentional inconsistencies for later cleaning.
# =====================================================================

# Load packages ---------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)

set.seed(2025)

# Ensure output directory exists ---------------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Create reference structures (regions, industries, legal forms)
# ----------------------------------------------------------------------

regions <- tibble(
  region_code = sprintf("R%02d", 1:10),
  region_name = paste("Region", 1:10)
)

industries <- tibble(
  nace_code = c("G47", "C10", "C29", "H49", "I55", "I56"),
  industry_name = c(
    "Retail Trade",
    "Food Manufacturing",
    "Automotive Manufacturing",
    "Land Transport",
    "Accommodation",
    "Food & Beverage Services"
  )
)

legal_forms <- tibble(
  legal_form = c("AG", "GmbH", "KG", "OHG", "Einzelunternehmen")
)

# ----------------------------------------------------------------------
# 2. Generate synthetic firm population
# ----------------------------------------------------------------------

n_firms <- 1500

firms <- tibble(
  firm_id         = sprintf("F%05d", 1:n_firms),
  region_code     = sample(regions$region_code, n_firms, replace = TRUE),
  nace_code       = sample(industries$nace_code, n_firms, replace = TRUE),
  legal_form      = sample(legal_forms$legal_form, n_firms, replace = TRUE),
  employees       = rpois(n_firms, lambda = 25) + 1,  # avoid zeros
  foundation_year = sample(1965:2022, n_firms, replace = TRUE),
  revenue_last_year = round(rlnorm(n_firms, meanlog = 12, sdlog = 1), 2)
)

# Inject inconsistencies to simulate real-world reporting errors --------

firms_inconsistent <- firms %>%
  mutate(
    employees = ifelse(runif(n()) < 0.02, NA, employees),  # missing values
    revenue_last_year = ifelse(
      runif(n()) < 0.02,
      -revenue_last_year,                                  # negative values
      revenue_last_year
    )
  )

# ----------------------------------------------------------------------
# 3. Create synthetic monthly employment dataset
# ----------------------------------------------------------------------

months <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = as.Date("2023-12-01"),
  by   = "month"
)

employment <- expand_grid(
  firm_id = firms$firm_id,
  month   = months
) %>%
  left_join(firms %>% select(firm_id, nace_code, region_code), by = "firm_id") %>%
  mutate(
    employees = pmax(1, rpois(n(), lambda = 20)),          # enforce >= 1
    employees = ifelse(runif(n()) < 0.01, NA, employees)   # random missing
  )

# ----------------------------------------------------------------------
# 4. Create synthetic monthly turnover dataset
# ----------------------------------------------------------------------

turnover <- expand_grid(
  firm_id = firms$firm_id,
  month   = months
) %>%
  left_join(firms %>% select(firm_id, nace_code, region_code), by = "firm_id") %>%
  mutate(
    turnover = round(rlnorm(n(), meanlog = 10.5, sdlog = 0.8), 2),
    turnover = ifelse(runif(n()) < 0.01, NA, turnover)         # missing values
  )

# ----------------------------------------------------------------------
# 5. Write datasets to disk
# ----------------------------------------------------------------------

write_csv(firms_inconsistent, "data/raw/firms.csv")
write_csv(employment,         "data/raw/employment.csv")
write_csv(turnover,           "data/raw/turnover.csv")

message("Synthetic datasets generated successfully.")
