# =====================================================================
# 01_generate_synthetic_data.R
# Synthetic Business Data Generator
# ---------------------------------------------------------------------
# This script creates realistic synthetic datasets resembling firm-level
# and survey-based data used in structural and short-term business
# statistics.
#
# Output:
#   data/raw/firms.csv
#   data/raw/employment.csv
#   data/raw/turnover.csv
#
# Notes:
#   - No real data is used; all values are simulated.
#   - Includes intentional inconsistencies for later cleaning.
#   - Industry-specific seasonality is added for employment.
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

  # structural business register fields
  employees         = rpois(n_firms, lambda = 25) + 1,  # avoid zero
  foundation_year   = sample(1965:2022, n_firms, replace = TRUE),
  revenue_last_year = round(rlnorm(n_firms, meanlog = 12, sdlog = 1), 2)
)

# Inject inconsistencies (missing / negative values) -------------------

firms_inconsistent <- firms %>%
  mutate(
    employees = ifelse(runif(n()) < 0.02, NA, employees),  # 2% missing
    revenue_last_year = ifelse(
      runif(n()) < 0.02,
      -revenue_last_year,                                  # 2% negative
      revenue_last_year
    )
  )

# ----------------------------------------------------------------------
# 3. Create synthetic monthly employment dataset (industry seasonality)
# ----------------------------------------------------------------------

months <- seq.Date(
  from = as.Date("2023-01-01"),
  to   = as.Date("2023-12-01"),
  by   = "month"
)

# Industry-specific seasonal multipliers (normalized so annual mean ≈ 1)
seasonality <- list(
  G47 = c(1.00, 0.98, 1.00, 1.02, 1.05, 1.08, 1.10, 1.12, 1.15, 1.20, 1.40, 1.60),
  C10 = c(1.00, 1.00, 1.01, 1.01, 1.02, 1.02, 1.03, 1.00, 1.00, 1.01, 1.01, 1.02),
  C29 = c(1.00, 1.00, 1.00, 1.02, 1.02, 1.03, 1.03, 0.80, 1.00, 1.02, 1.03, 1.05),
  H49 = c(1.00, 1.01, 1.01, 1.02, 1.03, 1.05, 1.07, 1.06, 1.05, 1.03, 1.02, 1.01),
  I55 = c(0.70, 0.75, 0.90, 1.10, 1.40, 1.60, 1.80, 1.70, 1.40, 1.10, 0.80, 0.70),
  I56 = c(0.85, 0.90, 0.95, 1.05, 1.15, 1.20, 1.30, 1.25, 1.10, 1.00, 0.95, 0.90)
)

employment <- expand_grid(
  firm_id = firms$firm_id,
  month   = months
) %>%
  left_join(firms %>% select(firm_id, nace_code, region_code), by = "firm_id") %>%
  mutate(
    month_num = as.integer(format(month, "%m")),

    # baseline Poisson employment
    base_emp = pmax(1, rpois(n(), lambda = 20)),

    # correctly indexed seasonal multiplier
    seasonal_factor = mapply(
      function(code, m) seasonality[[code]][m],
      nace_code, month_num
    ),

    employees = round(base_emp * seasonal_factor),

    # random missingness (1%)
    employees = ifelse(runif(n()) < 0.01, NA, employees)
  ) %>%
  select(-base_emp)

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
    turnover = ifelse(runif(n()) < 0.01, NA, turnover)  # 1% missing
  )

# ----------------------------------------------------------------------
# 5. Write datasets to disk
# ----------------------------------------------------------------------

write_csv(firms_inconsistent, "data/raw/firms.csv")
write_csv(employment,         "data/raw/employment.csv")
write_csv(turnover,           "data/raw/turnover.csv")

message("Synthetic datasets generated successfully.")
