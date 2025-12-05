# =====================================================================
# 04_compute_indicators.R
# Aggregate Economic Indicators by Sector and Region
# =====================================================================

library(dplyr)
library(readr)
library(lubridate)
library(janitor)

# Ensure output directory exists ---------------------------------------
dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Load integrated panel
# ----------------------------------------------------------------------

panel <- read_csv("data/processed/panel_data.csv", show_col_types = FALSE)

panel <- panel %>%
  clean_names() %>%
  mutate(
    month = as.Date(month),
    year  = year(month)
  )

message("Integrated panel loaded: ", nrow(panel), " rows.")

# ----------------------------------------------------------------------
# 2. Core indicator definitions
# ----------------------------------------------------------------------
# We compute annual indicators per sector & region:
# - total_turnover
# - average_turnover_per_firm
# - total_employees
# - average_employees_per_firm
# - mean_productivity
# - n_obs (monthly observations)
# - n_firms (distinct firms)

# Sector x Region x Year ------------------------------------------------

indicators_sector_region <- panel %>%
  group_by(year, nace_code, region_code) %>%
  summarise(
    n_obs                  = n(),
    n_firms                = n_distinct(firm_id),
    total_turnover         = sum(turnover_monthly, na.rm = TRUE),
    avg_turnover_per_firm  = total_turnover / n_firms,
    total_employees        = sum(employees_monthly, na.rm = TRUE),
    avg_employees_per_firm = total_employees / n_firms,
    mean_productivity      = mean(productivity, na.rm = TRUE),
    .groups = "drop"
  )

# Sector x Year ---------------------------------------------------------

indicators_sector <- panel %>%
  group_by(year, nace_code) %>%
  summarise(
    n_obs                  = n(),
    n_firms                = n_distinct(firm_id),
    total_turnover         = sum(turnover_monthly, na.rm = TRUE),
    avg_turnover_per_firm  = total_turnover / n_firms,
    total_employees        = sum(employees_monthly, na.rm = TRUE),
    avg_employees_per_firm = total_employees / n_firms,
    mean_productivity      = mean(productivity, na.rm = TRUE),
    .groups = "drop"
  )

# Region x Year ---------------------------------------------------------

indicators_region <- panel %>%
  group_by(year, region_code) %>%
  summarise(
    n_obs                  = n(),
    n_firms                = n_distinct(firm_id),
    total_turnover         = sum(turnover_monthly, na.rm = TRUE),
    avg_turnover_per_firm  = total_turnover / n_firms,
    total_employees        = sum(employees_monthly, na.rm = TRUE),
    avg_employees_per_firm = total_employees / n_firms,
    mean_productivity      = mean(productivity, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------------------------------------------------
# 3. Save indicator tables
# ----------------------------------------------------------------------

write_csv(indicators_sector_region, "output/tables/indicators_sector_region.csv")
write_csv(indicators_sector,       "output/tables/indicators_sector.csv")
write_csv(indicators_region,       "output/tables/indicators_region.csv")

message("Indicator tables written to 'output/tables/'.")
