# =====================================================================
# 03_integration_pipeline.R
# Integration of Firm-Level Data & Construction of Economic Indicators
# =====================================================================

library(dplyr)
library(readr)
library(lubridate)

# Ensure output directory exists ---------------------------------------
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Load cleaned datasets
# ----------------------------------------------------------------------

firms      <- read_csv("data/clean/firms_clean.csv", show_col_types = FALSE)
employment <- read_csv("data/clean/employment_clean.csv", show_col_types = FALSE)
turnover   <- read_csv("data/clean/turnover_clean.csv", show_col_types = FALSE)

# Rename columns to avoid duplication during joins ---------------------

employment <- employment %>% rename(employees_monthly = employees)
turnover   <- turnover   %>% rename(turnover_monthly  = turnover)
firms      <- firms      %>% rename(employees_firm    = employees)

# ----------------------------------------------------------------------
# 2. Build unified monthly panel
# ----------------------------------------------------------------------

message("Integrating datasets...")

panel <- employment %>%
  left_join(turnover, by = c("firm_id", "month")) %>%
  left_join(firms,    by = "firm_id") %>%
  mutate(
    month            = as.Date(month),
    employees_monthly = as.numeric(employees_monthly),
    turnover_monthly  = as.numeric(turnover_monthly)
  ) %>%
  arrange(firm_id, month)

# ----------------------------------------------------------------------
# 3. Derive key indicators
# ----------------------------------------------------------------------

message("Constructing indicators...")

panel <- panel %>%
  group_by(firm_id) %>%
  arrange(month) %>%
  mutate(
    # Year-over-year turnover growth (12-month lag)
    turnover_yoy = (turnover_monthly - lag(turnover_monthly, 12)) /
                    lag(turnover_monthly, 12),

    # Monthly employment growth
    emp_growth = (employees_monthly - lag(employees_monthly)) /
                  lag(employees_monthly),

    # Labor productivity
    productivity = turnover_monthly / employees_monthly,

    # Simple seasonality indicator (relative to firm mean)
    month_num       = month(month),
    seasonal_index  = turnover_monthly /
                      mean(turnover_monthly, na.rm = TRUE)
  ) %>%
  ungroup()

# ----------------------------------------------------------------------
# 4. Consistency checks
# ----------------------------------------------------------------------

message("Running consistency checks...")

# Productivity plausibility
implausible_prod <- panel %>% filter(productivity < 0 | productivity > 1e7)
if (nrow(implausible_prod) > 0) {
  warning("Implausible productivity values detected: ", nrow(implausible_prod))
}

# YoY turnover growth distribution check
yoy_summary <- summary(panel$turnover_yoy)
message("YoY turnover growth summary:")
print(yoy_summary)

# Basic check for non-positive employees
if (any(panel$employees_monthly <= 0, na.rm = TRUE)) {
  warning("Non-positive monthly employment values detected.")
}

# ----------------------------------------------------------------------
# 5. Create final analysis-ready dataset
# ----------------------------------------------------------------------

write_csv(panel, "data/processed/panel_data.csv")

message("Integration & indicator computation completed successfully.")
