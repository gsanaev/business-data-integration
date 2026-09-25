# =====================================================================
# 06_compute_indicators.R
# Annual Enterprise Indicators by Sector and Region
# ---------------------------------------------------------------------
# Monthly enterprise observations are first converted to explicit
# enterprise-year measures.
#
# Annual definitions:
#   annual_turnover
#     Sum of twelve usable monthly turnover observations.
#
#   annual_average_employment
#     Mean of twelve usable monthly employment observations.
#
#   turnover_per_employee
#     Annual turnover divided by annual average employment.
#
# Enterprise-years with incomplete monthly analytical coverage are not
# silently treated as complete annual observations.
#
# Output:
#   data/processed/enterprise_year_indicators.csv
#   output/tables/indicators_sector_region.csv
#   output/tables/indicators_sector.csv
#   output/tables/indicators_region.csv
# =====================================================================

library(dplyr)
library(readr)
library(lubridate)

source("R/enterprise_year.R")

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  "output/tables",
  showWarnings = FALSE,
  recursive = TRUE
)

required_months <- 12L

# ----------------------------------------------------------------------
# 1. Load integrated monthly panel
# ----------------------------------------------------------------------

panel <- read_csv(
  "data/processed/panel_data.csv",
  show_col_types = FALSE
) %>%
  mutate(
    month = as.Date(month),
    year = year(month)
  )

message(
  "Integrated monthly panel loaded: ",
  nrow(panel),
  " rows."
)

# ----------------------------------------------------------------------
# 2. Validate monthly panel structure
# ----------------------------------------------------------------------

validate_monthly_panel_structure(
  panel,
  required_months
)

# ----------------------------------------------------------------------
# 3. Build enterprise-year analytical measures
# ----------------------------------------------------------------------

enterprise_year <-
  build_enterprise_year(
    panel,
    required_months
  )

# ----------------------------------------------------------------------
# 4. Validate enterprise-year measures
# ----------------------------------------------------------------------

validate_enterprise_year(
  enterprise_year
)

message(
  "Enterprise-year observations: ",
  nrow(enterprise_year)
)

message("Annual coverage by year:")

print(
  summarise_enterprise_year_coverage(
    enterprise_year
  )
)

# ----------------------------------------------------------------------
# 5. Load aggregation helper
# ----------------------------------------------------------------------

source("R/indicators.R")

# ----------------------------------------------------------------------
# 6. Sector x region x year indicators
# ----------------------------------------------------------------------

indicators_sector_region <-
  aggregate_indicators(
    enterprise_year,
    c(
      "year",
      "nace_code",
      "region_code"
    )
  )

# ----------------------------------------------------------------------
# 7. Sector x year indicators
# ----------------------------------------------------------------------

indicators_sector <-
  aggregate_indicators(
    enterprise_year,
    c(
      "year",
      "nace_code"
    )
  )

# ----------------------------------------------------------------------
# 8. Region x year indicators
# ----------------------------------------------------------------------

indicators_region <-
  aggregate_indicators(
    enterprise_year,
    c(
      "year",
      "region_code"
    )
  )

# ----------------------------------------------------------------------
# 9. Final indicator checks
# ----------------------------------------------------------------------

indicator_tables <- list(
  sector_region =
    indicators_sector_region,

  sector =
    indicators_sector,

  region =
    indicators_region
)

for (
  table_name in
    names(indicator_tables)
) {
  x <- indicator_tables[[table_name]]

  if (
    any(
      x$n_complete_turnover >
        x$n_enterprises
    ) ||
      any(
        x$n_complete_employment >
          x$n_enterprises
      ) ||
      any(
        x$n_complete_both >
          x$n_enterprises
      )
  ) {
    stop(
      "Indicator coverage counts exceed enterprise counts in ",
      table_name,
      "."
    )
  }

  if (
    any(
      x$total_turnover < 0,
      na.rm = TRUE
    ) ||
      any(
        x$total_average_employment < 0,
        na.rm = TRUE
      ) ||
      any(
        x$turnover_complete_both < 0,
        na.rm = TRUE
      ) ||
      any(
        x$employment_complete_both < 0,
        na.rm = TRUE
      )
  ) {
    stop(
      "Negative aggregate values detected in ",
      table_name,
      "."
    )
  }

  if (
    any(
      x$turnover_complete_both >
        x$total_turnover,
      na.rm = TRUE
    ) ||
      any(
        x$employment_complete_both >
          x$total_average_employment,
        na.rm = TRUE
      )
  ) {
    stop(
      "Common-population aggregate exceeds its source aggregate in ",
      table_name,
      "."
    )
  }
}

# ----------------------------------------------------------------------
# 10. Write annual analytical and indicator datasets
# ----------------------------------------------------------------------

write_csv(
  enterprise_year,
  "data/processed/enterprise_year_indicators.csv"
)

write_csv(
  indicators_sector_region,
  "output/tables/indicators_sector_region.csv"
)

write_csv(
  indicators_sector,
  "output/tables/indicators_sector.csv"
)

write_csv(
  indicators_region,
  "output/tables/indicators_region.csv"
)

message(
  "Annual enterprise and aggregate indicators completed successfully."
)
