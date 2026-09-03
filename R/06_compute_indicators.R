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

duplicate_keys <- panel %>%
  count(
    canonical_firm_id,
    month
  ) %>%
  filter(
    n > 1L
  )

if (
  nrow(duplicate_keys) > 0L
) {
  stop(
    "Duplicate canonical enterprise-month keys detected: ",
    nrow(duplicate_keys)
  )
}

monthly_structure <- panel %>%
  count(
    canonical_firm_id,
    year,
    name = "monthly_observations"
  )

if (
  any(
    monthly_structure$monthly_observations !=
      required_months
  )
) {
  stop(
    "Expected exactly 12 structural monthly observations ",
    "per enterprise-year."
  )
}

# ----------------------------------------------------------------------
# 3. Build enterprise-year analytical measures
# ----------------------------------------------------------------------

enterprise_year <- panel %>%
  group_by(
    canonical_firm_id,
    year
  ) %>%
  summarise(
    nace_code =
      first(nace_code),

    region_code =
      first(region_code),

    monthly_observations =
      n(),

    usable_turnover_months =
      sum(
        !is.na(turnover_monthly)
      ),

    usable_employment_months =
      sum(
        !is.na(employees_monthly)
      ),

    turnover_imputed_months =
      sum(
        turnover_status == "imputed",
        na.rm = TRUE
      ),

    employment_imputed_months =
      sum(
        employment_status == "imputed",
        na.rm = TRUE
      ),

    annual_turnover = if (
      sum(!is.na(turnover_monthly)) ==
        required_months
    ) {
      sum(
        turnover_monthly,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },

    annual_average_employment = if (
      sum(!is.na(employees_monthly)) ==
        required_months
    ) {
      mean(
        employees_monthly,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },

    .groups = "drop"
  ) %>%
  mutate(
    complete_turnover =
      usable_turnover_months ==
        required_months,

    complete_employment =
      usable_employment_months ==
        required_months,

    complete_annual_measures =
      complete_turnover &
        complete_employment,

    turnover_per_employee =
      case_when(
        !is.na(annual_turnover) &
          !is.na(
            annual_average_employment
          ) &
          annual_average_employment > 0 ~
          annual_turnover /
            annual_average_employment,

        TRUE ~
          NA_real_
      )
  ) %>%
  arrange(
    canonical_firm_id,
    year
  )

# ----------------------------------------------------------------------
# 4. Validate enterprise-year measures
# ----------------------------------------------------------------------

if (
  anyDuplicated(
    enterprise_year[
      c(
        "canonical_firm_id",
        "year"
      )
    ]
  )
) {
  stop(
    "Duplicate canonical enterprise-year keys detected."
  )
}

if (
  any(
    enterprise_year$annual_turnover <= 0,
    na.rm = TRUE
  )
) {
  stop(
    "Non-positive annual turnover detected."
  )
}

if (
  any(
    enterprise_year$annual_average_employment <= 0,
    na.rm = TRUE
  )
) {
  stop(
    "Non-positive annual average employment detected."
  )
}

if (
  any(
    enterprise_year$turnover_per_employee <= 0,
    na.rm = TRUE
  )
) {
  stop(
    "Non-positive turnover-per-employee values detected."
  )
}

message(
  "Enterprise-year observations: ",
  nrow(enterprise_year)
)

message("Annual coverage by year:")

print(
  enterprise_year %>%
    group_by(year) %>%
    summarise(
      enterprises = n(),

      complete_turnover =
        sum(complete_turnover),

      complete_employment =
        sum(complete_employment),

      complete_both =
        sum(complete_annual_measures),

      .groups = "drop"
    )
)

# ----------------------------------------------------------------------
# 5. Aggregation helper
# ----------------------------------------------------------------------

aggregate_indicators <- function(
  data,
  grouping_variables
) {
  data %>%
    group_by(
      across(
        all_of(
          grouping_variables
        )
      )
    ) %>%
    summarise(
      n_enterprises =
        n_distinct(
          canonical_firm_id
        ),

      n_complete_turnover =
        sum(
          !is.na(
            annual_turnover
          )
        ),

      n_complete_employment =
        sum(
          !is.na(
            annual_average_employment
          )
        ),

      n_complete_both =
        sum(
          !is.na(
            turnover_per_employee
          )
        ),

      total_turnover =
        sum(
          annual_turnover,
          na.rm = TRUE
        ),

      average_turnover_per_enterprise =
        mean(
          annual_turnover,
          na.rm = TRUE
        ),

      total_average_employment =
        sum(
          annual_average_employment,
          na.rm = TRUE
        ),

      average_employment_per_enterprise =
        mean(
          annual_average_employment,
          na.rm = TRUE
        ),

      turnover_complete_both =
        sum(
          annual_turnover[
            complete_annual_measures
          ],
          na.rm = TRUE
        ),

      employment_complete_both =
        sum(
          annual_average_employment[
            complete_annual_measures
          ],
          na.rm = TRUE
        ),

      turnover_per_employee =
        case_when(
          employment_complete_both > 0 ~
            turnover_complete_both /
              employment_complete_both,

          TRUE ~
            NA_real_
        ),

      .groups = "drop"
    )
}

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
