# =====================================================================
# indicator_workflow.R
# Enterprise-year and indicator workflow assembly
# =====================================================================

build_indicator_results <- function(
  panel,
  required_months
) {
  validate_monthly_panel_structure(
    panel,
    required_months
  )

  enterprise_year <-
    build_enterprise_year(
      panel,
      required_months
    )

  validate_enterprise_year(
    enterprise_year
  )

  indicators_sector_region <-
    aggregate_indicators(
      enterprise_year,
      c(
        "year",
        "nace_code",
        "region_code"
      )
    )

  indicators_sector <-
    aggregate_indicators(
      enterprise_year,
      c(
        "year",
        "nace_code"
      )
    )

  indicators_region <-
    aggregate_indicators(
      enterprise_year,
      c(
        "year",
        "region_code"
      )
    )

  indicator_tables <-
    list(
      sector_region =
        indicators_sector_region,
      sector =
        indicators_sector,
      region =
        indicators_region
    )

  validate_indicator_tables(
    indicator_tables
  )

  list(
    enterprise_year =
      enterprise_year,
    sector_region =
      indicators_sector_region,
    sector =
      indicators_sector,
    region =
      indicators_region
  )
}
