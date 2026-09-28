# =====================================================================
# indicators.R
# Indicator aggregation functions
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Function behavior is intentionally unchanged at this stage.
# =====================================================================

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

validate_indicator_tables <- function(
  indicator_tables
) {
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

  invisible(TRUE)
}
