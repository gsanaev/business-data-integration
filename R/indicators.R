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
