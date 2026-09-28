# =====================================================================
# enterprise_year.R
# Enterprise-year analytical layer
# ---------------------------------------------------------------------
# Functionalized from the v2 indicator workflow during v3 refactoring.
# Statistical behavior is intentionally unchanged at this stage.
# =====================================================================

validate_monthly_panel_structure <- function(
  panel,
  required_months
) {
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

  invisible(TRUE)
}


build_enterprise_year <- function(
  panel,
  required_months
) {
  panel %>%
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
}


validate_enterprise_year <- function(
  enterprise_year
) {
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

  invisible(TRUE)
}
