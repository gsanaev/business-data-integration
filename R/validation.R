# =====================================================================
# validation.R
# Source validation and plausibility functions
# ---------------------------------------------------------------------
# Functionalized from the v2 validation workflow during v3 refactoring.
# Statistical behavior is intentionally unchanged at this stage.
# =====================================================================

validate_source_structures <- function(
  firms_raw,
  employment_raw,
  turnover_raw,
  accounting_raw
) {
  duplicate_firms <- firms_raw %>%
    count(register_id) %>%
    filter(n > 1)

  if (nrow(duplicate_firms) > 0) {
    stop(
      "Duplicate register IDs detected in firms.csv: ",
      nrow(duplicate_firms)
    )
  }

  duplicate_employment_keys <- employment_raw %>%
    count(
      employment_source_id,
      month
    ) %>%
    filter(n > 1)

  if (nrow(duplicate_employment_keys) > 0) {
    stop(
      "Duplicate employment source entity-month keys detected: ",
      nrow(duplicate_employment_keys)
    )
  }

  duplicate_turnover_keys <- turnover_raw %>%
    count(
      turnover_source_id,
      month
    ) %>%
    filter(n > 1)

  if (nrow(duplicate_turnover_keys) > 0) {
    stop(
      "Duplicate turnover source entity-month keys detected: ",
      nrow(duplicate_turnover_keys)
    )
  }

  duplicate_accounting_keys <- accounting_raw %>%
    count(
      accounting_source_id,
      reference_year
    ) %>%
    filter(n > 1)

  if (nrow(duplicate_accounting_keys) > 0) {
    stop(
      "Duplicate accounting source entity-year keys detected: ",
      nrow(duplicate_accounting_keys)
    )
  }

  if (
    any(
      !accounting_raw$reference_year %in%
        2023:2025
    )
  ) {
    stop(
      "Unexpected reference year detected in accounting source."
    )
  }

  invisible(TRUE)
}


validate_register_source <- function(
  firms_raw,
  foundation_year_min
) {
  max_register_year <- max(
    firms_raw$register_reference_year,
    na.rm = TRUE
  )

  firms_raw %>%
    clean_names() %>%
    mutate(
      employees_register_raw = as.numeric(employees),
      revenue_last_year_raw = as.numeric(revenue_last_year),
      foundation_year_raw = as.integer(foundation_year)
    ) %>%
    mutate(
      employees_register_status = case_when(
        is.na(employees_register_raw) ~ "review_required",

        employees_register_raw <= 0 ~ "rejected",

        TRUE ~ "accepted"
      ),

      employees_register_rule_id = case_when(
        is.na(employees_register_raw) ~ "REG_EMP_MISSING",

        employees_register_raw <= 0 ~ "REG_EMP_NONPOSITIVE",

        TRUE ~ NA_character_
      ),

      employees_register_imputed =
        FALSE,

      employees = case_when(
        employees_register_status == "accepted" ~
          employees_register_raw,

        TRUE ~ NA_real_
      ),

      revenue_status = case_when(
        is.na(revenue_last_year_raw) ~ "review_required",

        revenue_last_year_raw <= 0 ~ "rejected",

        TRUE ~ "accepted"
      ),

      revenue_rule_id = case_when(
        is.na(revenue_last_year_raw) ~ "REG_REV_MISSING",

        revenue_last_year_raw <= 0 ~ "REG_REV_NONPOSITIVE",

        TRUE ~ NA_character_
      ),

      revenue_imputed = FALSE,

      revenue_last_year = case_when(
        revenue_status == "accepted" ~
          revenue_last_year_raw,

        TRUE ~ NA_real_
      ),

      foundation_year_status = case_when(
        is.na(foundation_year_raw) ~ "review_required",

        foundation_year_raw < foundation_year_min |
          foundation_year_raw > max_register_year ~ "rejected",

        TRUE ~ "accepted"
      ),

      foundation_year_rule_id = case_when(
        is.na(foundation_year_raw) ~
          "FOUNDATION_YEAR_MISSING",

        foundation_year_raw < foundation_year_min |
          foundation_year_raw > max_register_year ~
          "FOUNDATION_YEAR_INVALID",

        TRUE ~ NA_character_
      ),

      foundation_year = case_when(
        foundation_year_status == "accepted" ~
          foundation_year_raw,

        TRUE ~ NA_integer_
      )
    )
}


validate_employment_source <- function(
  employment_raw,
  employment_spike_multiplier
) {
  employment_raw %>%
    clean_names() %>%
    mutate(
      month = as.Date(month),
      year = year(month),
      employees_monthly_raw = as.numeric(employees)
    ) %>%
    group_by(
      employment_source_id,
      year
    ) %>%
    mutate(
      firm_year_emp_median = safe_median(
        ifelse(
          !is.na(employees_monthly_raw) &
            employees_monthly_raw > 0,
          employees_monthly_raw,
          NA_real_
        )
      ),

      employment_spike =
        !is.na(employees_monthly_raw) &
        employees_monthly_raw > 0 &
        !is.na(firm_year_emp_median) &
        employees_monthly_raw >
          employment_spike_multiplier * firm_year_emp_median
    ) %>%
    ungroup() %>%
    group_by(employment_source_id) %>%
    arrange(
      month,
      .by_group = TRUE
    ) %>%
    mutate(
      employment_donor = case_when(
        !is.na(employees_monthly_raw) &
          employees_monthly_raw > 0 &
          !employment_spike ~ employees_monthly_raw,

        TRUE ~ NA_real_
      ),

      employment_imputed_candidate = interpolate_series(
        month,
        employment_donor
      ),

      employment_status = case_when(
        is.na(employees_monthly_raw) &
          !is.na(employment_imputed_candidate) ~ "imputed",

        is.na(employees_monthly_raw) ~ "review_required",

        employees_monthly_raw <= 0 ~ "rejected",

        employment_spike ~ "review_required",

        TRUE ~ "accepted"
      ),

      employment_rule_id = case_when(
        is.na(employees_monthly_raw) ~ "EMP_MISSING",

        employees_monthly_raw <= 0 ~ "EMP_NONPOSITIVE",

        employment_spike ~ "EMP_SPIKE",

        TRUE ~ NA_character_
      ),

      employment_imputed =
        employment_status == "imputed",

      employees = case_when(
        employment_status == "accepted" ~
          employees_monthly_raw,

        employment_status == "imputed" ~
          round(employment_imputed_candidate),

        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(
      -firm_year_emp_median,
      -employment_spike,
      -employment_donor,
      -employment_imputed_candidate
    )
}


validate_turnover_source <- function(
  turnover_raw
) {
  turnover_raw %>%
    clean_names() %>%
    mutate(
      month = as.Date(month),
      turnover_monthly_raw = as.numeric(turnover)
    ) %>%
    group_by(turnover_source_id) %>%
    arrange(
      month,
      .by_group = TRUE
    ) %>%
    mutate(
      turnover_donor = case_when(
        !is.na(turnover_monthly_raw) &
          turnover_monthly_raw > 0 ~ turnover_monthly_raw,

        TRUE ~ NA_real_
      ),

      turnover_imputed_candidate = interpolate_series(
        month,
        turnover_donor
      ),

      turnover_status = case_when(
        is.na(turnover_monthly_raw) &
          !is.na(turnover_imputed_candidate) ~ "imputed",

        is.na(turnover_monthly_raw) ~ "review_required",

        turnover_monthly_raw <= 0 ~ "rejected",

        TRUE ~ "accepted"
      ),

      turnover_rule_id = case_when(
        is.na(turnover_monthly_raw) ~ "TURN_MISSING",

        turnover_monthly_raw <= 0 ~ "TURN_NONPOSITIVE",

        TRUE ~ NA_character_
      ),

      turnover_imputed =
        turnover_status == "imputed",

      turnover = case_when(
        turnover_status == "accepted" ~
          turnover_monthly_raw,

        turnover_status == "imputed" ~
          turnover_imputed_candidate,

        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(
      -turnover_donor,
      -turnover_imputed_candidate
    )
}


validate_accounting_source <- function(
  accounting_raw
) {
  accounting_raw %>%
    clean_names() %>%
    mutate(
      reference_year =
        as.integer(reference_year),

      operating_revenue_raw =
        as.numeric(operating_revenue),

      purchases_goods_services_raw =
        as.numeric(purchases_goods_services),

      personnel_expense_raw =
        as.numeric(personnel_expense),

      operating_revenue_status = case_when(
        is.na(operating_revenue_raw) ~
          "review_required",

        operating_revenue_raw <= 0 ~
          "rejected",

        TRUE ~
          "accepted"
      ),

      operating_revenue_rule_id = case_when(
        is.na(operating_revenue_raw) ~
          "ACC_REV_MISSING",

        operating_revenue_raw <= 0 ~
          "ACC_REV_NONPOSITIVE",

        TRUE ~
          NA_character_
      ),

      operating_revenue = case_when(
        operating_revenue_status ==
          "accepted" ~
          operating_revenue_raw,

        TRUE ~
          NA_real_
      ),

      purchases_status = case_when(
        is.na(purchases_goods_services_raw) ~
          "review_required",

        purchases_goods_services_raw <= 0 ~
          "rejected",

        TRUE ~
          "accepted"
      ),

      purchases_rule_id = case_when(
        is.na(purchases_goods_services_raw) ~
          "ACC_PURCHASES_MISSING",

        purchases_goods_services_raw <= 0 ~
          "ACC_PURCHASES_NONPOSITIVE",

        TRUE ~
          NA_character_
      ),

      purchases_goods_services = case_when(
        purchases_status ==
          "accepted" ~
          purchases_goods_services_raw,

        TRUE ~
          NA_real_
      ),

      personnel_expense_status = case_when(
        is.na(personnel_expense_raw) ~
          "review_required",

        personnel_expense_raw <= 0 ~
          "rejected",

        TRUE ~
          "accepted"
      ),

      personnel_expense_rule_id = case_when(
        is.na(personnel_expense_raw) ~
          "ACC_PERSONNEL_MISSING",

        personnel_expense_raw <= 0 ~
          "ACC_PERSONNEL_NONPOSITIVE",

        TRUE ~
          NA_character_
      ),

      personnel_expense = case_when(
        personnel_expense_status ==
          "accepted" ~
          personnel_expense_raw,

        TRUE ~
          NA_real_
      )
    )
}


assert_validated_sources <- function(
  firms_clean,
  employment_clean,
  turnover_clean,
  accounting_clean
) {
  if (
    any(
      firms_clean$employees <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive analytical register employment remains after QA."
    )
  }

  if (
    any(
      firms_clean$revenue_last_year <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive analytical register revenue remains after QA."
    )
  }

  if (
    any(
      employment_clean$employees <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive analytical monthly employment remains after QA."
    )
  }

  if (
    any(
      turnover_clean$turnover <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive analytical monthly turnover remains after QA."
    )
  }

  if (
    any(
      accounting_clean$operating_revenue <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive analytical accounting operating revenue remains after QA."
    )
  }

  if (
    any(
      accounting_clean$purchases_goods_services <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive analytical accounting purchases remain after QA."
    )
  }

  if (
    any(
      accounting_clean$personnel_expense <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive analytical accounting personnel expense remains after QA."
    )
  }

  invisible(TRUE)
}
