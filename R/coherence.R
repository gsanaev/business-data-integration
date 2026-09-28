# =====================================================================
# coherence.R
# Coherence helper functions
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Function behavior is intentionally unchanged at this stage.
# =====================================================================

get_coherence_threshold <- function(
  rule_id,
  coherence_rules_config
) {
  values <-
    coherence_rules_config$expected_range_threshold[
      coherence_rules_config$rule_id ==
        rule_id
    ]

  if (length(values) != 1L) {
    stop(
      "Expected exactly one configured threshold for coherence rule ",
      rule_id,
      ".",
      call. = FALSE
    )
  }

  values[[1]]
}

get_contract <- function(
  source_name,
  variable_name,
  contracts
) {
  result <- contracts %>%
    filter(
      source == source_name,
      variable == variable_name
    )

  if (nrow(result) != 1L) {
    stop(
      "Expected exactly one source contract for ",
      source_name,
      " / ",
      variable_name,
      "; found ",
      nrow(result),
      "."
    )
  }

  result
}

assert_comparable_group <- function(
  left_source,
  left_variable,
  right_source,
  right_variable,
  expected_group,
  contracts
) {
  left_contract <- get_contract(
    left_source,
    left_variable,
    contracts
  )

  right_contract <- get_contract(
    right_source,
    right_variable,
    contracts
  )

  if (
    left_contract$comparability_group !=
      expected_group ||
      right_contract$comparability_group !=
        expected_group
  ) {
    stop(
      "Source-contract comparability mismatch for ",
      left_source,
      " / ",
      left_variable,
      " and ",
      right_source,
      " / ",
      right_variable,
      "."
    )
  }

  if (
    left_contract$statistical_unit !=
      "enterprise" ||
      right_contract$statistical_unit !=
        "enterprise"
  ) {
    stop(
      "Coherence rules require enterprise-level source contracts."
    )
  }

  invisible(TRUE)
}

coherence_relative_difference <- function(
  left_value,
  right_value
) {
  denominator <- pmax(
    abs(left_value),
    abs(right_value)
  )

  case_when(
    is.na(left_value) |
      is.na(right_value) ~
      NA_real_,

    denominator == 0 ~
      0,

    TRUE ~
      abs(
        left_value -
          right_value
      ) /
      denominator
  )
}

finalize_events <- function(
  events,
  threshold,
  materiality_medium_percentile,
  materiality_high_percentile
) {
  events %>%
    mutate(
      expected_range_threshold =
        threshold,

      absolute_difference = case_when(
        applicability_status ==
          "applicable" ~
          abs(
            left_value -
              right_value
          ),

        TRUE ~
          NA_real_
      ),

      relative_difference =
        coherence_relative_difference(
          left_value,
          right_value
        ),

      relative_difference = case_when(
        applicability_status ==
          "applicable" ~
          relative_difference,

        TRUE ~
          NA_real_
      ),

      coherence_status = case_when(
        applicability_status !=
          "applicable" ~
          "not_assessed",

        relative_difference <=
          expected_range_threshold ~
          "within_expected_range",

        TRUE ~
          "large_difference"
      )
    ) %>%
    group_by(rule_id) %>%
    mutate(
      materiality_percentile = case_when(
        applicability_status ==
          "applicable" ~
          percent_rank(
            absolute_difference
          ),

        TRUE ~
          NA_real_
      ),

      materiality = case_when(
        applicability_status !=
          "applicable" ~
          "not_assessed",

        materiality_percentile >=
          materiality_high_percentile ~
          "high",

        materiality_percentile >=
          materiality_medium_percentile ~
          "medium",

        TRUE ~
          "low"
      ),

      review_priority = case_when(
        coherence_status !=
          "large_difference" ~
          "none",

        materiality ==
          "high" ~
          "high",

        materiality ==
          "medium" ~
          "medium",

        TRUE ~
          "low"
      )
    ) %>%
    ungroup()
}

build_coherence_annual_panel <- function(
  panel,
  required_months
) {
  panel %>%
    group_by(
      canonical_firm_id,
      year
    ) %>%
    summarise(
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

      annual_mean_employment = if (
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

      register_id =
        first(register_id),

      register_employment =
        first(employees_firm),

      register_employment_status =
        first(employees_register_status),

      register_reference_year =
        first(register_reference_year),

      register_revenue =
        first(revenue_last_year),

      register_revenue_status =
        first(revenue_status),

      revenue_reference_year =
        first(revenue_reference_year),

      .groups = "drop"
    )
}


validate_coherence_annual_panel <- function(
  annual_panel
) {
  if (
    anyDuplicated(
      annual_panel[
        c(
          "canonical_firm_id",
          "year"
        )
      ]
    )
  ) {
    stop(
      "Duplicate enterprise-year keys detected in annual panel."
    )
  }

  invisible(TRUE)
}

build_revenue_accounting_events <- function(
  annual_panel,
  accounting,
  required_months,
  threshold,
  materiality_medium_percentile,
  materiality_high_percentile
) {
  annual_panel %>%
    select(
      canonical_firm_id,
      year,
      usable_turnover_months,
      turnover_imputed_months,
      annual_turnover
    ) %>%
    left_join(
      accounting %>%
        select(
          canonical_firm_id,
          reference_year,
          operating_revenue,
          operating_revenue_status
        ),
      by = c(
        "canonical_firm_id",
        "year" =
          "reference_year"
      )
    ) %>%
    transmute(
      canonical_firm_id,
      reference_year = year,

      rule_id =
        "COH_REV_ACCOUNTING",

      comparability_group =
        "annual_revenue_related",

      left_source =
        "turnover",

      left_variable =
        "annual_turnover",

      right_source =
        "accounting",

      right_variable =
        "operating_revenue",

      left_value =
        annual_turnover,

      right_value =
        operating_revenue,

      monthly_coverage =
        usable_turnover_months,

      required_monthly_coverage =
        required_months,

      imputed_months =
        turnover_imputed_months,

      right_quality_status =
        operating_revenue_status,

      applicability_status = case_when(
        usable_turnover_months <
          required_months ~
          "insufficient_monthly_coverage",

        is.na(operating_revenue) ~
          "right_value_unavailable",

        TRUE ~
          "applicable"
      ),

      comparison_note =
        paste(
          "Annual statistical turnover and accounting operating",
          "revenue are related but not assumed to be identical."
        )
    ) %>%
    finalize_events(
      threshold =
        threshold,
      materiality_medium_percentile =
        materiality_medium_percentile,
      materiality_high_percentile =
        materiality_high_percentile
    )
}


build_revenue_register_events <- function(
  annual_panel,
  required_months,
  threshold,
  materiality_medium_percentile,
  materiality_high_percentile
) {
  annual_panel %>%
    filter(
      year ==
        revenue_reference_year
    ) %>%
    transmute(
      canonical_firm_id,
      reference_year = year,

      rule_id =
        "COH_REV_REGISTER",

      comparability_group =
        "annual_revenue_related",

      left_source =
        "turnover",

      left_variable =
        "annual_turnover",

      right_source =
        "register",

      right_variable =
        "revenue_last_year",

      left_value =
        annual_turnover,

      right_value =
        register_revenue,

      monthly_coverage =
        usable_turnover_months,

      required_monthly_coverage =
        required_months,

      imputed_months =
        turnover_imputed_months,

      right_quality_status =
        register_revenue_status,

      applicability_status = case_when(
        usable_turnover_months <
          required_months ~
          "insufficient_monthly_coverage",

        is.na(register_revenue) ~
          "right_value_unavailable",

        TRUE ~
          "applicable"
      ),

      comparison_note =
        paste(
          "Annual statistical turnover is compared with",
          "register-style prior-year revenue for the same",
          "reference year."
        )
    ) %>%
    finalize_events(
      threshold =
        threshold,
      materiality_medium_percentile =
        materiality_medium_percentile,
      materiality_high_percentile =
        materiality_high_percentile
    )
}


build_employment_register_events <- function(
  annual_panel,
  required_months,
  threshold,
  materiality_medium_percentile,
  materiality_high_percentile
) {
  annual_panel %>%
    filter(
      year ==
        register_reference_year
    ) %>%
    transmute(
      canonical_firm_id,
      reference_year = year,

      rule_id =
        "COH_EMP_REGISTER",

      comparability_group =
        "employment",

      left_source =
        "employment",

      left_variable =
        "annual_mean_employment",

      right_source =
        "register",

      right_variable =
        "employees",

      left_value =
        annual_mean_employment,

      right_value =
        register_employment,

      monthly_coverage =
        usable_employment_months,

      required_monthly_coverage =
        required_months,

      imputed_months =
        employment_imputed_months,

      right_quality_status =
        register_employment_status,

      applicability_status = case_when(
        usable_employment_months <
          required_months ~
          "insufficient_monthly_coverage",

        is.na(register_employment) ~
          "right_value_unavailable",

        TRUE ~
          "applicable"
      ),

      comparison_note =
        paste(
          "Annual mean monthly employment is compared with",
          "a register-style employment snapshot; the concepts",
          "are related but not identical."
        )
    ) %>%
    finalize_events(
      threshold =
        threshold,
      materiality_medium_percentile =
        materiality_medium_percentile,
      materiality_high_percentile =
        materiality_high_percentile
    )
}

combine_coherence_events <- function(
  revenue_accounting_events,
  revenue_register_events,
  employment_register_events,
  accounting,
  annual_panel
) {
  coherence_events <- bind_rows(
    revenue_accounting_events,
    revenue_register_events,
    employment_register_events
  ) %>%
    arrange(
      rule_id,
      canonical_firm_id,
      reference_year
    )

  expected_event_rows <-
    nrow(accounting) +
    n_distinct(
      annual_panel$canonical_firm_id
    ) +
    n_distinct(
      annual_panel$canonical_firm_id
    )

  if (
    nrow(coherence_events) !=
      expected_event_rows
  ) {
    stop(
      "Unexpected number of coherence-event rows: ",
      nrow(coherence_events),
      "; expected ",
      expected_event_rows,
      "."
    )
  }

  coherence_events
}


build_coherence_review_queue <- function(
  coherence_events
) {
  coherence_events %>%
    filter(
      coherence_status ==
        "large_difference",
      review_priority %in%
        c(
          "high",
          "medium"
        )
    ) %>%
    mutate(
      priority_order = case_when(
        review_priority ==
          "high" ~
          1L,

        review_priority ==
          "medium" ~
          2L,

        TRUE ~
          3L
      )
    ) %>%
    arrange(
      priority_order,
      rule_id,
      desc(materiality_percentile),
      desc(relative_difference)
    ) %>%
    select(
      -priority_order
    )
}


build_coherence_results <- function(
  panel,
  accounting,
  contracts,
  coherence_rules_config,
  materiality_config,
  required_months
) {
  threshold_revenue_accounting <-
    get_coherence_threshold(
      "COH_REV_ACCOUNTING",
      coherence_rules_config
    )

  threshold_revenue_register <-
    get_coherence_threshold(
      "COH_REV_REGISTER",
      coherence_rules_config
    )

  threshold_employment_register <-
    get_coherence_threshold(
      "COH_EMP_REGISTER",
      coherence_rules_config
    )

  materiality_medium_percentile <-
    materiality_config$medium_percentile

  materiality_high_percentile <-
    materiality_config$high_percentile

  assert_comparable_group(
    "turnover",
    "turnover",
    "accounting",
    "operating_revenue",
    "annual_revenue_related",
    contracts
  )

  assert_comparable_group(
    "turnover",
    "turnover",
    "register",
    "revenue_last_year",
    "annual_revenue_related",
    contracts
  )

  assert_comparable_group(
    "employment",
    "employees",
    "register",
    "employees",
    "employment",
    contracts
  )

  annual_panel <-
    build_coherence_annual_panel(
      panel,
      required_months
    )

  validate_coherence_annual_panel(
    annual_panel
  )

  revenue_accounting_events <-
    build_revenue_accounting_events(
      annual_panel,
      accounting,
      required_months,
      threshold_revenue_accounting,
      materiality_medium_percentile,
      materiality_high_percentile
    )

  revenue_register_events <-
    build_revenue_register_events(
      annual_panel,
      required_months,
      threshold_revenue_register,
      materiality_medium_percentile,
      materiality_high_percentile
    )

  employment_register_events <-
    build_employment_register_events(
      annual_panel,
      required_months,
      threshold_employment_register,
      materiality_medium_percentile,
      materiality_high_percentile
    )

  coherence_events <-
    combine_coherence_events(
      revenue_accounting_events,
      revenue_register_events,
      employment_register_events,
      accounting,
      annual_panel
    )

  review_queue <-
    build_coherence_review_queue(
      coherence_events
    )

  list(
    events =
      coherence_events,
    review_queue =
      review_queue
  )
}
