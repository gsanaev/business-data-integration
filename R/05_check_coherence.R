# =====================================================================
# 05_check_coherence.R
# Cross-Source Coherence Checks
# ---------------------------------------------------------------------
# Integrated enterprise data are compared only where source concepts,
# reference periods, analytical values, and temporal coverage permit a
# meaningful comparison.
#
# Three bounded coherence rules are implemented:
#
#   COH_REV_ACCOUNTING
#     Annual monthly turnover vs annual accounting operating revenue.
#
#   COH_REV_REGISTER
#     Annual monthly turnover vs register-style prior-year revenue.
#
#   COH_EMP_REGISTER
#     Annual mean monthly employment vs register-style employment.
#
# The rule thresholds below are prototype parameters calibrated to the
# controlled synthetic source variation. They are not official
# statistical thresholds.
#
# Output:
#   data/processed/coherence_events.csv
#   data/processed/review_queue.csv
# =====================================================================

library(dplyr)
library(readr)
library(lubridate)

source("R/config.R")
source("R/coherence.R")

project_config <- load_project_config()

coherence_rules_config <-
  project_config$coherence_rules

materiality_config <-
  project_config$processing$coherence$materiality

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Rule parameters
# ----------------------------------------------------------------------


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

required_months <- 12L

# Materiality is assessed within each coherence rule using the
# distribution of absolute differences among applicable comparisons.
materiality_medium_percentile <-
  materiality_config$medium_percentile

materiality_high_percentile <-
  materiality_config$high_percentile

# ----------------------------------------------------------------------
# 2. Load integrated datasets and source contracts
# ----------------------------------------------------------------------

panel <- read_csv(
  "data/processed/panel_data.csv",
  show_col_types = FALSE
) %>%
  mutate(
    month = as.Date(month),
    year = year(month)
  )

accounting <- read_csv(
  "data/processed/accounting_annual.csv",
  show_col_types = FALSE
)

contracts <- read_csv(
  "config/source_contracts.csv",
  show_col_types = FALSE,
  col_types = cols(
    .default = col_character()
  )
)

# ----------------------------------------------------------------------
# 3. Validate source-contract definitions
# ----------------------------------------------------------------------



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

message("Source-contract comparability checks passed.")

# ----------------------------------------------------------------------
# 4. Build annual aggregates from the monthly panel
# ----------------------------------------------------------------------

annual_panel <- panel %>%
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

# ----------------------------------------------------------------------
# 5. Helpers for coherence-event construction
# ----------------------------------------------------------------------



# ----------------------------------------------------------------------
# 6. Rule COH_REV_ACCOUNTING
# ----------------------------------------------------------------------

revenue_accounting_events <- annual_panel %>%
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
      threshold_revenue_accounting,
    materiality_medium_percentile =
      materiality_medium_percentile,
    materiality_high_percentile =
      materiality_high_percentile
  )

# ----------------------------------------------------------------------
# 7. Rule COH_REV_REGISTER
# ----------------------------------------------------------------------

revenue_register_events <- annual_panel %>%
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
      threshold_revenue_register,
    materiality_medium_percentile =
      materiality_medium_percentile,
    materiality_high_percentile =
      materiality_high_percentile
  )

# ----------------------------------------------------------------------
# 8. Rule COH_EMP_REGISTER
# ----------------------------------------------------------------------

employment_register_events <- annual_panel %>%
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
      threshold_employment_register,
    materiality_medium_percentile =
      materiality_medium_percentile,
    materiality_high_percentile =
      materiality_high_percentile
  )

# ----------------------------------------------------------------------
# 9. Combine long-form coherence events
# ----------------------------------------------------------------------

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

# ----------------------------------------------------------------------
# 10. Build materiality-prioritized review queue
# ----------------------------------------------------------------------

review_queue <- coherence_events %>%
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

# ----------------------------------------------------------------------
# 11. Report coherence results
# ----------------------------------------------------------------------

message("Coherence applicability:")
print(
  coherence_events %>%
    count(
      rule_id,
      applicability_status
    )
)

message("Coherence status among applicable comparisons:")
print(
  coherence_events %>%
    filter(
      applicability_status ==
        "applicable"
    ) %>%
    count(
      rule_id,
      coherence_status
    )
)

message("Review priorities:")
print(
  coherence_events %>%
    filter(
      coherence_status ==
        "large_difference"
    ) %>%
    count(
      rule_id,
      review_priority
    )
)

message(
  "Materiality-prioritized review queue rows: ",
  nrow(review_queue)
)

# ----------------------------------------------------------------------
# 12. Write coherence outputs
# ----------------------------------------------------------------------

write_csv(
  coherence_events,
  "data/processed/coherence_events.csv"
)

write_csv(
  review_queue,
  "data/processed/review_queue.csv"
)

message("Cross-source coherence checks completed successfully.")
