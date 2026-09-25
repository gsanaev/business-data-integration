# =====================================================================
# coherence.R
# Coherence helper functions
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Function behavior is intentionally unchanged at this stage.
# =====================================================================

get_coherence_threshold <- function(rule_id) {
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
  variable_name
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
  expected_group
) {
  left_contract <- get_contract(
    left_source,
    left_variable
  )

  right_contract <- get_contract(
    right_source,
    right_variable
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

relative_difference <- function(
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
  threshold
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
        relative_difference(
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
