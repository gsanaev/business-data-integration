# =====================================================================
# test-config.R
# Tests for project configuration loading and validation
# =====================================================================

project_path <- function(...) {
  testthat::test_path(
    "..",
    "..",
    ...
  )
}

source(
  project_path(
    "R",
    "config.R"
  ),
  local = TRUE
)

config <- load_project_config(
  processing_path = project_path(
    "config",
    "processing.yml"
  ),
  scenarios_path = project_path(
    "config",
    "scenarios.yml"
  ),
  source_contracts_path = project_path(
    "config",
    "source_contracts.csv"
  ),
  coherence_rules_path = project_path(
    "config",
    "coherence_rules.csv"
  )
)


testthat::test_that("project configuration loads and validates", {
  testthat::expect_type(
    config,
    "list"
  )

  testthat::expect_setequal(
    names(config),
    c(
      "processing",
      "scenarios",
      "source_contracts",
      "coherence_rules"
    )
  )
})


testthat::test_that("baseline processing policy preserves v2 settings", {
  processing <-
    config$processing

  testthat::expect_equal(
    processing$validation$foundation_year_min,
    1900
  )

  testthat::expect_equal(
    processing$validation$employment_spike_multiplier,
    2.0
  )

  baseline <-
    processing$linkage$baseline_similarity

  testthat::expect_equal(
    baseline$score_threshold,
    0.85
  )

  testthat::expect_equal(
    baseline$margin_threshold,
    0.05
  )

  weights <-
    unlist(
      baseline$weights
    )

  testthat::expect_equal(
    sum(weights),
    1,
    tolerance = 1e-12
  )

  testthat::expect_equal(
    weights[
      c(
        "name_similarity",
        "street_similarity",
        "city_similarity",
        "postal_code_match",
        "legal_form_match",
        "nace_match"
      )
    ],
    c(
      name_similarity = 0.40,
      street_similarity = 0.30,
      city_similarity = 0.05,
      postal_code_match = 0.10,
      legal_form_match = 0.075,
      nace_match = 0.075
    )
  )

  testthat::expect_equal(
    processing$coherence$materiality$medium_percentile,
    0.75
  )

  testthat::expect_equal(
    processing$coherence$materiality$high_percentile,
    0.90
  )
})


testthat::test_that("scenario specification matches the frozen v3 design", {
  scenarios <-
    config$scenarios$scenarios

  testthat::expect_equal(
    names(scenarios),
    c(
      "baseline",
      "moderate",
      "difficult"
    )
  )

  expected <- list(
    baseline = c(
      missing_business_id = 0.10,
      invalid_or_unknown_business_id = 0.00,
      additional_name_typo = 0.00,
      substantial_name_degradation = 0.00,
      strong_street_discrepancy = 0.00,
      postal_code_missing_or_error = 0.00,
      nace_disagreement = 0.00,
      legal_form_disagreement = 0.00
    ),
    moderate = c(
      missing_business_id = 0.25,
      invalid_or_unknown_business_id = 0.03,
      additional_name_typo = 0.05,
      substantial_name_degradation = 0.03,
      strong_street_discrepancy = 0.05,
      postal_code_missing_or_error = 0.03,
      nace_disagreement = 0.02,
      legal_form_disagreement = 0.02
    ),
    difficult = c(
      missing_business_id = 0.40,
      invalid_or_unknown_business_id = 0.07,
      additional_name_typo = 0.12,
      substantial_name_degradation = 0.08,
      strong_street_discrepancy = 0.10,
      postal_code_missing_or_error = 0.07,
      nace_disagreement = 0.05,
      legal_form_disagreement = 0.05
    )
  )

  for (
    scenario_name in
      names(expected)
  ) {
    testthat::expect_equal(
      unlist(
        scenarios[[scenario_name]]
      ),
      expected[[scenario_name]]
    )
  }
})


testthat::test_that("coherence rules preserve the frozen baseline policy", {
  rules <-
    config$coherence_rules

  testthat::expect_setequal(
    rules$rule_id,
    c(
      "COH_REV_ACCOUNTING",
      "COH_REV_REGISTER",
      "COH_EMP_REGISTER"
    )
  )

  thresholds <-
    setNames(
      rules$expected_range_threshold,
      rules$rule_id
    )

  testthat::expect_equal(
    thresholds[
      c(
        "COH_REV_ACCOUNTING",
        "COH_REV_REGISTER",
        "COH_EMP_REGISTER"
      )
    ],
    c(
      COH_REV_ACCOUNTING = 0.05,
      COH_REV_REGISTER = 0.08,
      COH_EMP_REGISTER = 0.10
    )
  )
})


testthat::test_that("invalid processing policies are rejected", {
  bad_weights <-
    config$processing

  bad_weights$linkage$baseline_similarity$weights$name_similarity <-
    0.50

  testthat::expect_error(
    validate_processing_config(
      bad_weights
    ),
    "sum to 1"
  )

  bad_materiality <-
    config$processing

  bad_materiality$coherence$materiality$medium_percentile <-
    0.95

  testthat::expect_error(
    validate_processing_config(
      bad_materiality
    ),
    "0 <= medium < high <= 1"
  )
})


testthat::test_that("invalid scenario specifications are rejected", {
  out_of_range <-
    config$scenarios

  out_of_range$scenarios$difficult$missing_business_id <-
    1.10

  testthat::expect_error(
    validate_scenarios_config(
      out_of_range
    ),
    "within \\[0, 1\\]"
  )

  reversed_difficulty <-
    config$scenarios

  reversed_difficulty$scenarios$moderate$missing_business_id <-
    0.05

  testthat::expect_error(
    validate_scenarios_config(
      reversed_difficulty
    ),
    "non-decreasing"
  )
})


testthat::test_that("coherence rules must agree with source contracts", {
  bad_rules <-
    config$coherence_rules

  bad_rules$left_variable[
    1
  ] <-
    "unknown_variable"

  testthat::expect_error(
    validate_coherence_rules(
      bad_rules,
      config$source_contracts
    ),
    "absent from source_contracts.csv"
  )

  bad_group <-
    config$coherence_rules

  bad_group$comparability_group[
    1
  ] <-
    "wrong_group"

  testthat::expect_error(
    validate_coherence_rules(
      bad_group,
      config$source_contracts
    ),
    "inconsistent with source-contract"
  )
})
