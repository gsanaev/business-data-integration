# =====================================================================
# config.R
# Configuration loading and validation
# =====================================================================

config_error <- function(...) {
  stop(
    ...,
    call. = FALSE
  )
}


is_scalar_number <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x)
}


validate_source_contracts <- function(contracts) {
  required_columns <- c(
    "source",
    "variable",
    "statistical_unit",
    "grain",
    "frequency",
    "reference_period",
    "concept",
    "source_role",
    "comparability_group",
    "limitation"
  )

  missing_columns <- setdiff(
    required_columns,
    names(contracts)
  )

  if (length(missing_columns) > 0L) {
    config_error(
      "Source contracts are missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      "."
    )
  }

  if (nrow(contracts) == 0L) {
    config_error(
      "Source contracts must contain at least one row."
    )
  }

  source_variable_key <- paste(
    contracts$source,
    contracts$variable,
    sep = "::"
  )

  if (
    any(
      is.na(contracts$source) |
        !nzchar(trimws(contracts$source)) |
        is.na(contracts$variable) |
        !nzchar(trimws(contracts$variable))
    )
  ) {
    config_error(
      "Source-contract source and variable values must be non-missing."
    )
  }

  if (anyDuplicated(source_variable_key)) {
    config_error(
      "Source contracts contain duplicate source-variable definitions."
    )
  }

  if (
    any(
      is.na(contracts$comparability_group) |
        !nzchar(trimws(contracts$comparability_group))
    )
  ) {
    config_error(
      "Every source contract must define a comparability group."
    )
  }

  invisible(TRUE)
}


validate_processing_config <- function(config) {
  if (
    !is_scalar_number(
      config$schema_version
    ) ||
      config$schema_version != 1
  ) {
    config_error(
      "processing.yml must use schema_version 1."
    )
  }

  foundation_year_min <-
    config$validation$foundation_year_min

  if (
    !is_scalar_number(
      foundation_year_min
    ) ||
      foundation_year_min <= 0 ||
      foundation_year_min !=
        floor(
          foundation_year_min
        )
  ) {
    config_error(
      "foundation_year_min must be a positive integer."
    )
  }

  employment_spike_multiplier <-
    config$validation$employment_spike_multiplier

  if (
    !is_scalar_number(
      employment_spike_multiplier
    ) ||
      employment_spike_multiplier <= 1
  ) {
    config_error(
      "employment_spike_multiplier must be greater than 1."
    )
  }

  baseline_similarity <-
    config$linkage$baseline_similarity

  score_threshold <-
    baseline_similarity$score_threshold

  margin_threshold <-
    baseline_similarity$margin_threshold

  for (
    item in
      list(
        score_threshold = score_threshold,
        margin_threshold = margin_threshold
      )
  ) {
    if (
      !is_scalar_number(item) ||
        item < 0 ||
        item > 1
    ) {
      config_error(
        "Baseline similarity thresholds must lie within [0, 1]."
      )
    }
  }

  expected_weight_names <- c(
    "name_similarity",
    "street_similarity",
    "city_similarity",
    "postal_code_match",
    "legal_form_match",
    "nace_match"
  )

  weights <-
    unlist(
      baseline_similarity$weights,
      use.names = TRUE
    )

  if (
    !setequal(
      names(weights),
      expected_weight_names
    ) ||
      length(weights) !=
        length(expected_weight_names)
  ) {
    config_error(
      "Baseline similarity weights do not contain the expected features."
    )
  }

  if (
    !is.numeric(weights) ||
      any(
        !is.finite(weights) |
          weights < 0 |
          weights > 1
      )
  ) {
    config_error(
      "Baseline similarity weights must be finite values within [0, 1]."
    )
  }

  if (
    abs(
      sum(weights) -
        1
    ) >
      1e-12
  ) {
    config_error(
      "Baseline similarity weights must sum to 1."
    )
  }

  materiality <-
    config$coherence$materiality

  medium_percentile <-
    materiality$medium_percentile

  high_percentile <-
    materiality$high_percentile

  if (
    !is_scalar_number(
      medium_percentile
    ) ||
      !is_scalar_number(
        high_percentile
      ) ||
      medium_percentile < 0 ||
      high_percentile > 1 ||
      medium_percentile >=
        high_percentile
  ) {
    config_error(
      paste(
        "Materiality percentiles must satisfy",
        "0 <= medium < high <= 1."
      )
    )
  }

  invisible(TRUE)
}


validate_scenarios_config <- function(config) {
  if (
    !is_scalar_number(
      config$schema_version
    ) ||
      config$schema_version != 1
  ) {
    config_error(
      "scenarios.yml must use schema_version 1."
    )
  }

  expected_scenarios <- c(
    "baseline",
    "moderate",
    "difficult"
  )

  scenarios <-
    config$scenarios

  if (
    !setequal(
      names(scenarios),
      expected_scenarios
    ) ||
      length(scenarios) !=
        length(expected_scenarios)
  ) {
    config_error(
      paste(
        "Scenarios must contain exactly:",
        paste(
          expected_scenarios,
          collapse = ", "
        )
      )
    )
  }

  expected_parameters <- c(
    "missing_business_id",
    "invalid_or_unknown_business_id",
    "additional_name_typo",
    "substantial_name_degradation",
    "strong_street_discrepancy",
    "postal_code_missing_or_error",
    "nace_disagreement",
    "legal_form_disagreement"
  )

  for (
    scenario_name in
      expected_scenarios
  ) {
    scenario <-
      scenarios[[scenario_name]]

    if (
      !setequal(
        names(scenario),
        expected_parameters
      ) ||
        length(scenario) !=
          length(expected_parameters)
    ) {
      config_error(
        "Scenario ",
        scenario_name,
        " does not contain the expected parameters."
      )
    }

    values <-
      unlist(
        scenario,
        use.names = TRUE
      )

    if (
      !is.numeric(values) ||
        any(
          !is.finite(values) |
            values < 0 |
            values > 1
        )
    ) {
      config_error(
        "Scenario probabilities must lie within [0, 1]."
      )
    }
  }

  for (
    parameter_name in
      expected_parameters
  ) {
    values <- c(
      scenarios$baseline[[parameter_name]],
      scenarios$moderate[[parameter_name]],
      scenarios$difficult[[parameter_name]]
    )

    if (
      any(
        diff(values) <
          -1e-12
      )
    ) {
      config_error(
        "Scenario difficulty must be non-decreasing for parameter ",
        parameter_name,
        "."
      )
    }
  }

  invisible(TRUE)
}


validate_coherence_rules <- function(
  rules,
  contracts
) {
  validate_source_contracts(
    contracts
  )

  required_columns <- c(
    "rule_id",
    "left_source",
    "left_variable",
    "right_source",
    "right_variable",
    "comparability_group",
    "expected_range_threshold"
  )

  missing_columns <- setdiff(
    required_columns,
    names(rules)
  )

  if (length(missing_columns) > 0L) {
    config_error(
      "Coherence rules are missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      "."
    )
  }

  if (nrow(rules) == 0L) {
    config_error(
      "At least one coherence rule must be defined."
    )
  }

  if (
    any(
      is.na(rules$rule_id) |
        !nzchar(trimws(rules$rule_id))
    ) ||
      anyDuplicated(
        rules$rule_id
      )
  ) {
    config_error(
      "Coherence rule identifiers must be unique and non-missing."
    )
  }

  thresholds <-
    rules$expected_range_threshold

  if (
    !is.numeric(thresholds) ||
      any(
        !is.finite(thresholds) |
          thresholds < 0 |
          thresholds > 1
      )
  ) {
    config_error(
      "Coherence-rule thresholds must lie within [0, 1]."
    )
  }

  contract_key <- paste(
    contracts$source,
    contracts$variable,
    sep = "::"
  )

  for (
    row_number in
      seq_len(
        nrow(rules)
      )
  ) {
    rule <-
      rules[
        row_number,
        ,
        drop = FALSE
      ]

    left_key <- paste(
      rule$left_source,
      rule$left_variable,
      sep = "::"
    )

    right_key <- paste(
      rule$right_source,
      rule$right_variable,
      sep = "::"
    )

    left_position <-
      match(
        left_key,
        contract_key
      )

    right_position <-
      match(
        right_key,
        contract_key
      )

    if (
      is.na(left_position) ||
        is.na(right_position)
    ) {
      config_error(
        "Coherence rule ",
        rule$rule_id,
        " references a source-variable pair absent from source_contracts.csv."
      )
    }

    expected_group <-
      rule$comparability_group

    if (
      contracts$comparability_group[
        left_position
      ] !=
        expected_group ||
        contracts$comparability_group[
          right_position
        ] !=
          expected_group
    ) {
      config_error(
        "Coherence rule ",
        rule$rule_id,
        " is inconsistent with source-contract comparability groups."
      )
    }
  }

  invisible(TRUE)
}


load_project_config <- function(
  processing_path = "config/processing.yml",
  scenarios_path = "config/scenarios.yml",
  source_contracts_path = "config/source_contracts.csv",
  coherence_rules_path = "config/coherence_rules.csv"
) {
  processing <-
    yaml::read_yaml(
      processing_path
    )

  scenarios <-
    yaml::read_yaml(
      scenarios_path
    )

  source_contracts <-
    read.csv(
      source_contracts_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

  coherence_rules <-
    read.csv(
      coherence_rules_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

  validate_processing_config(
    processing
  )

  validate_scenarios_config(
    scenarios
  )

  validate_source_contracts(
    source_contracts
  )

  validate_coherence_rules(
    coherence_rules,
    source_contracts
  )

  list(
    processing = processing,
    scenarios = scenarios,
    source_contracts = source_contracts,
    coherence_rules = coherence_rules
  )
}
