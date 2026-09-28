# =====================================================================
# downstream_evaluation.R
# Downstream propagation of frozen held-out linkage decisions
# =====================================================================

assert_downstream_columns <- function(data, required, label) {
  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      label,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      "."
    )
  }

  invisible(TRUE)
}


validate_downstream_sources <- function(
  operational_sources,
  validation_config
) {
  required_sources <- c(
    "firms",
    "employment",
    "turnover",
    "accounting"
  )

  if (!all(required_sources %in% names(operational_sources))) {
    stop("Operational source bundle is incomplete.")
  }

  validate_source_structures(
    operational_sources$firms,
    operational_sources$employment,
    operational_sources$turnover,
    operational_sources$accounting
  )

  firms <- validate_register_source(
    operational_sources$firms,
    validation_config$foundation_year_min
  )

  employment <- validate_employment_source(
    operational_sources$employment,
    validation_config$employment_spike_multiplier
  )

  turnover <- validate_turnover_source(
    operational_sources$turnover
  )

  accounting <- validate_accounting_source(
    operational_sources$accounting
  )

  assert_validated_sources(
    firms,
    employment,
    turnover,
    accounting
  )

  list(
    firms = firms,
    employment = employment,
    turnover = turnover,
    accounting = accounting
  )
}


subset_downstream_sources <- function(
  validated_sources,
  linkage_records,
  scenario_name
) {
  assert_downstream_columns(
    linkage_records,
    c("scenario", "source", "source_record_id"),
    "Complete linkage records"
  )

  keys <- linkage_records %>%
    dplyr::filter(
      .data$scenario == .env$scenario_name,
      .data$source %in%
        c("employment", "turnover", "accounting")
    ) %>%
    dplyr::distinct(
      .data$source,
      .data$source_record_id
    )

  ids_for <- function(source_name) {
    keys$source_record_id[keys$source == source_name]
  }

  result <- list(
    firms = validated_sources$firms,

    employment = validated_sources$employment %>%
      dplyr::filter(
        .data$employment_source_id %in%
          ids_for("employment")
      ),

    turnover = validated_sources$turnover %>%
      dplyr::filter(
        .data$turnover_source_id %in%
          ids_for("turnover")
      ),

    accounting = validated_sources$accounting %>%
      dplyr::filter(
        .data$accounting_source_id %in%
          ids_for("accounting")
      )
  )

  observed_counts <- c(
    employment =
      dplyr::n_distinct(
        result$employment$employment_source_id
      ),
    turnover =
      dplyr::n_distinct(
        result$turnover$turnover_source_id
      ),
    accounting =
      dplyr::n_distinct(
        result$accounting$accounting_source_id
      )
  )

  expected_counts <- c(
    employment = length(ids_for("employment")),
    turnover = length(ids_for("turnover")),
    accounting = length(ids_for("accounting"))
  )

  if (!identical(observed_counts, expected_counts)) {
    stop("Held-out source subsetting changed source-entity coverage.")
  }

  result
}


resolve_downstream_assignment_collisions <- function(assignments) {
  required_columns <- c(
    "source",
    "source_record_id",
    "decision_stage",
    "decision_status",
    "assigned_register_id",
    "assigned_canonical_firm_id"
  )

  assert_downstream_columns(
    assignments,
    required_columns,
    "Linkage assignments"
  )

  context_columns <- intersect(
    c("scenario", "method"),
    names(assignments)
  )

  collision_columns <- c(
    context_columns,
    "source",
    "assigned_canonical_firm_id"
  )

  automatic_assignments <- assignments %>%
    dplyr::filter(
      .data$decision_status == "auto_link",
      !is.na(.data$assigned_canonical_firm_id)
    )

  collisions <- automatic_assignments %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(collision_columns)
      )
    ) %>%
    dplyr::summarise(
      n_assignments = dplyr::n(),
      n_level1 =
        sum(.data$decision_stage == "level1"),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      .data$n_assignments > 1L
    )

  if (any(collisions$n_level1 > 1L)) {
    stop(
      "Multiple deterministic Level-1 assignments map to the same ",
      "canonical enterprise."
    )
  }

  result <- assignments %>%
    dplyr::left_join(
      collisions,
      by = collision_columns
    ) %>%
    dplyr::mutate(
      assignment_collision =
        !is.na(.data$n_assignments),

      downstream_status =
        dplyr::case_when(
          .data$decision_status != "auto_link" ~
            .data$decision_status,

          !.data$assignment_collision ~
            "auto_link",

          .data$n_level1 == 1L &
            .data$decision_stage == "level1" ~
            "auto_link",

          TRUE ~
            "collision_review"
        ),

      downstream_automatic_link =
        .data$downstream_status == "auto_link",

      downstream_assigned_register_id =
        dplyr::if_else(
          .data$downstream_automatic_link,
          .data$assigned_register_id,
          NA_character_
        ),

      downstream_assigned_canonical_firm_id =
        dplyr::if_else(
          .data$downstream_automatic_link,
          .data$assigned_canonical_firm_id,
          NA_character_
        )
    )

  duplicate_final_assignments <- result %>%
    dplyr::filter(
      .data$downstream_automatic_link
    ) %>%
    dplyr::count(
      dplyr::across(
        dplyr::all_of(
          c(
            context_columns,
            "source",
            "downstream_assigned_canonical_firm_id"
          )
        )
      )
    ) %>%
    dplyr::filter(
      .data$n > 1L
    )

  if (nrow(duplicate_final_assignments) > 0L) {
    stop(
      "Collision resolution left duplicate automatic canonical assignments."
    )
  }

  diagnostics <- collisions %>%
    dplyr::mutate(
      collision_action =
        dplyr::if_else(
          .data$n_level1 == 1L,
          "retain_level1_review_level2",
          "review_all_level2"
        )
    )

  list(
    assignments = result,
    collisions = diagnostics
  )
}


build_truth_downstream_assignments <- function(
  linkage_records,
  scenario_name
) {
  required_columns <- c(
    "scenario",
    "source",
    "source_record_id",
    "true_register_id",
    "true_canonical_firm_id"
  )

  assert_downstream_columns(
    linkage_records,
    required_columns,
    "Complete linkage records"
  )

  truth_assignments <- linkage_records %>%
    dplyr::filter(
      .data$scenario == .env$scenario_name,
      .data$source %in%
        c("employment", "turnover", "accounting")
    ) %>%
    dplyr::distinct(
      .data$source,
      .data$source_record_id,
      .data$true_register_id,
      .data$true_canonical_firm_id
    )

  duplicate_keys <- truth_assignments %>%
    dplyr::count(
      .data$source,
      .data$source_record_id
    ) %>%
    dplyr::filter(
      .data$n > 1L
    )

  duplicate_canonical <- truth_assignments %>%
    dplyr::count(
      .data$source,
      .data$true_canonical_firm_id
    ) %>%
    dplyr::filter(
      .data$n > 1L
    )

  if (
    nrow(duplicate_keys) > 0L ||
      nrow(duplicate_canonical) > 0L
  ) {
    stop("Truth linkage assignments are not one-to-one within source.")
  }

  truth_assignments
}


build_downstream_crosswalk <- function(
  register_data,
  assignments,
  canonical_column
) {
  assert_downstream_columns(
    assignments,
    c("source", "source_record_id", canonical_column),
    "Downstream assignments"
  )

  register_reference <-
    prepare_register_linkage_reference(
      register_data
    )

  register_links <- register_reference$entities %>%
    dplyr::transmute(
      source = "register",
      source_record_id = .data$register_id,
      canonical_firm_id = .data$canonical_firm_id
    )

  source_links <- assignments %>%
    dplyr::filter(
      .data$source %in%
        c("employment", "turnover", "accounting"),
      !is.na(.data[[canonical_column]])
    ) %>%
    dplyr::transmute(
      source,
      source_record_id,
      canonical_firm_id =
        .data[[canonical_column]]
    )

  if (
    anyDuplicated(
      source_links[
        c("source", "source_record_id")
      ]
    ) ||
      anyDuplicated(
        source_links[
          c("source", "canonical_firm_id")
        ]
      )
  ) {
    stop(
      "Downstream source crosswalk is not one-to-one within source."
    )
  }

  unknown_canonical <- setdiff(
    source_links$canonical_firm_id,
    register_links$canonical_firm_id
  )

  if (length(unknown_canonical) > 0L) {
    stop(
      "Downstream crosswalk contains canonical identifiers ",
      "outside the register universe."
    )
  }

  dplyr::bind_rows(
    register_links,
    source_links
  )
}


build_downstream_indicator_bundle <- function(
  validated_sources,
  crosswalk,
  required_months = 12L
) {
  source_maps <-
    build_source_maps(
      crosswalk
    )

  linked_sources <-
    attach_canonical_identifiers(
      validated_sources$firms,
      validated_sources$employment,
      validated_sources$turnover,
      validated_sources$accounting,
      source_maps
    )

  common_firms <-
    get_common_canonical_firms(
      linked_sources$firms_linked,
      linked_sources$employment_linked,
      linked_sources$turnover_linked
    )

  accounting_annual <-
    prepare_accounting_annual(
      linked_sources$accounting_linked
    )

  source_panels <-
    prepare_monthly_source_panels(
      linked_sources$firms_linked,
      linked_sources$employment_linked,
      linked_sources$turnover_linked,
      common_firms
    )

  panel <-
    build_monthly_panel(
      source_panels$employment_panel,
      source_panels$turnover_panel,
      source_panels$firms_panel
    ) %>%
    derive_panel_indicators() %>%
    dplyr::mutate(
      year =
        lubridate::year(.data$month)
    )

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

  indicators <- list(
    year =
      aggregate_indicators(
        enterprise_year,
        "year"
      ),

    sector =
      aggregate_indicators(
        enterprise_year,
        c("year", "nace_code")
      ),

    region =
      aggregate_indicators(
        enterprise_year,
        c("year", "region_code")
      ),

    sector_region =
      aggregate_indicators(
        enterprise_year,
        c(
          "year",
          "nace_code",
          "region_code"
        )
      )
  )

  validate_indicator_tables(
    indicators
  )

  list(
    common_firms = common_firms,
    panel = panel,
    accounting_annual = accounting_annual,
    enterprise_year = enterprise_year,
    indicators = indicators
  )
}


compare_downstream_indicator_table <- function(
  observed,
  truth,
  grouping_variables,
  aggregation_level,
  scenario_name,
  method_name
) {
  metrics <- c(
    "n_enterprises",
    "total_turnover",
    "total_average_employment",
    "turnover_per_employee"
  )

  required_columns <- c(
    grouping_variables,
    metrics
  )

  assert_downstream_columns(
    observed,
    required_columns,
    "Observed indicator table"
  )

  assert_downstream_columns(
    truth,
    required_columns,
    "Truth indicator table"
  )

  observed_values <- observed %>%
    dplyr::select(
      dplyr::all_of(required_columns)
    ) %>%
    dplyr::mutate(
      observed_cell = TRUE
    )

  truth_values <- truth %>%
    dplyr::select(
      dplyr::all_of(required_columns)
    ) %>%
    dplyr::mutate(
      truth_cell = TRUE
    )

  joined <- dplyr::full_join(
    observed_values,
    truth_values,
    by = grouping_variables,
    suffix = c("_observed", "_truth")
  ) %>%
    dplyr::mutate(
      observed_cell =
        dplyr::coalesce(
          .data$observed_cell,
          FALSE
        ),
      truth_cell =
        dplyr::coalesce(
          .data$truth_cell,
          FALSE
        ),
      cell_status =
        dplyr::case_when(
          .data$observed_cell &
            .data$truth_cell ~
            "both",
          .data$truth_cell ~
            "truth_only",
          TRUE ~
            "observed_only"
        )
    )

  additive_metrics <- c(
    "n_enterprises",
    "total_turnover",
    "total_average_employment"
  )

  metric_results <- lapply(
    metrics,
    function(metric) {
      observed_value <-
        joined[[paste0(metric, "_observed")]]

      truth_value <-
        joined[[paste0(metric, "_truth")]]

      if (metric %in% additive_metrics) {
        observed_value <-
          dplyr::coalesce(
            observed_value,
            0
          )

        truth_value <-
          dplyr::coalesce(
            truth_value,
            0
          )
      }

      difference <-
        observed_value -
          truth_value

      relative_error <-
        dplyr::if_else(
          !is.na(truth_value) &
            truth_value != 0,
          difference /
            truth_value,
          NA_real_
        )

      dplyr::bind_cols(
        joined[
          grouping_variables
        ],
        tibble::tibble(
          scenario = scenario_name,
          method = method_name,
          aggregation_level =
            aggregation_level,
          metric = metric,
          cell_status =
            joined$cell_status,
          observed_value =
            observed_value,
          truth_value =
            truth_value,
          difference =
            difference,
          absolute_difference =
            abs(difference),
          relative_error =
            relative_error,
          absolute_relative_error =
            abs(relative_error)
        )
      )
    }
  )

  dplyr::bind_rows(
    metric_results
  )
}


compare_downstream_indicator_tables <- function(
  observed_tables,
  truth_tables,
  scenario_name,
  method_name
) {
  grouping_map <- list(
    year =
      "year",
    sector =
      c("year", "nace_code"),
    region =
      c("year", "region_code"),
    sector_region =
      c(
        "year",
        "nace_code",
        "region_code"
      )
  )

  if (
    !all(names(grouping_map) %in% names(observed_tables)) ||
      !all(names(grouping_map) %in% names(truth_tables))
  ) {
    stop("Downstream indicator bundle is incomplete.")
  }

  results <- lapply(
    names(grouping_map),
    function(level_name) {
      compare_downstream_indicator_table(
        observed =
          observed_tables[[level_name]],
        truth =
          truth_tables[[level_name]],
        grouping_variables =
          grouping_map[[level_name]],
        aggregation_level =
          level_name,
        scenario_name =
          scenario_name,
        method_name =
          method_name
      )
    }
  )

  dplyr::bind_rows(
    results
  )
}


summarise_downstream_indicator_errors <- function(
  error_records
) {
  assert_downstream_columns(
    error_records,
    c(
      "scenario",
      "method",
      "aggregation_level",
      "metric",
      "cell_status",
      "absolute_difference",
      "absolute_relative_error"
    ),
    "Downstream indicator errors"
  )

  mean_or_na <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      mean(x, na.rm = TRUE)
    }
  }

  max_or_na <- function(x) {
    if (all(is.na(x))) {
      NA_real_
    } else {
      max(x, na.rm = TRUE)
    }
  }

  error_records %>%
    dplyr::group_by(
      .data$scenario,
      .data$method,
      .data$aggregation_level,
      .data$metric
    ) %>%
    dplyr::summarise(
      comparison_cells = dplyr::n(),
      truth_only_cells =
        sum(.data$cell_status == "truth_only"),
      observed_only_cells =
        sum(.data$cell_status == "observed_only"),
      mean_absolute_difference =
        mean_or_na(.data$absolute_difference),
      mean_absolute_relative_error =
        mean_or_na(.data$absolute_relative_error),
      max_absolute_relative_error =
        max_or_na(.data$absolute_relative_error),
      .groups = "drop"
    )
}


evaluate_downstream_method <- function(
  heldout_sources,
  linkage_records,
  truth_bundle,
  scenario_name,
  method_name,
  required_months
) {
  method_assignments <-
    linkage_records %>%
    dplyr::filter(
      .data$scenario ==
        .env$scenario_name,
      .data$method ==
        .env$method_name,
      .data$source %in%
        c(
          "employment",
          "turnover",
          "accounting"
        )
    )

  collision_result <-
    resolve_downstream_assignment_collisions(
      method_assignments
    )

  observed_crosswalk <-
    build_downstream_crosswalk(
      heldout_sources$firms,
      collision_result$assignments,
      "downstream_assigned_canonical_firm_id"
    )

  observed_bundle <-
    build_downstream_indicator_bundle(
      heldout_sources,
      observed_crosswalk,
      required_months
    )

  error_records <-
    compare_downstream_indicator_tables(
      observed_bundle$indicators,
      truth_bundle$indicators,
      scenario_name,
      method_name
    )

  coverage <-
    tibble::tibble(
      scenario =
        scenario_name,
      method =
        method_name,
      truth_common_firms =
        length(
          truth_bundle$common_firms
        ),
      observed_common_firms =
        length(
          observed_bundle$common_firms
        ),
      truth_enterprise_years =
        nrow(
          truth_bundle$enterprise_year
        ),
      observed_enterprise_years =
        nrow(
          observed_bundle$enterprise_year
        )
    )

  list(
    error_records =
      error_records,
    collision_records =
      collision_result$collisions,
    coverage =
      coverage,
    observed_indicators =
      observed_bundle$indicators
  )
}


run_downstream_scenario_evaluation <- function(
  operational_sources,
  linkage_records,
  validation_config,
  scenario_name,
  methods = c(
    "weighted_similarity",
    "random_forest"
  ),
  required_months = 12L
) {
  validated_sources <-
    validate_downstream_sources(
      operational_sources,
      validation_config
    )

  heldout_sources <-
    subset_downstream_sources(
      validated_sources,
      linkage_records,
      scenario_name
    )

  truth_assignments <-
    build_truth_downstream_assignments(
      linkage_records,
      scenario_name
    )

  truth_crosswalk <-
    build_downstream_crosswalk(
      heldout_sources$firms,
      truth_assignments,
      "true_canonical_firm_id"
    )

  truth_bundle <-
    build_downstream_indicator_bundle(
      heldout_sources,
      truth_crosswalk,
      required_months
    )

  method_results <-
    lapply(
      methods,
      function(method_name) {
        evaluate_downstream_method(
          heldout_sources =
            heldout_sources,
          linkage_records =
            linkage_records,
          truth_bundle =
            truth_bundle,
          scenario_name =
            scenario_name,
          method_name =
            method_name,
          required_months =
            required_months
        )
      }
    )

  names(method_results) <-
    methods

  list(
    error_records =
      dplyr::bind_rows(
        lapply(
          method_results,
          `[[`,
          "error_records"
        )
      ),

    collision_records =
      dplyr::bind_rows(
        lapply(
          method_results,
          `[[`,
          "collision_records"
        )
      ),

    coverage =
      dplyr::bind_rows(
        lapply(
          method_results,
          `[[`,
          "coverage"
        )
      ),

    truth_indicators =
      truth_bundle$indicators,

    observed_indicators =
      lapply(
        method_results,
        `[[`,
        "observed_indicators"
      )
  )
}


run_downstream_scenario_evaluations <- function(
  operational_sources,
  linkage_records,
  validation_config,
  scenario_names
) {
  results <-
    lapply(
      scenario_names,
      function(
        scenario_name
      ) {
        run_downstream_scenario_evaluation(
          operational_sources =
            operational_sources[[scenario_name]],
          linkage_records =
            linkage_records,
          validation_config =
            validation_config,
          scenario_name =
            scenario_name
        )
      }
    )

  names(results) <-
    scenario_names

  results
}
