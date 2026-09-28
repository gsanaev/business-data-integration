# =====================================================================
# evaluation_orchestration.R
# Scenario-by-source orchestration for linkage evaluation
# =====================================================================

linkage_evaluation_source_specs <- function() {
  c(
    employment =
      "employment_source_id",
    turnover =
      "turnover_source_id",
    accounting =
      "accounting_source_id"
  )
}


linkage_evaluation_scenario_names <- function() {
  c(
    "baseline",
    "moderate",
    "difficult"
  )
}


build_scenario_candidate_records <- function(
  identity_scenarios,
  enterprise_split,
  sample_role = "development"
) {
  sample_role <-
    match.arg(
      sample_role,
      c(
        "development",
        "heldout"
      )
    )

  source_specs <-
    linkage_evaluation_source_specs()

  scenario_names <-
    linkage_evaluation_scenario_names()

  results <- list()
  result_index <- 1L

  for (
    scenario_name in
      scenario_names
  ) {
    scenario_sources <-
      identity_scenarios[[scenario_name]]$sources

    for (
      source_name in
        names(source_specs)
    ) {
      results[[result_index]] <-
        evaluate_candidate_generation(
          source_data =
            scenario_sources[[source_name]],
          source_id_column =
            source_specs[[source_name]],
          register_data =
            scenario_sources$firms,
          enterprise_split =
            enterprise_split,
          scenario_name =
            scenario_name,
          source_name =
            source_name,
          sample_role =
            sample_role
        )

      result_index <-
        result_index + 1L
    }
  }

  dplyr::bind_rows(
    results
  )
}


build_scenario_similarity_records <- function(
  identity_scenarios,
  enterprise_split,
  similarity_weights,
  sample_role = "development"
) {
  sample_role <-
    match.arg(
      sample_role,
      c(
        "development",
        "heldout"
      )
    )

  source_specs <-
    linkage_evaluation_source_specs()

  scenario_names <-
    linkage_evaluation_scenario_names()

  results <- list()
  result_index <- 1L

  for (
    scenario_name in
      scenario_names
  ) {
    scenario_sources <-
      identity_scenarios[[scenario_name]]$sources

    for (
      source_name in
        names(source_specs)
    ) {
      results[[result_index]] <-
        build_similarity_benchmark_records(
          source_data =
            scenario_sources[[source_name]],
          source_id_column =
            source_specs[[source_name]],
          register_data =
            scenario_sources$firms,
          enterprise_split =
            enterprise_split,
          scenario_name =
            scenario_name,
          source_name =
            source_name,
          similarity_weights =
            similarity_weights,
          sample_role =
            sample_role
        )

      result_index <-
        result_index + 1L
    }
  }

  dplyr::bind_rows(
    results
  )
}


build_scenario_ml_candidate_pairs <- function(
  identity_scenarios,
  enterprise_split,
  sample_role = "development"
) {
  sample_role <-
    match.arg(
      sample_role,
      c(
        "development",
        "heldout"
      )
    )

  source_specs <-
    linkage_evaluation_source_specs()

  scenario_names <-
    linkage_evaluation_scenario_names()

  results <- list()
  result_index <- 1L

  for (
    scenario_name in
      scenario_names
  ) {
    scenario_sources <-
      identity_scenarios[[scenario_name]]$sources

    for (
      source_name in
        names(source_specs)
    ) {
      results[[result_index]] <-
        build_ml_candidate_pairs(
          source_data =
            scenario_sources[[source_name]],
          source_id_column =
            source_specs[[source_name]],
          register_data =
            scenario_sources$firms,
          enterprise_split =
            enterprise_split,
          scenario_name =
            scenario_name,
          source_name =
            source_name,
          sample_role =
            sample_role
        )

      result_index <-
        result_index + 1L
    }
  }

  dplyr::bind_rows(
    results
  )
}


build_scenario_complete_linkage_records <- function(
  identity_scenarios,
  enterprise_split,
  similarity_records,
  similarity_policy,
  rf_records,
  rf_assignments,
  rf_policy,
  sample_role = "heldout"
) {
  sample_role <-
    match.arg(
      sample_role,
      c(
        "development",
        "heldout"
      )
    )

  source_specs <-
    linkage_evaluation_source_specs()

  scenario_names <-
    linkage_evaluation_scenario_names()

  results <- list()
  result_index <- 1L

  for (
    scenario_name in
      scenario_names
  ) {
    scenario_sources <-
      identity_scenarios[[scenario_name]]$sources

    for (
      source_name in
        names(source_specs)
    ) {
      results[[result_index]] <-
        build_complete_linkage_evaluation_records(
          source_data =
            scenario_sources[[source_name]],
          source_id_column =
            source_specs[[source_name]],
          register_data =
            scenario_sources$firms,
          enterprise_split =
            enterprise_split,
          scenario_name =
            scenario_name,
          source_name =
            source_name,
          similarity_records =
            similarity_records,
          similarity_policy =
            similarity_policy,
          rf_records =
            rf_records,
          rf_assignments =
            rf_assignments,
          rf_policy =
            rf_policy,
          sample_role =
            sample_role
        )

      result_index <-
        result_index + 1L
    }
  }

  dplyr::bind_rows(
    results
  )
}
