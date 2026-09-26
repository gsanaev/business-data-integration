# =====================================================================
# linkage_ml.R
# Candidate-pair preparation for bounded ML-assisted linkage
# =====================================================================


ml_linkage_feature_columns <- function() {
  c(
    "name_similarity",
    "street_similarity",
    "city_similarity",
    "postal_code_match",
    "legal_form_match",
    "nace_match"
  )
}


build_ml_candidate_pairs <- function(
  source_data,
  source_id_column,
  register_data,
  enterprise_split,
  scenario_name,
  source_name
) {
  source_entities <-
    source_data %>%
    dplyr::distinct(
      .data$truth_firm_id,
      source_record_id =
        .data[[source_id_column]],
      .data$business_id,
      .data$enterprise_name,
      .data$street,
      .data$postal_code,
      .data$city,
      .data$legal_form,
      .data$nace_code
    ) %>%
    dplyr::left_join(
      enterprise_split,
      by = "truth_firm_id"
    )

  if (
    anyNA(
      source_entities$sample_role
    )
  ) {
    stop(
      "Enterprise split could not be joined to all source enterprises."
    )
  }

  source_entities <-
    source_entities %>%
    dplyr::filter(
      .data$sample_role ==
        "development"
    )

  register_entities <-
    register_data %>%
    dplyr::distinct(
      .data$truth_firm_id,
      .data$register_id,
      .data$business_id,
      .data$enterprise_name,
      .data$street,
      .data$postal_code,
      .data$city,
      .data$legal_form,
      .data$nace_code
    ) %>%
    dplyr::arrange(
      .data$register_id
    ) %>%
    dplyr::mutate(
      canonical_firm_id =
        sprintf(
          "C%06d",
          dplyr::row_number()
        )
    )

  register_lookup <-
    register_entities %>%
    dplyr::select(
      canonical_firm_id,
      register_id,
      business_id
    )

  truth_register_map <-
    register_entities %>%
    dplyr::select(
      truth_firm_id,
      true_register_id =
        register_id
    )

  unresolved <-
    source_entities %>%
    dplyr::left_join(
      register_lookup,
      by = "business_id"
    ) %>%
    dplyr::filter(
      is.na(
        .data$canonical_firm_id
      )
    ) %>%
    dplyr::left_join(
      truth_register_map,
      by = "truth_firm_id"
    )

  if (
    anyNA(
      unresolved$true_register_id
    )
  ) {
    stop(
      "True register identifiers are missing for unresolved development records."
    )
  }

  if (
    nrow(
      unresolved
    ) == 0L
  ) {
    return(
      tibble::tibble(
        scenario = character(),
        source = character(),
        truth_firm_id = character(),
        source_record_id = character(),
        register_id = character(),
        canonical_firm_id = character(),
        true_register_id = character(),
        name_similarity = double(),
        street_similarity = double(),
        city_similarity = double(),
        postal_code_match = double(),
        legal_form_match = double(),
        nace_match = double(),
        is_true_candidate = logical()
      )
    )
  }

  source_for_matching <-
    unresolved %>%
    dplyr::transmute(
      source_record_id =
        .data$source_record_id,

      enterprise_name_source =
        .data$enterprise_name,

      street_source =
        .data$street,

      postal_code_source =
        as.character(
          .data$postal_code
        ),

      city_source =
        .data$city,

      legal_form_source =
        .data$legal_form,

      nace_code_source =
        .data$nace_code
    )

  register_for_matching <-
    prepare_register_linkage_records(
      register_entities
    )

  candidate_pairs <-
    generate_linkage_candidates(
      source_for_matching,
      register_for_matching
    ) %>%
    add_linkage_features() %>%
    dplyr::left_join(
      unresolved %>%
        dplyr::select(
          truth_firm_id,
          source_record_id,
          true_register_id
        ),
      by = "source_record_id"
    ) %>%
    dplyr::mutate(
      is_true_candidate =
        .data$register_id ==
          .data$true_register_id,

      scenario =
        scenario_name,

      source =
        source_name,

      .before =
        "truth_firm_id"
    )

  candidate_pairs %>%
    dplyr::select(
      scenario,
      source,
      truth_firm_id,
      source_record_id,
      register_id,
      canonical_firm_id,
      true_register_id,
      dplyr::all_of(
        ml_linkage_feature_columns()
      ),
      is_true_candidate
    )
}
