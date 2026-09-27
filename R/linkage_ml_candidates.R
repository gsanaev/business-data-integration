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
  source_name,
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
        .env$sample_role
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
      "True register identifiers are missing for unresolved sampled records."
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


# =====================================================================
# ML dataset validation and grouped cross-validation
# =====================================================================

validate_ml_candidate_pairs <- function(
  candidate_pairs,
  enterprise_split = NULL,
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

  feature_columns <-
    ml_linkage_feature_columns()

  required_columns <-
    c(
      "scenario",
      "source",
      "truth_firm_id",
      "source_record_id",
      "register_id",
      "canonical_firm_id",
      "true_register_id",
      feature_columns,
      "is_true_candidate"
    )

  missing_columns <-
    setdiff(
      required_columns,
      names(
        candidate_pairs
      )
    )

  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      "ML candidate pairs are missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }

  if (
    nrow(
      candidate_pairs
    ) == 0L
  ) {
    stop(
      "ML candidate-pair dataset must not be empty."
    )
  }

  if (
    anyNA(
      candidate_pairs[
        required_columns
      ]
    )
  ) {
    stop(
      "ML candidate-pair dataset contains missing required values."
    )
  }

  feature_matrix <-
    as.matrix(
      candidate_pairs[
        feature_columns
      ]
    )

  if (
    !is.numeric(
      feature_matrix
    )
  ) {
    stop(
      "All ML linkage features must be numeric."
    )
  }

  if (
    any(
      !is.finite(
        feature_matrix
      )
    )
  ) {
    stop(
      "ML linkage features must contain only finite values."
    )
  }

  if (
    any(
      feature_matrix < 0 |
        feature_matrix > 1
    )
  ) {
    stop(
      "ML linkage features must lie in [0, 1]."
    )
  }

  if (
    !is.logical(
      candidate_pairs$is_true_candidate
    )
  ) {
    stop(
      "is_true_candidate must be logical."
    )
  }

  duplicate_pairs <-
    candidate_pairs %>%
    dplyr::count(
      .data$scenario,
      .data$source,
      .data$source_record_id,
      .data$register_id,
      name = "pair_count"
    ) %>%
    dplyr::filter(
      .data$pair_count >
        1L
    )

  if (
    nrow(
      duplicate_pairs
    ) > 0L
  ) {
    stop(
      "Duplicate ML candidate pairs detected."
    )
  }

  positive_counts <-
    candidate_pairs %>%
    dplyr::group_by(
      .data$scenario,
      .data$source,
      .data$source_record_id
    ) %>%
    dplyr::summarise(
      positive_candidates =
        sum(
          .data$is_true_candidate
        ),
      .groups = "drop"
    )

  if (
    any(
      positive_counts$positive_candidates >
        1L
    )
  ) {
    stop(
      "A source record has more than one true candidate."
    )
  }

  if (
    !is.null(
      enterprise_split
    )
  ) {
    sample_ids <-
      enterprise_split %>%
      dplyr::filter(
        .data$sample_role ==
          .env$sample_role
      ) %>%
      dplyr::pull(
        .data$truth_firm_id
      )

    out_of_sample_ids <-
      setdiff(
        unique(
          candidate_pairs$truth_firm_id
        ),
        sample_ids
      )

    if (
      length(
        out_of_sample_ids
      ) > 0L
    ) {
      stop(
        sprintf(
          "ML candidate pairs contain non-%s enterprises.",
          sample_role
        )
      )
    }
  }

  invisible(
    TRUE
  )
}
