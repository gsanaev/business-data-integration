# =====================================================================
# linkage_ml_candidates.R
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

  prepared <-
    prepare_sampled_linkage_entities(
      source_data = source_data,
      source_id_column = source_id_column,
      register_data = register_data,
      enterprise_split = enterprise_split,
      sample_role = sample_role
    )

  unresolved <-
    prepared$unresolved

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

  candidate_pairs <-
    build_unresolved_linkage_candidates(
      unresolved,
      prepared$register_entities
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

validate_ml_candidate_pair_structure <- function(
  candidate_pairs,
  required_columns
) {
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

  invisible(TRUE)
}


validate_ml_candidate_feature_values <- function(
  candidate_pairs,
  feature_columns
) {
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

  invisible(TRUE)
}


validate_ml_candidate_pair_integrity <- function(
  candidate_pairs
) {
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

  invisible(TRUE)
}


validate_ml_candidate_sample_membership <- function(
  candidate_pairs,
  enterprise_split,
  sample_role
) {
  if (
    is.null(
      enterprise_split
    )
  ) {
    return(
      invisible(TRUE)
    )
  }

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

  invisible(TRUE)
}


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

  validate_ml_candidate_pair_structure(
    candidate_pairs,
    required_columns
  )

  validate_ml_candidate_feature_values(
    candidate_pairs,
    feature_columns
  )

  validate_ml_candidate_pair_integrity(
    candidate_pairs
  )

  validate_ml_candidate_sample_membership(
    candidate_pairs,
    enterprise_split,
    sample_role
  )

  invisible(
    TRUE
  )
}
