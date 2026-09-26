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


# =====================================================================
# ML dataset validation and grouped cross-validation
# =====================================================================

validate_ml_candidate_pairs <- function(
  candidate_pairs,
  enterprise_split = NULL
) {
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
    development_ids <-
      enterprise_split %>%
      dplyr::filter(
        .data$sample_role ==
          "development"
      ) %>%
      dplyr::pull(
        .data$truth_firm_id
      )

    nondevelopment_ids <-
      setdiff(
        unique(
          candidate_pairs$truth_firm_id
        ),
        development_ids
      )

    if (
      length(
        nondevelopment_ids
      ) > 0L
    ) {
      stop(
        "ML candidate pairs contain non-development enterprises."
      )
    }
  }

  invisible(
    TRUE
  )
}


create_grouped_cv_folds <- function(
  candidate_pairs,
  n_folds = 5L,
  seed = 202605L
) {
  if (
    length(
      n_folds
    ) != 1L ||
      !is.numeric(
        n_folds
      ) ||
      !is.finite(
        n_folds
      ) ||
      n_folds < 2 ||
      n_folds !=
        as.integer(
          n_folds
        )
  ) {
    stop(
      "n_folds must be an integer of at least 2."
    )
  }

  if (
    length(
      seed
    ) != 1L ||
      !is.numeric(
        seed
      ) ||
      !is.finite(
        seed
      )
  ) {
    stop(
      "seed must be a finite numeric scalar."
    )
  }

  truth_ids <-
    sort(
      unique(
        as.character(
          candidate_pairs$truth_firm_id
        )
      )
    )

  if (
    length(
      truth_ids
    ) <
      n_folds
  ) {
    stop(
      "Number of enterprise groups must be at least n_folds."
    )
  }

  had_seed <-
    exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )

  if (
    had_seed
  ) {
    previous_seed <-
      get(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )
  }

  on.exit(
    {
      if (
        had_seed
      ) {
        assign(
          ".Random.seed",
          previous_seed,
          envir = .GlobalEnv
        )
      } else if (
        exists(
          ".Random.seed",
          envir = .GlobalEnv,
          inherits = FALSE
        )
      ) {
        rm(
          ".Random.seed",
          envir = .GlobalEnv
        )
      }
    },
    add = TRUE
  )

  set.seed(
    as.integer(
      seed
    )
  )

  shuffled_ids <-
    sample(
      truth_ids,
      size =
        length(
          truth_ids
        ),
      replace = FALSE
    )

  tibble::tibble(
    truth_firm_id =
      shuffled_ids,

    cv_fold =
      rep(
        seq_len(
          as.integer(
            n_folds
          )
        ),
        length.out =
          length(
            shuffled_ids
          )
      )
  ) %>%
    dplyr::arrange(
      .data$truth_firm_id
    )
}


attach_grouped_cv_folds <- function(
  candidate_pairs,
  fold_map
) {
  result <-
    candidate_pairs %>%
    dplyr::left_join(
      fold_map,
      by =
        "truth_firm_id"
    )

  if (
    anyNA(
      result$cv_fold
    )
  ) {
    stop(
      "CV fold assignment is missing for one or more candidate pairs."
    )
  }

  result
}


validate_grouped_cv_assignment <- function(
  candidate_pairs_cv,
  n_folds = 5L
) {
  fold_counts_per_enterprise <-
    candidate_pairs_cv %>%
    dplyr::group_by(
      .data$truth_firm_id
    ) %>%
    dplyr::summarise(
      n_folds_observed =
        dplyr::n_distinct(
          .data$cv_fold
        ),
      .groups = "drop"
    )

  if (
    any(
      fold_counts_per_enterprise$n_folds_observed !=
        1L
    )
  ) {
    stop(
      "At least one enterprise spans multiple CV folds."
    )
  }

  observed_folds <-
    sort(
      unique(
        candidate_pairs_cv$cv_fold
      )
    )

  expected_folds <-
    seq_len(
      as.integer(
        n_folds
      )
    )

  if (
    !identical(
      observed_folds,
      expected_folds
    )
  ) {
    stop(
      "Observed CV folds do not match the expected fold set."
    )
  }

  enterprise_counts <-
    candidate_pairs_cv %>%
    dplyr::distinct(
      .data$truth_firm_id,
      .data$cv_fold
    ) %>%
    dplyr::count(
      .data$cv_fold,
      name =
        "enterprises"
    )

  if (
    max(
      enterprise_counts$enterprises
    ) -
      min(
        enterprise_counts$enterprises
      ) >
      1L
  ) {
    stop(
      "Enterprise groups are not balanced across CV folds."
    )
  }

  invisible(
    TRUE
  )
}


summarise_ml_cv_folds <- function(
  candidate_pairs_cv
) {
  candidate_pairs_cv %>%
    dplyr::group_by(
      .data$cv_fold
    ) %>%
    dplyr::summarise(
      enterprises =
        dplyr::n_distinct(
          .data$truth_firm_id
        ),

      source_records =
        dplyr::n_distinct(
          paste(
            .data$scenario,
            .data$source,
            .data$source_record_id,
            sep = "::"
          )
        ),

      candidate_pairs =
        dplyr::n(),

      positive_pairs =
        sum(
          .data$is_true_candidate
        ),

      negative_pairs =
        sum(
          !.data$is_true_candidate
        ),

      positive_share =
        mean(
          .data$is_true_candidate
        ),

      .groups =
        "drop"
    )
}
