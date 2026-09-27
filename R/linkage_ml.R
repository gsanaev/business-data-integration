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


# =====================================================================
# Bounded Random Forest linkage model
# =====================================================================

rf_linkage_tuning_grid <- function() {
  tidyr::crossing(
    mtry =
      c(
        2L,
        4L
      ),

    min_node_size =
      c(
        1L,
        10L
      )
  ) %>%
    dplyr::mutate(
      config_id =
        sprintf(
          "rf_mtry%d_node%d",
          .data$mtry,
          .data$min_node_size
        ),
      num_trees =
        200L,
      .before =
        "mtry"
    )
}


compute_rf_class_weights <- function(
  is_true_candidate
) {
  if (
    !is.logical(
      is_true_candidate
    ) ||
      anyNA(
        is_true_candidate
      )
  ) {
    stop(
      "RF class labels must be non-missing logical values."
    )
  }

  positive_count <-
    sum(
      is_true_candidate
    )

  negative_count <-
    sum(
      !is_true_candidate
    )

  if (
    positive_count == 0L ||
      negative_count == 0L
  ) {
    stop(
      "Both RF training classes must be present."
    )
  }

  c(
    nonmatch =
      1,

    match =
      negative_count /
        positive_count
  )
}


prepare_rf_training_frame <- function(
  candidate_pairs
) {
  feature_columns <-
    ml_linkage_feature_columns()

  training_frame <-
    candidate_pairs %>%
    dplyr::select(
      dplyr::all_of(
        feature_columns
      )
    )

  training_frame$match_label <-
    factor(
      ifelse(
        candidate_pairs$is_true_candidate,
        "match",
        "nonmatch"
      ),
      levels =
        c(
          "nonmatch",
          "match"
        )
    )

  training_frame %>%
    dplyr::select(
      match_label,
      dplyr::everything()
    ) %>%
    as.data.frame()
}


fit_rf_candidate_model <- function(
  training_pairs,
  mtry,
  min_node_size,
  num_trees = 200L,
  seed = 202606L,
  num_threads = 2L
) {
  if (
    mtry < 1L ||
      mtry >
        length(
          ml_linkage_feature_columns()
        )
  ) {
    stop(
      "mtry is outside the available feature range."
    )
  }

  class_weights <-
    compute_rf_class_weights(
      training_pairs$is_true_candidate
    )

  training_frame <-
    prepare_rf_training_frame(
      training_pairs
    )

  model <-
    ranger::ranger(
      dependent.variable.name =
        "match_label",
      data =
        training_frame,
      probability =
        TRUE,
      num.trees =
        as.integer(
          num_trees
        ),
      mtry =
        as.integer(
          mtry
        ),
      min.node.size =
        as.integer(
          min_node_size
        ),
      class.weights =
        class_weights,
      importance =
        "none",
      seed =
        as.integer(
          seed
        ),
      num.threads =
        as.integer(
          num_threads
        ),
      write.forest =
        TRUE,
      verbose =
        FALSE
    )

  list(
    model =
      model,

    class_weights =
      class_weights
  )
}


predict_rf_match_probability <- function(
  fitted_model,
  candidate_pairs,
  num_threads = 2L
) {
  feature_columns <-
    ml_linkage_feature_columns()

  prediction_data <-
    candidate_pairs %>%
    dplyr::select(
      dplyr::all_of(
        feature_columns
      )
    ) %>%
    as.data.frame()

  probabilities <-
    predict(
      fitted_model,
      data =
        prediction_data,
      num.threads =
        as.integer(
          num_threads
        )
    )$predictions

  if (
    !is.matrix(
      probabilities
    ) ||
      !"match" %in%
        colnames(
          probabilities
        )
  ) {
    stop(
      "RF probability predictions do not contain the match class."
    )
  }

  as.numeric(
    probabilities[
      ,
      "match"
    ]
  )
}


score_rf_candidate_records <- function(
  candidate_pairs,
  match_probability
) {
  if (
    length(
      match_probability
    ) !=
      nrow(
        candidate_pairs
      )
  ) {
    stop(
      "RF probability vector does not match candidate-pair rows."
    )
  }

  if (
    anyNA(
      match_probability
    ) ||
      any(
        !is.finite(
          match_probability
        )
      ) ||
      any(
        match_probability < 0 |
          match_probability > 1
      )
  ) {
    stop(
      "RF match probabilities must be finite values in [0, 1]."
    )
  }

  if (
    !"cv_fold" %in%
      names(
        candidate_pairs
      )
  ) {
    candidate_pairs <-
      candidate_pairs %>%
      dplyr::mutate(
        cv_fold =
          NA_integer_
      )
  }

  candidate_pairs %>%
    dplyr::mutate(
      rf_match_probability =
        match_probability
    ) %>%
    dplyr::arrange(
      .data$scenario,
      .data$source,
      .data$source_record_id,
      dplyr::desc(
        .data$rf_match_probability
      ),
      .data$register_id
    ) %>%
    dplyr::group_by(
      .data$scenario,
      .data$source,
      .data$source_record_id
    ) %>%
    dplyr::summarise(
      truth_firm_id =
        dplyr::first(
          .data$truth_firm_id
        ),

      cv_fold =
        dplyr::first(
          .data$cv_fold
        ),

      candidate_count =
        dplyr::n(),

      true_candidate_present =
        any(
          .data$is_true_candidate
        ),

      top_candidate_correct =
        dplyr::first(
          .data$is_true_candidate
        ),

      top_probability =
        dplyr::first(
          .data$rf_match_probability
        ),

      second_probability =
        if (
          dplyr::n() >=
            2L
        ) {
          dplyr::nth(
            .data$rf_match_probability,
            2L
          )
        } else {
          NA_real_
        },

      probability_margin =
        ifelse(
          is.na(
            second_probability
          ),
          top_probability,
          top_probability -
            second_probability
        ),

      true_candidate_rank =
        {
          true_index <-
            which(
              .data$is_true_candidate
            )

          if (
            length(
              true_index
            ) > 0L
          ) {
            true_index[[1]]
          } else {
            NA_integer_
          }
        },

      reciprocal_rank =
        ifelse(
          is.na(
            true_candidate_rank
          ),
          0,
          1 /
            true_candidate_rank
        ),

      .groups =
        "drop"
    )
}


summarise_rf_top_candidate_assignments <- function(
  candidate_pairs,
  match_score
) {
  required_columns <-
    c(
      "scenario",
      "source",
      "truth_firm_id",
      "source_record_id",
      "register_id",
      "canonical_firm_id",
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
      "RF assignment candidate pairs are missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }

  if (
    length(
      match_score
    ) !=
      nrow(
        candidate_pairs
      )
  ) {
    stop(
      "RF match-score vector does not match candidate-pair rows."
    )
  }

  if (
    anyNA(
      match_score
    ) ||
      any(
        !is.finite(
          match_score
        )
      ) ||
      any(
        match_score < 0 |
          match_score > 1
      )
  ) {
    stop(
      "RF match scores must be finite values in [0, 1]."
    )
  }

  candidate_pairs %>%
    dplyr::mutate(
      rf_match_score =
        match_score
    ) %>%
    dplyr::arrange(
      .data$scenario,
      .data$source,
      .data$source_record_id,
      dplyr::desc(
        .data$rf_match_score
      ),
      .data$register_id
    ) %>%
    dplyr::group_by(
      .data$scenario,
      .data$source,
      .data$source_record_id
    ) %>%
    dplyr::summarise(
      truth_firm_id =
        dplyr::first(
          .data$truth_firm_id
        ),

      candidate_count =
        dplyr::n(),

      top_candidate_register_id =
        dplyr::first(
          .data$register_id
        ),

      top_candidate_canonical_firm_id =
        dplyr::first(
          .data$canonical_firm_id
        ),

      top_match_score =
        dplyr::first(
          .data$rf_match_score
        ),

      top_candidate_correct =
        dplyr::first(
          .data$is_true_candidate
        ),

      .groups =
        "drop"
    )
}


build_rf_top_candidate_assignments <- function(
  fitted_model,
  candidate_pairs,
  num_threads = 2L
) {
  match_score <-
    predict_rf_match_probability(
      fitted_model,
      candidate_pairs,
      num_threads =
        num_threads
    )

  summarise_rf_top_candidate_assignments(
    candidate_pairs =
      candidate_pairs,
    match_score =
      match_score
  )
}


evaluate_rf_cv_configuration <- function(
  candidate_pairs_cv,
  config_id,
  mtry,
  min_node_size,
  num_trees = 200L,
  seed = 202606L,
  num_threads = 2L
) {
  fold_ids <-
    sort(
      unique(
        candidate_pairs_cv$cv_fold
      )
    )

  record_results <-
    vector(
      "list",
      length(
        fold_ids
      )
    )

  weight_results <-
    vector(
      "list",
      length(
        fold_ids
      )
    )

  for (
    index in
      seq_along(
        fold_ids
      )
  ) {
    fold_id <-
      fold_ids[[index]]

    message(
      sprintf(
        "%s: fold %d/%d",
        config_id,
        index,
        length(
          fold_ids
        )
      )
    )

    training_pairs <-
      candidate_pairs_cv %>%
      dplyr::filter(
        .data$cv_fold !=
          fold_id
      )

    validation_pairs <-
      candidate_pairs_cv %>%
      dplyr::filter(
        .data$cv_fold ==
          fold_id
      )

    fitted <-
      fit_rf_candidate_model(
        training_pairs =
          training_pairs,
        mtry =
          mtry,
        min_node_size =
          min_node_size,
        num_trees =
          num_trees,
        seed =
          seed +
            as.integer(
              fold_id
            ),
        num_threads =
          num_threads
      )

    probabilities <-
      predict_rf_match_probability(
        fitted$model,
        validation_pairs,
        num_threads =
          num_threads
      )

    record_results[[index]] <-
      score_rf_candidate_records(
        validation_pairs,
        probabilities
      ) %>%
      dplyr::mutate(
        config_id =
          config_id,
        mtry =
          as.integer(
            mtry
          ),
        min_node_size =
          as.integer(
            min_node_size
          ),
        num_trees =
          as.integer(
            num_trees
          ),
        .before =
          "scenario"
      )

    weight_results[[index]] <-
      tibble::tibble(
        config_id =
          config_id,
        cv_fold =
          fold_id,
        positive_class_weight =
          unname(
            fitted$class_weights[
              "match"
            ]
          )
      )

    rm(
      fitted
    )

    gc(
      verbose =
        FALSE
    )
  }

  records <-
    dplyr::bind_rows(
      record_results
    )

  weights <-
    dplyr::bind_rows(
      weight_results
    )

  overall <-
    records %>%
    dplyr::summarise(
      config_id =
        dplyr::first(
          .data$config_id
        ),

      mtry =
        dplyr::first(
          .data$mtry
        ),

      min_node_size =
        dplyr::first(
          .data$min_node_size
        ),

      num_trees =
        dplyr::first(
          .data$num_trees
        ),

      source_records =
        dplyr::n(),

      true_candidate_present =
        sum(
          .data$true_candidate_present
        ),

      top1_correct =
        sum(
          .data$top_candidate_correct
        ),

      top1_accuracy =
        mean(
          .data$top_candidate_correct
        ),

      mean_reciprocal_rank =
        mean(
          .data$reciprocal_rank
        )
    ) %>%
    dplyr::left_join(
      weights %>%
        dplyr::summarise(
          config_id =
            dplyr::first(
              .data$config_id
            ),

          mean_positive_class_weight =
            mean(
              .data$positive_class_weight
            ),

          min_positive_class_weight =
            min(
              .data$positive_class_weight
            ),

          max_positive_class_weight =
            max(
              .data$positive_class_weight
            )
        ),
      by =
        "config_id"
    )

  fold_summary <-
    records %>%
    dplyr::group_by(
      .data$config_id,
      .data$mtry,
      .data$min_node_size,
      .data$num_trees,
      .data$cv_fold
    ) %>%
    dplyr::summarise(
      source_records =
        dplyr::n(),

      top1_correct =
        sum(
          .data$top_candidate_correct
        ),

      top1_accuracy =
        mean(
          .data$top_candidate_correct
        ),

      mean_reciprocal_rank =
        mean(
          .data$reciprocal_rank
        ),

      .groups =
        "drop"
    ) %>%
    dplyr::left_join(
      weights,
      by =
        c(
          "config_id",
          "cv_fold"
        )
    )

  list(
    summary =
      overall,
    fold_summary =
      fold_summary,
    record_metrics =
      records
  )
}


run_rf_cv_tuning <- function(
  candidate_pairs_cv,
  tuning_grid =
    rf_linkage_tuning_grid(),
  seed = 202606L,
  num_threads = 2L
) {
  results <-
    vector(
      "list",
      nrow(
        tuning_grid
      )
    )

  for (
    index in
      seq_len(
        nrow(
          tuning_grid
        )
      )
  ) {
    config <-
      tuning_grid[
        index,
        ,
        drop = FALSE
      ]

    message(
      sprintf(
        "RF configuration %d/%d: %s",
        index,
        nrow(
          tuning_grid
        ),
        config$config_id[[1]]
      )
    )

    results[[index]] <-
      evaluate_rf_cv_configuration(
        candidate_pairs_cv =
          candidate_pairs_cv,
        config_id =
          config$config_id[[1]],
        mtry =
          config$mtry[[1]],
        min_node_size =
          config$min_node_size[[1]],
        num_trees =
          config$num_trees[[1]],
        seed =
          seed +
            index * 100L,
        num_threads =
          num_threads
      )
  }

  list(
    summary =
      dplyr::bind_rows(
        lapply(
          results,
          `[[`,
          "summary"
        )
      ),

    fold_summary =
      dplyr::bind_rows(
        lapply(
          results,
          `[[`,
          "fold_summary"
        )
      ),

    record_metrics =
      dplyr::bind_rows(
        lapply(
          results,
          `[[`,
          "record_metrics"
        )
      )
  )
}


select_rf_configuration <- function(
  tuning_summary
) {
  required_columns <-
    c(
      "config_id",
      "mtry",
      "min_node_size",
      "num_trees",
      "top1_accuracy",
      "mean_reciprocal_rank"
    )

  missing_columns <-
    setdiff(
      required_columns,
      names(
        tuning_summary
      )
    )

  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      "RF tuning summary is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }

  tuning_summary %>%
    dplyr::arrange(
      dplyr::desc(
        .data$top1_accuracy
      ),
      dplyr::desc(
        .data$mean_reciprocal_rank
      ),
      .data$mtry,
      dplyr::desc(
        .data$min_node_size
      )
    ) %>%
    dplyr::slice_head(
      n =
        1L
    ) %>%
    dplyr::mutate(
      selection_rule =
        paste(
          "maximize grouped-CV top-1 accuracy;",
          "then mean reciprocal rank;",
          "tie-break by lower mtry and larger min.node.size"
        )
    )
}
