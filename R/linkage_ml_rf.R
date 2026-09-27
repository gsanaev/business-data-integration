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
