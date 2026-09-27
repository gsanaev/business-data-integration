# =====================================================================
# Random Forest cross-validation and tuning
# =====================================================================

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
