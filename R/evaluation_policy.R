# =====================================================================
# Linkage decision-policy evaluation and selection
# =====================================================================

validate_policy_search_inputs <- function(
  precision_target,
  margin_grid
) {
  if (
    length(precision_target) != 1L ||
      !is.numeric(precision_target) ||
      !is.finite(precision_target) ||
      precision_target <= 0 ||
      precision_target > 1
  ) {
    stop(
      "precision_target must lie in (0, 1]."
    )
  }

  if (any(margin_grid <= 0)) {
    stop(
      "margin_grid must contain strictly positive thresholds."
    )
  }

  invisible(TRUE)
}


evaluate_linkage_policy <- function(
  records,
  score_column,
  margin_column,
  score_threshold,
  margin_threshold,
  threshold_column
) {
  has_candidates <-
    if ("candidate_count" %in% names(records)) {
      records$candidate_count > 0L
    } else {
      !is.na(records[[score_column]])
    }

  top_score <-
    dplyr::coalesce(
      records[[score_column]],
      -Inf
    )

  margin <-
    dplyr::coalesce(
      records[[margin_column]],
      0
    )

  top_candidate_correct <-
    dplyr::coalesce(
      records$top_candidate_correct,
      FALSE
    )

  auto_link <-
    has_candidates &
      top_score >= score_threshold &
      margin >= margin_threshold

  review <-
    has_candidates &
      top_score >= score_threshold &
      margin < margin_threshold

  unmatched <-
    !has_candidates |
      top_score < score_threshold

  auto_links <-
    sum(auto_link)

  correct_auto_links <-
    sum(
      auto_link &
        top_candidate_correct
    )

  false_auto_links <-
    sum(
      auto_link &
        !top_candidate_correct
    )

  result <-
    tibble::tibble(
      threshold = score_threshold,
      margin_threshold = margin_threshold,
      unresolved_records = nrow(records),
      auto_links = auto_links,
      correct_auto_links = correct_auto_links,
      false_auto_links = false_auto_links,
      auto_precision =
        if (auto_links > 0L) {
          correct_auto_links / auto_links
        } else {
          NA_real_
        },
      automation_rate =
        auto_links / nrow(records),
      review_records =
        sum(review),
      review_rate =
        mean(review),
      unmatched_records =
        sum(unmatched),
      unmatched_rate =
        mean(unmatched)
    )

  names(result)[1] <-
    threshold_column

  result
}


search_linkage_policy_grid <- function(
  records,
  score_grid,
  margin_grid,
  precision_target,
  evaluator
) {
  validate_policy_search_inputs(
    precision_target,
    margin_grid
  )

  results <-
    vector(
      "list",
      length(score_grid) *
        length(margin_grid)
    )

  index <- 1L

  for (score_threshold in score_grid) {
    for (margin_threshold in margin_grid) {
      results[[index]] <-
        evaluator(
          records,
          score_threshold,
          margin_threshold
        )

      index <-
        index + 1L
    }
  }

  dplyr::bind_rows(
    results
  )
}


select_linkage_policy <- function(
  policy_grid,
  precision_target,
  threshold_column,
  no_feasible_message,
  threshold_label
) {
  feasible <-
    policy_grid %>%
    dplyr::filter(
      !is.na(
        .data$auto_precision
      ),
      .data$auto_precision >=
        .env$precision_target
    )

  if (nrow(feasible) == 0L) {
    stop(
      no_feasible_message
    )
  }

  maximum_auto_links <-
    max(
      feasible$auto_links
    )

  feasible %>%
    dplyr::filter(
      .data$auto_links ==
        maximum_auto_links
    ) %>%
    dplyr::arrange(
      dplyr::desc(
        .data[[threshold_column]]
      ),
      dplyr::desc(
        .data$margin_threshold
      )
    ) %>%
    dplyr::slice_head(
      n = 1L
    ) %>%
    dplyr::mutate(
      precision_target =
        .env$precision_target,
      selection_rule =
        paste(
          "precision >=",
          .env$precision_target,
          "; maximize auto-links;",
          paste0(
            "tie-break by higher ",
            .env$threshold_label,
            " then margin threshold"
          )
        )
    )
}


# =====================================================================
# Weighted-similarity policy
# =====================================================================

evaluate_similarity_policy <- function(
  benchmark_records,
  score_threshold,
  margin_threshold
) {
  evaluate_linkage_policy(
    records = benchmark_records,
    score_column = "top_similarity_score",
    margin_column = "similarity_margin",
    score_threshold = score_threshold,
    margin_threshold = margin_threshold,
    threshold_column = "score_threshold"
  )
}


search_similarity_policy_grid <- function(
  benchmark_records,
  precision_target = 0.99,
  score_grid =
    seq(
      0,
      1,
      by = 0.005
    ),
  margin_grid =
    seq(
      0.005,
      0.5,
      by = 0.005
    )
) {
  search_linkage_policy_grid(
    records = benchmark_records,
    score_grid = score_grid,
    margin_grid = margin_grid,
    precision_target = precision_target,
    evaluator =
      function(
        records,
        score_threshold,
        margin_threshold
      ) {
        evaluate_similarity_policy(
          records,
          score_threshold,
          margin_threshold
        )
      }
  )
}


select_similarity_policy <- function(
  policy_grid,
  precision_target = 0.99
) {
  select_linkage_policy(
    policy_grid = policy_grid,
    precision_target = precision_target,
    threshold_column = "score_threshold",
    no_feasible_message =
      "No similarity policy satisfies the precision target.",
    threshold_label = "score"
  )
}


# =====================================================================
# Random Forest policy
# =====================================================================

evaluate_rf_policy <- function(
  oof_records,
  probability_threshold,
  margin_threshold
) {
  evaluate_linkage_policy(
    records = oof_records,
    score_column = "top_probability",
    margin_column = "probability_margin",
    score_threshold = probability_threshold,
    margin_threshold = margin_threshold,
    threshold_column = "probability_threshold"
  )
}


search_rf_policy_grid <- function(
  oof_records,
  precision_target = 0.99,
  probability_grid =
    seq(
      0,
      1,
      by = 0.005
    ),
  margin_grid =
    seq(
      0.005,
      0.5,
      by = 0.005
    )
) {
  search_linkage_policy_grid(
    records = oof_records,
    score_grid = probability_grid,
    margin_grid = margin_grid,
    precision_target = precision_target,
    evaluator =
      function(
        records,
        probability_threshold,
        margin_threshold
      ) {
        evaluate_rf_policy(
          records,
          probability_threshold,
          margin_threshold
        )
      }
  )
}


select_rf_policy <- function(
  policy_grid,
  precision_target = 0.99
) {
  select_linkage_policy(
    policy_grid = policy_grid,
    precision_target = precision_target,
    threshold_column = "probability_threshold",
    no_feasible_message =
      "No RF linkage policy satisfies the precision target.",
    threshold_label = "probability"
  )
}

# =====================================================================
# Frozen development comparison
# =====================================================================

build_development_method_comparison <- function(
  similarity_records,
  similarity_policy,
  rf_records,
  rf_policy
) {
  similarity_result <-
    evaluate_similarity_policy(
      similarity_records,
      score_threshold =
        similarity_policy$score_threshold[[1]],
      margin_threshold =
        similarity_policy$margin_threshold[[1]]
    )

  rf_result <-
    evaluate_rf_policy(
      rf_records,
      probability_threshold =
        rf_policy$probability_threshold[[1]],
      margin_threshold =
        rf_policy$margin_threshold[[1]]
    )

  dplyr::bind_rows(
    similarity_result %>%
      dplyr::transmute(
        method =
          "weighted_similarity",
        unresolved_records,
        auto_links,
        correct_auto_links,
        false_auto_links,
        auto_precision,
        automation_rate,
        review_records,
        review_rate,
        unmatched_records,
        unmatched_rate
      ),

    rf_result %>%
      dplyr::transmute(
        method =
          "random_forest",
        unresolved_records,
        auto_links,
        correct_auto_links,
        false_auto_links,
        auto_precision,
        automation_rate,
        review_records,
        review_rate,
        unmatched_records,
        unmatched_rate
      )
  )
}
