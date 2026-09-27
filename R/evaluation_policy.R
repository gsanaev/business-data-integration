# =====================================================================
# Similarity decision-policy selection
# =====================================================================

evaluate_similarity_policy <- function(
  benchmark_records,
  score_threshold,
  margin_threshold
) {
  has_candidates <-
    if (
      "candidate_count" %in%
        names(
          benchmark_records
        )
    ) {
      benchmark_records$candidate_count >
        0L
    } else {
      !is.na(
        benchmark_records$top_similarity_score
      )
    }

  top_score <-
    dplyr::coalesce(
      benchmark_records$top_similarity_score,
      -Inf
    )

  margin <-
    dplyr::coalesce(
      benchmark_records$similarity_margin,
      0
    )

  top_candidate_correct <-
    dplyr::coalesce(
      benchmark_records$top_candidate_correct,
      FALSE
    )

  auto_link <-
    has_candidates &
      top_score >=
        score_threshold &
      margin >=
        margin_threshold

  review <-
    has_candidates &
      top_score >=
        score_threshold &
      margin <
        margin_threshold

  unmatched <-
    !has_candidates |
      top_score <
        score_threshold

  auto_links <-
    sum(
      auto_link
    )

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

  tibble::tibble(
    score_threshold =
      score_threshold,

    margin_threshold =
      margin_threshold,

    unresolved_records =
      nrow(
        benchmark_records
      ),

    auto_links =
      auto_links,

    correct_auto_links =
      correct_auto_links,

    false_auto_links =
      false_auto_links,

    auto_precision =
      if (
        auto_links >
          0L
      ) {
        correct_auto_links /
          auto_links
      } else {
        NA_real_
      },

    automation_rate =
      auto_links /
        nrow(
          benchmark_records
        ),

    review_records =
      sum(
        review
      ),

    review_rate =
      mean(
        review
      ),

    unmatched_records =
      sum(
        unmatched
      ),

    unmatched_rate =
      mean(
        unmatched
      )
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
  if (
    length(precision_target) != 1L ||
      !is.numeric(
        precision_target
      ) ||
      !is.finite(
        precision_target
      ) ||
      precision_target <= 0 ||
      precision_target > 1
  ) {
    stop(
      "precision_target must lie in (0, 1]."
    )
  }

  if (
    any(
      margin_grid <= 0
    )
  ) {
    stop(
      "margin_grid must contain strictly positive thresholds."
    )
  }

  results <-
    vector(
      "list",
      length(
        score_grid
      ) *
        length(
          margin_grid
        )
    )

  index <- 1L

  for (
    score_threshold in
      score_grid
  ) {
    for (
      margin_threshold in
        margin_grid
    ) {
      results[[index]] <-
        evaluate_similarity_policy(
          benchmark_records,
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


select_similarity_policy <- function(
  policy_grid,
  precision_target = 0.99
) {
  feasible <-
    policy_grid %>%
    dplyr::filter(
      !is.na(
        .data$auto_precision
      ),
      .data$auto_precision >=
        precision_target
    )

  if (
    nrow(
      feasible
    ) == 0L
  ) {
    stop(
      "No similarity policy satisfies the precision target."
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
        .data$score_threshold
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
        precision_target,

      selection_rule =
        paste(
          "precision >=",
          precision_target,
          "; maximize auto-links;",
          "tie-break by higher score then margin threshold"
        )
    )
}


# =====================================================================
# Random Forest linkage decision-policy selection
# =====================================================================

evaluate_rf_policy <- function(
  oof_records,
  probability_threshold,
  margin_threshold
) {
  has_candidates <-
    if (
      "candidate_count" %in%
        names(
          oof_records
        )
    ) {
      oof_records$candidate_count >
        0L
    } else {
      !is.na(
        oof_records$top_probability
      )
    }

  top_score <-
    dplyr::coalesce(
      oof_records$top_probability,
      -Inf
    )

  margin <-
    dplyr::coalesce(
      oof_records$probability_margin,
      0
    )

  top_candidate_correct <-
    dplyr::coalesce(
      oof_records$top_candidate_correct,
      FALSE
    )

  auto_link <-
    has_candidates &
      top_score >=
        probability_threshold &
      margin >=
        margin_threshold

  review <-
    has_candidates &
      top_score >=
        probability_threshold &
      margin <
        margin_threshold

  unmatched <-
    !has_candidates |
      top_score <
        probability_threshold

  auto_links <-
    sum(
      auto_link
    )

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

  tibble::tibble(
    probability_threshold =
      probability_threshold,

    margin_threshold =
      margin_threshold,

    unresolved_records =
      nrow(
        oof_records
      ),

    auto_links =
      auto_links,

    correct_auto_links =
      correct_auto_links,

    false_auto_links =
      false_auto_links,

    auto_precision =
      if (
        auto_links >
          0L
      ) {
        correct_auto_links /
          auto_links
      } else {
        NA_real_
      },

    automation_rate =
      auto_links /
        nrow(
          oof_records
        ),

    review_records =
      sum(
        review
      ),

    review_rate =
      mean(
        review
      ),

    unmatched_records =
      sum(
        unmatched
      ),

    unmatched_rate =
      mean(
        unmatched
      )
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
  if (
    length(
      precision_target
    ) != 1L ||
      !is.numeric(
        precision_target
      ) ||
      !is.finite(
        precision_target
      ) ||
      precision_target <= 0 ||
      precision_target > 1
  ) {
    stop(
      "precision_target must lie in (0, 1]."
    )
  }

  if (
    any(
      margin_grid <= 0
    )
  ) {
    stop(
      "margin_grid must contain strictly positive thresholds."
    )
  }

  results <-
    vector(
      "list",
      length(
        probability_grid
      ) *
        length(
          margin_grid
        )
    )

  index <-
    1L

  for (
    probability_threshold in
      probability_grid
  ) {
    for (
      margin_threshold in
        margin_grid
    ) {
      results[[index]] <-
        evaluate_rf_policy(
          oof_records,
          probability_threshold,
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


select_rf_policy <- function(
  policy_grid,
  precision_target = 0.99
) {
  feasible <-
    policy_grid %>%
    dplyr::filter(
      !is.na(
        .data$auto_precision
      ),
      .data$auto_precision >=
        precision_target
    )

  if (
    nrow(
      feasible
    ) == 0L
  ) {
    stop(
      "No RF linkage policy satisfies the precision target."
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
        .data$probability_threshold
      ),
      dplyr::desc(
        .data$margin_threshold
      )
    ) %>%
    dplyr::slice_head(
      n =
        1L
    ) %>%
    dplyr::mutate(
      precision_target =
        precision_target,

      selection_rule =
        paste(
          "precision >=",
          precision_target,
          "; maximize auto-links;",
          "tie-break by higher probability then margin threshold"
        )
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
