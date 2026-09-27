# =====================================================================
# evaluation.R
# Helpers for reproducible development and held-out evaluation splits
# =====================================================================

create_enterprise_split <- function(
  truth_firm_id,
  development_share = 0.70,
  seed = 202604L
) {
  if (
    length(development_share) != 1L ||
      !is.numeric(development_share) ||
      !is.finite(development_share) ||
      development_share <= 0 ||
      development_share >= 1
  ) {
    stop(
      "development_share must lie strictly between 0 and 1."
    )
  }

  if (
    length(seed) != 1L ||
      !is.numeric(seed) ||
      !is.finite(seed)
  ) {
    stop(
      "seed must be a finite numeric scalar."
    )
  }

  ids <-
    sort(
      unique(
        as.character(
          truth_firm_id
        )
      )
    )

  if (
    length(ids) == 0L ||
      anyNA(ids) ||
      any(!nzchar(ids))
  ) {
    stop(
      "truth_firm_id must contain non-missing, non-empty identifiers."
    )
  }

  n_development <-
    floor(
      length(ids) *
        development_share
    )

  had_seed <-
    exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )

  if (had_seed) {
    previous_seed <-
      get(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )
  }

  on.exit(
    {
      if (had_seed) {
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
    as.integer(seed)
  )

  development_ids <-
    sample(
      ids,
      size = n_development,
      replace = FALSE
    )

  tibble::tibble(
    truth_firm_id = ids,
    sample_role =
      ifelse(
        ids %in% development_ids,
        "development",
        "heldout"
      )
  )
}


validate_enterprise_split <- function(
  split,
  truth_firm_id,
  development_share = 0.70
) {
  required_columns <- c(
    "truth_firm_id",
    "sample_role"
  )

  missing_columns <-
    setdiff(
      required_columns,
      names(split)
    )

  if (
    length(missing_columns) > 0L
  ) {
    stop(
      "Enterprise split is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }

  expected_ids <-
    sort(
      unique(
        as.character(
          truth_firm_id
        )
      )
    )

  if (
    nrow(split) !=
      length(expected_ids)
  ) {
    stop(
      "Enterprise split must contain exactly one row per enterprise."
    )
  }

  if (
    anyDuplicated(
      split$truth_firm_id
    )
  ) {
    stop(
      "Enterprise split contains duplicate truth_firm_id values."
    )
  }

  if (
    !setequal(
      split$truth_firm_id,
      expected_ids
    )
  ) {
    stop(
      "Enterprise split does not match the canonical enterprise universe."
    )
  }

  allowed_roles <- c(
    "development",
    "heldout"
  )

  if (
    anyNA(
      split$sample_role
    ) ||
      any(
        !split$sample_role %in%
          allowed_roles
      )
  ) {
    stop(
      "Enterprise split contains invalid sample roles."
    )
  }

  expected_development <-
    floor(
      length(expected_ids) *
        development_share
    )

  observed_development <-
    sum(
      split$sample_role ==
        "development"
    )

  if (
    observed_development !=
      expected_development
  ) {
    stop(
      "Enterprise split has an unexpected development-sample size."
    )
  }

  invisible(TRUE)
}


# =====================================================================
# Scenario calibration
# =====================================================================

scenario_corruption_columns <- function() {
  c(
    "missing_business_id",
    "invalid_or_unknown_business_id",
    "additional_name_typo",
    "substantial_name_degradation",
    "strong_street_discrepancy",
    "postal_code_missing_or_error",
    "nace_disagreement",
    "legal_form_disagreement"
  )
}


summarise_scenario_corruption_rates <- function(
  corruption_log,
  sample_role = "development"
) {
  flag_columns <-
    scenario_corruption_columns()

  corruption_log %>%
    dplyr::filter(
      .data$sample_role ==
        .env$sample_role
    ) %>%
    dplyr::group_by(
      .data$scenario,
      .data$source
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(
          flag_columns
        ),
        mean
      ),
      .groups = "drop"
    )
}


summarise_scenario_corruption_counts <- function(
  corruption_log,
  sample_role = "development"
) {
  flag_columns <-
    scenario_corruption_columns()

  corruption_log %>%
    dplyr::filter(
      .data$sample_role ==
        .env$sample_role
    ) %>%
    dplyr::mutate(
      corruption_count =
        rowSums(
          dplyr::across(
            dplyr::all_of(
              flag_columns
            )
          )
        )
    ) %>%
    dplyr::count(
      .data$scenario,
      .data$source,
      .data$corruption_count,
      name = "n"
    ) %>%
    dplyr::group_by(
      .data$scenario,
      .data$source
    ) %>%
    dplyr::mutate(
      share =
        .data$n /
          sum(
            .data$n
          )
    ) %>%
    dplyr::ungroup()
}


extract_calibration_source_entities <- function(
  source_data,
  source_id_column
) {
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
    )
}


prepare_calibration_register_entities <- function(
  register_data
) {
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
}


evaluate_candidate_generation <- function(
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
    extract_calibration_source_entities(
      source_data,
      source_id_column
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
    prepare_calibration_register_entities(
      register_data
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
    nrow(unresolved) == 0L
  ) {
    return(
      tibble::tibble(
        scenario = character(),
        source = character(),
        truth_firm_id = character(),
        source_record_id = character(),
        business_id = character(),
        candidate_count = integer(),
        true_candidate_present = logical()
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

  candidates <-
    generate_linkage_candidates(
      source_for_matching,
      register_for_matching
    )

  candidate_diagnostics <-
    candidates %>%
    dplyr::left_join(
      unresolved %>%
        dplyr::select(
          source_record_id,
          true_register_id
        ),
      by = "source_record_id"
    ) %>%
    dplyr::group_by(
      .data$source_record_id
    ) %>%
    dplyr::summarise(
      candidate_count =
        dplyr::n(),
      true_candidate_present =
        any(
          .data$register_id ==
            .data$true_register_id
        ),
      .groups = "drop"
    )

  unresolved %>%
    dplyr::select(
      truth_firm_id,
      source_record_id,
      business_id
    ) %>%
    dplyr::left_join(
      candidate_diagnostics,
      by = "source_record_id"
    ) %>%
    dplyr::mutate(
      candidate_count =
        dplyr::coalesce(
          .data$candidate_count,
          0L
        ),
      true_candidate_present =
        dplyr::coalesce(
          .data$true_candidate_present,
          FALSE
        ),
      scenario =
        scenario_name,
      source =
        source_name,
      .before =
        "truth_firm_id"
    )
}


summarise_candidate_generation <- function(
  candidate_records
) {
  candidate_records %>%
    dplyr::group_by(
      .data$scenario,
      .data$source
    ) %>%
    dplyr::summarise(
      unresolved_records =
        dplyr::n(),

      zero_candidate_records =
        sum(
          .data$candidate_count == 0L
        ),

      mean_candidates =
        mean(
          .data$candidate_count
        ),

      median_candidates =
        stats::median(
          .data$candidate_count
        ),

      p95_candidates =
        as.numeric(
          stats::quantile(
            .data$candidate_count,
            probs = 0.95,
            names = FALSE,
            type = 7
          )
        ),

      max_candidates =
        max(
          .data$candidate_count
        ),

      candidate_recall =
        mean(
          .data$true_candidate_present
        ),

      .groups = "drop"
    )
}


# =====================================================================
# Weighted-similarity benchmark evidence
# =====================================================================

build_similarity_benchmark_records <- function(
  source_data,
  source_id_column,
  register_data,
  enterprise_split,
  scenario_name,
  source_name,
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
  source_entities <-
    extract_calibration_source_entities(
      source_data,
      source_id_column
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
    prepare_calibration_register_entities(
      register_data
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
    nrow(unresolved) == 0L
  ) {
    return(
      tibble::tibble(
        scenario = character(),
        source = character(),
        truth_firm_id = character(),
        source_record_id = character(),
        business_id = character(),
        identifier_issue = character(),
        true_register_id = character(),
        candidate_count = integer(),
        true_candidate_present = logical(),
        true_candidate_rank = integer(),
        true_candidate_score = double(),
        top_candidate_register_id = character(),
        top_candidate_canonical_firm_id = character(),
        top_similarity_score = double(),
        second_similarity_score = double(),
        similarity_margin = double(),
        top_candidate_correct = logical()
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

  ranked_candidates <-
    generate_linkage_candidates(
      source_for_matching,
      register_for_matching
    ) %>%
    add_linkage_features() %>%
    score_and_rank_similarity_candidates(
      similarity_weights
    ) %>%
    dplyr::left_join(
      unresolved %>%
        dplyr::select(
          source_record_id,
          true_register_id
        ),
      by = "source_record_id"
    ) %>%
    dplyr::mutate(
      is_true_candidate =
        .data$register_id ==
          .data$true_register_id
    )

  candidate_evidence <-
    ranked_candidates %>%
    dplyr::group_by(
      .data$source_record_id
    ) %>%
    dplyr::summarise(
      candidate_count =
        dplyr::n(),

      true_candidate_present =
        any(
          .data$is_true_candidate
        ),

      true_candidate_rank = {
        index <-
          which(
            .data$is_true_candidate
          )

        if (
          length(index) > 0L
        ) {
          .data$candidate_rank[
            index[1]
          ]
        } else {
          NA_integer_
        }
      },

      true_candidate_score = {
        index <-
          which(
            .data$is_true_candidate
          )

        if (
          length(index) > 0L
        ) {
          .data$similarity_score[
            index[1]
          ]
        } else {
          NA_real_
        }
      },

      top_candidate_register_id =
        dplyr::first(
          .data$register_id
        ),

      top_candidate_canonical_firm_id =
        dplyr::first(
          .data$canonical_firm_id
        ),

      top_similarity_score =
        dplyr::first(
          .data$similarity_score
        ),

      second_similarity_score =
        if (
          dplyr::n() >= 2L
        ) {
          dplyr::nth(
            .data$similarity_score,
            2
          )
        } else {
          NA_real_
        },

      similarity_margin =
        ifelse(
          is.na(
            second_similarity_score
          ),
          top_similarity_score,
          top_similarity_score -
            second_similarity_score
        ),

      top_candidate_correct =
        dplyr::first(
          .data$is_true_candidate
        ),

      .groups = "drop"
    )

  unresolved %>%
    dplyr::select(
      truth_firm_id,
      source_record_id,
      business_id,
      true_register_id
    ) %>%
    dplyr::left_join(
      candidate_evidence,
      by = "source_record_id"
    ) %>%
    dplyr::mutate(
      identifier_issue =
        dplyr::if_else(
          is.na(
            .data$business_id
          ),
          "missing_identifier",
          "identifier_not_found"
        ),

      candidate_count =
        dplyr::coalesce(
          .data$candidate_count,
          0L
        ),

      true_candidate_present =
        dplyr::coalesce(
          .data$true_candidate_present,
          FALSE
        ),

      top_candidate_correct =
        dplyr::coalesce(
          .data$top_candidate_correct,
          FALSE
        ),

      scenario =
        scenario_name,

      source =
        source_name,

      .before =
        "truth_firm_id"
    )
}


summarise_similarity_benchmark <- function(
  benchmark_records
) {
  benchmark_records %>%
    dplyr::group_by(
      .data$scenario,
      .data$source
    ) %>%
    dplyr::summarise(
      unresolved_records =
        dplyr::n(),

      missing_identifier =
        sum(
          .data$identifier_issue ==
            "missing_identifier"
        ),

      identifier_not_found =
        sum(
          .data$identifier_issue ==
            "identifier_not_found"
        ),

      candidate_recall =
        mean(
          .data$true_candidate_present
        ),

      top1_accuracy =
        mean(
          .data$top_candidate_correct
        ),

      scorable_top_rate =
        mean(
          !is.na(
            .data$top_similarity_score
          )
        ),

      median_top_score =
        if (
          all(
            is.na(
              .data$top_similarity_score
            )
          )
        ) {
          NA_real_
        } else {
          stats::median(
            .data$top_similarity_score,
            na.rm = TRUE
          )
        },

      median_margin =
        if (
          all(
            is.na(
              .data$similarity_margin
            )
          )
        ) {
          NA_real_
        } else {
          stats::median(
            .data$similarity_margin,
            na.rm = TRUE
          )
        },

      .groups = "drop"
    )
}


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

# =====================================================================
# Stage 10B final held-out evaluation helpers
# =====================================================================

complete_rf_evaluation_records <- function(
  rf_records,
  reference_records
) {
  key_columns <-
    c(
      "scenario",
      "source",
      "truth_firm_id",
      "source_record_id"
    )

  required_rf_columns <-
    c(
      key_columns,
      "candidate_count",
      "true_candidate_present",
      "top_candidate_correct",
      "top_probability",
      "second_probability",
      "probability_margin",
      "true_candidate_rank",
      "reciprocal_rank"
    )

  missing_rf_columns <-
    setdiff(
      required_rf_columns,
      names(
        rf_records
      )
    )

  if (
    length(
      missing_rf_columns
    ) > 0L
  ) {
    stop(
      "RF evaluation records are missing required columns: ",
      paste(
        missing_rf_columns,
        collapse = ", "
      )
    )
  }

  missing_reference_columns <-
    setdiff(
      key_columns,
      names(
        reference_records
      )
    )

  if (
    length(
      missing_reference_columns
    ) > 0L
  ) {
    stop(
      "RF evaluation reference is missing required columns: ",
      paste(
        missing_reference_columns,
        collapse = ", "
      )
    )
  }

  duplicate_reference_keys <-
    reference_records %>%
    dplyr::count(
      dplyr::across(
        dplyr::all_of(
          key_columns
        )
      ),
      name =
        "key_count"
    ) %>%
    dplyr::filter(
      .data$key_count >
        1L
    )

  if (
    nrow(
      duplicate_reference_keys
    ) > 0L
  ) {
    stop(
      "RF evaluation reference contains duplicate source-record keys."
    )
  }

  duplicate_rf_keys <-
    rf_records %>%
    dplyr::count(
      dplyr::across(
        dplyr::all_of(
          key_columns
        )
      ),
      name =
        "key_count"
    ) %>%
    dplyr::filter(
      .data$key_count >
        1L
    )

  if (
    nrow(
      duplicate_rf_keys
    ) > 0L
  ) {
    stop(
      "RF evaluation records contain duplicate source-record keys."
    )
  }

  if (
    !"cv_fold" %in%
      names(
        rf_records
      )
  ) {
    rf_records <-
      rf_records %>%
      dplyr::mutate(
        cv_fold =
          NA_integer_
      )
  }

  reference_keys <-
    reference_records %>%
    dplyr::select(
      dplyr::all_of(
        key_columns
      )
    )

  result <-
    reference_keys %>%
    dplyr::left_join(
      rf_records,
      by =
        key_columns
    )

  if (
    nrow(
      result
    ) !=
      nrow(
        reference_keys
      )
  ) {
    stop(
      "RF evaluation completion changed the source-record count."
    )
  }

  result %>%
    dplyr::mutate(
      candidate_count =
        dplyr::coalesce(
          as.integer(
            .data$candidate_count
          ),
          0L
        ),

      true_candidate_present =
        dplyr::coalesce(
          .data$true_candidate_present,
          FALSE
        ),

      top_candidate_correct =
        dplyr::coalesce(
          .data$top_candidate_correct,
          FALSE
        ),

      top_probability =
        dplyr::coalesce(
          .data$top_probability,
          0
        ),

      probability_margin =
        dplyr::coalesce(
          .data$probability_margin,
          0
        ),

      reciprocal_rank =
        dplyr::coalesce(
          .data$reciprocal_rank,
          0
        )
    )
}


summarise_rf_benchmark <- function(
  rf_records
) {
  rf_records %>%
    dplyr::group_by(
      .data$scenario,
      .data$source
    ) %>%
    dplyr::summarise(
      unresolved_records =
        dplyr::n(),

      candidate_recall =
        mean(
          .data$true_candidate_present
        ),

      top1_accuracy =
        mean(
          .data$top_candidate_correct
        ),

      scorable_top_rate =
        mean(
          .data$candidate_count >
            0L
        ),

      median_top_match_score =
        if (
          all(
            .data$candidate_count ==
              0L
          )
        ) {
          NA_real_
        } else {
          stats::median(
            .data$top_probability[
              .data$candidate_count >
                0L
            ],
            na.rm = TRUE
          )
        },

      median_margin =
        if (
          all(
            .data$candidate_count ==
              0L
          )
        ) {
          NA_real_
        } else {
          stats::median(
            .data$probability_margin[
              .data$candidate_count >
                0L
            ],
            na.rm = TRUE
          )
        },

      .groups =
        "drop"
    )
}


build_linkage_method_comparison <- function(
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


build_linkage_method_comparison_by_group <- function(
  similarity_records,
  similarity_policy,
  rf_records,
  rf_policy,
  group_column
) {
  if (
    length(
      group_column
    ) !=
      1L ||
      !is.character(
        group_column
      ) ||
      !nzchar(
        group_column
      )
  ) {
    stop(
      "group_column must be one non-empty column name."
    )
  }

  if (
    !group_column %in%
      names(
        similarity_records
      ) ||
      !group_column %in%
        names(
          rf_records
        )
  ) {
    stop(
      "Grouping column is missing from linkage-evaluation records."
    )
  }

  similarity_groups <-
    sort(
      unique(
        as.character(
          similarity_records[[group_column]]
        )
      )
    )

  rf_groups <-
    sort(
      unique(
        as.character(
          rf_records[[group_column]]
        )
      )
    )

  if (
    !identical(
      similarity_groups,
      rf_groups
    )
  ) {
    stop(
      "Similarity and RF records do not contain the same evaluation groups."
    )
  }

  results <-
    lapply(
      similarity_groups,
      function(
        group_value
      ) {
        similarity_subset <-
          similarity_records[
            as.character(
              similarity_records[[group_column]]
            ) ==
              group_value,
            ,
            drop = FALSE
          ]

        rf_subset <-
          rf_records[
            as.character(
              rf_records[[group_column]]
            ) ==
              group_value,
            ,
            drop = FALSE
          ]

        result <-
          build_linkage_method_comparison(
            similarity_records =
              similarity_subset,
            similarity_policy =
              similarity_policy,
            rf_records =
              rf_subset,
            rf_policy =
              rf_policy
          )

        result[[group_column]] <-
          group_value

        result %>%
          dplyr::relocate(
            dplyr::all_of(
              group_column
            ),
            .before =
              method
          )
      }
    )

  dplyr::bind_rows(
    results
  )
}


# =====================================================================
# Stage 10B complete held-out linkage workflow
# =====================================================================

build_complete_linkage_evaluation_records <- function(
  source_data,
  source_id_column,
  register_data,
  enterprise_split,
  scenario_name,
  source_name,
  similarity_records,
  similarity_policy,
  rf_records,
  rf_assignments,
  rf_policy,
  sample_role = "heldout"
) {
  required_source_columns <-
    c(
      "truth_firm_id",
      source_id_column,
      "business_id"
    )

  missing_source_columns <-
    setdiff(
      required_source_columns,
      names(
        source_data
      )
    )

  if (
    length(
      missing_source_columns
    ) > 0L
  ) {
    stop(
      "Source data are missing required linkage-evaluation columns: ",
      paste(
        missing_source_columns,
        collapse = ", "
      )
    )
  }

  source_entities <-
    source_data %>%
    dplyr::transmute(
      truth_firm_id =
        .data$truth_firm_id,
      source_record_id =
        .data[[source_id_column]],
      business_id =
        .data$business_id
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      enterprise_split %>%
        dplyr::filter(
          .data$sample_role ==
            .env$sample_role
        ),
      by =
        "truth_firm_id"
    ) %>%
    dplyr::mutate(
      scenario =
        scenario_name,
      source =
        source_name
    )

  duplicate_truth_ids <-
    source_entities %>%
    dplyr::count(
      .data$truth_firm_id
    ) %>%
    dplyr::filter(
      .data$n >
        1L
    )

  duplicate_source_ids <-
    source_entities %>%
    dplyr::count(
      .data$source_record_id
    ) %>%
    dplyr::filter(
      .data$n >
        1L
    )

  if (
    nrow(
      duplicate_truth_ids
    ) > 0L ||
      nrow(
        duplicate_source_ids
      ) > 0L
  ) {
    stop(
      "Complete linkage evaluation requires one source entity per truth and source identifier."
    )
  }

  register_reference <-
    prepare_register_linkage_reference(
      register_data
    )

  register_entities <-
    register_reference$entities

  truth_map <-
    register_entities %>%
    dplyr::transmute(
      truth_firm_id =
        .data$truth_firm_id,
      true_register_id =
        .data$register_id,
      true_canonical_firm_id =
        .data$canonical_firm_id
    )

  deterministic_lookup <-
    register_entities %>%
    dplyr::transmute(
      business_id =
        .data$business_id,
      level1_truth_firm_id =
        .data$truth_firm_id,
      level1_register_id =
        .data$register_id,
      level1_canonical_firm_id =
        .data$canonical_firm_id
    )

  base_records <-
    source_entities %>%
    dplyr::left_join(
      truth_map,
      by =
        "truth_firm_id"
    ) %>%
    dplyr::left_join(
      deterministic_lookup,
      by =
        "business_id"
    ) %>%
    dplyr::mutate(
      level1_resolved =
        !is.na(
          .data$level1_register_id
        ),

      identifier_issue =
        dplyr::case_when(
          .data$level1_resolved ~
            "none",

          is.na(
            .data$business_id
          ) ~
            "missing_identifier",

          TRUE ~
            "identifier_not_found"
        )
    )

  if (
    anyNA(
      base_records$true_register_id
    ) ||
      anyNA(
        base_records$true_canonical_firm_id
      )
  ) {
    stop(
      "True register identifiers are missing from complete linkage evaluation."
    )
  }

  unresolved <-
    base_records %>%
    dplyr::filter(
      !.data$level1_resolved
    )

  key_columns <-
    c(
      "scenario",
      "source",
      "truth_firm_id",
      "source_record_id"
    )

  similarity_subset <-
    similarity_records %>%
    dplyr::filter(
      .data$scenario ==
        scenario_name,
      .data$source ==
        source_name
    ) %>%
    dplyr::select(
      dplyr::all_of(
        key_columns
      ),
      candidate_count,
      top_candidate_register_id,
      top_candidate_canonical_firm_id,
      top_similarity_score,
      similarity_margin
    )

  rf_subset <-
    rf_records %>%
    dplyr::filter(
      .data$scenario ==
        scenario_name,
      .data$source ==
        source_name
    ) %>%
    dplyr::select(
      dplyr::all_of(
        key_columns
      ),
      candidate_count,
      top_probability,
      probability_margin
    )

  rf_assignment_subset <-
    rf_assignments %>%
    dplyr::filter(
      .data$scenario ==
        scenario_name,
      .data$source ==
        source_name
    ) %>%
    dplyr::select(
      dplyr::all_of(
        key_columns
      ),
      top_candidate_register_id,
      top_candidate_canonical_firm_id
    )

  unresolved_keys <-
    unresolved %>%
    dplyr::select(
      dplyr::all_of(
        key_columns
      )
    )

  if (
    nrow(
      similarity_subset
    ) !=
      nrow(
        unresolved
      ) ||
      nrow(
        dplyr::anti_join(
          unresolved_keys,
          similarity_subset,
          by =
            key_columns
        )
      ) > 0L
  ) {
    stop(
      "Similarity records do not cover all unresolved source entities."
    )
  }

  if (
    nrow(
      rf_subset
    ) !=
      nrow(
        unresolved
      ) ||
      nrow(
        dplyr::anti_join(
          unresolved_keys,
          rf_subset,
          by =
            key_columns
        )
      ) > 0L
  ) {
    stop(
      "RF records do not cover all unresolved source entities."
    )
  }

  build_level1_records <-
    function(
      method_name
    ) {
      base_records %>%
        dplyr::filter(
          .data$level1_resolved
        ) %>%
        dplyr::transmute(
          scenario,
          source,
          truth_firm_id,
          source_record_id,
          business_id,
          true_register_id,
          true_canonical_firm_id,
          method =
            method_name,
          decision_stage =
            "level1",
          decision_status =
            "auto_link",
          linkage_method =
            "business_id_exact",
          identifier_issue,
          candidate_count =
            NA_integer_,
          proposed_register_id =
            .data$level1_register_id,
          proposed_canonical_firm_id =
            .data$level1_canonical_firm_id,
          assigned_register_id =
            .data$level1_register_id,
          assigned_canonical_firm_id =
            .data$level1_canonical_firm_id,
          top_score =
            NA_real_,
          margin =
            NA_real_,
          automatic_link =
            TRUE,
          assignment_correct =
            .data$level1_register_id ==
              .data$true_register_id
        )
    }

  similarity_threshold <-
    similarity_policy$score_threshold[[1]]

  similarity_margin_threshold <-
    similarity_policy$margin_threshold[[1]]

  similarity_level2 <-
    unresolved %>%
    dplyr::select(
      dplyr::all_of(
        c(
          key_columns,
          "business_id",
          "true_register_id",
          "true_canonical_firm_id",
          "identifier_issue"
        )
      )
    ) %>%
    dplyr::left_join(
      similarity_subset,
      by =
        key_columns
    ) %>%
    dplyr::mutate(
      candidate_count =
        dplyr::coalesce(
          as.integer(
            .data$candidate_count
          ),
          0L
        ),

      top_score =
        dplyr::coalesce(
          .data$top_similarity_score,
          -Inf
        ),

      margin =
        dplyr::coalesce(
          .data$similarity_margin,
          0
        ),

      decision_status =
        dplyr::case_when(
          .data$candidate_count >
            0L &
            .data$top_score >=
              similarity_threshold &
            .data$margin >=
              similarity_margin_threshold ~
            "auto_link",

          .data$candidate_count >
            0L &
            .data$top_score >=
              similarity_threshold ~
            "review",

          TRUE ~
            "unmatched"
        ),

      automatic_link =
        .data$decision_status ==
          "auto_link",

      proposed_register_id =
        .data$top_candidate_register_id,

      proposed_canonical_firm_id =
        .data$top_candidate_canonical_firm_id,

      assigned_register_id =
        dplyr::if_else(
          .data$automatic_link,
          .data$proposed_register_id,
          NA_character_
        ),

      assigned_canonical_firm_id =
        dplyr::if_else(
          .data$automatic_link,
          .data$proposed_canonical_firm_id,
          NA_character_
        ),

      assignment_correct =
        dplyr::if_else(
          .data$automatic_link,
          .data$assigned_register_id ==
            .data$true_register_id,
          NA
        )
    ) %>%
    dplyr::transmute(
      scenario,
      source,
      truth_firm_id,
      source_record_id,
      business_id,
      true_register_id,
      true_canonical_firm_id,
      method =
        "weighted_similarity",
      decision_stage =
        "level2",
      decision_status,
      linkage_method =
        "weighted_edit_similarity",
      identifier_issue,
      candidate_count,
      proposed_register_id,
      proposed_canonical_firm_id,
      assigned_register_id,
      assigned_canonical_firm_id,
      top_score,
      margin,
      automatic_link,
      assignment_correct
    )

  rf_threshold <-
    rf_policy$probability_threshold[[1]]

  rf_margin_threshold <-
    rf_policy$margin_threshold[[1]]

  rf_level2 <-
    unresolved %>%
    dplyr::select(
      dplyr::all_of(
        c(
          key_columns,
          "business_id",
          "true_register_id",
          "true_canonical_firm_id",
          "identifier_issue"
        )
      )
    ) %>%
    dplyr::left_join(
      rf_subset,
      by =
        key_columns
    ) %>%
    dplyr::left_join(
      rf_assignment_subset,
      by =
        key_columns
    ) %>%
    dplyr::mutate(
      candidate_count =
        dplyr::coalesce(
          as.integer(
            .data$candidate_count
          ),
          0L
        ),

      top_score =
        dplyr::coalesce(
          .data$top_probability,
          -Inf
        ),

      margin =
        dplyr::coalesce(
          .data$probability_margin,
          0
        )
    )

  if (
    any(
      rf_level2$candidate_count >
        0L &
        is.na(
          rf_level2$top_candidate_register_id
        )
    )
  ) {
    stop(
      "RF candidate assignments are missing for records with candidates."
    )
  }

  rf_level2 <-
    rf_level2 %>%
    dplyr::mutate(
      decision_status =
        dplyr::case_when(
          .data$candidate_count >
            0L &
            .data$top_score >=
              rf_threshold &
            .data$margin >=
              rf_margin_threshold ~
            "auto_link",

          .data$candidate_count >
            0L &
            .data$top_score >=
              rf_threshold ~
            "review",

          TRUE ~
            "unmatched"
        ),

      automatic_link =
        .data$decision_status ==
          "auto_link",

      proposed_register_id =
        .data$top_candidate_register_id,

      proposed_canonical_firm_id =
        .data$top_candidate_canonical_firm_id,

      assigned_register_id =
        dplyr::if_else(
          .data$automatic_link,
          .data$proposed_register_id,
          NA_character_
        ),

      assigned_canonical_firm_id =
        dplyr::if_else(
          .data$automatic_link,
          .data$proposed_canonical_firm_id,
          NA_character_
        ),

      assignment_correct =
        dplyr::if_else(
          .data$automatic_link,
          .data$assigned_register_id ==
            .data$true_register_id,
          NA
        )
    ) %>%
    dplyr::transmute(
      scenario,
      source,
      truth_firm_id,
      source_record_id,
      business_id,
      true_register_id,
      true_canonical_firm_id,
      method =
        "random_forest",
      decision_stage =
        "level2",
      decision_status,
      linkage_method =
        "random_forest",
      identifier_issue,
      candidate_count,
      proposed_register_id,
      proposed_canonical_firm_id,
      assigned_register_id,
      assigned_canonical_firm_id,
      top_score,
      margin,
      automatic_link,
      assignment_correct
    )

  result <-
    dplyr::bind_rows(
      build_level1_records(
        "weighted_similarity"
      ),
      similarity_level2,
      build_level1_records(
        "random_forest"
      ),
      rf_level2
    ) %>%
    dplyr::arrange(
      .data$method,
      .data$scenario,
      .data$source,
      .data$truth_firm_id
    )

  expected_rows <-
    2L *
      nrow(
        base_records
      )

  if (
    nrow(
      result
    ) !=
      expected_rows
  ) {
    stop(
      "Complete linkage evaluation changed the expected record count."
    )
  }

  result
}


summarise_complete_linkage_evaluation <- function(
  records,
  group_columns = character()
) {
  required_columns <-
    c(
      group_columns,
      "method",
      "decision_stage",
      "decision_status",
      "automatic_link",
      "assignment_correct"
    )

  missing_columns <-
    setdiff(
      required_columns,
      names(
        records
      )
    )

  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      "Complete linkage records are missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }

  result <-
    records %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(
            group_columns,
            "method"
          )
        )
      )
    ) %>%
    dplyr::summarise(
      total_records =
        dplyr::n(),

      level1_deterministic_links =
        sum(
          .data$decision_stage ==
            "level1"
        ),

      level2_records =
        sum(
          .data$decision_stage ==
            "level2"
        ),

      level2_auto_links =
        sum(
          .data$decision_stage ==
            "level2" &
            .data$automatic_link
        ),

      automatic_links =
        sum(
          .data$automatic_link
        ),

      correct_automatic_links =
        sum(
          .data$automatic_link &
            dplyr::coalesce(
              .data$assignment_correct,
              FALSE
            )
        ),

      false_automatic_links =
        sum(
          .data$automatic_link &
            !dplyr::coalesce(
              .data$assignment_correct,
              FALSE
            )
        ),

      review_records =
        sum(
          .data$decision_status ==
            "review"
        ),

      unmatched_records =
        sum(
          .data$decision_status ==
            "unmatched"
        ),

      .groups =
        "drop"
    ) %>%
    dplyr::mutate(
      level1_resolution_rate =
        .data$level1_deterministic_links /
          .data$total_records,

      level2_share =
        .data$level2_records /
          .data$total_records,

      auto_precision =
        dplyr::if_else(
          .data$automatic_links >
            0L,
          .data$correct_automatic_links /
            .data$automatic_links,
          NA_real_
        ),

      automation_rate =
        .data$automatic_links /
          .data$total_records,

      correct_automatic_resolution_rate =
        .data$correct_automatic_links /
          .data$total_records,

      review_rate =
        .data$review_records /
          .data$total_records,

      unmatched_rate =
        .data$unmatched_records /
          .data$total_records
    )

  result %>%
    dplyr::relocate(
      dplyr::all_of(
        group_columns
      ),
      "method"
    )
}
