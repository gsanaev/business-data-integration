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
  auto_link <-
    benchmark_records$top_similarity_score >=
      score_threshold &
    benchmark_records$similarity_margin >=
      margin_threshold

  review <-
    benchmark_records$top_similarity_score >=
      score_threshold &
    benchmark_records$similarity_margin <
      margin_threshold

  unmatched <-
    benchmark_records$top_similarity_score <
      score_threshold

  auto_links <-
    sum(
      auto_link
    )

  correct_auto_links <-
    sum(
      auto_link &
        benchmark_records$top_candidate_correct
    )

  false_auto_links <-
    sum(
      auto_link &
        !benchmark_records$top_candidate_correct
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
        auto_links > 0L
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
  auto_link <-
    oof_records$top_probability >=
      probability_threshold &
    oof_records$probability_margin >=
      margin_threshold

  review <-
    oof_records$top_probability >=
      probability_threshold &
    oof_records$probability_margin <
      margin_threshold

  unmatched <-
    oof_records$top_probability <
      probability_threshold

  auto_links <-
    sum(
      auto_link
    )

  correct_auto_links <-
    sum(
      auto_link &
        oof_records$top_candidate_correct
    )

  false_auto_links <-
    sum(
      auto_link &
        !oof_records$top_candidate_correct
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
        auto_links > 0L
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
