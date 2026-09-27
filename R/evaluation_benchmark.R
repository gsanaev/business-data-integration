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


prepare_linkage_evaluation_sample <- function(
  source_data,
  source_id_column,
  register_data,
  enterprise_split,
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

  if (anyNA(source_entities$sample_role)) {
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

  if (anyNA(unresolved$true_register_id)) {
    stop(
      "True register identifiers are missing for unresolved sampled records."
    )
  }

  list(
    unresolved = unresolved,
    register_entities = register_entities
  )
}


build_evaluation_candidate_pairs <- function(
  unresolved,
  register_entities
) {
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

  generate_linkage_candidates(
    source_for_matching,
    register_for_matching
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
  prepared <-
    prepare_linkage_evaluation_sample(
      source_data = source_data,
      source_id_column = source_id_column,
      register_data = register_data,
      enterprise_split = enterprise_split,
      sample_role = sample_role
    )

  unresolved <-
    prepared$unresolved

  if (nrow(unresolved) == 0L) {
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

  candidates <-
    build_evaluation_candidate_pairs(
      unresolved,
      prepared$register_entities
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
  prepared <-
    prepare_linkage_evaluation_sample(
      source_data = source_data,
      source_id_column = source_id_column,
      register_data = register_data,
      enterprise_split = enterprise_split,
      sample_role = sample_role
    )

  unresolved <-
    prepared$unresolved

  if (nrow(unresolved) == 0L) {
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

  ranked_candidates <-
    build_evaluation_candidate_pairs(
      unresolved,
      prepared$register_entities
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

        if (length(index) > 0L) {
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

        if (length(index) > 0L) {
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
        if (dplyr::n() >= 2L) {
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
