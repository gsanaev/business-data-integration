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
