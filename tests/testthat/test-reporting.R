# =====================================================================
# test-reporting.R
# Tests for quality-evidence and process-metadata reporting
# =====================================================================

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "reporting.R"
  )
)


testthat::test_that(
  "quality evidence registry defines unique predeclared measures",
  {
    registry <-
      build_quality_evidence_registry()

    testthat::expect_false(
      anyDuplicated(
        registry$evidence_id
      ) > 0L
    )

    expected_evidence <-
      c(
        "candidate_recall",
        "top1_accuracy",
        "auto_precision",
        "false_auto_links",
        "automation_rate",
        "review_rate",
        "unmatched_rate",
        "scenario_robustness",
        "source_robustness",
        "enterprise_count_error",
        "turnover_error",
        "employment_error",
        "turnover_per_employee_error",
        "traceability",
        "reproducibility"
      )

    testthat::expect_setequal(
      registry$evidence_id,
      expected_evidence
    )

    testthat::expect_true(
      all(
        nzchar(
          registry$measure
        )
      )
    )
  }
)


testthat::test_that(
  "process metadata records frozen linkage specification",
  {
    rf_spec <-
      tibble::tibble(
        mtry =
          2L,
        min_node_size =
          1L,
        num_trees =
          200L
      )

    rf_policy <-
      tibble::tibble(
        probability_threshold =
          0.01,
        margin_threshold =
          0.005,
        precision_target =
          0.99
      )

    similarity_policy <-
      tibble::tibble(
        score_threshold =
          0.55,
        margin_threshold =
          0.005,
        precision_target =
          0.99
      )

    metadata <-
      build_linkage_process_metadata(
        rf_spec =
          rf_spec,
        rf_policy =
          rf_policy,
        similarity_policy =
          similarity_policy,
        feature_columns =
          c(
            "name_similarity",
            "street_similarity",
            "city_similarity",
            "postal_code_match",
            "legal_form_match",
            "nace_match"
          )
      )

    testthat::expect_false(
      anyDuplicated(
        metadata$metadata_key
      ) > 0L
    )

    metadata_lookup <-
      stats::setNames(
        metadata$metadata_value,
        metadata$metadata_key
      )

    testthat::expect_equal(
      metadata_lookup[["rf_num_trees"]],
      "200"
    )

    testthat::expect_equal(
      metadata_lookup[["rf_mtry"]],
      "2"
    )

    testthat::expect_equal(
      metadata_lookup[["rf_score_threshold"]],
      "0.01"
    )

    testthat::expect_equal(
      metadata_lookup[["similarity_score_threshold"]],
      "0.55"
    )

    testthat::expect_equal(
      metadata_lookup[["grouped_cv_folds"]],
      "5"
    )

    testthat::expect_match(
      metadata_lookup[["heldout_role"]],
      "not used",
      ignore.case = TRUE
    )
  }
)
