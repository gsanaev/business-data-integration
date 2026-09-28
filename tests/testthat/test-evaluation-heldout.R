suppressPackageStartupMessages(
  library(dplyr)
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_candidates.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_features.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_similarity.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_split.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_benchmark.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_policy.R"
  )
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation_heldout.R"
  )
)


testthat::test_that(
  "RF evaluation completion preserves every unresolved record",
  {
    reference_records <-
      tibble::tibble(
        scenario =
          c(
            "baseline",
            "baseline"
          ),
        source =
          c(
            "employment",
            "employment"
          ),
        truth_firm_id =
          c(
            "T001",
            "T002"
          ),
        source_record_id =
          c(
            "E001",
            "E002"
          )
      )

    rf_records <-
      tibble::tibble(
        scenario =
          "baseline",
        source =
          "employment",
        truth_firm_id =
          "T001",
        source_record_id =
          "E001",
        cv_fold =
          NA_integer_,
        candidate_count =
          2L,
        true_candidate_present =
          TRUE,
        top_candidate_correct =
          TRUE,
        top_probability =
          0.90,
        second_probability =
          0.20,
        probability_margin =
          0.70,
        true_candidate_rank =
          1L,
        reciprocal_rank =
          1
      )

    result <-
      complete_rf_evaluation_records(
        rf_records,
        reference_records
      )

    testthat::expect_equal(
      nrow(
        result
      ),
      2L
    )

    zero_candidate <-
      result %>%
      dplyr::filter(
        .data$source_record_id ==
          "E002"
      )

    testthat::expect_equal(
      zero_candidate$candidate_count,
      0L
    )

    testthat::expect_false(
      zero_candidate$true_candidate_present
    )

    testthat::expect_false(
      zero_candidate$top_candidate_correct
    )

    testthat::expect_equal(
      zero_candidate$top_probability,
      0
    )

    testthat::expect_equal(
      zero_candidate$probability_margin,
      0
    )

    testthat::expect_equal(
      zero_candidate$reciprocal_rank,
      0
    )
  }
)

testthat::test_that(
  "complete linkage evaluation combines Level 1 and frozen Level 2 decisions",
  {
    register_data <-
      tibble::tibble(
        truth_firm_id =
          c(
            "F001",
            "F002",
            "F003",
            "F004"
          ),
        register_id =
          c(
            "R001",
            "R002",
            "R003",
            "R004"
          ),
        business_id =
          c(
            "B001",
            "B002",
            "B003",
            "B004"
          )
      )

    source_data <-
      tibble::tibble(
        truth_firm_id =
          rep(
            c(
              "F001",
              "F002",
              "F003",
              "F004"
            ),
            each = 2L
          ),
        source_id =
          rep(
            c(
              "S001",
              "S002",
              "S003",
              "S004"
            ),
            each = 2L
          ),
        business_id =
          rep(
            c(
              "B001",
              NA_character_,
              "UNKNOWN",
              NA_character_
            ),
            each = 2L
          )
      )

    enterprise_split <-
      tibble::tibble(
        truth_firm_id =
          c(
            "F001",
            "F002",
            "F003",
            "F004"
          ),
        sample_role =
          rep(
            "heldout",
            4L
          )
      )

    similarity_records <-
      tibble::tibble(
        scenario =
          rep(
            "baseline",
            3L
          ),
        source =
          rep(
            "employment",
            3L
          ),
        truth_firm_id =
          c(
            "F002",
            "F003",
            "F004"
          ),
        source_record_id =
          c(
            "S002",
            "S003",
            "S004"
          ),
        candidate_count =
          c(
            2L,
            2L,
            2L
          ),
        top_candidate_register_id =
          c(
            "R002",
            "R001",
            "R004"
          ),
        top_candidate_canonical_firm_id =
          c(
            "C000002",
            "C000001",
            "C000004"
          ),
        top_similarity_score =
          c(
            0.80,
            0.80,
            0.80
          ),
        similarity_margin =
          c(
            0.10,
            0.10,
            0.01
          )
      )

    similarity_policy <-
      tibble::tibble(
        score_threshold =
          0.50,
        margin_threshold =
          0.05
      )

    rf_records <-
      tibble::tibble(
        scenario =
          rep(
            "baseline",
            3L
          ),
        source =
          rep(
            "employment",
            3L
          ),
        truth_firm_id =
          c(
            "F002",
            "F003",
            "F004"
          ),
        source_record_id =
          c(
            "S002",
            "S003",
            "S004"
          ),
        candidate_count =
          c(
            2L,
            2L,
            2L
          ),
        top_probability =
          c(
            0.80,
            0.40,
            0.80
          ),
        probability_margin =
          c(
            0.01,
            0.10,
            0.10
          )
      )

    rf_assignments <-
      tibble::tibble(
        scenario =
          rep(
            "baseline",
            3L
          ),
        source =
          rep(
            "employment",
            3L
          ),
        truth_firm_id =
          c(
            "F002",
            "F003",
            "F004"
          ),
        source_record_id =
          c(
            "S002",
            "S003",
            "S004"
          ),
        top_candidate_register_id =
          c(
            "R002",
            "R001",
            "R004"
          ),
        top_candidate_canonical_firm_id =
          c(
            "C000002",
            "C000001",
            "C000004"
          )
      )

    rf_policy <-
      tibble::tibble(
        probability_threshold =
          0.50,
        margin_threshold =
          0.05
      )

    register_data <-
      dplyr::bind_rows(
        register_data,
        tibble::tibble(
          truth_firm_id =
            "F005",
          register_id =
            "R005",
          business_id =
            "B005"
        )
      )

    source_data <-
      dplyr::bind_rows(
        source_data,
        tibble::tibble(
          truth_firm_id =
            "F005",
          source_id =
            "S005",
          business_id =
            "B005"
        )
      )

    enterprise_split <-
      dplyr::bind_rows(
        enterprise_split,
        tibble::tibble(
          truth_firm_id =
            "F005",
          sample_role =
            "development"
        )
      )

    result <-
      build_complete_linkage_evaluation_records(
        source_data =
          source_data,
        source_id_column =
          "source_id",
        register_data =
          register_data,
        enterprise_split =
          enterprise_split,
        scenario_name =
          "baseline",
        source_name =
          "employment",
        similarity_records =
          similarity_records,
        similarity_policy =
          similarity_policy,
        rf_records =
          rf_records,
        rf_assignments =
          rf_assignments,
        rf_policy =
          rf_policy
      )

    testthat::expect_equal(
      nrow(result),
      8L
    )

    similarity <-
      result %>%
      dplyr::filter(
        .data$method ==
          "weighted_similarity"
      )

    rf <-
      result %>%
      dplyr::filter(
        .data$method ==
          "random_forest"
      )

    testthat::expect_equal(
      similarity$decision_status[
        similarity$truth_firm_id ==
          "F001"
      ],
      "auto_link"
    )

    testthat::expect_equal(
      similarity$decision_stage[
        similarity$truth_firm_id ==
          "F001"
      ],
      "level1"
    )

    testthat::expect_false(
      similarity$assignment_correct[
        similarity$truth_firm_id ==
          "F003"
      ]
    )

    testthat::expect_equal(
      similarity$decision_status[
        similarity$truth_firm_id ==
          "F004"
      ],
      "review"
    )

    testthat::expect_equal(
      rf$decision_status[
        rf$truth_firm_id ==
          "F002"
      ],
      "review"
    )

    testthat::expect_equal(
      rf$decision_status[
        rf$truth_firm_id ==
          "F003"
      ],
      "unmatched"
    )

    testthat::expect_equal(
      rf$decision_status[
        rf$truth_firm_id ==
          "F004"
      ],
      "auto_link"
    )
  }
)


testthat::test_that(
  "complete linkage summary reports end-to-end workflow counts",
  {
    records <-
      tibble::tibble(
        scenario =
          rep(
            "baseline",
            4L
          ),
        source =
          rep(
            "employment",
            4L
          ),
        method =
          rep(
            "random_forest",
            4L
          ),
        decision_stage =
          c(
            "level1",
            "level2",
            "level2",
            "level2"
          ),
        decision_status =
          c(
            "auto_link",
            "auto_link",
            "review",
            "unmatched"
          ),
        automatic_link =
          c(
            TRUE,
            TRUE,
            FALSE,
            FALSE
          ),
        assignment_correct =
          c(
            TRUE,
            FALSE,
            NA,
            NA
          )
      )

    summary <-
      summarise_complete_linkage_evaluation(
        records
      )

    testthat::expect_equal(
      summary$total_records,
      4L
    )

    testthat::expect_equal(
      summary$level1_deterministic_links,
      1L
    )

    testthat::expect_equal(
      summary$level2_records,
      3L
    )

    testthat::expect_equal(
      summary$automatic_links,
      2L
    )

    testthat::expect_equal(
      summary$correct_automatic_links,
      1L
    )

    testthat::expect_equal(
      summary$false_automatic_links,
      1L
    )

    testthat::expect_equal(
      summary$review_records,
      1L
    )

    testthat::expect_equal(
      summary$unmatched_records,
      1L
    )

    testthat::expect_equal(
      summary$automation_rate,
      0.50
    )

    testthat::expect_equal(
      summary$auto_precision,
      0.50
    )

    testthat::expect_equal(
      summary$correct_automatic_resolution_rate,
      0.25
    )
  }
)
