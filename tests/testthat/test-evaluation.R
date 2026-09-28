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
  "enterprise split is reproducible and has the frozen 70/30 size",
  {
    ids <-
      sprintf(
        "F%05d",
        1:1500
      )

    split_1 <-
      create_enterprise_split(
        ids,
        development_share = 0.70,
        seed = 202604L
      )

    split_2 <-
      create_enterprise_split(
        rev(ids),
        development_share = 0.70,
        seed = 202604L
      )

    testthat::expect_identical(
      split_1,
      split_2
    )

    testthat::expect_equal(
      sum(
        split_1$sample_role ==
          "development"
      ),
      1050L
    )

    testthat::expect_equal(
      sum(
        split_1$sample_role ==
          "heldout"
      ),
      450L
    )

    testthat::expect_setequal(
      split_1$truth_firm_id,
      ids
    )

    testthat::expect_false(
      anyDuplicated(
        split_1$truth_firm_id
      ) > 0L
    )

    testthat::expect_silent(
      validate_enterprise_split(
        split_1,
        ids,
        development_share = 0.70
      )
    )
  }
)


testthat::test_that(
  "enterprise split rejects invalid specifications",
  {
    ids <-
      sprintf(
        "F%05d",
        1:10
      )

    testthat::expect_error(
      create_enterprise_split(
        ids,
        development_share = 1
      ),
      "strictly between 0 and 1"
    )

    split <-
      create_enterprise_split(
        ids,
        development_share = 0.70,
        seed = 202604L
      )

    invalid_split <-
      split

    invalid_split$sample_role[1] <-
      "other"

    testthat::expect_error(
      validate_enterprise_split(
        invalid_split,
        ids,
        development_share = 0.70
      ),
      "invalid sample roles"
    )
  }
)


testthat::test_that(
  "corruption summaries use development enterprises only",
  {
    log <-
      tibble::tibble(
        scenario =
          c(
            "baseline",
            "baseline",
            "baseline"
          ),
        source =
          c(
            "employment",
            "employment",
            "employment"
          ),
        truth_firm_id =
          c(
            "F1",
            "F2",
            "F3"
          ),
        missing_business_id =
          c(
            FALSE,
            TRUE,
            TRUE
          ),
        invalid_or_unknown_business_id =
          FALSE,
        additional_name_typo =
          FALSE,
        substantial_name_degradation =
          FALSE,
        strong_street_discrepancy =
          FALSE,
        postal_code_missing_or_error =
          FALSE,
        nace_disagreement =
          FALSE,
        legal_form_disagreement =
          FALSE,
        sample_role =
          c(
            "development",
            "development",
            "heldout"
          )
      )

    rates <-
      summarise_scenario_corruption_rates(
        log
      )

    testthat::expect_equal(
      rates$missing_business_id,
      0.5
    )

    counts <-
      summarise_scenario_corruption_counts(
        log
      )

    testthat::expect_equal(
      sum(
        counts$n
      ),
      2L
    )

    testthat::expect_equal(
      sum(
        counts$share
      ),
      1
    )
  }
)


testthat::test_that(
  "candidate summary reports recall and candidate-set sizes",
  {
    records <-
      tibble::tibble(
        scenario =
          rep(
            "moderate",
            3
          ),
        source =
          rep(
            "employment",
            3
          ),
        truth_firm_id =
          c(
            "F1",
            "F2",
            "F3"
          ),
        source_record_id =
          c(
            "E1",
            "E2",
            "E3"
          ),
        business_id =
          c(
            NA,
            "UNKNOWN",
            NA
          ),
        candidate_count =
          c(
            5L,
            0L,
            10L
          ),
        true_candidate_present =
          c(
            TRUE,
            FALSE,
            TRUE
          )
      )

    summary <-
      summarise_candidate_generation(
        records
      )

    testthat::expect_equal(
      summary$unresolved_records,
      3L
    )

    testthat::expect_equal(
      summary$zero_candidate_records,
      1L
    )

    testthat::expect_equal(
      summary$mean_candidates,
      5
    )

    testthat::expect_equal(
      summary$median_candidates,
      5
    )

    testthat::expect_equal(
      summary$max_candidates,
      10L
    )

    testthat::expect_equal(
      summary$candidate_recall,
      2 / 3
    )
  }
)


testthat::test_that(
  "similarity benchmark summary preserves unresolved-link diagnostics",
  {
    records <-
      tibble::tibble(
        scenario =
          rep(
            "moderate",
            3
          ),
        source =
          rep(
            "employment",
            3
          ),
        identifier_issue =
          c(
            "missing_identifier",
            "missing_identifier",
            "identifier_not_found"
          ),
        true_candidate_present =
          c(
            TRUE,
            TRUE,
            FALSE
          ),
        top_candidate_correct =
          c(
            TRUE,
            FALSE,
            FALSE
          ),
        top_similarity_score =
          c(
            0.95,
            0.90,
            NA_real_
          ),
        similarity_margin =
          c(
            0.20,
            0.04,
            NA_real_
          )
      )

    summary <-
      summarise_similarity_benchmark(
        records
      )

    testthat::expect_equal(
      summary$unresolved_records,
      3L
    )

    testthat::expect_equal(
      summary$missing_identifier,
      2L
    )

    testthat::expect_equal(
      summary$identifier_not_found,
      1L
    )

    testthat::expect_equal(
      summary$candidate_recall,
      2 / 3
    )

    testthat::expect_equal(
      summary$top1_accuracy,
      1 / 3
    )

    testthat::expect_equal(
      summary$scorable_top_rate,
      2 / 3
    )

    testthat::expect_equal(
      summary$median_top_score,
      0.925
    )

    testthat::expect_equal(
      summary$median_margin,
      0.12
    )
  }
)


testthat::test_that(
  "similarity policy evaluation separates auto-link review and unmatched",
  {
    records <-
      tibble::tibble(
        top_similarity_score =
          c(
            0.90,
            0.80,
            0.70,
            0.40
          ),
        similarity_margin =
          c(
            0.20,
            0.01,
            0.30,
            0.40
          ),
        top_candidate_correct =
          c(
            TRUE,
            FALSE,
            TRUE,
            TRUE
          )
      )

    result <-
      evaluate_similarity_policy(
        records,
        score_threshold =
          0.50,
        margin_threshold =
          0.05
      )

    testthat::expect_equal(
      result$auto_links,
      2L
    )

    testthat::expect_equal(
      result$correct_auto_links,
      2L
    )

    testthat::expect_equal(
      result$false_auto_links,
      0L
    )

    testthat::expect_equal(
      result$auto_precision,
      1
    )

    testthat::expect_equal(
      result$automation_rate,
      0.5
    )

    testthat::expect_equal(
      result$review_records,
      1L
    )

    testthat::expect_equal(
      result$unmatched_records,
      1L
    )
  }
)


testthat::test_that(
  "policy selection maximizes automation and uses conservative tie-break",
  {
    policy_grid <-
      tibble::tibble(
        score_threshold =
          c(
            0.50,
            0.55,
            0.90
          ),
        margin_threshold =
          c(
            0.005,
            0.005,
            0.10
          ),
        unresolved_records =
          c(
            100L,
            100L,
            100L
          ),
        auto_links =
          c(
            100L,
            100L,
            99L
          ),
        correct_auto_links =
          c(
            99L,
            99L,
            99L
          ),
        false_auto_links =
          c(
            1L,
            1L,
            0L
          ),
        auto_precision =
          c(
            0.99,
            0.99,
            1
          ),
        automation_rate =
          c(
            1,
            1,
            0.99
          ),
        review_records =
          c(
            0L,
            0L,
            1L
          ),
        review_rate =
          c(
            0,
            0,
            0.01
          ),
        unmatched_records =
          c(
            0L,
            0L,
            0L
          ),
        unmatched_rate =
          c(
            0,
            0,
            0
          )
      )

    selected <-
      select_similarity_policy(
        policy_grid,
        precision_target =
          0.99
      )

    testthat::expect_equal(
      selected$score_threshold,
      0.55
    )

    testthat::expect_equal(
      selected$margin_threshold,
      0.005
    )

    testthat::expect_equal(
      selected$auto_links,
      100L
    )
  }
)


testthat::test_that(
  "RF policy separates auto-link review and unmatched records",
  {
    records <-
      tibble::tibble(
        top_probability =
          c(
            0.90,
            0.80,
            0.70,
            0.20
          ),
        probability_margin =
          c(
            0.30,
            0.01,
            0.20,
            0.10
          ),
        top_candidate_correct =
          c(
            TRUE,
            FALSE,
            TRUE,
            TRUE
          )
      )

    result <-
      evaluate_rf_policy(
        records,
        probability_threshold =
          0.50,
        margin_threshold =
          0.05
      )

    testthat::expect_equal(
      result$auto_links,
      2L
    )

    testthat::expect_equal(
      result$correct_auto_links,
      2L
    )

    testthat::expect_equal(
      result$false_auto_links,
      0L
    )

    testthat::expect_equal(
      result$review_records,
      1L
    )

    testthat::expect_equal(
      result$unmatched_records,
      1L
    )

    testthat::expect_equal(
      result$auto_precision,
      1
    )
  }
)


testthat::test_that(
  "RF policy selection maximizes automation with conservative tie-break",
  {
    policy_grid <-
      tibble::tibble(
        probability_threshold =
          c(
            0.10,
            0.20,
            0.80
          ),
        margin_threshold =
          c(
            0.005,
            0.005,
            0.10
          ),
        unresolved_records =
          c(
            100L,
            100L,
            100L
          ),
        auto_links =
          c(
            100L,
            100L,
            99L
          ),
        correct_auto_links =
          c(
            99L,
            99L,
            99L
          ),
        false_auto_links =
          c(
            1L,
            1L,
            0L
          ),
        auto_precision =
          c(
            0.99,
            0.99,
            1
          ),
        automation_rate =
          c(
            1,
            1,
            0.99
          ),
        review_records =
          c(
            0L,
            0L,
            1L
          ),
        review_rate =
          c(
            0,
            0,
            0.01
          ),
        unmatched_records =
          c(
            0L,
            0L,
            0L
          ),
        unmatched_rate =
          c(
            0,
            0,
            0
          )
      )

    selected <-
      select_rf_policy(
        policy_grid,
        precision_target =
          0.99
      )

    testthat::expect_equal(
      selected$probability_threshold,
      0.20
    )

    testthat::expect_equal(
      selected$margin_threshold,
      0.005
    )

    testthat::expect_equal(
      selected$auto_links,
      100L
    )
  }
)


testthat::test_that(
  "development comparison reports both linkage methods consistently",
  {
    similarity_records <-
      tibble::tibble(
        top_similarity_score =
          c(
            0.90,
            0.80
          ),
        similarity_margin =
          c(
            0.20,
            0.10
          ),
        top_candidate_correct =
          c(
            TRUE,
            FALSE
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
        top_probability =
          c(
            0.90,
            0.20
          ),
        probability_margin =
          c(
            0.30,
            0.01
          ),
        top_candidate_correct =
          c(
            TRUE,
            FALSE
          )
      )

    rf_policy <-
      tibble::tibble(
        probability_threshold =
          0.10,
        margin_threshold =
          0.05
      )

    result <-
      build_development_method_comparison(
        similarity_records,
        similarity_policy,
        rf_records,
        rf_policy
      )

    testthat::expect_equal(
      result$method,
      c(
        "weighted_similarity",
        "random_forest"
      )
    )

    testthat::expect_equal(
      result$unresolved_records,
      c(
        2L,
        2L
      )
    )

    testthat::expect_equal(
      result$false_auto_links,
      c(
        1L,
        0L
      )
    )
  }
)

# =====================================================================
# Stage 10A held-out evaluation infrastructure
# =====================================================================

testthat::test_that(
  "candidate and similarity evaluation support held-out enterprises",
  {
    source_data <-
      tibble::tibble(
        truth_firm_id =
          c(
            "T001",
            "T002"
          ),
        employment_source_id =
          c(
            "E001",
            "E002"
          ),
        business_id =
          c(
            NA_character_,
            NA_character_
          ),
        enterprise_name =
          c(
            "Alpha GmbH",
            "Beta AG"
          ),
        street =
          c(
            "Alphaweg 1",
            "Betastrasse 2"
          ),
        postal_code =
          c(
            "12345",
            "99999"
          ),
        city =
          c(
            "Berlin",
            "Hamburg"
          ),
        legal_form =
          c(
            "GmbH",
            "AG"
          ),
        nace_code =
          c(
            "G47",
            "C10"
          )
      )

    register_data <-
      tibble::tibble(
        truth_firm_id =
          c(
            "T001",
            "T002"
          ),
        register_id =
          c(
            "R001",
            "R002"
          ),
        business_id =
          c(
            "B001",
            "B002"
          ),
        enterprise_name =
          c(
            "Alpha GmbH",
            "Beta AG"
          ),
        street =
          c(
            "Alphaweg 1",
            "Betastrasse 2"
          ),
        postal_code =
          c(
            "12345",
            "99999"
          ),
        city =
          c(
            "Berlin",
            "Hamburg"
          ),
        legal_form =
          c(
            "GmbH",
            "AG"
          ),
        nace_code =
          c(
            "G47",
            "C10"
          )
      )

    enterprise_split <-
      tibble::tibble(
        truth_firm_id =
          c(
            "T001",
            "T002"
          ),
        sample_role =
          c(
            "development",
            "heldout"
          )
      )

    similarity_weights <-
      c(
        name_similarity =
          0.40,
        street_similarity =
          0.30,
        city_similarity =
          0.05,
        postal_code_match =
          0.10,
        legal_form_match =
          0.075,
        nace_match =
          0.075
      )

    candidate_result <-
      evaluate_candidate_generation(
        source_data =
          source_data,
        source_id_column =
          "employment_source_id",
        register_data =
          register_data,
        enterprise_split =
          enterprise_split,
        scenario_name =
          "baseline",
        source_name =
          "employment",
        sample_role =
          "heldout"
      )

    similarity_result <-
      build_similarity_benchmark_records(
        source_data =
          source_data,
        source_id_column =
          "employment_source_id",
        register_data =
          register_data,
        enterprise_split =
          enterprise_split,
        scenario_name =
          "baseline",
        source_name =
          "employment",
        similarity_weights =
          similarity_weights,
        sample_role =
          "heldout"
      )

    testthat::expect_equal(
      nrow(
        candidate_result
      ),
      1L
    )

    testthat::expect_identical(
      candidate_result$truth_firm_id,
      "T002"
    )

    testthat::expect_equal(
      candidate_result$candidate_count,
      1L
    )

    testthat::expect_true(
      candidate_result$true_candidate_present
    )

    testthat::expect_equal(
      nrow(
        similarity_result
      ),
      1L
    )

    testthat::expect_identical(
      similarity_result$truth_firm_id,
      "T002"
    )

    testthat::expect_identical(
      similarity_result$top_candidate_register_id,
      "R002"
    )

    testthat::expect_true(
      similarity_result$true_candidate_present
    )

    testthat::expect_true(
      similarity_result$top_candidate_correct
    )

    testthat::expect_equal(
      similarity_result$top_similarity_score,
      1
    )
  }
)

# =====================================================================
# Stage 10B policy and RF completion tests
# =====================================================================

testthat::test_that(
  "similarity policy treats zero-candidate records as unmatched",
  {
    records <-
      tibble::tibble(
        candidate_count =
          c(
            1L,
            0L
          ),
        top_similarity_score =
          c(
            0.90,
            NA_real_
          ),
        similarity_margin =
          c(
            0.20,
            NA_real_
          ),
        top_candidate_correct =
          c(
            TRUE,
            FALSE
          )
      )

    result <-
      evaluate_similarity_policy(
        records,
        score_threshold =
          0.55,
        margin_threshold =
          0.005
      )

    testthat::expect_equal(
      result$auto_links,
      1L
    )

    testthat::expect_equal(
      result$correct_auto_links,
      1L
    )

    testthat::expect_equal(
      result$false_auto_links,
      0L
    )

    testthat::expect_equal(
      result$review_records,
      0L
    )

    testthat::expect_equal(
      result$unmatched_records,
      1L
    )
  }
)


testthat::test_that(
  "RF policy treats zero-candidate records as unmatched",
  {
    records <-
      tibble::tibble(
        candidate_count =
          c(
            1L,
            0L
          ),
        top_probability =
          c(
            0.90,
            0
          ),
        probability_margin =
          c(
            0.20,
            0
          ),
        top_candidate_correct =
          c(
            TRUE,
            FALSE
          )
      )

    result <-
      evaluate_rf_policy(
        records,
        probability_threshold =
          0.01,
        margin_threshold =
          0.005
      )

    testthat::expect_equal(
      result$auto_links,
      1L
    )

    testthat::expect_equal(
      result$correct_auto_links,
      1L
    )

    testthat::expect_equal(
      result$false_auto_links,
      0L
    )

    testthat::expect_equal(
      result$review_records,
      0L
    )

    testthat::expect_equal(
      result$unmatched_records,
      1L
    )
  }
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
