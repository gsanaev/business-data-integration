source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "evaluation.R"
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
