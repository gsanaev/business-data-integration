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
