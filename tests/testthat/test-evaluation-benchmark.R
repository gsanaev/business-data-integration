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

