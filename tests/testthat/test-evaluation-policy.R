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
