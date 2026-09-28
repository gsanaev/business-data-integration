# =====================================================================
# test-linkage-ml.R
# Tests for ML linkage candidate-pair preparation
# =====================================================================

suppressPackageStartupMessages(
  library(dplyr)
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_similarity.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_candidates.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_features.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_ml_candidates.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_ml_cv.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_ml_rf.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_ml_tuning.R"
  ),
  local = TRUE
)


make_small_rf_cv_pairs <- function() {
  tibble::tibble(
    scenario =
      rep(
        "baseline",
        8L
      ),
    source =
      rep(
        "employment",
        8L
      ),
    truth_firm_id =
      rep(
        c(
          "T001",
          "T002",
          "T003",
          "T004"
        ),
        each = 2L
      ),
    source_record_id =
      rep(
        c(
          "E001",
          "E002",
          "E003",
          "E004"
        ),
        each = 2L
      ),
    register_id =
      sprintf(
        "R%03d",
        seq_len(
          8L
        )
      ),
    canonical_firm_id =
      sprintf(
        "C%06d",
        seq_len(
          8L
        )
      ),
    true_register_id =
      rep(
        c(
          "R001",
          "R003",
          "R005",
          "R007"
        ),
        each = 2L
      ),
    name_similarity =
      rep(
        c(
          0.95,
          0.25
        ),
        4L
      ),
    street_similarity =
      rep(
        c(
          0.90,
          0.20
        ),
        4L
      ),
    city_similarity =
      rep(
        c(
          1,
          0
        ),
        4L
      ),
    postal_code_match =
      rep(
        c(
          1,
          0
        ),
        4L
      ),
    legal_form_match =
      rep(
        c(
          1,
          0
        ),
        4L
      ),
    nace_match =
      rep(
        c(
          1,
          0
        ),
        4L
      ),
    is_true_candidate =
      rep(
        c(
          TRUE,
          FALSE
        ),
        4L
      ),
    cv_fold =
      rep(
        c(
          1L,
          1L,
          2L,
          2L
        ),
        each = 2L
      )
  )
}


testthat::test_that(
  "RF CV configuration evaluation returns complete summaries",
  {
    candidate_pairs_cv <-
      make_small_rf_cv_pairs()

    result <-
      evaluate_rf_cv_configuration(
        candidate_pairs_cv =
          candidate_pairs_cv,
        config_id =
          "rf_test",
        mtry =
          2L,
        min_node_size =
          1L,
        num_trees =
          20L,
        seed =
          202606L,
        num_threads =
          1L
      )

    testthat::expect_named(
      result,
      c(
        "summary",
        "fold_summary",
        "record_metrics"
      )
    )

    testthat::expect_equal(
      result$summary$config_id,
      "rf_test"
    )

    testthat::expect_equal(
      nrow(
        result$fold_summary
      ),
      2L
    )

    testthat::expect_equal(
      sort(
        result$fold_summary$cv_fold
      ),
      1:2
    )

    testthat::expect_equal(
      nrow(
        result$record_metrics
      ),
      4L
    )

    testthat::expect_true(
      all(
        c(
          "top1_accuracy",
          "mean_reciprocal_rank",
          "mean_positive_class_weight"
        ) %in%
          names(
            result$summary
          )
      )
    )
  }
)


testthat::test_that(
  "RF CV tuning combines configuration outputs",
  {
    candidate_pairs_cv <-
      make_small_rf_cv_pairs()

    tuning_grid <-
      tibble::tibble(
        config_id =
          "rf_test",
        num_trees =
          20L,
        mtry =
          2L,
        min_node_size =
          1L
      )

    result <-
      run_rf_cv_tuning(
        candidate_pairs_cv =
          candidate_pairs_cv,
        tuning_grid =
          tuning_grid,
        seed =
          202606L,
        num_threads =
          1L
      )

    testthat::expect_named(
      result,
      c(
        "summary",
        "fold_summary",
        "record_metrics"
      )
    )

    testthat::expect_equal(
      nrow(
        result$summary
      ),
      1L
    )

    testthat::expect_equal(
      result$summary$config_id,
      "rf_test"
    )

    testthat::expect_equal(
      nrow(
        result$fold_summary
      ),
      2L
    )

    testthat::expect_equal(
      nrow(
        result$record_metrics
      ),
      4L
    )
  }
)


testthat::test_that(
  "RF configuration selection follows the frozen ranking rule",
  {
    tuning_summary <-
      tibble::tibble(
        config_id =
          c(
            "A",
            "B",
            "C",
            "D"
          ),
        mtry =
          c(
            4L,
            2L,
            2L,
            4L
          ),
        min_node_size =
          c(
            10L,
            1L,
            10L,
            1L
          ),
        num_trees =
          rep(
            200L,
            4
          ),
        top1_accuracy =
          c(
            0.99,
            0.99,
            0.99,
            0.98
          ),
        mean_reciprocal_rank =
          c(
            0.995,
            0.995,
            0.995,
            0.999
          )
      )

    selected <-
      select_rf_configuration(
        tuning_summary
      )

    testthat::expect_equal(
      selected$config_id,
      "C"
    )

    testthat::expect_equal(
      selected$mtry,
      2L
    )

    testthat::expect_equal(
      selected$min_node_size,
      10L
    )
  }
)
