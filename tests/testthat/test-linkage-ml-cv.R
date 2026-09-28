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


testthat::test_that(
  "grouped CV assignment is deterministic and enterprise-safe",
  {
    candidate_pairs <-
      tibble::tibble(
        truth_firm_id =
          rep(
            sprintf(
              "T%03d",
              1:20
            ),
            each = 3
          )
      )

    folds_a <-
      create_grouped_cv_folds(
        candidate_pairs,
        n_folds =
          5L,
        seed =
          202605L
      )

    folds_b <-
      create_grouped_cv_folds(
        candidate_pairs[
          nrow(
            candidate_pairs
          ):1,
          ,
          drop = FALSE
        ],
        n_folds =
          5L,
        seed =
          202605L
      )

    testthat::expect_equal(
      folds_a,
      folds_b
    )

    testthat::expect_equal(
      nrow(
        folds_a
      ),
      20L
    )

    testthat::expect_equal(
      sort(
        unique(
          folds_a$cv_fold
        )
      ),
      1:5
    )

    testthat::expect_equal(
      sort(
        as.integer(
          table(
            folds_a$cv_fold
          )
        )
      ),
      rep(
        4L,
        5L
      )
    )
  }
)


testthat::test_that(
  "all candidate versions of an enterprise remain in one fold",
  {
    candidate_pairs <-
      tibble::tibble(
        truth_firm_id =
          rep(
            c(
              "T001",
              "T002",
              "T003",
              "T004",
              "T005"
            ),
            each = 4
          ),
        candidate_row =
          seq_len(
            20L
          )
      )

    fold_map <-
      create_grouped_cv_folds(
        candidate_pairs,
        n_folds =
          5L,
        seed =
          202605L
      )

    result <-
      attach_grouped_cv_folds(
        candidate_pairs,
        fold_map
      )

    testthat::expect_silent(
      validate_grouped_cv_assignment(
        result,
        n_folds =
          5L
      )
    )

    testthat::expect_true(
      all(
        result %>%
          dplyr::group_by(
            .data$truth_firm_id
          ) %>%
          dplyr::summarise(
            folds =
              dplyr::n_distinct(
                .data$cv_fold
              ),
            .groups =
              "drop"
          ) %>%
          dplyr::pull(
            .data$folds
          ) ==
          1L
      )
    )
  }
)


testthat::test_that(
  "grouped CV creation rejects invalid configuration",
  {
    candidate_pairs <-
      tibble::tibble(
        truth_firm_id =
          sprintf(
            "T%03d",
            1:5
          )
      )

    testthat::expect_error(
      create_grouped_cv_folds(
        candidate_pairs,
        n_folds =
          1L
      ),
      "n_folds must be an integer of at least 2"
    )

    testthat::expect_error(
      create_grouped_cv_folds(
        candidate_pairs,
        n_folds =
          2.5
      ),
      "n_folds must be an integer of at least 2"
    )

    testthat::expect_error(
      create_grouped_cv_folds(
        candidate_pairs,
        seed =
          Inf
      ),
      "seed must be a finite numeric scalar"
    )

    testthat::expect_error(
      create_grouped_cv_folds(
        candidate_pairs[
          1:2,
          ,
          drop = FALSE
        ],
        n_folds =
          3L
      ),
      "Number of enterprise groups must be at least n_folds"
    )
  }
)


testthat::test_that(
  "grouped CV creation preserves the caller RNG state",
  {
    candidate_pairs <-
      tibble::tibble(
        truth_firm_id =
          sprintf(
            "T%03d",
            1:10
          )
      )

    set.seed(
      4242L
    )

    seed_before <-
      .Random.seed

    create_grouped_cv_folds(
      candidate_pairs,
      n_folds =
        5L,
      seed =
        202605L
    )

    testthat::expect_identical(
      .Random.seed,
      seed_before
    )
  }
)


testthat::test_that(
  "CV fold attachment rejects incomplete fold maps",
  {
    candidate_pairs <-
      tibble::tibble(
        truth_firm_id =
          c(
            "T001",
            "T002"
          )
      )

    fold_map <-
      tibble::tibble(
        truth_firm_id =
          "T001",
        cv_fold =
          1L
      )

    testthat::expect_error(
      attach_grouped_cv_folds(
        candidate_pairs,
        fold_map
      ),
      "CV fold assignment is missing for one or more candidate pairs"
    )
  }
)


testthat::test_that(
  "grouped CV validation rejects invalid assignments",
  {
    spanning_enterprise <-
      tibble::tibble(
        truth_firm_id =
          c(
            "T001",
            "T001",
            "T002"
          ),
        cv_fold =
          c(
            1L,
            2L,
            2L
          )
      )

    testthat::expect_error(
      validate_grouped_cv_assignment(
        spanning_enterprise,
        n_folds =
          2L
      ),
      "At least one enterprise spans multiple CV folds"
    )

    incomplete_fold_set <-
      tibble::tibble(
        truth_firm_id =
          c(
            "T001",
            "T002"
          ),
        cv_fold =
          c(
            1L,
            1L
          )
      )

    testthat::expect_error(
      validate_grouped_cv_assignment(
        incomplete_fold_set,
        n_folds =
          2L
      ),
      "Observed CV folds do not match the expected fold set"
    )

    unbalanced_assignment <-
      tibble::tibble(
        truth_firm_id =
          c(
            "T001",
            "T002",
            "T003",
            "T004",
            "T005"
          ),
        cv_fold =
          c(
            1L,
            1L,
            1L,
            1L,
            2L
          )
      )

    testthat::expect_error(
      validate_grouped_cv_assignment(
        unbalanced_assignment,
        n_folds =
          2L
      ),
      "Enterprise groups are not balanced across CV folds"
    )
  }
)


testthat::test_that(
  "CV fold summary reports enterprise and candidate composition",
  {
    candidate_pairs_cv <-
      tibble::tibble(
        scenario =
          c(
            "baseline",
            "baseline",
            "baseline",
            "moderate"
          ),
        source =
          c(
            "employment",
            "employment",
            "employment",
            "turnover"
          ),
        source_record_id =
          c(
            "E001",
            "E001",
            "E002",
            "U001"
          ),
        truth_firm_id =
          c(
            "T001",
            "T001",
            "T002",
            "T003"
          ),
        cv_fold =
          c(
            1L,
            1L,
            1L,
            2L
          ),
        is_true_candidate =
          c(
            TRUE,
            FALSE,
            TRUE,
            FALSE
          )
      )

    result <-
      summarise_ml_cv_folds(
        candidate_pairs_cv
      )

    expected <-
      tibble::tibble(
        cv_fold =
          c(
            1L,
            2L
          ),
        enterprises =
          c(
            2L,
            1L
          ),
        source_records =
          c(
            2L,
            1L
          ),
        candidate_pairs =
          c(
            3L,
            1L
          ),
        positive_pairs =
          c(
            2L,
            0L
          ),
        negative_pairs =
          c(
            1L,
            1L
          ),
        positive_share =
          c(
            2 / 3,
            0
          )
      )

    testthat::expect_equal(
      result,
      expected
    )
  }
)
