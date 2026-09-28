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


make_valid_ml_candidate_pairs <- function() {
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
        "T001"
      ),
    source_record_id =
      c(
        "E001",
        "E001"
      ),
    register_id =
      c(
        "R001",
        "R002"
      ),
    canonical_firm_id =
      c(
        "C000001",
        "C000002"
      ),
    true_register_id =
      c(
        "R001",
        "R001"
      ),
    name_similarity =
      c(
        1,
        0.40
      ),
    street_similarity =
      c(
        1,
        0.30
      ),
    city_similarity =
      c(
        1,
        0
      ),
    postal_code_match =
      c(
        1,
        0
      ),
    legal_form_match =
      c(
        1,
        1
      ),
    nace_match =
      c(
        1,
        0
      ),
    is_true_candidate =
      c(
        TRUE,
        FALSE
      )
  )
}


testthat::test_that(
  "RF tuning grid is deliberately bounded",
  {
    grid <-
      rf_linkage_tuning_grid()

    testthat::expect_equal(
      nrow(
        grid
      ),
      4L
    )

    testthat::expect_equal(
      sort(
        unique(
          grid$mtry
        )
      ),
      c(
        2L,
        4L
      )
    )

    testthat::expect_equal(
      sort(
        unique(
          grid$min_node_size
        )
      ),
      c(
        1L,
        10L
      )
    )

    testthat::expect_true(
      all(
        grid$num_trees ==
          200L
      )
    )
  }
)


testthat::test_that(
  "RF class weighting balances effective class contribution",
  {
    labels <-
      c(
        rep(
          TRUE,
          2
        ),
        rep(
          FALSE,
          8
        )
      )

    weights <-
      compute_rf_class_weights(
        labels
      )

    testthat::expect_equal(
      unname(
        weights[
          "nonmatch"
        ]
      ),
      1
    )

    testthat::expect_equal(
      unname(
        weights[
          "match"
        ]
      ),
      4
    )
  }
)


testthat::test_that(
  "RF class weighting rejects invalid training labels",
  {
    testthat::expect_error(
      compute_rf_class_weights(c(1, 0)),
      "RF class labels must be non-missing logical values"
    )

    testthat::expect_error(
      compute_rf_class_weights(c(TRUE, NA)),
      "RF class labels must be non-missing logical values"
    )

    testthat::expect_error(
      compute_rf_class_weights(c(TRUE, TRUE)),
      "Both RF training classes must be present"
    )
  }
)


testthat::test_that(
  "RF model fitting rejects mtry outside the feature range",
  {
    candidate_pairs <-
      make_valid_ml_candidate_pairs()

    testthat::expect_error(
      fit_rf_candidate_model(
        training_pairs = candidate_pairs,
        mtry = 0L,
        min_node_size = 1L
      ),
      "mtry is outside the available feature range"
    )
  }
)


testthat::test_that(
  "RF record scoring rejects invalid probability vectors",
  {
    candidate_pairs <-
      make_valid_ml_candidate_pairs()

    testthat::expect_error(
      score_rf_candidate_records(
        candidate_pairs,
        0.5
      ),
      "RF probability vector does not match candidate-pair rows"
    )

    testthat::expect_error(
      score_rf_candidate_records(
        candidate_pairs,
        c(0.8, NA_real_)
      ),
      "RF match probabilities must be finite values in \\[0, 1\\]"
    )

    testthat::expect_error(
      score_rf_candidate_records(
        candidate_pairs,
        c(0.8, 1.1)
      ),
      "RF match probabilities must be finite values in \\[0, 1\\]"
    )
  }
)


testthat::test_that(
  "RF assignment summarisation rejects malformed inputs",
  {
    candidate_pairs <-
      make_valid_ml_candidate_pairs()

    testthat::expect_error(
      summarise_rf_top_candidate_assignments(
        candidate_pairs =
          dplyr::select(
            candidate_pairs,
            -canonical_firm_id
          ),
        match_score =
          c(0.8, 0.2)
      ),
      "RF assignment candidate pairs are missing required columns"
    )

    testthat::expect_error(
      summarise_rf_top_candidate_assignments(
        candidate_pairs =
          candidate_pairs,
        match_score =
          0.8
      ),
      "RF match-score vector does not match candidate-pair rows"
    )

    testthat::expect_error(
      summarise_rf_top_candidate_assignments(
        candidate_pairs =
          candidate_pairs,
        match_score =
          c(Inf, 0.2)
      ),
      "RF match scores must be finite values in \\[0, 1\\]"
    )
  }
)


testthat::test_that(
  "RF record scoring ranks probabilities within source records",
  {
    candidate_pairs <-
      tibble::tibble(
        scenario =
          rep(
            "baseline",
            3
          ),
        source =
          rep(
            "employment",
            3
          ),
        truth_firm_id =
          rep(
            "T001",
            3
          ),
        source_record_id =
          rep(
            "E001",
            3
          ),
        register_id =
          c(
            "R001",
            "R002",
            "R003"
          ),
        cv_fold =
          rep(
            1L,
            3
          ),
        is_true_candidate =
          c(
            FALSE,
            TRUE,
            FALSE
          )
      )

    result <-
      score_rf_candidate_records(
        candidate_pairs,
        c(
          0.10,
          0.90,
          0.40
        )
      )

    testthat::expect_true(
      result$top_candidate_correct
    )

    testthat::expect_equal(
      result$true_candidate_rank,
      1L
    )

    testthat::expect_equal(
      result$top_probability,
      0.90
    )

    testthat::expect_equal(
      result$second_probability,
      0.40
    )

    testthat::expect_equal(
      result$probability_margin,
      0.50
    )

    testthat::expect_equal(
      result$reciprocal_rank,
      1
    )
  }
)

testthat::test_that(
  "RF record scoring supports data without CV folds",
  {
    candidate_pairs <-
      tibble::tibble(
        scenario =
          rep(
            "baseline",
            3
          ),
        source =
          rep(
            "employment",
            3
          ),
        truth_firm_id =
          rep(
            "T002",
            3
          ),
        source_record_id =
          rep(
            "E002",
            3
          ),
        register_id =
          c(
            "R001",
            "R002",
            "R003"
          ),
        is_true_candidate =
          c(
            FALSE,
            TRUE,
            FALSE
          )
      )

    result <-
      score_rf_candidate_records(
        candidate_pairs,
        c(
          0.10,
          0.90,
          0.40
        )
      )

    testthat::expect_true(
      "cv_fold" %in%
        names(
          result
        )
    )

    testthat::expect_true(
      all(
        is.na(
          result$cv_fold
        )
      )
    )

    testthat::expect_true(
      result$top_candidate_correct
    )

    testthat::expect_equal(
      result$top_probability,
      0.90
    )

    testthat::expect_equal(
      result$probability_margin,
      0.50
    )
  }
)

testthat::test_that(
  "RF top-candidate assignments retain selected enterprise identifiers",
  {
    candidate_pairs <-
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
            "F001",
            "F001",
            "F002"
          ),
        source_record_id =
          c(
            "E001",
            "E001",
            "E002"
          ),
        register_id =
          c(
            "R002",
            "R001",
            "R003"
          ),
        canonical_firm_id =
          c(
            "C002",
            "C001",
            "C003"
          ),
        is_true_candidate =
          c(
            FALSE,
            TRUE,
            TRUE
          )
      )

    match_score <-
      c(
        0.20,
        0.80,
        0.70
      )

    result <-
      summarise_rf_top_candidate_assignments(
        candidate_pairs =
          candidate_pairs,
        match_score =
          match_score
      )

    testthat::expect_equal(
      nrow(result),
      2L
    )

    first_record <-
      result %>%
      dplyr::filter(
        .data$source_record_id ==
          "E001"
      )

    testthat::expect_equal(
      first_record$top_candidate_register_id,
      "R001"
    )

    testthat::expect_equal(
      first_record$top_candidate_canonical_firm_id,
      "C001"
    )

    testthat::expect_true(
      first_record$top_candidate_correct
    )

    testthat::expect_equal(
      first_record$top_match_score,
      0.80
    )

    testthat::expect_equal(
      first_record$candidate_count,
      2L
    )
  }
)
