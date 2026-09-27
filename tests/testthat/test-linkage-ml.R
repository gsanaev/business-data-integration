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
    "helpers",
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
    "linkage_ml.R"
  ),
  local = TRUE
)


testthat::test_that(
  "ML linkage features are bounded and operational",
  {
    testthat::expect_equal(
      ml_linkage_feature_columns(),
      c(
        "name_similarity",
        "street_similarity",
        "city_similarity",
        "postal_code_match",
        "legal_form_match",
        "nace_match"
      )
    )
  }
)


testthat::test_that(
  "ML candidate pairs contain labelled development candidates",
  {
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
            "Hauptstrasse 1",
            "Nebenweg 9"
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
            "Hauptstrasse 1",
            "Nebenweg 9"
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

    result <-
      build_ml_candidate_pairs(
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
          "employment"
      )

    testthat::expect_true(
      all(
        result$truth_firm_id ==
          "T001"
      )
    )

    testthat::expect_equal(
      sum(
        result$is_true_candidate
      ),
      1L
    )

    testthat::expect_equal(
      result$register_id[
        result$is_true_candidate
      ],
      "R001"
    )

    testthat::expect_true(
      all(
        ml_linkage_feature_columns() %in%
          names(
            result
          )
      )
    )
  }
)


testthat::test_that(
  "ML candidate validation rejects non-development enterprises",
  {
    candidate_pairs <-
      tibble::tibble(
        scenario =
          "baseline",
        source =
          "employment",
        truth_firm_id =
          "T002",
        source_record_id =
          "E002",
        register_id =
          "R002",
        canonical_firm_id =
          "C000002",
        true_register_id =
          "R002",
        name_similarity =
          1,
        street_similarity =
          1,
        city_similarity =
          1,
        postal_code_match =
          1,
        legal_form_match =
          1,
        nace_match =
          1,
        is_true_candidate =
          TRUE
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

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs,
        enterprise_split
      ),
      "non-development"
    )
  }
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

# =====================================================================
# Stage 10A held-out infrastructure
# =====================================================================

testthat::test_that(
  "ML candidate construction supports held-out enterprises",
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

    result <-
      build_ml_candidate_pairs(
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

    testthat::expect_true(
      nrow(
        result
      ) > 0L
    )

    testthat::expect_true(
      all(
        result$truth_firm_id ==
          "T002"
      )
    )

    testthat::expect_equal(
      sum(
        result$is_true_candidate
      ),
      1L
    )

    testthat::expect_equal(
      result$register_id[
        result$is_true_candidate
      ],
      "R002"
    )
  }
)


testthat::test_that(
  "ML candidate validation respects the requested sample role",
  {
    heldout_pairs <-
      tibble::tibble(
        scenario =
          "baseline",
        source =
          "employment",
        truth_firm_id =
          "T002",
        source_record_id =
          "E002",
        register_id =
          "R002",
        canonical_firm_id =
          "C000002",
        true_register_id =
          "R002",
        name_similarity =
          1,
        street_similarity =
          1,
        city_similarity =
          1,
        postal_code_match =
          1,
        legal_form_match =
          1,
        nace_match =
          1,
        is_true_candidate =
          TRUE
      )

    development_pairs <-
      heldout_pairs %>%
      dplyr::mutate(
        truth_firm_id =
          "T001",
        source_record_id =
          "E001",
        register_id =
          "R001",
        canonical_firm_id =
          "C000001",
        true_register_id =
          "R001"
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

    testthat::expect_no_error(
      validate_ml_candidate_pairs(
        heldout_pairs,
        enterprise_split,
        sample_role =
          "heldout"
      )
    )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        development_pairs,
        enterprise_split,
        sample_role =
          "heldout"
      ),
      "non-heldout"
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
