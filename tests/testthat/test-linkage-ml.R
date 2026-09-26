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
