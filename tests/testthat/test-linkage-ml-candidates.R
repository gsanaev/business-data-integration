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
  "ML candidate validation rejects malformed structure and labels",
  {
    candidate_pairs <-
      make_valid_ml_candidate_pairs()

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs %>%
          dplyr::select(
            -source
          )
      ),
      "missing required columns"
    )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs[
          0,
        ]
      ),
      "must not be empty"
    )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs %>%
          dplyr::mutate(
            source =
              NA_character_
          )
      ),
      "missing required values"
    )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs %>%
          dplyr::mutate(
            is_true_candidate =
              as.integer(
                .data$is_true_candidate
              )
          )
      ),
      "must be logical"
    )
  }
)


testthat::test_that(
  "ML candidate validation rejects invalid feature values",
  {
    candidate_pairs <-
      make_valid_ml_candidate_pairs()

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs %>%
          dplyr::mutate(
            name_similarity =
              as.character(
                .data$name_similarity
              )
          )
      ),
      "must be numeric"
    )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs %>%
          dplyr::mutate(
            name_similarity =
              Inf
          )
      ),
      "finite values"
    )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        candidate_pairs %>%
          dplyr::mutate(
            name_similarity =
              1.1
          )
      ),
      "must lie in"
    )
  }
)


testthat::test_that(
  "ML candidate validation rejects pair-integrity violations",
  {
    candidate_pairs <-
      make_valid_ml_candidate_pairs()

    duplicate_pairs <-
      dplyr::bind_rows(
        candidate_pairs,
        candidate_pairs[
          1,
        ]
      )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        duplicate_pairs
      ),
      "Duplicate ML candidate pairs"
    )

    multiple_true_candidates <-
      candidate_pairs %>%
      dplyr::mutate(
        is_true_candidate =
          TRUE
      )

    testthat::expect_error(
      validate_ml_candidate_pairs(
        multiple_true_candidates
      ),
      "more than one true candidate"
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


