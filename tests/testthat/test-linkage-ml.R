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
