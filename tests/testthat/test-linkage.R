# =====================================================================
# test-linkage.R
# Unit tests for enterprise linkage workflow
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
    "linkage_similarity.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_decision.R"
  ),
  local = TRUE
)


testthat::test_that(
  "unknown non-missing identifier enters Level-2 similarity linkage",
  {
    register_entities <-
      tibble::tibble(
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

    register_lookup <-
      register_entities %>%
      select(
        canonical_firm_id,
        register_id,
        business_id
      )

    source_entities <-
      tibble::tibble(
        employment_source_id =
          c(
            "E001",
            "E002"
          ),
        business_id =
          c(
            "UNKNOWN_EMPLOYMENT_000001",
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

    similarity_weights <-
      c(
        name_similarity = 0.40,
        street_similarity = 0.30,
        city_similarity = 0.05,
        postal_code_match = 0.10,
        legal_form_match = 0.075,
        nace_match = 0.075
      )

    result <-
      link_source_entities(
        source_entities =
          source_entities,
        source_id_column =
          "employment_source_id",
        register_lookup =
          register_lookup,
        register_entities =
          register_entities,
        similarity_weights =
          similarity_weights,
        score_threshold =
          0.85,
        margin_threshold =
          0.05
      )

    unknown_link <-
      result$links %>%
      filter(
        employment_source_id ==
          "E001"
      )

    deterministic_link <-
      result$links %>%
      filter(
        employment_source_id ==
          "E002"
      )

    testthat::expect_equal(
      unknown_link$business_id,
      "UNKNOWN_EMPLOYMENT_000001"
    )

    testthat::expect_equal(
      unknown_link$linkage_status,
      "matched_similarity"
    )

    testthat::expect_equal(
      unknown_link$linkage_method,
      "weighted_edit_similarity"
    )

    testthat::expect_equal(
      unknown_link$register_id,
      "R001"
    )

    testthat::expect_equal(
      unknown_link$canonical_firm_id,
      "C000001"
    )

    testthat::expect_equal(
      unknown_link$candidate_register_id,
      "R001"
    )

    testthat::expect_equal(
      unknown_link$top_similarity_score,
      1
    )

    testthat::expect_equal(
      deterministic_link$linkage_status,
      "matched_deterministic"
    )

    testthat::expect_equal(
      deterministic_link$linkage_method,
      "business_id_exact"
    )

    testthat::expect_equal(
      deterministic_link$register_id,
      "R002"
    )

    testthat::expect_equal(
      nrow(
        result$similarity$candidates
      ),
      1L
    )
  }
)
