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


testthat::test_that(
  "similarity decisions distinguish match ambiguity and low score",
  {
    ranked_candidates <-
      tibble::tibble(
        source_record_id =
          c(
            "E001",
            "E001",
            "E002",
            "E002",
            "E003"
          ),
        register_id =
          c(
            "R001",
            "R002",
            "R003",
            "R004",
            "R005"
          ),
        canonical_firm_id =
          c(
            "C000001",
            "C000002",
            "C000003",
            "C000004",
            "C000005"
          ),
        similarity_score =
          c(
            0.95,
            0.70,
            0.90,
            0.88,
            0.70
          ),
        candidate_rank =
          c(
            1L,
            2L,
            1L,
            2L,
            1L
          )
      )

    result <-
      decide_similarity_candidates(
        ranked_candidates =
          ranked_candidates,
        score_threshold =
          0.85,
        margin_threshold =
          0.05
      )

    matched <-
      result %>%
      dplyr::filter(
        .data$source_record_id ==
          "E001"
      )

    ambiguous <-
      result %>%
      dplyr::filter(
        .data$source_record_id ==
          "E002"
      )

    low_score <-
      result %>%
      dplyr::filter(
        .data$source_record_id ==
          "E003"
      )

    testthat::expect_equal(
      matched$similarity_status,
      "matched_similarity"
    )

    testthat::expect_equal(
      matched$similarity_margin,
      0.25
    )

    testthat::expect_equal(
      ambiguous$similarity_status,
      "review_required_similarity_ambiguous"
    )

    testthat::expect_equal(
      ambiguous$similarity_margin,
      0.02,
      tolerance = 1e-12
    )

    testthat::expect_equal(
      low_score$similarity_status,
      "unmatched_low_similarity"
    )

    testthat::expect_equal(
      low_score$similarity_margin,
      0.70
    )
  }
)


testthat::test_that(
  "missing postcode contributes zero evidence without making score missing",
  {
    candidate <-
      tibble::tibble(
        source_record_id =
          "E001",
        register_id =
          "R001",
        canonical_firm_id =
          "C000001",
        enterprise_name_source =
          "Alpha GmbH",
        enterprise_name_register =
          "Alpha GmbH",
        street_source =
          "Hauptstrasse 1",
        street_register =
          "Hauptstrasse 1",
        postal_code_source =
          NA_character_,
        postal_code_register =
          "12345",
        city_source =
          "Berlin",
        city_register =
          "Berlin",
        legal_form_source =
          "GmbH",
        legal_form_register =
          "GmbH",
        nace_code_source =
          "G47",
        nace_code_register =
          "G47"
      )

    weights <-
      c(
        name_similarity = 0.40,
        street_similarity = 0.30,
        city_similarity = 0.05,
        postal_code_match = 0.10,
        legal_form_match = 0.075,
        nace_match = 0.075
      )

    scored <-
      candidate %>%
      add_linkage_features() %>%
      score_and_rank_similarity_candidates(
        weights
      )

    testthat::expect_equal(
      scored$postal_code_match,
      0
    )

    testthat::expect_false(
      is.na(
        scored$similarity_score
      )
    )

    testthat::expect_equal(
      scored$similarity_score,
      0.90
    )
  }
)
