# =====================================================================
# test-linkage-similarity.R
# Tests for transparent enterprise-linkage similarity helpers
# =====================================================================

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "linkage_similarity.R"
  ),
  local = TRUE
)

testthat::test_that("normalize_linkage_text standardizes linkage text", {
  testthat::expect_equal(
    normalize_linkage_text(
      "  Alpha-GMBH!!  "
    ),
    "alpha gmbh"
  )

  testthat::expect_equal(
    normalize_linkage_text(
      "Alpha   Beta"
    ),
    "alpha beta"
  )

  testthat::expect_equal(
    normalize_linkage_text(
      c(
        "Alpha GmbH",
        "BETA AG"
      )
    ),
    c(
      "alpha gmbh",
      "beta ag"
    )
  )

  testthat::expect_true(
    is.na(
      normalize_linkage_text(
        NA_character_
      )
    )
  )
})


testthat::test_that("normalized_edit_similarity follows the existing v2 definition", {
  testthat::expect_equal(
    normalized_edit_similarity(
      "Alpha GmbH",
      "alpha gmbh"
    ),
    1
  )

  testthat::expect_equal(
    normalized_edit_similarity(
      "abcd",
      "abc"
    ),
    0.75
  )

  similarity_xy <- normalized_edit_similarity(
    "alpha",
    "alfa"
  )

  similarity_yx <- normalized_edit_similarity(
    "alfa",
    "alpha"
  )

  testthat::expect_equal(
    similarity_xy,
    similarity_yx
  )

  testthat::expect_true(
    similarity_xy >= 0 &&
      similarity_xy <= 1
  )

  testthat::expect_true(
    is.na(
      normalized_edit_similarity(
        "",
        "alpha"
      )
    )
  )

  testthat::expect_true(
    is.na(
      normalized_edit_similarity(
        NA_character_,
        "alpha"
      )
    )
  )

  vector_similarity <- normalized_edit_similarity(
    c(
      "Alpha",
      "Beta"
    ),
    c(
      "alpha",
      "bet"
    )
  )

  testthat::expect_equal(
    vector_similarity,
    c(
      1,
      0.75
    )
  )
})


testthat::test_that("normalized_exact_match handles normalized and missing values", {
  testthat::expect_equal(
    normalized_exact_match(
      "Alpha-GmbH",
      "alpha gmbh"
    ),
    1
  )

  testthat::expect_equal(
    normalized_exact_match(
      "Alpha GmbH",
      "Alpha AG"
    ),
    0
  )

  testthat::expect_equal(
    normalized_exact_match(
      c(
        "Alpha GmbH",
        "Beta AG",
        NA_character_,
        ""
      ),
      c(
        "alpha gmbh",
        "Beta-AG",
        "Gamma GmbH",
        ""
      )
    ),
    c(
      1,
      1,
      0,
      0
    )
  )
})
