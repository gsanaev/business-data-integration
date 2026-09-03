# =====================================================================
# test_linkage_similarity.R
# Tests for transparent enterprise-linkage similarity helpers
# =====================================================================

source(
  "tests/helpers/assertions.R"
)

source(
  "R/helpers/linkage_similarity.R"
)

# ----------------------------------------------------------------------
# normalize_linkage_text()
# ----------------------------------------------------------------------

assert_equal(
  normalize_linkage_text(
    "  Alpha-GMBH!!  "
  ),
  "alpha gmbh",
  "Linkage text normalization should remove punctuation and normalize case."
)

assert_equal(
  normalize_linkage_text(
    "Alpha   Beta"
  ),
  "alpha beta",
  "Linkage text normalization should collapse repeated whitespace."
)

assert_equal(
  normalize_linkage_text(
    c(
      "Alpha GmbH",
      "BETA AG"
    )
  ),
  c(
    "alpha gmbh",
    "beta ag"
  ),
  "Linkage text normalization should operate vector-wise."
)

assert_true(
  is.na(
    normalize_linkage_text(
      NA_character_
    )
  ),
  "Missing linkage text should remain missing."
)

# ----------------------------------------------------------------------
# normalized_edit_similarity()
# ----------------------------------------------------------------------

assert_equal(
  normalized_edit_similarity(
    "Alpha GmbH",
    "alpha gmbh"
  ),
  1,
  "Equivalent normalized strings should have similarity 1."
)

assert_equal(
  normalized_edit_similarity(
    "abcd",
    "abc"
  ),
  0.75,
  "Normalized edit similarity should scale edit distance by the longer string."
)

similarity_xy <- normalized_edit_similarity(
  "alpha",
  "alfa"
)

similarity_yx <- normalized_edit_similarity(
  "alfa",
  "alpha"
)

assert_equal(
  similarity_xy,
  similarity_yx,
  "Normalized edit similarity should be symmetric."
)

assert_true(
  similarity_xy >= 0 &&
    similarity_xy <= 1,
  "Normalized edit similarity should remain within [0, 1]."
)

assert_true(
  is.na(
    normalized_edit_similarity(
      "",
      "alpha"
    )
  ),
  "Empty normalized strings should not receive a similarity score."
)

assert_true(
  is.na(
    normalized_edit_similarity(
      NA_character_,
      "alpha"
    )
  ),
  "Missing strings should not receive a similarity score."
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

assert_equal(
  vector_similarity,
  c(
    1,
    0.75
  ),
  "Normalized edit similarity should operate vector-wise."
)

# ----------------------------------------------------------------------
# normalized_exact_match()
# ----------------------------------------------------------------------

assert_equal(
  normalized_exact_match(
    "Alpha-GmbH",
    "alpha gmbh"
  ),
  1,
  "Equivalent normalized strings should be exact matches."
)

assert_equal(
  normalized_exact_match(
    "Alpha GmbH",
    "Alpha AG"
  ),
  0,
  "Different normalized strings should not be exact matches."
)

assert_equal(
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
  ),
  "Exact matching should handle vectors, missing values, and empty strings."
)

cat(
  "test_linkage_similarity.R: PASS\n"
)
