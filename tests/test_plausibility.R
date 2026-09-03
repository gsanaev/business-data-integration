# =====================================================================
# test_plausibility.R
# Tests for validation and plausibility helper functions
# =====================================================================

source(
  "tests/helpers/assertions.R"
)

source(
  "R/helpers/plausibility.R"
)

# ----------------------------------------------------------------------
# safe_median()
# ----------------------------------------------------------------------

assert_equal(
  safe_median(
    c(
      1,
      2,
      3,
      NA,
      Inf
    )
  ),
  2,
  "safe_median() should ignore missing and non-finite values."
)

assert_true(
  is.na(
    safe_median(
      c(
        NA,
        Inf,
        -Inf
      )
    )
  ),
  "safe_median() should return NA when no finite values are available."
)

# ----------------------------------------------------------------------
# interpolate_series()
# ----------------------------------------------------------------------

index <- as.Date(
  "2026-01-01"
) + 0:4

internal_gap <- interpolate_series(
  index,
  c(
    10,
    NA,
    NA,
    40,
    50
  )
)

assert_equal(
  internal_gap,
  c(
    10,
    20,
    30,
    40,
    50
  ),
  "Internal gaps should be linearly interpolated."
)

boundary_gaps <- interpolate_series(
  index,
  c(
    NA,
    20,
    NA,
    40,
    NA
  )
)

assert_equal(
  boundary_gaps,
  c(
    NA,
    20,
    30,
    40,
    NA
  ),
  "Boundary gaps should remain missing."
)

single_observation <- interpolate_series(
  index,
  c(
    NA,
    NA,
    30,
    NA,
    NA
  )
)

assert_equal(
  single_observation,
  c(
    NA,
    NA,
    30,
    NA,
    NA
  ),
  "A single observation should not be extrapolated across the series."
)

no_observations <- interpolate_series(
  index,
  rep(
    NA_real_,
    5
  )
)

assert_all_na(
  no_observations,
  "A series without observations should remain entirely missing."
)

complete_series <- c(
  10,
  20,
  30,
  40,
  50
)

assert_equal(
  interpolate_series(
    index,
    complete_series
  ),
  complete_series,
  "Observed complete series should remain unchanged."
)

# ----------------------------------------------------------------------
# relative_difference()
# ----------------------------------------------------------------------

assert_equal(
  relative_difference(
    100,
    90
  ),
  0.1,
  "relative_difference() should use the larger absolute value as denominator."
)

assert_equal(
  relative_difference(
    c(
      100,
      50
    ),
    c(
      90,
      50
    )
  ),
  c(
    0.1,
    0
  ),
  "relative_difference() should operate vector-wise."
)

assert_true(
  is.na(
    relative_difference(
      NA_real_,
      100
    )
  ),
  "relative_difference() should return NA when one value is missing."
)

assert_true(
  is.na(
    relative_difference(
      0,
      0
    )
  ),
  "relative_difference() should return NA when both values are zero."
)

cat(
  "test_plausibility.R: PASS\n"
)
