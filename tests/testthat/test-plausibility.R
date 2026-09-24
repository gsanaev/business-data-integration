# =====================================================================
# test-plausibility.R
# Tests for validation and plausibility helper functions
# =====================================================================

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "helpers",
    "plausibility.R"
  ),
  local = TRUE
)

testthat::test_that("safe_median handles finite and non-finite values", {
  testthat::expect_equal(
    safe_median(
      c(
        1,
        2,
        3,
        NA,
        Inf
      )
    ),
    2
  )

  testthat::expect_true(
    is.na(
      safe_median(
        c(
          NA,
          Inf,
          -Inf
        )
      )
    )
  )
})


testthat::test_that("interpolate_series interpolates only internal gaps", {
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

  testthat::expect_equal(
    internal_gap,
    c(
      10,
      20,
      30,
      40,
      50
    )
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

  testthat::expect_equal(
    boundary_gaps,
    c(
      NA,
      20,
      30,
      40,
      NA
    )
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

  testthat::expect_equal(
    single_observation,
    c(
      NA,
      NA,
      30,
      NA,
      NA
    )
  )

  no_observations <- interpolate_series(
    index,
    rep(
      NA_real_,
      5
    )
  )

  testthat::expect_true(
    all(
      is.na(
        no_observations
      )
    )
  )

  complete_series <- c(
    10,
    20,
    30,
    40,
    50
  )

  testthat::expect_equal(
    interpolate_series(
      index,
      complete_series
    ),
    complete_series
  )
})


testthat::test_that("relative_difference follows the existing v2 definition", {
  testthat::expect_equal(
    relative_difference(
      100,
      90
    ),
    0.1
  )

  testthat::expect_equal(
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
    )
  )

  testthat::expect_true(
    is.na(
      relative_difference(
        NA_real_,
        100
      )
    )
  )

  testthat::expect_true(
    is.na(
      relative_difference(
        0,
        0
      )
    )
  )
})
