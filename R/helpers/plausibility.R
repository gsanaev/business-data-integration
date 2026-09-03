# =====================================================================
# plausibility.R
# Helper functions for validation and plausibility processing
# =====================================================================

# Return a numeric median while handling groups without usable values.
safe_median <- function(x) {
  usable <- x[is.finite(x)]

  if (length(usable) == 0) {
    return(NA_real_)
  }

  median(usable)
}


# Interpolate missing values only between observed points.
# Invalid values should be converted to NA before calling this function.
# Missing values before the first or after the last observed value remain
# missing rather than being extrapolated.
interpolate_series <- function(index, values) {
  observed <- !is.na(values) & is.finite(values)

  if (sum(observed) == 0) {
    return(
      rep(
        NA_real_,
        length(values)
      )
    )
  }

  if (sum(observed) == 1) {
    result <- rep(
      NA_real_,
      length(values)
    )

    result[observed] <-
      values[observed]

    return(result)
  }

  approx(
    x = as.numeric(
      index[observed]
    ),
    y = values[observed],
    xout = as.numeric(index),
    rule = 1
  )$y
}


# Relative difference for comparisons between two positive quantities.
relative_difference <- function(x, y) {
  denominator <- pmax(abs(x), abs(y))

  ifelse(
    is.na(x) | is.na(y) | denominator == 0,
    NA_real_,
    abs(x - y) / denominator
  )
}
