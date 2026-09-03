# =====================================================================
# assertions.R
# Minimal assertion helpers for dependency-free project tests
# =====================================================================

assert_true <- function(condition, message) {
  if (
    length(condition) != 1L ||
      is.na(condition) ||
      !condition
  ) {
    stop(
      message,
      call. = FALSE
    )
  }

  invisible(TRUE)
}


assert_equal <- function(actual, expected, message) {
  equal <- isTRUE(
    all.equal(
      actual,
      expected,
      check.attributes = FALSE
    )
  )

  if (!equal) {
    stop(
      paste0(
        message,
        "\nExpected: ",
        paste(
          expected,
          collapse = ", "
        ),
        "\nActual: ",
        paste(
          actual,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


assert_all_na <- function(x, message) {
  assert_true(
    all(
      is.na(x)
    ),
    message
  )
}
