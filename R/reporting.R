# =====================================================================
# reporting.R
# Reporting and display helper functions
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Function behavior is intentionally unchanged at this stage.
# =====================================================================

format_millions <- function(x) {
  paste0(
    format(
      round(
        x / 1e6,
        1
      ),
      trim = TRUE,
      scientific = FALSE
    ),
    " M"
  )
}

format_thousands <- function(x) {
  paste0(
    format(
      round(
        x / 1e3,
        0
      ),
      big.mark = ",",
      trim = TRUE,
      scientific = FALSE
    ),
    "k"
  )
}
