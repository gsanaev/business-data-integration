# =====================================================================
# synthetic.R
# Synthetic-data generation helper functions
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Function behavior is intentionally unchanged at this stage.
# =====================================================================

normalize_mean_one <- function(x) {
  x / mean(x)
}
