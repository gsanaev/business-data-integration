# =====================================================================
# 04_integrate_sources.R
# Integration of Linked Enterprise Sources
# ---------------------------------------------------------------------
# Validated source records are integrated only after entity linkage.
# The operational analytical key is canonical_firm_id.
#
# Records that remain unresolved in linkage are not silently assigned
# to another enterprise.
#
# Output:
#   data/processed/panel_data.csv
#   data/processed/accounting_annual.csv
# =====================================================================

library(dplyr)
library(readr)
library(lubridate)

source("R/integration.R")

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Load validated sources and linkage crosswalk
# ----------------------------------------------------------------------

firms <- read_csv(
  "data/clean/firms_clean.csv",
  show_col_types = FALSE
)

employment <- read_csv(
  "data/clean/employment_clean.csv",
  show_col_types = FALSE
)

turnover <- read_csv(
  "data/clean/turnover_clean.csv",
  show_col_types = FALSE
)

accounting <- read_csv(
  "data/clean/accounting_clean.csv",
  show_col_types = FALSE
)

crosswalk <- read_csv(
  "data/processed/linkage_crosswalk.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------------------
# 2. Build source-to-canonical maps
# ----------------------------------------------------------------------

source_maps <-
  build_source_maps(
    crosswalk
  )

# ----------------------------------------------------------------------
# 3. Attach canonical identifiers
# ----------------------------------------------------------------------

linked_sources <-
  attach_canonical_identifiers(
    firms,
    employment,
    turnover,
    accounting,
    source_maps
  )

firms_linked <-
  linked_sources$firms_linked

employment_linked <-
  linked_sources$employment_linked

turnover_linked <-
  linked_sources$turnover_linked

accounting_linked <-
  linked_sources$accounting_linked

# ----------------------------------------------------------------------
# 4. Report linkage coverage entering integration
# ----------------------------------------------------------------------

common_firms <-
  get_common_canonical_firms(
    firms_linked,
    employment_linked,
    turnover_linked
  )

message(
  "Canonical enterprises available in register, employment, and turnover: ",
  length(common_firms)
)

message(
  "Employment enterprises linked: ",
  n_distinct(employment_linked$canonical_firm_id)
)

message(
  "Turnover enterprises linked: ",
  n_distinct(turnover_linked$canonical_firm_id)
)

message(
  "Accounting enterprises linked: ",
  n_distinct(accounting_linked$canonical_firm_id)
)

# ----------------------------------------------------------------------
# 5. Prepare annual accounting analytical dataset
# ----------------------------------------------------------------------

accounting_annual <-
  prepare_accounting_annual(
    accounting_linked
  )

# ----------------------------------------------------------------------
# 6. Prepare monthly source-specific analytical columns
# ----------------------------------------------------------------------

source_panels <-
  prepare_monthly_source_panels(
    firms_linked,
    employment_linked,
    turnover_linked,
    common_firms
  )

firms_panel <-
  source_panels$firms_panel

employment_panel <-
  source_panels$employment_panel

turnover_panel <-
  source_panels$turnover_panel

# ----------------------------------------------------------------------
# 7. Build unified monthly panel
# ----------------------------------------------------------------------

message("Integrating linked datasets...")

panel <-
  build_monthly_panel(
    employment_panel,
    turnover_panel,
    firms_panel
  )

# ----------------------------------------------------------------------
# 8. Derive current analytical indicators
# ----------------------------------------------------------------------

message("Constructing monthly analytical indicators...")

panel <-
  derive_panel_indicators(
    panel
  )

# ----------------------------------------------------------------------
# 9. Diagnostic summary
# ----------------------------------------------------------------------

message("YoY turnover growth summary:")
print(
  summary(
    panel$turnover_yoy
  )
)

if (
  any(
    panel$employees_monthly <= 0,
    na.rm = TRUE
  )
) {
  warning(
    "Non-positive monthly employment values detected."
  )
}

# ----------------------------------------------------------------------
# 10. Write analysis-ready datasets
# ----------------------------------------------------------------------

write_csv(
  panel,
  "data/processed/panel_data.csv"
)

write_csv(
  accounting_annual,
  "data/processed/accounting_annual.csv"
)

message("Linked-source integration completed successfully.")
