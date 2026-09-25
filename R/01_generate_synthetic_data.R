# =====================================================================
# 01_generate_synthetic_data.R
# Synthetic Enterprise Data Generator
# ---------------------------------------------------------------------
# This script creates coherent synthetic enterprise datasets resembling
# register, employment, turnover, and annual accounting sources used
# in structural and short-term business-statistics workflows.
#
# The data-generating process first creates a common latent enterprise
# reality. Source-specific observations are then derived from that common
# structure and receive controlled measurement error and missingness.
#
# Output:
#   data/raw/firms.csv
#   data/raw/employment.csv
#   data/raw/turnover.csv
#   data/raw/accounting.csv
#
# Notes:
#   - No real data is used; all values are simulated.
#   - Monthly data cover January 2023 through December 2025.
#   - Register, employment, and turnover observations describe the same
#     underlying enterprises.
#   - Intentional source-specific imperfections are added only after the
#     coherent underlying values have been generated.
# =====================================================================

# Load packages ---------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)

source("R/helpers/synthetic_identity.R")
source("R/synthetic.R")

set.seed(2025)

# Ensure output directories exist ------------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/truth", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Create reference structures
# ----------------------------------------------------------------------

reference_structures <-
  create_synthetic_reference_structures()

regions <-
  reference_structures$regions

industry_params <-
  reference_structures$industry_params

legal_forms <-
  reference_structures$legal_forms

# ----------------------------------------------------------------------
# 2. Generate stable latent enterprise characteristics
# ----------------------------------------------------------------------

n_firms <- 1500

firm_truth <-
  generate_latent_enterprises(
    regions,
    industry_params,
    legal_forms,
    n_firms
  )

# ----------------------------------------------------------------------
# 3. Generate annual latent enterprise states, 2023-2025
# ----------------------------------------------------------------------

years <- 2023:2025

annual_truth <-
  generate_annual_latent_states(
    firm_truth,
    years
  )

# ----------------------------------------------------------------------
# 4. Create business-register-style snapshot
# ----------------------------------------------------------------------

firms_inconsistent <-
  generate_register_source(
    annual_truth
  )

# ----------------------------------------------------------------------
# 5. Define monthly reference period and seasonal profiles
# ----------------------------------------------------------------------

monthly_reference <-
  create_monthly_reference_profiles()

months <-
  monthly_reference$months

employment_seasonality <-
  monthly_reference$employment_seasonality

turnover_seasonality <-
  monthly_reference$turnover_seasonality

# ----------------------------------------------------------------------
# 6. Create coherent monthly employment observations
# ----------------------------------------------------------------------

employment <-
  generate_monthly_employment(
    firm_truth,
    annual_truth,
    months,
    employment_seasonality
  )

# ----------------------------------------------------------------------
# 7. Create coherent monthly turnover observations
# ----------------------------------------------------------------------

turnover <-
  generate_monthly_turnover(
    firm_truth,
    annual_truth,
    months,
    turnover_seasonality
  )

# ----------------------------------------------------------------------
# 8. Create stable synthetic enterprise identities
# ----------------------------------------------------------------------

identity_truth <-
  create_enterprise_identity_truth(
    firm_truth,
    regions
  )

# ----------------------------------------------------------------------
# 9. Derive source-specific enterprise identities
# ----------------------------------------------------------------------

primary_identities <-
  generate_primary_source_identities(
    identity_truth,
    n_firms
  )

register_identity <-
  primary_identities$register

employment_identity <-
  primary_identities$employment

turnover_identity <-
  primary_identities$turnover

# ----------------------------------------------------------------------
# 10. Generate annual accounting source
# ----------------------------------------------------------------------

accounting <-
  generate_accounting_source(
    annual_truth
  )

accounting_identity <-
  generate_accounting_identity(
    identity_truth,
    n_firms
  )

# ----------------------------------------------------------------------
# 11. Attach source identities
# ----------------------------------------------------------------------

attached_sources <-
  attach_synthetic_source_identities(
    firms_inconsistent,
    employment,
    turnover,
    accounting,
    register_identity,
    employment_identity,
    turnover_identity,
    accounting_identity
  )

# ----------------------------------------------------------------------
# 12. Build operational source datasets
# ----------------------------------------------------------------------

operational_sources <-
  build_operational_synthetic_sources(
    attached_sources
  )

firms_operational <-
  operational_sources$firms

employment_operational <-
  operational_sources$employment

turnover_operational <-
  operational_sources$turnover

accounting_operational <-
  operational_sources$accounting

# ----------------------------------------------------------------------
# 13. Build hidden truth datasets
# ----------------------------------------------------------------------

truth_outputs <-
  build_synthetic_truth_outputs(
    identity_truth,
    register_identity,
    employment_identity,
    turnover_identity,
    accounting_identity,
    attached_sources
  )

enterprise_truth <-
  truth_outputs$enterprise

linkage_truth <-
  truth_outputs$linkage

value_truth <-
  truth_outputs$value

# ----------------------------------------------------------------------
# 14. Write operational and hidden datasets
# ----------------------------------------------------------------------

write_csv(
  firms_operational,
  "data/raw/firms.csv"
)

write_csv(
  employment_operational,
  "data/raw/employment.csv"
)

write_csv(
  turnover_operational,
  "data/raw/turnover.csv"
)

write_csv(
  accounting_operational,
  "data/raw/accounting.csv"
)

write_csv(
  enterprise_truth,
  "data/truth/enterprise_truth.csv"
)

write_csv(
  linkage_truth,
  "data/truth/linkage_truth.csv"
)

write_csv(
  value_truth,
  "data/truth/value_truth.csv"
)

message("Synthetic enterprise datasets generated successfully.")
message("Operational sources do not expose truth_firm_id.")
