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

firms_with_identity <- firms_inconsistent %>%
  left_join(
    register_identity,
    by = "truth_firm_id"
  )

employment_with_identity <- employment %>%
  left_join(
    employment_identity,
    by = "truth_firm_id"
  )

turnover_with_identity <- turnover %>%
  left_join(
    turnover_identity,
    by = "truth_firm_id"
  )

accounting_with_identity <- accounting %>%
  left_join(
    accounting_identity,
    by = c(
      "truth_firm_id",
      "nace_code"
    )
  )

# ----------------------------------------------------------------------
# 12. Build operational source datasets
# ----------------------------------------------------------------------

firms_operational <- firms_with_identity %>%
  select(
    register_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    region_code,
    nace_code,
    legal_form,
    employees,
    foundation_year,
    revenue_last_year,
    register_reference_year,
    revenue_reference_year
  )

employment_operational <- employment_with_identity %>%
  select(
    employment_source_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    legal_form,
    month,
    nace_code,
    region_code,
    seasonal_factor,
    employees
  )

turnover_operational <- turnover_with_identity %>%
  select(
    turnover_source_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    legal_form,
    month,
    nace_code,
    region_code,
    turnover
  )

accounting_operational <- accounting_with_identity %>%
  select(
    accounting_source_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    legal_form,
    reference_year,
    nace_code,
    operating_revenue,
    purchases_goods_services,
    personnel_expense
  )

# ----------------------------------------------------------------------
# 13. Build hidden truth datasets
# ----------------------------------------------------------------------

enterprise_truth <- identity_truth %>%
  select(
    truth_firm_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    region_code,
    nace_code,
    legal_form,
    foundation_year
  )

linkage_truth <- bind_rows(
  register_identity %>%
    transmute(
      source = "register",
      source_record_id = register_id,
      truth_firm_id
    ),

  employment_identity %>%
    transmute(
      source = "employment",
      source_record_id = employment_source_id,
      truth_firm_id
    ),

  turnover_identity %>%
    transmute(
      source = "turnover",
      source_record_id = turnover_source_id,
      truth_firm_id
    ),

  accounting_identity %>%
    transmute(
      source = "accounting",
      source_record_id = accounting_source_id,
      truth_firm_id
    )
)

value_truth <- bind_rows(
  firms_with_identity %>%
    transmute(
      source = "register",
      source_record_id = register_id,
      truth_firm_id,
      reference_period = as.character(
        register_reference_year
      ),
      variable = "employees",
      truth_value = as.numeric(
        employees_register_complete
      )
    ),

  firms_with_identity %>%
    transmute(
      source = "register",
      source_record_id = register_id,
      truth_firm_id,
      reference_period = as.character(
        revenue_reference_year
      ),
      variable = "revenue_last_year",
      truth_value = as.numeric(
        revenue_last_year_complete
      )
    ),

  employment_with_identity %>%
    transmute(
      source = "employment",
      source_record_id = employment_source_id,
      truth_firm_id,
      reference_period = format(
        month,
        "%Y-%m"
      ),
      variable = "employees",
      truth_value = as.numeric(
        employees_source_complete
      )
    ),

  turnover_with_identity %>%
    transmute(
      source = "turnover",
      source_record_id = turnover_source_id,
      truth_firm_id,
      reference_period = format(
        month,
        "%Y-%m"
      ),
      variable = "turnover",
      truth_value = as.numeric(
        turnover_source_complete
      )
    ),

  accounting_with_identity %>%
    transmute(
      source = "accounting",
      source_record_id = accounting_source_id,
      truth_firm_id,
      reference_period = as.character(
        reference_year
      ),
      variable = "operating_revenue",
      truth_value = as.numeric(
        operating_revenue_complete
      )
    ),

  accounting_with_identity %>%
    transmute(
      source = "accounting",
      source_record_id = accounting_source_id,
      truth_firm_id,
      reference_period = as.character(
        reference_year
      ),
      variable = "purchases_goods_services",
      truth_value = as.numeric(
        purchases_goods_services_complete
      )
    ),

  accounting_with_identity %>%
    transmute(
      source = "accounting",
      source_record_id = accounting_source_id,
      truth_firm_id,
      reference_period = as.character(
        reference_year
      ),
      variable = "personnel_expense",
      truth_value = as.numeric(
        personnel_expense_complete
      )
    )
)

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
