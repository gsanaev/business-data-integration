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

employment <- expand_grid(
  truth_firm_id = firm_truth$truth_firm_id,
  month = months
) %>%
  mutate(
    year = as.integer(format(month, "%Y")),
    month_num = as.integer(format(month, "%m"))
  ) %>%
  left_join(
    annual_truth %>%
      select(
        truth_firm_id,
        year,
        employees_true
      ),
    by = c("truth_firm_id", "year")
  ) %>%
  left_join(
    firm_truth %>%
      select(
        truth_firm_id,
        nace_code,
        region_code
      ),
    by = "truth_firm_id"
  ) %>%
  mutate(
    seasonal_factor = mapply(
      function(code, m) {
        employment_seasonality[[code]][m]
      },
      nace_code,
      month_num
    ),

    monthly_noise = exp(
      rnorm(n(), mean = 0, sd = 0.020)
    ),

    employment_weight =
      seasonal_factor * monthly_noise
  ) %>%
  group_by(truth_firm_id, year) %>%
  mutate(
    # Re-normalise firm-year fluctuations so annual average employment
    # remains close to the latent annual employment level.
    employment_weight =
      employment_weight / mean(employment_weight),

    employees = pmax(
      1,
      round(
        employees_true * employment_weight
      )
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Preserve the complete source value before injected imperfections.
    employees_source_complete = employees,

    # Rare reporting spikes.
    employees = ifelse(
      runif(n()) < 0.003,
      round(
        employees *
          runif(n(), min = 1.8, max = 2.8)
      ),
      employees
    ),

    # 1% missing monthly employment observations.
    employees = ifelse(
      runif(n()) < 0.01,
      NA,
      employees
    )
  ) %>%
  select(
    truth_firm_id,
    month,
    nace_code,
    region_code,
    seasonal_factor,
    employees_source_complete,
    employees
  )

# ----------------------------------------------------------------------
# 7. Create coherent monthly turnover observations
# ----------------------------------------------------------------------

turnover <- expand_grid(
  truth_firm_id = firm_truth$truth_firm_id,
  month = months
) %>%
  mutate(
    year = as.integer(format(month, "%Y")),
    month_num = as.integer(format(month, "%m"))
  ) %>%
  left_join(
    annual_truth %>%
      select(
        truth_firm_id,
        year,
        annual_turnover_true
      ),
    by = c("truth_firm_id", "year")
  ) %>%
  left_join(
    firm_truth %>%
      select(
        truth_firm_id,
        nace_code,
        region_code
      ),
    by = "truth_firm_id"
  ) %>%
  mutate(
    seasonal_factor = mapply(
      function(code, m) {
        turnover_seasonality[[code]][m]
      },
      nace_code,
      month_num
    ),

    allocation_noise = exp(
      rnorm(n(), mean = 0, sd = 0.040)
    ),

    allocation_weight =
      seasonal_factor * allocation_noise
  ) %>%
  group_by(truth_firm_id, year) %>%
  mutate(
    monthly_share =
      allocation_weight / sum(allocation_weight),

    # Latent monthly turnover sums exactly to latent annual turnover.
    turnover_true =
      annual_turnover_true * monthly_share,

    # Reported monthly turnover contains modest measurement variation.
    turnover = round(
      turnover_true *
        exp(rnorm(n(), mean = 0, sd = 0.020)),
      2
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Preserve the complete source value before injected imperfections.
    turnover_source_complete = turnover,

    # Rare sign/reporting errors.
    turnover = ifelse(
      runif(n()) < 0.002,
      -turnover,
      turnover
    ),

    # 1% missing monthly turnover observations.
    turnover = ifelse(
      runif(n()) < 0.01,
      NA,
      turnover
    )
  ) %>%
  select(
    truth_firm_id,
    month,
    nace_code,
    region_code,
    turnover_source_complete,
    turnover
  )

# ----------------------------------------------------------------------
# 8. Create stable synthetic enterprise identities
# ----------------------------------------------------------------------

location_lookup <- tibble(
  region_code = regions$region_code,
  city = c(
    "Frankfurt am Main",
    "Wiesbaden",
    "Darmstadt",
    "Mainz",
    "Kassel",
    "Mannheim",
    "Heidelberg",
    "Karlsruhe",
    "Fulda",
    "Giessen"
  ),
  postal_code = c(
    "60311",
    "65183",
    "64283",
    "55116",
    "34117",
    "68159",
    "69117",
    "76133",
    "36037",
    "35390"
  )
)

name_prefixes <- c(
  "Nordstern",
  "Rheinblick",
  "Mainwerk",
  "Hansa",
  "Bergtal",
  "Westtor",
  "Suedpark",
  "Adler",
  "Linden",
  "Taunus",
  "Neckar",
  "Waldhof",
  "Mittelrhein",
  "Eichen",
  "Silber",
  "Kronen",
  "Markt",
  "Feldberg",
  "Rosen",
  "Central"
)

name_activities <- c(
  "Handel",
  "Logistik",
  "Industrie",
  "Technik",
  "Produktion",
  "Vertrieb",
  "Transport",
  "Lebensmittel",
  "Bau & Service",
  "Hotel",
  "Gastronomie",
  "Mobilitaet",
  "Dienstleistungen",
  "Versorgung",
  "Werk"
)

street_names <- c(
  "Hauptstrasse",
  "Bahnhofstrasse",
  "Industriestrasse",
  "Marktstrasse",
  "Rheinstrasse",
  "Goethestrasse",
  "Schillerstrasse",
  "Gartenweg",
  "Feldweg",
  "Lindenweg"
)

identity_truth <- firm_truth %>%
  select(
    truth_firm_id,
    region_code,
    nace_code,
    legal_form,
    foundation_year
  ) %>%
  left_join(
    location_lookup,
    by = "region_code"
  ) %>%
  mutate(
    business_id = sprintf(
      "B%07d",
      seq_len(n())
    ),

    legal_form_label = case_when(
      legal_form == "Einzelunternehmen" ~ "",
      TRUE ~ legal_form
    ),

    enterprise_name = trimws(
      paste(
        sample(
          name_prefixes,
          n(),
          replace = TRUE
        ),
        sample(
          name_activities,
          n(),
          replace = TRUE
        ),
        legal_form_label
      )
    ),

    street = paste(
      sample(
        street_names,
        n(),
        replace = TRUE
      ),
      sample(
        1:180,
        n(),
        replace = TRUE
      )
    )
  ) %>%
  select(
    -legal_form_label
  )

# ----------------------------------------------------------------------
# 9. Derive source-specific enterprise identities
# ----------------------------------------------------------------------

register_identity <- identity_truth %>%
  transmute(
    truth_firm_id,
    register_id = sample(
      make_source_id(
        "REG",
        n_firms
      )
    ),
    business_id,
    enterprise_name = perturb_company_name(
      enterprise_name
    ),
    street = perturb_street(
      street
    ),
    postal_code,
    city = perturb_city(
      city
    )
  )

employment_identity <- identity_truth %>%
  transmute(
    truth_firm_id,
    employment_source_id = sample(
      make_source_id(
        "EMP",
        n_firms
      )
    ),
    business_id = drop_identifier(
      business_id,
      probability = 0.10
    ),
    enterprise_name = perturb_company_name(
      enterprise_name
    ),
    street = perturb_street(
      street
    ),
    postal_code,
    city = perturb_city(
      city
    ),
    legal_form
  )

turnover_identity <- identity_truth %>%
  transmute(
    truth_firm_id,
    turnover_source_id = sample(
      make_source_id(
        "TUR",
        n_firms
      )
    ),
    business_id = drop_identifier(
      business_id,
      probability = 0.10
    ),
    enterprise_name = perturb_company_name(
      enterprise_name
    ),
    street = perturb_street(
      street
    ),
    postal_code,
    city = perturb_city(
      city
    ),
    legal_form
  )

# ----------------------------------------------------------------------
# 10. Generate annual accounting source
# ----------------------------------------------------------------------

accounting_params <- tibble(
  nace_code = c(
    "G47",
    "C10",
    "C29",
    "H49",
    "I55",
    "I56"
  ),
  accounting_revenue_factor = c(
    1.01,
    0.99,
    1.02,
    1.00,
    0.98,
    1.01
  ),
  purchases_share_center = c(
    0.72,
    0.58,
    0.62,
    0.45,
    0.40,
    0.48
  ),
  personnel_cost_per_employee = c(
    38000,
    45000,
    55000,
    42000,
    34000,
    32000
  )
)

accounting <- annual_truth %>%
  select(
    truth_firm_id,
    year,
    nace_code,
    employees_true,
    annual_turnover_true
  ) %>%
  left_join(
    accounting_params,
    by = "nace_code"
  ) %>%
  mutate(
    reference_year = year,

    # Accounting operating revenue is related to statistical turnover,
    # but is deliberately not treated as an identical concept.
    operating_revenue_complete = round(
      annual_turnover_true *
        accounting_revenue_factor *
        exp(
          rnorm(
            n(),
            mean = 0,
            sd = 0.020
          )
        ),
      2
    ),

    purchases_share = pmin(
      0.85,
      pmax(
        0.20,
        purchases_share_center +
          rnorm(
            n(),
            mean = 0,
            sd = 0.025
          )
      )
    ),

    purchases_goods_services_complete = round(
      operating_revenue_complete *
        purchases_share,
      2
    ),

    personnel_expense_complete = round(
      employees_true *
        personnel_cost_per_employee *
        exp(
          rnorm(
            n(),
            mean = 0,
            sd = 0.030
          )
        ),
      2
    ),

    # Controlled source-specific imperfections.
    operating_revenue = ifelse(
      runif(n()) < 0.01,
      NA,
      operating_revenue_complete
    ),

    purchases_goods_services = ifelse(
      runif(n()) < 0.005,
      -purchases_goods_services_complete,
      purchases_goods_services_complete
    ),

    personnel_expense = ifelse(
      runif(n()) < 0.01,
      NA,
      personnel_expense_complete
    )
  ) %>%
  select(
    truth_firm_id,
    reference_year,
    nace_code,
    operating_revenue_complete,
    purchases_goods_services_complete,
    personnel_expense_complete,
    operating_revenue,
    purchases_goods_services,
    personnel_expense
  )

accounting_identity <- identity_truth %>%
  transmute(
    truth_firm_id,

    accounting_source_id = sample(
      make_source_id(
        "ACC",
        n_firms
      )
    ),

    business_id = drop_identifier(
      business_id,
      probability = 0.10
    ),

    enterprise_name = perturb_company_name(
      enterprise_name
    ),

    street = perturb_street(
      street
    ),

    postal_code,

    city = perturb_city(
      city
    ),

    legal_form,
    nace_code
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
