# =====================================================================
# 01_generate_synthetic_data.R
# Synthetic Enterprise Data Generator
# ---------------------------------------------------------------------
# This script creates coherent synthetic enterprise datasets resembling
# register, employment, and turnover sources used in structural and
# short-term business statistics.
#
# The data-generating process first creates a common latent enterprise
# reality. Source-specific observations are then derived from that common
# structure and receive controlled measurement error and missingness.
#
# Output:
#   data/raw/firms.csv
#   data/raw/employment.csv
#   data/raw/turnover.csv
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

set.seed(2025)

# Ensure output directory exists ---------------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Create reference structures
# ----------------------------------------------------------------------

regions <- tibble(
  region_code = sprintf("R%02d", 1:10),
  region_name = paste("Region", 1:10)
)

industry_params <- tibble(
  nace_code = c("G47", "C10", "C29", "H49", "I55", "I56"),
  industry_name = c(
    "Retail Trade",
    "Food Manufacturing",
    "Automotive Manufacturing",
    "Land Transport",
    "Accommodation",
    "Food & Beverage Services"
  ),
  employment_center = c(18, 35, 70, 28, 20, 15),
  turnover_per_employee = c(
    180000,
    140000,
    210000,
    120000,
    110000,
    90000
  ),
  employment_growth_mean = c(
    0.015,
    0.010,
    0.008,
    0.012,
    0.020,
    0.018
  ),
  productivity_growth_mean = c(
    0.035,
    0.030,
    0.030,
    0.025,
    0.040,
    0.035
  )
)

legal_forms <- tibble(
  legal_form = c("AG", "GmbH", "KG", "OHG", "Einzelunternehmen")
)

# ----------------------------------------------------------------------
# 2. Generate stable latent enterprise characteristics
# ----------------------------------------------------------------------

n_firms <- 1500

firm_truth <- tibble(
  firm_id = sprintf("F%05d", 1:n_firms),
  region_code = sample(
    regions$region_code,
    n_firms,
    replace = TRUE
  ),
  nace_code = sample(
    industry_params$nace_code,
    n_firms,
    replace = TRUE
  ),
  legal_form = sample(
    legal_forms$legal_form,
    n_firms,
    replace = TRUE
  ),
  foundation_year = sample(
    1965:2022,
    n_firms,
    replace = TRUE
  )
) %>%
  left_join(industry_params, by = "nace_code") %>%
  mutate(
    # Firm-specific employment scale around the industry centre.
    baseline_employment = pmax(
      1,
      round(
        rlnorm(
          n(),
          meanlog = log(employment_center),
          sdlog = 0.65
        )
      )
    ),

    # Persistent productivity heterogeneity across firms.
    firm_productivity_factor = rlnorm(
      n(),
      meanlog = 0,
      sdlog = 0.30
    ),

    # Firm-specific medium-term employment growth.
    employment_growth_rate = pmin(
      0.12,
      pmax(
        -0.08,
        rnorm(
          n(),
          mean = employment_growth_mean,
          sd = 0.025
        )
      )
    ),

    # Firm-specific growth in turnover per employee.
    productivity_growth_rate = pmin(
      0.12,
      pmax(
        -0.06,
        rnorm(
          n(),
          mean = productivity_growth_mean,
          sd = 0.025
        )
      )
    ),

    turnover_per_employee_2023 =
      turnover_per_employee * firm_productivity_factor
  )

# ----------------------------------------------------------------------
# 3. Generate annual latent enterprise states, 2023-2025
# ----------------------------------------------------------------------

years <- 2023:2025

annual_truth <- expand_grid(
  firm_id = firm_truth$firm_id,
  year = years
) %>%
  left_join(firm_truth, by = "firm_id") %>%
  mutate(
    years_since_2023 = year - 2023L,

    # Small annual deviations around the firm-specific employment path.
    employment_noise = exp(
      rnorm(n(), mean = 0, sd = 0.015)
    ),

    employees_true = pmax(
      1,
      round(
        baseline_employment *
          (1 + employment_growth_rate)^years_since_2023 *
          employment_noise
      )
    ),

    # Small annual deviations around the productivity path.
    productivity_noise = exp(
      rnorm(n(), mean = 0, sd = 0.020)
    ),

    turnover_per_employee_true =
      turnover_per_employee_2023 *
      (1 + productivity_growth_rate)^years_since_2023 *
      productivity_noise,

    annual_turnover_true = round(
      employees_true * turnover_per_employee_true,
      2
    )
  )

# ----------------------------------------------------------------------
# 4. Create business-register-style snapshot
# ----------------------------------------------------------------------

register_2025 <- annual_truth %>%
  filter(year == 2025) %>%
  transmute(
    firm_id,
    region_code,
    nace_code,
    legal_form,
    foundation_year,
    employees_true_2025 = employees_true
  )

revenue_2024 <- annual_truth %>%
  filter(year == 2024) %>%
  transmute(
    firm_id,
    revenue_true_2024 = annual_turnover_true
  )

firms <- register_2025 %>%
  left_join(revenue_2024, by = "firm_id") %>%
  mutate(
    register_reference_year = 2025L,
    revenue_reference_year = 2024L,

    # Register values are related to the latent enterprise state but
    # contain modest source-specific measurement variation.
    employees = pmax(
      1,
      round(
        employees_true_2025 *
          exp(rnorm(n(), mean = 0, sd = 0.030))
      )
    ),

    revenue_last_year = round(
      revenue_true_2024 *
        exp(rnorm(n(), mean = 0, sd = 0.040)),
      2
    )
  ) %>%
  select(
    firm_id,
    region_code,
    nace_code,
    legal_form,
    employees,
    foundation_year,
    revenue_last_year,
    register_reference_year,
    revenue_reference_year
  )

# Inject controlled register imperfections -----------------------------

firms_inconsistent <- firms %>%
  mutate(
    # 2% missing employment values.
    employees = ifelse(
      runif(n()) < 0.02,
      NA,
      employees
    ),

    # 2% sign errors in reported prior-year revenue.
    revenue_last_year = ifelse(
      runif(n()) < 0.02,
      -revenue_last_year,
      revenue_last_year
    )
  )

# ----------------------------------------------------------------------
# 5. Define monthly reference period and seasonal profiles
# ----------------------------------------------------------------------

months <- seq.Date(
  from = as.Date("2023-01-01"),
  to = as.Date("2025-12-01"),
  by = "month"
)

normalize_mean_one <- function(x) {
  x / mean(x)
}

employment_seasonality <- list(
  G47 = c(
    1.00, 0.98, 1.00, 1.02, 1.04, 1.05,
    1.06, 1.07, 1.08, 1.10, 1.18, 1.25
  ),
  C10 = c(
    1.00, 1.00, 1.01, 1.01, 1.02, 1.02,
    1.03, 1.00, 1.00, 1.01, 1.01, 1.02
  ),
  C29 = c(
    1.00, 1.00, 1.00, 1.02, 1.02, 1.03,
    1.03, 0.80, 1.00, 1.02, 1.03, 1.05
  ),
  H49 = c(
    1.00, 1.01, 1.01, 1.02, 1.03, 1.05,
    1.07, 1.06, 1.05, 1.03, 1.02, 1.01
  ),
  I55 = c(
    0.70, 0.75, 0.90, 1.10, 1.40, 1.60,
    1.80, 1.70, 1.40, 1.10, 0.80, 0.70
  ),
  I56 = c(
    0.85, 0.90, 0.95, 1.05, 1.15, 1.20,
    1.30, 1.25, 1.10, 1.00, 0.95, 0.90
  )
)

employment_seasonality <- lapply(
  employment_seasonality,
  normalize_mean_one
)

turnover_seasonality <- list(
  G47 = c(
    0.86, 0.84, 0.88, 0.91, 0.94, 0.96,
    0.98, 0.97, 1.00, 1.06, 1.24, 1.46
  ),
  C10 = c(
    0.97, 0.98, 1.00, 1.02, 1.03, 1.04,
    1.02, 0.97, 1.00, 1.03, 1.04, 0.90
  ),
  C29 = c(
    0.96, 0.99, 1.02, 1.04, 1.05, 1.06,
    1.02, 0.74, 1.05, 1.09, 1.10, 0.88
  ),
  H49 = c(
    0.93, 0.95, 0.98, 1.01, 1.04, 1.07,
    1.10, 1.09, 1.06, 1.02, 0.98, 0.94
  ),
  I55 = c(
    0.61, 0.66, 0.82, 1.06, 1.34, 1.57,
    1.74, 1.66, 1.34, 1.04, 0.76, 0.60
  ),
  I56 = c(
    0.82, 0.87, 0.93, 1.04, 1.13, 1.20,
    1.27, 1.23, 1.09, 1.02, 0.96, 0.88
  )
)

turnover_seasonality <- lapply(
  turnover_seasonality,
  normalize_mean_one
)

# ----------------------------------------------------------------------
# 6. Create coherent monthly employment observations
# ----------------------------------------------------------------------

employment <- expand_grid(
  firm_id = firm_truth$firm_id,
  month = months
) %>%
  mutate(
    year = as.integer(format(month, "%Y")),
    month_num = as.integer(format(month, "%m"))
  ) %>%
  left_join(
    annual_truth %>%
      select(
        firm_id,
        year,
        employees_true
      ),
    by = c("firm_id", "year")
  ) %>%
  left_join(
    firm_truth %>%
      select(
        firm_id,
        nace_code,
        region_code
      ),
    by = "firm_id"
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
  group_by(firm_id, year) %>%
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
    firm_id,
    month,
    nace_code,
    region_code,
    seasonal_factor,
    employees
  )

# ----------------------------------------------------------------------
# 7. Create coherent monthly turnover observations
# ----------------------------------------------------------------------

turnover <- expand_grid(
  firm_id = firm_truth$firm_id,
  month = months
) %>%
  mutate(
    year = as.integer(format(month, "%Y")),
    month_num = as.integer(format(month, "%m"))
  ) %>%
  left_join(
    annual_truth %>%
      select(
        firm_id,
        year,
        annual_turnover_true
      ),
    by = c("firm_id", "year")
  ) %>%
  left_join(
    firm_truth %>%
      select(
        firm_id,
        nace_code,
        region_code
      ),
    by = "firm_id"
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
  group_by(firm_id, year) %>%
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
    firm_id,
    month,
    nace_code,
    region_code,
    turnover
  )

# ----------------------------------------------------------------------
# 8. Write raw source datasets
# ----------------------------------------------------------------------

write_csv(
  firms_inconsistent,
  "data/raw/firms.csv"
)

write_csv(
  employment,
  "data/raw/employment.csv"
)

write_csv(
  turnover,
  "data/raw/turnover.csv"
)

message("Synthetic enterprise datasets generated successfully.")
