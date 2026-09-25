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

create_synthetic_reference_structures <- function() {
  regions <- tibble(
    region_code = sprintf("R%02d", 1:10),
    region_name = paste("Region", 1:10)
  )

  industry_params <- tibble(
    nace_code = c(
      "G47",
      "C10",
      "C29",
      "H49",
      "I55",
      "I56"
    ),
    industry_name = c(
      "Retail Trade",
      "Food Manufacturing",
      "Automotive Manufacturing",
      "Land Transport",
      "Accommodation",
      "Food & Beverage Services"
    ),
    employment_center = c(
      18,
      35,
      70,
      28,
      20,
      15
    ),
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
    legal_form = c(
      "AG",
      "GmbH",
      "KG",
      "OHG",
      "Einzelunternehmen"
    )
  )

  list(
    regions = regions,
    industry_params = industry_params,
    legal_forms = legal_forms
  )
}


generate_latent_enterprises <- function(
  regions,
  industry_params,
  legal_forms,
  n_firms
) {
  tibble(
    truth_firm_id =
      sprintf(
        "F%05d",
        1:n_firms
      ),

    region_code =
      sample(
        regions$region_code,
        n_firms,
        replace = TRUE
      ),

    nace_code =
      sample(
        industry_params$nace_code,
        n_firms,
        replace = TRUE
      ),

    legal_form =
      sample(
        legal_forms$legal_form,
        n_firms,
        replace = TRUE
      ),

    foundation_year =
      sample(
        1965:2022,
        n_firms,
        replace = TRUE
      )
  ) %>%
    left_join(
      industry_params,
      by = "nace_code"
    ) %>%
    mutate(
      baseline_employment = pmax(
        1,
        round(
          rlnorm(
            n(),
            meanlog = log(
              employment_center
            ),
            sdlog = 0.65
          )
        )
      ),

      firm_productivity_factor =
        rlnorm(
          n(),
          meanlog = 0,
          sdlog = 0.30
        ),

      employment_growth_rate =
        pmin(
          0.12,
          pmax(
            -0.08,
            rnorm(
              n(),
              mean =
                employment_growth_mean,
              sd = 0.025
            )
          )
        ),

      productivity_growth_rate =
        pmin(
          0.12,
          pmax(
            -0.06,
            rnorm(
              n(),
              mean =
                productivity_growth_mean,
              sd = 0.025
            )
          )
        ),

      turnover_per_employee_2023 =
        turnover_per_employee *
          firm_productivity_factor
    )
}


generate_annual_latent_states <- function(
  firm_truth,
  years
) {
  expand_grid(
    truth_firm_id =
      firm_truth$truth_firm_id,
    year =
      years
  ) %>%
    left_join(
      firm_truth,
      by = "truth_firm_id"
    ) %>%
    mutate(
      years_since_2023 =
        year - 2023L,

      employment_noise =
        exp(
          rnorm(
            n(),
            mean = 0,
            sd = 0.015
          )
        ),

      employees_true =
        pmax(
          1,
          round(
            baseline_employment *
              (1 + employment_growth_rate)^
                years_since_2023 *
              employment_noise
          )
        ),

      productivity_noise =
        exp(
          rnorm(
            n(),
            mean = 0,
            sd = 0.020
          )
        ),

      turnover_per_employee_true =
        turnover_per_employee_2023 *
          (1 + productivity_growth_rate)^
            years_since_2023 *
          productivity_noise,

      annual_turnover_true =
        round(
          employees_true *
            turnover_per_employee_true,
          2
        )
    )
}

generate_register_source <- function(
  annual_truth
) {
  register_2025 <-
    annual_truth %>%
    filter(
      year == 2025
    ) %>%
    transmute(
      truth_firm_id,
      region_code,
      nace_code,
      legal_form,
      foundation_year,
      employees_true_2025 =
        employees_true
    )

  revenue_2024 <-
    annual_truth %>%
    filter(
      year == 2024
    ) %>%
    transmute(
      truth_firm_id,
      revenue_true_2024 =
        annual_turnover_true
    )

  firms <-
    register_2025 %>%
    left_join(
      revenue_2024,
      by = "truth_firm_id"
    ) %>%
    mutate(
      register_reference_year =
        2025L,

      revenue_reference_year =
        2024L,

      employees =
        pmax(
          1,
          round(
            employees_true_2025 *
              exp(
                rnorm(
                  n(),
                  mean = 0,
                  sd = 0.030
                )
              )
          )
        ),

      revenue_last_year =
        round(
          revenue_true_2024 *
            exp(
              rnorm(
                n(),
                mean = 0,
                sd = 0.040
              )
            ),
          2
        )
    ) %>%
    select(
      truth_firm_id,
      region_code,
      nace_code,
      legal_form,
      employees,
      foundation_year,
      revenue_last_year,
      register_reference_year,
      revenue_reference_year
    )

  firms %>%
    mutate(
      employees_register_complete =
        employees,

      revenue_last_year_complete =
        revenue_last_year,

      employees =
        ifelse(
          runif(n()) < 0.02,
          NA,
          employees
        ),

      revenue_last_year =
        ifelse(
          runif(n()) < 0.02,
          -revenue_last_year,
          revenue_last_year
        )
    )
}


create_monthly_reference_profiles <- function() {
  months <- seq.Date(
    from = as.Date("2023-01-01"),
    to = as.Date("2025-12-01"),
    by = "month"
  )

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

  employment_seasonality <-
    lapply(
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

  turnover_seasonality <-
    lapply(
      turnover_seasonality,
      normalize_mean_one
    )

  list(
    months = months,
    employment_seasonality =
      employment_seasonality,
    turnover_seasonality =
      turnover_seasonality
  )
}

generate_monthly_employment <- function(
  firm_truth,
  annual_truth,
  months,
  employment_seasonality
) {
  expand_grid(
    truth_firm_id =
      firm_truth$truth_firm_id,
    month =
      months
  ) %>%
    mutate(
      year =
        as.integer(
          format(
            month,
            "%Y"
          )
        ),

      month_num =
        as.integer(
          format(
            month,
            "%m"
          )
        )
    ) %>%
    left_join(
      annual_truth %>%
        select(
          truth_firm_id,
          year,
          employees_true
        ),
      by = c(
        "truth_firm_id",
        "year"
      )
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
      seasonal_factor =
        mapply(
          function(code, m) {
            employment_seasonality[[code]][m]
          },
          nace_code,
          month_num
        ),

      monthly_noise =
        exp(
          rnorm(
            n(),
            mean = 0,
            sd = 0.020
          )
        ),

      employment_weight =
        seasonal_factor *
          monthly_noise
    ) %>%
    group_by(
      truth_firm_id,
      year
    ) %>%
    mutate(
      employment_weight =
        employment_weight /
          mean(
            employment_weight
          ),

      employees =
        pmax(
          1,
          round(
            employees_true *
              employment_weight
          )
        )
    ) %>%
    ungroup() %>%
    mutate(
      employees_source_complete =
        employees,

      employees =
        ifelse(
          runif(n()) < 0.003,
          round(
            employees *
              runif(
                n(),
                min = 1.8,
                max = 2.8
              )
          ),
          employees
        ),

      employees =
        ifelse(
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
}


generate_monthly_turnover <- function(
  firm_truth,
  annual_truth,
  months,
  turnover_seasonality
) {
  expand_grid(
    truth_firm_id =
      firm_truth$truth_firm_id,
    month =
      months
  ) %>%
    mutate(
      year =
        as.integer(
          format(
            month,
            "%Y"
          )
        ),

      month_num =
        as.integer(
          format(
            month,
            "%m"
          )
        )
    ) %>%
    left_join(
      annual_truth %>%
        select(
          truth_firm_id,
          year,
          annual_turnover_true
        ),
      by = c(
        "truth_firm_id",
        "year"
      )
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
      seasonal_factor =
        mapply(
          function(code, m) {
            turnover_seasonality[[code]][m]
          },
          nace_code,
          month_num
        ),

      allocation_noise =
        exp(
          rnorm(
            n(),
            mean = 0,
            sd = 0.040
          )
        ),

      allocation_weight =
        seasonal_factor *
          allocation_noise
    ) %>%
    group_by(
      truth_firm_id,
      year
    ) %>%
    mutate(
      monthly_share =
        allocation_weight /
          sum(
            allocation_weight
          ),

      turnover_true =
        annual_turnover_true *
          monthly_share,

      turnover =
        round(
          turnover_true *
            exp(
              rnorm(
                n(),
                mean = 0,
                sd = 0.020
              )
            ),
          2
        )
    ) %>%
    ungroup() %>%
    mutate(
      turnover_source_complete =
        turnover,

      turnover =
        ifelse(
          runif(n()) < 0.002,
          -turnover,
          turnover
        ),

      turnover =
        ifelse(
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
}
