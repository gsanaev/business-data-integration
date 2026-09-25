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
