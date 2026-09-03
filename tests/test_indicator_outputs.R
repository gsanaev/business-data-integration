# =====================================================================
# test_indicator_outputs.R
# Integration tests for annual and aggregate indicator outputs
# =====================================================================

source(
  "tests/helpers/assertions.R"
)

annual_path <-
  "data/processed/enterprise_year_indicators.csv"

assert_true(
  file.exists(annual_path),
  "Enterprise-year indicator output is missing."
)

annual <- read.csv(
  annual_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# ----------------------------------------------------------------------
# 1. Enterprise-year structure
# ----------------------------------------------------------------------

required_annual_columns <- c(
  "canonical_firm_id",
  "year",
  "nace_code",
  "region_code",
  "monthly_observations",
  "usable_turnover_months",
  "usable_employment_months",
  "turnover_imputed_months",
  "employment_imputed_months",
  "annual_turnover",
  "annual_average_employment",
  "complete_turnover",
  "complete_employment",
  "complete_annual_measures",
  "turnover_per_employee"
)

assert_true(
  all(
    required_annual_columns %in%
      names(annual)
  ),
  "Enterprise-year output is missing required columns."
)

annual_key <- paste(
  annual$canonical_firm_id,
  annual$year,
  sep = "::"
)

assert_true(
  !anyDuplicated(annual_key),
  "Enterprise-year output contains duplicate enterprise-year keys."
)

assert_true(
  all(
    annual$monthly_observations ==
      12L
  ),
  "Every enterprise-year should contain twelve structural monthly observations."
)

# ----------------------------------------------------------------------
# 2. Monthly coverage semantics
# ----------------------------------------------------------------------

assert_true(
  all(
    annual$usable_turnover_months >= 0 &
      annual$usable_turnover_months <= 12
  ),
  "Usable turnover-month counts must lie between 0 and 12."
)

assert_true(
  all(
    annual$usable_employment_months >= 0 &
      annual$usable_employment_months <= 12
  ),
  "Usable employment-month counts must lie between 0 and 12."
)

assert_equal(
  annual$complete_turnover,
  annual$usable_turnover_months ==
    12L,
  "Turnover completeness must require twelve usable months."
)

assert_equal(
  annual$complete_employment,
  annual$usable_employment_months ==
    12L,
  "Employment completeness must require twelve usable months."
)

assert_equal(
  annual$complete_annual_measures,
  annual$complete_turnover &
    annual$complete_employment,
  paste(
    "Complete annual measures must require",
    "both turnover and employment completeness."
  )
)

# ----------------------------------------------------------------------
# 3. Annual measure availability
# ----------------------------------------------------------------------

assert_true(
  all(
    is.finite(
      annual$annual_turnover[
        annual$complete_turnover
      ]
    )
  ),
  "Complete turnover enterprise-years require annual turnover."
)

assert_true(
  all(
    is.na(
      annual$annual_turnover[
        !annual$complete_turnover
      ]
    )
  ),
  "Incomplete turnover enterprise-years must not receive annual turnover."
)

assert_true(
  all(
    is.finite(
      annual$annual_average_employment[
        annual$complete_employment
      ]
    )
  ),
  "Complete employment enterprise-years require annual average employment."
)

assert_true(
  all(
    is.na(
      annual$annual_average_employment[
        !annual$complete_employment
      ]
    )
  ),
  "Incomplete employment enterprise-years must not receive annual average employment."
)

# ----------------------------------------------------------------------
# 4. Enterprise-level turnover per employee
# ----------------------------------------------------------------------

ratio_applicable <-
  annual$complete_annual_measures &
    annual$annual_average_employment >
      0

expected_ratio <-
  annual$annual_turnover[
    ratio_applicable
  ] /
  annual$annual_average_employment[
    ratio_applicable
  ]

assert_equal(
  annual$turnover_per_employee[
    ratio_applicable
  ],
  expected_ratio,
  paste(
    "Enterprise turnover per employee must equal",
    "annual turnover divided by annual average employment."
  )
)

assert_true(
  all(
    is.na(
      annual$turnover_per_employee[
        !ratio_applicable
      ]
    )
  ),
  "Turnover per employee must be missing outside its valid population."
)

# ----------------------------------------------------------------------
# 5. Aggregate indicator tables
# ----------------------------------------------------------------------

aggregate_paths <- c(
  sector =
    "output/tables/indicators_sector.csv",

  region =
    "output/tables/indicators_region.csv",

  sector_region =
    "output/tables/indicators_sector_region.csv"
)

required_aggregate_columns <- c(
  "year",
  "n_enterprises",
  "n_complete_turnover",
  "n_complete_employment",
  "n_complete_both",
  "total_turnover",
  "average_turnover_per_enterprise",
  "total_average_employment",
  "average_employment_per_enterprise",
  "turnover_complete_both",
  "employment_complete_both",
  "turnover_per_employee"
)

for (
  table_name in
    names(aggregate_paths)
) {
  path <-
    aggregate_paths[[table_name]]

  assert_true(
    file.exists(path),
    paste0(
      "Aggregate indicator table is missing: ",
      path
    )
  )

  x <- read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  assert_true(
    all(
      required_aggregate_columns %in%
        names(x)
    ),
    paste0(
      "Aggregate indicator table is missing required columns: ",
      table_name
    )
  )

  assert_true(
    all(
      x$n_complete_turnover <=
        x$n_enterprises &
      x$n_complete_employment <=
        x$n_enterprises &
      x$n_complete_both <=
        x$n_complete_turnover &
      x$n_complete_both <=
        x$n_complete_employment
    ),
    paste0(
      "Coverage counts are inconsistent in ",
      table_name,
      "."
    )
  )

  assert_true(
    all(
      x$total_turnover >= 0 &
      x$total_average_employment >= 0 &
      x$turnover_complete_both >= 0 &
      x$employment_complete_both >= 0
    ),
    paste0(
      "Negative aggregate measures detected in ",
      table_name,
      "."
    )
  )

  ratio_rows <-
    x$employment_complete_both >
      0

  expected_aggregate_ratio <-
    x$turnover_complete_both[
      ratio_rows
    ] /
    x$employment_complete_both[
      ratio_rows
    ]

  assert_equal(
    x$turnover_per_employee[
      ratio_rows
    ],
    expected_aggregate_ratio,
    paste0(
      "Common-population turnover-per-employee identity failed in ",
      table_name,
      "."
    )
  )

  assert_true(
    all(
      is.na(
        x$turnover_per_employee[
          !ratio_rows
        ]
      )
    ),
    paste0(
      "Turnover per employee should be missing without common-population employment in ",
      table_name,
      "."
    )
  )
}

cat(
  "test_indicator_outputs.R: PASS\n"
)
