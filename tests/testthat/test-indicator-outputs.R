# =====================================================================
# test-indicator-outputs.R
# Integration tests for annual and aggregate indicator outputs
# =====================================================================

project_path <- function(...) {
  testthat::test_path(
    "..",
    "..",
    ...
  )
}

annual_path <- project_path(
  "data",
  "processed",
  "enterprise_year_indicators.csv"
)


testthat::test_that("enterprise-year indicator output exists", {
  testthat::expect_true(
    file.exists(
      annual_path
    )
  )
})


annual <- read.csv(
  annual_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


testthat::test_that("enterprise-year structure is valid", {
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

  testthat::expect_true(
    all(
      required_annual_columns %in%
        names(
          annual
        )
    )
  )

  annual_key <- paste(
    annual$canonical_firm_id,
    annual$year,
    sep = "::"
  )

  testthat::expect_false(
    anyDuplicated(
      annual_key
    ) > 0L
  )

  testthat::expect_true(
    all(
      annual$monthly_observations ==
        12L
    )
  )
})


testthat::test_that("monthly coverage semantics are preserved", {
  testthat::expect_true(
    all(
      annual$usable_turnover_months >= 0 &
        annual$usable_turnover_months <= 12
    )
  )

  testthat::expect_true(
    all(
      annual$usable_employment_months >= 0 &
        annual$usable_employment_months <= 12
    )
  )

  testthat::expect_equal(
    unname(
      annual$complete_turnover
    ),
    unname(
      annual$usable_turnover_months ==
        12L
    )
  )

  testthat::expect_equal(
    unname(
      annual$complete_employment
    ),
    unname(
      annual$usable_employment_months ==
        12L
    )
  )

  testthat::expect_equal(
    unname(
      annual$complete_annual_measures
    ),
    unname(
      annual$complete_turnover &
        annual$complete_employment
    )
  )
})


testthat::test_that("annual measures follow completeness rules", {
  testthat::expect_true(
    all(
      is.finite(
        annual$annual_turnover[
          annual$complete_turnover
        ]
      )
    )
  )

  testthat::expect_true(
    all(
      is.na(
        annual$annual_turnover[
          !annual$complete_turnover
        ]
      )
    )
  )

  testthat::expect_true(
    all(
      is.finite(
        annual$annual_average_employment[
          annual$complete_employment
        ]
      )
    )
  )

  testthat::expect_true(
    all(
      is.na(
        annual$annual_average_employment[
          !annual$complete_employment
        ]
      )
    )
  )
})


testthat::test_that("enterprise turnover per employee uses the valid population", {
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

  testthat::expect_equal(
    unname(
      annual$turnover_per_employee[
        ratio_applicable
      ]
    ),
    unname(
      expected_ratio
    )
  )

  testthat::expect_true(
    all(
      is.na(
        annual$turnover_per_employee[
          !ratio_applicable
        ]
      )
    )
  )
})


testthat::test_that("aggregate indicator tables preserve coverage and ratio identities", {
  aggregate_paths <- c(
    sector =
      project_path(
        "output",
        "tables",
        "indicators_sector.csv"
      ),

    region =
      project_path(
        "output",
        "tables",
        "indicators_region.csv"
      ),

    sector_region =
      project_path(
        "output",
        "tables",
        "indicators_sector_region.csv"
      )
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
      names(
        aggregate_paths
      )
  ) {
    path <-
      aggregate_paths[[table_name]]

    testthat::expect_true(
      file.exists(
        path
      ),
      info = paste0(
        "Aggregate indicator table is missing: ",
        path
      )
    )

    x <- read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    testthat::expect_true(
      all(
        required_aggregate_columns %in%
          names(
            x
          )
      ),
      info = paste0(
        "Aggregate indicator table is missing required columns: ",
        table_name
      )
    )

    testthat::expect_true(
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
      info = paste0(
        "Coverage counts are inconsistent in ",
        table_name,
        "."
      )
    )

    testthat::expect_true(
      all(
        x$total_turnover >= 0 &
          x$total_average_employment >= 0 &
          x$turnover_complete_both >= 0 &
          x$employment_complete_both >= 0
      ),
      info = paste0(
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

    testthat::expect_equal(
      unname(
        x$turnover_per_employee[
          ratio_rows
        ]
      ),
      unname(
        expected_aggregate_ratio
      ),
      info = paste0(
        "Common-population turnover-per-employee identity failed in ",
        table_name,
        "."
      )
    )

    testthat::expect_true(
      all(
        is.na(
          x$turnover_per_employee[
            !ratio_rows
          ]
        )
      ),
      info = paste0(
        "Turnover per employee should be missing without common-population employment in ",
        table_name,
        "."
      )
    )
  }
})
