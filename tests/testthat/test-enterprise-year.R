# =====================================================================
# test-enterprise-year.R
# Direct characterization tests for enterprise-year validation
# =====================================================================

library(dplyr)

project_path <- function(...) {
  testthat::test_path(
    "..",
    "..",
    ...
  )
}

source(
  project_path(
    "R",
    "enterprise_year.R"
  ),
  local = TRUE
)


make_monthly_panel_structure_fixture <- function() {
  tibble::tibble(
    canonical_firm_id =
      rep(
        "C001",
        12L
      ),
    month =
      seq.Date(
        from =
          as.Date(
            "2025-01-01"
          ),
        by =
          "month",
        length.out =
          12L
      ),
    year =
      rep(
        2025L,
        12L
      )
  )
}


testthat::test_that(
  "monthly panel validation rejects duplicate and incomplete structures",
  {
    valid_panel <-
      make_monthly_panel_structure_fixture()

    testthat::expect_silent(
      validate_monthly_panel_structure(
        valid_panel,
        required_months =
          12L
      )
    )

    duplicate_panel <-
      dplyr::bind_rows(
        valid_panel,
        valid_panel[1, ]
      )

    testthat::expect_error(
      validate_monthly_panel_structure(
        duplicate_panel,
        required_months =
          12L
      ),
      "Duplicate canonical enterprise-month keys"
    )

    incomplete_panel <-
      valid_panel[
        -12,
      ]

    testthat::expect_error(
      validate_monthly_panel_structure(
        incomplete_panel,
        required_months =
          12L
      ),
      "Expected exactly 12 structural monthly observations"
    )
  }
)


make_enterprise_year_fixture <- function() {
  tibble::tibble(
    canonical_firm_id =
      c(
        "C001",
        "C002"
      ),
    year =
      c(
        2025L,
        2025L
      ),
    annual_turnover =
      c(
        1000,
        2000
      ),
    annual_average_employment =
      c(
        10,
        20
      ),
    turnover_per_employee =
      c(
        100,
        100
      )
  )
}


testthat::test_that(
  "enterprise-year validation rejects invalid analytical structures",
  {
    valid <-
      make_enterprise_year_fixture()

    testthat::expect_silent(
      validate_enterprise_year(
        valid
      )
    )

    duplicate <-
      dplyr::bind_rows(
        valid,
        valid[1, ]
      )

    testthat::expect_error(
      validate_enterprise_year(
        duplicate
      ),
      "Duplicate canonical enterprise-year keys"
    )

    bad_turnover <-
      valid

    bad_turnover$annual_turnover[1] <-
      0

    testthat::expect_error(
      validate_enterprise_year(
        bad_turnover
      ),
      "Non-positive annual turnover"
    )

    bad_employment <-
      valid

    bad_employment$annual_average_employment[1] <-
      0

    testthat::expect_error(
      validate_enterprise_year(
        bad_employment
      ),
      "Non-positive annual average employment"
    )

    bad_ratio <-
      valid

    bad_ratio$turnover_per_employee[1] <-
      0

    testthat::expect_error(
      validate_enterprise_year(
        bad_ratio
      ),
      "Non-positive turnover-per-employee"
    )
  }
)
