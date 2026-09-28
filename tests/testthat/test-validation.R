# =====================================================================
# test-validation.R
# Direct characterization tests for source validation
# =====================================================================

library(dplyr)
library(janitor)
library(lubridate)

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
    "helpers",
    "plausibility.R"
  ),
  local = TRUE
)

source(
  project_path(
    "R",
    "validation.R"
  ),
  local = TRUE
)


make_register_raw <- function() {
  tibble::tibble(
    register_id =
      c(
        "R001",
        "R002",
        "R003"
      ),
    register_reference_year =
      c(
        2025L,
        2025L,
        2025L
      ),
    employees =
      c(
        10,
        NA,
        0
      ),
    revenue_last_year =
      c(
        100,
        NA,
        -1
      ),
    foundation_year =
      c(
        2000,
        NA,
        1800
      )
  )
}


make_employment_raw <- function() {
  tibble::tibble(
    employment_source_id =
      rep(
        "E001",
        4
      ),
    month =
      as.Date(
        c(
          "2025-01-01",
          "2025-02-01",
          "2025-03-01",
          "2025-04-01"
        )
      ),
    employees =
      c(
        10,
        NA,
        12,
        100
      )
  )
}


make_turnover_raw <- function() {
  tibble::tibble(
    turnover_source_id =
      rep(
        "T001",
        4
      ),
    month =
      as.Date(
        c(
          "2025-01-01",
          "2025-02-01",
          "2025-03-01",
          "2025-04-01"
        )
      ),
    turnover =
      c(
        100,
        NA,
        120,
        0
      )
  )
}


make_accounting_raw <- function() {
  tibble::tibble(
    accounting_source_id =
      c(
        "A001",
        "A002",
        "A003"
      ),
    reference_year =
      c(
        2023L,
        2024L,
        2025L
      ),
    operating_revenue =
      c(
        100,
        NA,
        0
      ),
    purchases_goods_services =
      c(
        50,
        -1,
        NA
      ),
    personnel_expense =
      c(
        20,
        NA,
        -2
      )
  )
}


testthat::test_that(
  "source structures enforce unique keys and valid accounting years",
  {
    firms_raw <-
      make_register_raw()

    employment_raw <-
      make_employment_raw()

    turnover_raw <-
      make_turnover_raw()

    accounting_raw <-
      make_accounting_raw()

    testthat::expect_silent(
      validate_source_structures(
        firms_raw,
        employment_raw,
        turnover_raw,
        accounting_raw
      )
    )

    duplicate_firms <-
      dplyr::bind_rows(
        firms_raw,
        firms_raw[1, ]
      )

    testthat::expect_error(
      validate_source_structures(
        duplicate_firms,
        employment_raw,
        turnover_raw,
        accounting_raw
      ),
      "Duplicate register IDs"
    )

    invalid_year <-
      accounting_raw

    invalid_year$reference_year[1] <-
      2026L

    testthat::expect_error(
      validate_source_structures(
        firms_raw,
        employment_raw,
        turnover_raw,
        invalid_year
      ),
      "Unexpected reference year"
    )
  }
)


testthat::test_that(
  "register validation preserves accepted review and rejected semantics",
  {
    result <-
      validate_register_source(
        make_register_raw(),
        foundation_year_min =
          1900L
      )

    testthat::expect_equal(
      result$employees_register_status,
      c(
        "accepted",
        "review_required",
        "rejected"
      )
    )

    testthat::expect_equal(
      result$employees_register_rule_id,
      c(
        NA_character_,
        "REG_EMP_MISSING",
        "REG_EMP_NONPOSITIVE"
      )
    )

    testthat::expect_equal(
      result$revenue_status,
      c(
        "accepted",
        "review_required",
        "rejected"
      )
    )

    testthat::expect_equal(
      result$foundation_year_status,
      c(
        "accepted",
        "review_required",
        "rejected"
      )
    )

    testthat::expect_equal(
      result$foundation_year_rule_id,
      c(
        NA_character_,
        "FOUNDATION_YEAR_MISSING",
        "FOUNDATION_YEAR_INVALID"
      )
    )
  }
)


testthat::test_that(
  "employment validation characterizes interpolation and spike review",
  {
    result <-
      validate_employment_source(
        make_employment_raw(),
        employment_spike_multiplier =
          2
      )

    testthat::expect_equal(
      result$employment_status,
      c(
        "accepted",
        "imputed",
        "accepted",
        "review_required"
      )
    )

    testthat::expect_equal(
      result$employment_rule_id,
      c(
        NA_character_,
        "EMP_MISSING",
        NA_character_,
        "EMP_SPIKE"
      )
    )

    testthat::expect_equal(
      result$employees,
      c(
        10,
        11,
        12,
        NA_real_
      )
    )

    testthat::expect_equal(
      result$employment_imputed,
      c(
        FALSE,
        TRUE,
        FALSE,
        FALSE
      )
    )
  }
)


testthat::test_that(
  "turnover validation characterizes interpolation and rejection",
  {
    result <-
      validate_turnover_source(
        make_turnover_raw()
      )

    testthat::expect_equal(
      result$turnover_status,
      c(
        "accepted",
        "imputed",
        "accepted",
        "rejected"
      )
    )

    testthat::expect_equal(
      result$turnover_rule_id,
      c(
        NA_character_,
        "TURN_MISSING",
        NA_character_,
        "TURN_NONPOSITIVE"
      )
    )

    testthat::expect_equal(
      result$turnover,
      c(
        100,
        100 + 20 * 31 / 59,
        120,
        NA_real_
      )
    )

    testthat::expect_equal(
      result$turnover_imputed,
      c(
        FALSE,
        TRUE,
        FALSE,
        FALSE
      )
    )
  }
)


testthat::test_that(
  "accounting validation preserves variable-specific QA semantics",
  {
    result <-
      validate_accounting_source(
        make_accounting_raw()
      )

    testthat::expect_equal(
      result$operating_revenue_status,
      c(
        "accepted",
        "review_required",
        "rejected"
      )
    )

    testthat::expect_equal(
      result$operating_revenue_rule_id,
      c(
        NA_character_,
        "ACC_REV_MISSING",
        "ACC_REV_NONPOSITIVE"
      )
    )

    testthat::expect_equal(
      result$purchases_status,
      c(
        "accepted",
        "rejected",
        "review_required"
      )
    )

    testthat::expect_equal(
      result$personnel_expense_status,
      c(
        "accepted",
        "review_required",
        "rejected"
      )
    )
  }
)


testthat::test_that(
  "validated analytical values must remain positive",
  {
    firms_clean <-
      validate_register_source(
        make_register_raw(),
        foundation_year_min =
          1900L
      )

    employment_clean <-
      validate_employment_source(
        make_employment_raw(),
        employment_spike_multiplier =
          2
      )

    turnover_clean <-
      validate_turnover_source(
        make_turnover_raw()
      )

    accounting_clean <-
      validate_accounting_source(
        make_accounting_raw()
      )

    testthat::expect_silent(
      assert_validated_sources(
        firms_clean,
        employment_clean,
        turnover_clean,
        accounting_clean
      )
    )

    invalid_turnover <-
      turnover_clean

    invalid_turnover$turnover[1] <-
      -1

    testthat::expect_error(
      assert_validated_sources(
        firms_clean,
        employment_clean,
        invalid_turnover,
        accounting_clean
      ),
      "Non-positive analytical monthly turnover"
    )
  }
)
