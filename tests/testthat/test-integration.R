# =====================================================================
# test-integration.R
# Direct characterization tests for linked-source integration
# =====================================================================

library(dplyr)
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
    "integration.R"
  ),
  local = TRUE
)


testthat::test_that(
  "source maps retain only resolved canonical identifiers",
  {
    crosswalk <-
      tibble::tibble(
        source =
          c(
            "register",
            "register",
            "employment",
            "employment",
            "turnover",
            "accounting"
          ),
        source_record_id =
          c(
            "R001",
            "R002",
            "E001",
            "E002",
            "T001",
            "A001"
          ),
        canonical_firm_id =
          c(
            "C001",
            NA_character_,
            "C001",
            "C002",
            "C001",
            "C001"
          )
      )

    maps <-
      build_source_maps(
        crosswalk
      )

    testthat::expect_equal(
      maps$register_map$register_id,
      "R001"
    )

    testthat::expect_equal(
      maps$employment_map$employment_source_id,
      c(
        "E001",
        "E002"
      )
    )

    testthat::expect_false(
      anyNA(
        maps$register_map$canonical_firm_id
      )
    )

    testthat::expect_false(
      anyNA(
        maps$employment_map$canonical_firm_id
      )
    )
  }
)


testthat::test_that(
  "canonical identifiers are attached only to resolved source records",
  {
    crosswalk <-
      tibble::tibble(
        source =
          c(
            "register",
            "employment",
            "turnover",
            "accounting"
          ),
        source_record_id =
          c(
            "R001",
            "E001",
            "T001",
            "A001"
          ),
        canonical_firm_id =
          rep(
            "C001",
            4
          )
      )

    maps <-
      build_source_maps(
        crosswalk
      )

    linked <-
      attach_canonical_identifiers(
        firms =
          tibble::tibble(
            register_id =
              c(
                "R001",
                "R999"
              )
          ),
        employment =
          tibble::tibble(
            employment_source_id =
              c(
                "E001",
                "E999"
              )
          ),
        turnover =
          tibble::tibble(
            turnover_source_id =
              c(
                "T001",
                "T999"
              )
          ),
        accounting =
          tibble::tibble(
            accounting_source_id =
              c(
                "A001",
                "A999"
              )
          ),
        source_maps =
          maps
      )

    testthat::expect_equal(
      nrow(
        linked$firms_linked
      ),
      1L
    )

    testthat::expect_equal(
      nrow(
        linked$employment_linked
      ),
      1L
    )

    testthat::expect_equal(
      nrow(
        linked$turnover_linked
      ),
      1L
    )

    testthat::expect_equal(
      nrow(
        linked$accounting_linked
      ),
      1L
    )

    testthat::expect_equal(
      linked$firms_linked$canonical_firm_id,
      "C001"
    )
  }
)


testthat::test_that(
  "common analytical population requires register employment and turnover",
  {
    result <-
      get_common_canonical_firms(
        firms_linked =
          tibble::tibble(
            canonical_firm_id =
              c(
                "C001",
                "C002",
                "C003"
              )
          ),
        employment_linked =
          tibble::tibble(
            canonical_firm_id =
              c(
                "C001",
                "C002"
              )
          ),
        turnover_linked =
          tibble::tibble(
            canonical_firm_id =
              c(
                "C001",
                "C003"
              )
          )
      )

    testthat::expect_equal(
      result,
      "C001"
    )
  }
)


make_accounting_linked <- function() {
  tibble::tibble(
    canonical_firm_id =
      c(
        "C001",
        "C002"
      ),
    accounting_source_id =
      c(
        "A001",
        "A002"
      ),
    reference_year =
      c(
        2024L,
        2024L
      ),
    nace_code =
      c(
        "C10",
        "G47"
      ),
    operating_revenue_raw =
      c(
        100,
        200
      ),
    operating_revenue_status =
      "accepted",
    operating_revenue_rule_id =
      NA_character_,
    operating_revenue =
      c(
        100,
        200
      ),
    purchases_goods_services_raw =
      c(
        40,
        80
      ),
    purchases_status =
      "accepted",
    purchases_rule_id =
      NA_character_,
    purchases_goods_services =
      c(
        40,
        80
      ),
    personnel_expense_raw =
      c(
        20,
        30
      ),
    personnel_expense_status =
      "accepted",
    personnel_expense_rule_id =
      NA_character_,
    personnel_expense =
      c(
        20,
        30
      )
  )
}


testthat::test_that(
  "accounting annual data preserve unique enterprise-year keys",
  {
    accounting <-
      make_accounting_linked()

    result <-
      prepare_accounting_annual(
        accounting
      )

    testthat::expect_equal(
      nrow(result),
      2L
    )

    testthat::expect_equal(
      result$nace_code_accounting,
      c(
        "C10",
        "G47"
      )
    )

    duplicate <-
      dplyr::bind_rows(
        accounting,
        accounting[1, ]
      )

    testthat::expect_error(
      prepare_accounting_annual(
        duplicate
      ),
      "Duplicate canonical enterprise-year keys"
    )
  }
)


testthat::test_that(
  "monthly integration preserves panel and derived indicator semantics",
  {
    months <-
      seq.Date(
        from =
          as.Date(
            "2024-01-01"
          ),
        by =
          "month",
        length.out =
          13L
      )

    firms_linked <-
      tibble::tibble(
        canonical_firm_id =
          "C001",
        register_id =
          "R001",
        employees =
          20
      )

    employment_linked <-
      tibble::tibble(
        canonical_firm_id =
          "C001",
        employment_source_id =
          "E001",
        month =
          months,
        employees =
          10:22
      )

    turnover_linked <-
      tibble::tibble(
        canonical_firm_id =
          "C001",
        turnover_source_id =
          "T001",
        month =
          months,
        turnover =
          100:112
      )

    prepared <-
      prepare_monthly_source_panels(
        firms_linked =
          firms_linked,
        employment_linked =
          employment_linked,
        turnover_linked =
          turnover_linked,
        common_firms =
          "C001"
      )

    panel <-
      build_monthly_panel(
        employment_panel =
          prepared$employment_panel,
        turnover_panel =
          prepared$turnover_panel,
        firms_panel =
          prepared$firms_panel
      )

    result <-
      derive_panel_indicators(
        panel
      )

    testthat::expect_equal(
      nrow(result),
      13L
    )

    testthat::expect_true(
      all(
        is.na(
          result$turnover_yoy[
            1:12
          ]
        )
      )
    )

    testthat::expect_equal(
      result$turnover_yoy[13],
      0.12
    )

    testthat::expect_equal(
      result$emp_growth[2],
      0.10
    )

    testthat::expect_equal(
      mean(
        result$seasonal_index
      ),
      1,
      tolerance =
        1e-12
    )

    testthat::expect_equal(
      unique(
        result$employees_firm
      ),
      20
    )
  }
)
