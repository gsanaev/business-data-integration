# =====================================================================
# test-synthetic.R
# Unit tests for synthetic-data and scenario helpers
# =====================================================================

suppressPackageStartupMessages(
  library(dplyr)
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "helpers",
    "synthetic_identity.R"
  ),
  local = TRUE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "synthetic.R"
  ),
  local = TRUE
)


testthat::test_that(
  "additional missing probability preserves the requested total rate",
  {
    moderate_increment <-
      additional_missing_probability(
        target_probability = 0.25,
        baseline_probability = 0.10
      )

    difficult_increment <-
      additional_missing_probability(
        target_probability = 0.40,
        baseline_probability = 0.10
      )

    testthat::expect_equal(
      moderate_increment,
      1 / 6
    )

    testthat::expect_equal(
      difficult_increment,
      1 / 3
    )

    testthat::expect_equal(
      additional_missing_probability(
        target_probability = 0.10,
        baseline_probability = 0.10
      ),
      0
    )
  }
)


testthat::test_that(
  "zero-probability scenario flags do not consume RNG",
  {
    set.seed(
      12345
    )

    seed_before <-
      .Random.seed

    flags <-
      draw_scenario_flag(
        n = 10L,
        probability = 0
      )

    seed_after <-
      .Random.seed

    testthat::expect_false(
      any(flags)
    )

    testthat::expect_identical(
      seed_after,
      seed_before
    )
  }
)


testthat::test_that(
  "baseline identity scenario preserves identities and RNG state",
  {
    identity_table <-
      tibble::tibble(
        truth_firm_id =
          c(
            "F00001",
            "F00002"
          ),
        business_id =
          c(
            "B0000001",
            "B0000002"
          ),
        enterprise_name =
          c(
            "Alpha GmbH",
            "Beta AG"
          ),
        street =
          c(
            "Hauptstrasse 1",
            "Marktstrasse 2"
          ),
        postal_code =
          c(
            "60311",
            "65183"
          ),
        city =
          c(
            "Frankfurt am Main",
            "Wiesbaden"
          ),
        legal_form =
          c(
            "GmbH",
            "AG"
          ),
        nace_code =
          c(
            "G47",
            "C29"
          )
      )

    baseline_scenario <-
      list(
        missing_business_id = 0.10,
        invalid_or_unknown_business_id = 0,
        additional_name_typo = 0,
        substantial_name_degradation = 0,
        strong_street_discrepancy = 0,
        postal_code_missing_or_error = 0,
        nace_disagreement = 0,
        legal_form_disagreement = 0
      )

    set.seed(
      24680
    )

    seed_before <-
      .Random.seed

    result <-
      apply_identity_scenario_to_table(
        identity_table =
          identity_table,
        scenario =
          baseline_scenario,
        baseline_scenario =
          baseline_scenario,
        source_label =
          "employment"
      )

    seed_after <-
      .Random.seed

    testthat::expect_identical(
      result$identity,
      identity_table
    )

    testthat::expect_identical(
      seed_after,
      seed_before
    )

    testthat::expect_false(
      any(
        result$corruption_log[
          c(
            "invalid_or_unknown_business_id",
            "additional_name_typo",
            "substantial_name_degradation",
            "strong_street_discrepancy",
            "postal_code_missing_or_error",
            "nace_disagreement",
            "legal_form_disagreement"
          )
        ]
      )
    )
  }
)


testthat::test_that(
  "synthetic truth outputs preserve the three truth products",
  {
    identity_truth <-
      tibble::tibble(
        truth_firm_id = "F00001",
        business_id = "B0000001",
        enterprise_name = "Alpha GmbH",
        street = "Hauptstrasse 1",
        postal_code = "60311",
        city = "Frankfurt am Main",
        region_code = "R01",
        nace_code = "G47",
        legal_form = "GmbH",
        foundation_year = 2000L
      )

    register_identity <-
      tibble::tibble(
        truth_firm_id = "F00001",
        register_id = "REG000001"
      )

    employment_identity <-
      tibble::tibble(
        truth_firm_id = "F00001",
        employment_source_id = "EMP000001"
      )

    turnover_identity <-
      tibble::tibble(
        truth_firm_id = "F00001",
        turnover_source_id = "TUR000001"
      )

    accounting_identity <-
      tibble::tibble(
        truth_firm_id = "F00001",
        accounting_source_id = "ACC000001"
      )

    attached_sources <-
      list(
        firms =
          tibble::tibble(
            truth_firm_id = "F00001",
            register_id = "REG000001",
            register_reference_year = 2025L,
            employees_register_complete = 10,
            revenue_reference_year = 2024L,
            revenue_last_year_complete = 1000
          ),

        employment =
          tibble::tibble(
            truth_firm_id = "F00001",
            employment_source_id = "EMP000001",
            month =
              as.Date(
                "2025-01-01"
              ),
            employees_source_complete = 11
          ),

        turnover =
          tibble::tibble(
            truth_firm_id = "F00001",
            turnover_source_id = "TUR000001",
            month =
              as.Date(
                "2025-01-01"
              ),
            turnover_source_complete = 1200
          ),

        accounting =
          tibble::tibble(
            truth_firm_id = "F00001",
            accounting_source_id = "ACC000001",
            reference_year = 2025L,
            operating_revenue_complete = 1250,
            purchases_goods_services_complete = 700,
            personnel_expense_complete = 350
          )
      )

    result <-
      build_synthetic_truth_outputs(
        identity_truth =
          identity_truth,
        register_identity =
          register_identity,
        employment_identity =
          employment_identity,
        turnover_identity =
          turnover_identity,
        accounting_identity =
          accounting_identity,
        attached_sources =
          attached_sources
      )

    testthat::expect_named(
      result,
      c(
        "enterprise",
        "linkage",
        "value"
      )
    )

    testthat::expect_equal(
      nrow(
        result$enterprise
      ),
      1L
    )

    testthat::expect_equal(
      nrow(
        result$linkage
      ),
      4L
    )

    testthat::expect_equal(
      sort(
        result$linkage$source
      ),
      sort(
        c(
          "register",
          "employment",
          "turnover",
          "accounting"
        )
      )
    )

    testthat::expect_equal(
      nrow(
        result$value
      ),
      7L
    )

    testthat::expect_equal(
      sort(
        unique(
          result$value$variable
        )
      ),
      sort(
        c(
          "employees",
          "revenue_last_year",
          "turnover",
          "operating_revenue",
          "purchases_goods_services",
          "personnel_expense"
        )
      )
    )
  }
)
