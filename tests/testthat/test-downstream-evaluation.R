source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "downstream_evaluation.R"
  )
)


testthat::test_that(
  "downstream collisions retain trusted Level 1 assignments",
  {
    assignments <-
      tibble::tibble(
        scenario =
          rep("difficult", 6L),
        method =
          rep("random_forest", 6L),
        source =
          rep("employment", 6L),
        source_record_id =
          paste0("S", 1:6),
        decision_stage =
          c(
            "level1",
            "level2",
            "level2",
            "level2",
            "level2",
            "level2"
          ),
        decision_status =
          c(
            "auto_link",
            "auto_link",
            "auto_link",
            "auto_link",
            "auto_link",
            "review"
          ),
        assigned_register_id =
          c(
            "R1",
            "R1",
            "R2",
            "R2",
            "R3",
            NA_character_
          ),
        assigned_canonical_firm_id =
          c(
            "C1",
            "C1",
            "C2",
            "C2",
            "C3",
            NA_character_
          )
      )

    result <-
      resolve_downstream_assignment_collisions(
        assignments
      )

    resolved <-
      result$assignments

    testthat::expect_equal(
      nrow(result$collisions),
      2L
    )

    testthat::expect_equal(
      resolved$downstream_status,
      c(
        "auto_link",
        "collision_review",
        "collision_review",
        "collision_review",
        "auto_link",
        "review"
      )
    )

    testthat::expect_equal(
      sum(
        resolved$downstream_automatic_link
      ),
      2L
    )

    testthat::expect_false(
      anyDuplicated(
        resolved[
          resolved$downstream_automatic_link,
          c(
            "source",
            "downstream_assigned_canonical_firm_id"
          )
        ]
      ) >
        0L
    )
  }
)


testthat::test_that(
  "multiple Level 1 assignments to one canonical enterprise fail",
  {
    assignments <-
      tibble::tibble(
        source =
          c(
            "employment",
            "employment"
          ),
        source_record_id =
          c(
            "S1",
            "S2"
          ),
        decision_stage =
          c(
            "level1",
            "level1"
          ),
        decision_status =
          c(
            "auto_link",
            "auto_link"
          ),
        assigned_register_id =
          c(
            "R1",
            "R1"
          ),
        assigned_canonical_firm_id =
          c(
            "C1",
            "C1"
          )
      )

    testthat::expect_error(
      resolve_downstream_assignment_collisions(
        assignments
      ),
      "Multiple deterministic Level-1 assignments"
    )
  }
)


testthat::test_that(
  "truth assignments collapse method duplication without changing identity",
  {
    records <-
      tibble::tibble(
        scenario =
          rep("baseline", 4L),
        method =
          rep(
            c(
              "weighted_similarity",
              "random_forest"
            ),
            each = 2L
          ),
        source =
          rep("employment", 4L),
        source_record_id =
          rep(
            c("S1", "S2"),
            2L
          ),
        true_register_id =
          rep(
            c("R1", "R2"),
            2L
          ),
        true_canonical_firm_id =
          rep(
            c("C1", "C2"),
            2L
          )
      )

    result <-
      build_truth_downstream_assignments(
        records,
        "baseline"
      )

    testthat::expect_equal(
      nrow(result),
      2L
    )

    testthat::expect_equal(
      result$true_canonical_firm_id,
      c(
        "C1",
        "C2"
      )
    )
  }
)


testthat::test_that(
  "indicator comparison preserves absolute and relative errors",
  {
    truth <-
      tibble::tibble(
        year = 2025L,
        n_enterprises = 10L,
        total_turnover = 100,
        total_average_employment = 20,
        turnover_per_employee = 5
      )

    observed <-
      tibble::tibble(
        year = 2025L,
        n_enterprises = 9L,
        total_turnover = 90,
        total_average_employment = 18,
        turnover_per_employee = 5
      )

    result <-
      compare_downstream_indicator_table(
        observed,
        truth,
        grouping_variables =
          "year",
        aggregation_level =
          "year",
        scenario_name =
          "difficult",
        method_name =
          "random_forest"
      )

    enterprise_count <-
      result %>%
      dplyr::filter(
        .data$metric ==
          "n_enterprises"
      )

    turnover <-
      result %>%
      dplyr::filter(
        .data$metric ==
          "total_turnover"
      )

    ratio <-
      result %>%
      dplyr::filter(
        .data$metric ==
          "turnover_per_employee"
      )

    testthat::expect_equal(
      enterprise_count$difference,
      -1
    )

    testthat::expect_equal(
      enterprise_count$relative_error,
      -0.10
    )

    testthat::expect_equal(
      turnover$absolute_difference,
      10
    )

    testthat::expect_equal(
      turnover$absolute_relative_error,
      0.10
    )

    testthat::expect_equal(
      ratio$difference,
      0
    )
  }
)


testthat::test_that(
  "missing additive indicator cells are represented as zero",
  {
    truth <-
      tibble::tibble(
        year = 2025L,
        nace_code = "A",
        n_enterprises = 2L,
        total_turnover = 40,
        total_average_employment = 8,
        turnover_per_employee = 5
      )

    observed <-
      tibble::tibble(
        year = integer(),
        nace_code = character(),
        n_enterprises = integer(),
        total_turnover = numeric(),
        total_average_employment = numeric(),
        turnover_per_employee = numeric()
      )

    result <-
      compare_downstream_indicator_table(
        observed,
        truth,
        grouping_variables =
          c(
            "year",
            "nace_code"
          ),
        aggregation_level =
          "sector",
        scenario_name =
          "difficult",
        method_name =
          "random_forest"
      )

    enterprise_count <-
      result %>%
      dplyr::filter(
        .data$metric ==
          "n_enterprises"
      )

    ratio <-
      result %>%
      dplyr::filter(
        .data$metric ==
          "turnover_per_employee"
      )

    testthat::expect_equal(
      enterprise_count$cell_status,
      "truth_only"
    )

    testthat::expect_equal(
      enterprise_count$observed_value,
      0
    )

    testthat::expect_equal(
      enterprise_count$difference,
      -2
    )

    testthat::expect_true(
      is.na(
        ratio$observed_value
      )
    )
  }
)
