# =====================================================================
# test-coherence-outputs.R
# Integration tests for cross-source coherence outputs
# =====================================================================

project_path <- function(...) {
  testthat::test_path(
    "..",
    "..",
    ...
  )
}

events_path <- project_path(
  "data",
  "processed",
  "coherence_events.csv"
)

queue_path <- project_path(
  "data",
  "processed",
  "review_queue.csv"
)


testthat::test_that("required coherence outputs exist", {
  testthat::expect_true(
    file.exists(
      events_path
    )
  )

  testthat::expect_true(
    file.exists(
      queue_path
    )
  )
})


events <- read.csv(
  events_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

queue <- read.csv(
  queue_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


testthat::test_that("coherence outputs contain the required structure", {
  required_columns <- c(
    "canonical_firm_id",
    "reference_year",
    "rule_id",
    "comparability_group",
    "left_source",
    "left_variable",
    "right_source",
    "right_variable",
    "left_value",
    "right_value",
    "monthly_coverage",
    "required_monthly_coverage",
    "imputed_months",
    "right_quality_status",
    "applicability_status",
    "expected_range_threshold",
    "absolute_difference",
    "relative_difference",
    "coherence_status",
    "materiality_percentile",
    "materiality",
    "review_priority"
  )

  testthat::expect_true(
    all(
      required_columns %in%
        names(
          events
        )
    )
  )

  testthat::expect_true(
    all(
      required_columns %in%
        names(
          queue
        )
    )
  )
})


testthat::test_that("coherence event keys are unique", {
  event_key <- paste(
    events$canonical_firm_id,
    events$reference_year,
    events$rule_id,
    sep = "::"
  )

  testthat::expect_false(
    anyDuplicated(
      event_key
    ) > 0L
  )
})


testthat::test_that("coherence applicability semantics are preserved", {
  applicable <-
    events$applicability_status ==
      "applicable"

  not_applicable <-
    !applicable

  testthat::expect_true(
    all(
      is.finite(
        events$absolute_difference[
          applicable
        ]
      )
    )
  )

  testthat::expect_true(
    all(
      is.finite(
        events$relative_difference[
          applicable
        ]
      )
    )
  )

  testthat::expect_true(
    all(
      is.na(
        events$absolute_difference[
          not_applicable
        ]
      )
    )
  )

  testthat::expect_true(
    all(
      is.na(
        events$relative_difference[
          not_applicable
        ]
      )
    )
  )

  testthat::expect_true(
    all(
      events$coherence_status[
        not_applicable
      ] ==
        "not_assessed"
    )
  )
})


testthat::test_that("coherence status follows the documented thresholds", {
  applicable <-
    events$applicability_status ==
      "applicable"

  expected_large <-
    events$relative_difference[
      applicable
    ] >
      events$expected_range_threshold[
        applicable
      ]

  actual_large <-
    events$coherence_status[
      applicable
    ] ==
      "large_difference"

  testthat::expect_equal(
    unname(
      actual_large
    ),
    unname(
      expected_large
    )
  )

  allowed_applicable_statuses <- c(
    "within_expected_range",
    "large_difference"
  )

  testthat::expect_true(
    all(
      events$coherence_status[
        applicable
      ] %in%
        allowed_applicable_statuses
    )
  )
})


testthat::test_that("materiality and review priority follow coherence semantics", {
  applicable <-
    events$applicability_status ==
      "applicable"

  not_applicable <-
    !applicable

  testthat::expect_true(
    all(
      is.na(
        events$materiality_percentile[
          not_applicable
        ]
      )
    )
  )

  applicable_percentiles <-
    events$materiality_percentile[
      applicable
    ]

  testthat::expect_true(
    all(
      is.finite(
        applicable_percentiles
      ) &
        applicable_percentiles >= 0 &
        applicable_percentiles <= 1
    )
  )

  within_range <-
    events$coherence_status ==
      "within_expected_range"

  testthat::expect_true(
    all(
      events$review_priority[
        within_range
      ] ==
        "none"
    )
  )
})


testthat::test_that("review queue exactly represents prioritized coherence cases", {
  event_key <- paste(
    events$canonical_firm_id,
    events$reference_year,
    events$rule_id,
    sep = "::"
  )

  testthat::expect_true(
    nrow(
      queue
    ) > 0L
  )

  testthat::expect_true(
    all(
      queue$applicability_status ==
        "applicable"
    )
  )

  testthat::expect_true(
    all(
      queue$coherence_status ==
        "large_difference"
    )
  )

  testthat::expect_true(
    all(
      queue$review_priority %in%
        c(
          "high",
          "medium"
        )
    )
  )

  queue_key <- paste(
    queue$canonical_firm_id,
    queue$reference_year,
    queue$rule_id,
    sep = "::"
  )

  testthat::expect_false(
    anyDuplicated(
      queue_key
    ) > 0L
  )

  testthat::expect_true(
    all(
      queue_key %in%
        event_key
    )
  )

  expected_queue_keys <- event_key[
    events$coherence_status ==
      "large_difference" &
      events$review_priority %in%
        c(
          "high",
          "medium"
        )
  ]

  testthat::expect_equal(
    unname(
      sort(
        queue_key
      )
    ),
    unname(
      sort(
        expected_queue_keys
      )
    )
  )
})
