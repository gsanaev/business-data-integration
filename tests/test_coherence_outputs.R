# =====================================================================
# test_coherence_outputs.R
# Integration tests for cross-source coherence outputs
# =====================================================================

source(
  "tests/helpers/assertions.R"
)

events_path <-
  "data/processed/coherence_events.csv"

queue_path <-
  "data/processed/review_queue.csv"

assert_true(
  file.exists(events_path),
  "Coherence-event output is missing."
)

assert_true(
  file.exists(queue_path),
  "Review-queue output is missing."
)

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

# ----------------------------------------------------------------------
# 1. Required structure
# ----------------------------------------------------------------------

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

assert_true(
  all(
    required_columns %in%
      names(events)
  ),
  "Coherence events are missing required columns."
)

assert_true(
  all(
    required_columns %in%
      names(queue)
  ),
  "Review queue is missing required columns."
)

# ----------------------------------------------------------------------
# 2. Event key integrity
# ----------------------------------------------------------------------

event_key <- paste(
  events$canonical_firm_id,
  events$reference_year,
  events$rule_id,
  sep = "::"
)

assert_true(
  !anyDuplicated(event_key),
  "Coherence events contain duplicate enterprise-year-rule keys."
)

# ----------------------------------------------------------------------
# 3. Applicability semantics
# ----------------------------------------------------------------------

applicable <-
  events$applicability_status ==
    "applicable"

not_applicable <-
  !applicable

assert_true(
  all(
    is.finite(
      events$absolute_difference[
        applicable
      ]
    )
  ),
  "Applicable coherence events require an absolute difference."
)

assert_true(
  all(
    is.finite(
      events$relative_difference[
        applicable
      ]
    )
  ),
  "Applicable coherence events require a relative difference."
)

assert_true(
  all(
    is.na(
      events$absolute_difference[
        not_applicable
      ]
    )
  ),
  "Non-applicable events must not receive an absolute difference."
)

assert_true(
  all(
    is.na(
      events$relative_difference[
        not_applicable
      ]
    )
  ),
  "Non-applicable events must not receive a relative difference."
)

assert_true(
  all(
    events$coherence_status[
      not_applicable
    ] ==
      "not_assessed"
  ),
  "Non-applicable events must have not_assessed coherence status."
)

# ----------------------------------------------------------------------
# 4. Threshold classification
# ----------------------------------------------------------------------

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

assert_equal(
  actual_large,
  expected_large,
  paste(
    "Coherence status should follow",
    "the documented relative-difference threshold."
  )
)

allowed_applicable_statuses <- c(
  "within_expected_range",
  "large_difference"
)

assert_true(
  all(
    events$coherence_status[
      applicable
    ] %in%
      allowed_applicable_statuses
  ),
  "Applicable events contain an unsupported coherence status."
)

# ----------------------------------------------------------------------
# 5. Materiality and priority semantics
# ----------------------------------------------------------------------

assert_true(
  all(
    is.na(
      events$materiality_percentile[
        not_applicable
      ]
    )
  ),
  "Non-applicable events must not receive a materiality percentile."
)

applicable_percentiles <-
  events$materiality_percentile[
    applicable
  ]

assert_true(
  all(
    is.finite(
      applicable_percentiles
    ) &
      applicable_percentiles >= 0 &
      applicable_percentiles <= 1
  ),
  "Applicable materiality percentiles must lie within [0, 1]."
)

within_range <-
  events$coherence_status ==
    "within_expected_range"

assert_true(
  all(
    events$review_priority[
      within_range
    ] ==
      "none"
  ),
  "Within-range coherence events must not receive review priority."
)

# ----------------------------------------------------------------------
# 6. Review queue semantics
# ----------------------------------------------------------------------

assert_true(
  nrow(queue) > 0L,
  "Review queue should contain prioritized coherence cases."
)

assert_true(
  all(
    queue$applicability_status ==
      "applicable"
  ),
  "Review queue may contain only applicable comparisons."
)

assert_true(
  all(
    queue$coherence_status ==
      "large_difference"
  ),
  "Review queue may contain only large-difference events."
)

assert_true(
  all(
    queue$review_priority %in%
      c(
        "high",
        "medium"
      )
  ),
  "Review queue may contain only high- or medium-priority cases."
)

queue_key <- paste(
  queue$canonical_firm_id,
  queue$reference_year,
  queue$rule_id,
  sep = "::"
)

assert_true(
  !anyDuplicated(queue_key),
  "Review queue contains duplicate enterprise-year-rule keys."
)

assert_true(
  all(
    queue_key %in%
      event_key
  ),
  "Every review-queue row must correspond to a coherence event."
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

assert_equal(
  sort(queue_key),
  sort(expected_queue_keys),
  "Review queue does not exactly match the prioritized coherence events."
)

cat(
  "test_coherence_outputs.R: PASS\n"
)
