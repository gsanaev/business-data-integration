# =====================================================================
# test_linkage_outputs.R
# Integration tests for enterprise record-linkage outputs
# =====================================================================

source(
  "tests/helpers/assertions.R"
)

crosswalk_path <-
  "data/processed/linkage_crosswalk.csv"

candidates_path <-
  "data/processed/linkage_candidates.csv"

assert_true(
  file.exists(
    crosswalk_path
  ),
  "Linkage crosswalk output is missing."
)

assert_true(
  file.exists(
    candidates_path
  ),
  "Linkage candidate output is missing."
)

crosswalk <- read.csv(
  crosswalk_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

candidates <- read.csv(
  candidates_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

firms <- read.csv(
  "data/clean/firms_clean.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

employment <- read.csv(
  "data/clean/employment_clean.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

turnover <- read.csv(
  "data/clean/turnover_clean.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

accounting <- read.csv(
  "data/clean/accounting_clean.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# ----------------------------------------------------------------------
# 1. Required output structure
# ----------------------------------------------------------------------

required_crosswalk_columns <- c(
  "source",
  "source_record_id",
  "business_id",
  "canonical_firm_id",
  "register_id",
  "candidate_register_id",
  "linkage_status",
  "linkage_method",
  "top_similarity_score",
  "second_similarity_score",
  "similarity_margin"
)

assert_true(
  all(
    required_crosswalk_columns %in%
      names(crosswalk)
  ),
  "Linkage crosswalk is missing required columns."
)

required_candidate_columns <- c(
  "source",
  "source_record_id",
  "register_id",
  "canonical_firm_id",
  "name_similarity",
  "street_similarity",
  "city_similarity",
  "postal_code_match",
  "legal_form_match",
  "nace_match",
  "similarity_score",
  "candidate_rank"
)

assert_true(
  all(
    required_candidate_columns %in%
      names(candidates)
  ),
  "Linkage candidate evidence is missing required columns."
)

# ----------------------------------------------------------------------
# 2. One crosswalk row per source entity
# ----------------------------------------------------------------------

crosswalk_key <- paste(
  crosswalk$source,
  crosswalk$source_record_id,
  sep = "::"
)

assert_true(
  !anyDuplicated(
    crosswalk_key
  ),
  "Linkage crosswalk contains duplicate source-record keys."
)

expected_source_counts <- c(
  register =
    nrow(firms),

  employment =
    length(
      unique(
        employment$employment_source_id
      )
    ),

  turnover =
    length(
      unique(
        turnover$turnover_source_id
      )
    ),

  accounting =
    length(
      unique(
        accounting$accounting_source_id
      )
    )
)

actual_source_counts <- table(
  crosswalk$source
)

for (
  source_name in
    names(expected_source_counts)
) {
  assert_true(
    source_name %in%
      names(actual_source_counts),
    paste0(
      "Crosswalk is missing source: ",
      source_name
    )
  )

  assert_equal(
    unname(
      actual_source_counts[
        source_name
      ]
    ),
    unname(
      expected_source_counts[
        source_name
      ]
    ),
    paste0(
      "Crosswalk row count is inconsistent for source ",
      source_name,
      "."
    )
  )
}

# ----------------------------------------------------------------------
# 3. Register reference integrity
# ----------------------------------------------------------------------

register_rows <- crosswalk[
  crosswalk$source ==
    "register",
]

assert_true(
  all(
    register_rows$linkage_status ==
      "reference"
  ),
  "Register rows should have reference linkage status."
)

assert_true(
  all(
    register_rows$linkage_method ==
      "register_reference"
  ),
  "Register rows should use register_reference linkage method."
)

assert_true(
  !any(
    is.na(
      register_rows$canonical_firm_id
    ) |
      register_rows$canonical_firm_id ==
        ""
  ),
  "Register reference rows must have canonical enterprise identifiers."
)

assert_true(
  !anyDuplicated(
    register_rows$canonical_firm_id
  ),
  "Canonical enterprise identifiers must be unique in the register reference."
)

# ----------------------------------------------------------------------
# 4. Deterministic linkage integrity
# ----------------------------------------------------------------------

deterministic <- crosswalk[
  crosswalk$linkage_status ==
    "matched_deterministic",
]

assert_true(
  all(
    deterministic$linkage_method ==
      "business_id_exact"
  ),
  "Deterministic matches should use business_id_exact."
)

assert_true(
  all(
    !is.na(
      deterministic$business_id
    ) &
      deterministic$business_id !=
        ""
  ),
  "Deterministic matches require a business identifier."
)

assert_true(
  all(
    !is.na(
      deterministic$canonical_firm_id
    ) &
      deterministic$canonical_firm_id !=
        ""
  ),
  "Deterministic matches require a canonical enterprise identifier."
)

# ----------------------------------------------------------------------
# 5. Candidate score construction
# ----------------------------------------------------------------------

score_components <- c(
  "name_similarity",
  "street_similarity",
  "city_similarity",
  "postal_code_match",
  "legal_form_match",
  "nace_match"
)

for (
  column_name in
    score_components
) {
  values <-
    candidates[[column_name]]

  assert_true(
    all(
      is.na(values) |
        (
          values >= 0 &
            values <= 1
        )
    ),
    paste0(
      column_name,
      " contains values outside [0, 1]."
    )
  )
}

assert_true(
  all(
    is.na(
      candidates$similarity_score
    ) |
      (
        candidates$similarity_score >=
          0 &
          candidates$similarity_score <=
            1
      )
  ),
  "Similarity score contains values outside [0, 1]."
)

complete_scores <- complete.cases(
  candidates[
    c(
      score_components,
      "similarity_score"
    )
  ]
)

expected_score <-
  0.40 *
    candidates$name_similarity[
      complete_scores
    ] +
  0.30 *
    candidates$street_similarity[
      complete_scores
    ] +
  0.05 *
    candidates$city_similarity[
      complete_scores
    ] +
  0.10 *
    candidates$postal_code_match[
      complete_scores
    ] +
  0.075 *
    candidates$legal_form_match[
      complete_scores
    ] +
  0.075 *
    candidates$nace_match[
      complete_scores
    ]

score_difference <- abs(
  candidates$similarity_score[
    complete_scores
  ] -
    expected_score
)

assert_true(
  all(
    score_difference <
      1e-12
  ),
  "Stored candidate similarity scores do not match the documented weighting."
)

# ----------------------------------------------------------------------
# 6. Candidate ranking integrity
# ----------------------------------------------------------------------

candidate_groups <- split(
  candidates,
  paste(
    candidates$source,
    candidates$source_record_id,
    sep = "::"
  )
)

for (
  candidate_group in
    candidate_groups
) {
  ranks <- candidate_group$candidate_rank

  assert_equal(
    sort(ranks),
    seq_len(
      nrow(
        candidate_group
      )
    ),
    "Candidate ranks should form a complete sequence beginning at one."
  )

  ordered <- candidate_group[
    order(
      candidate_group$candidate_rank
    ),
  ]

  usable_scores <-
    ordered$similarity_score[
      !is.na(
        ordered$similarity_score
      )
    ]

  if (
    length(
      usable_scores
    ) >= 2L
  ) {
    assert_true(
      all(
        diff(
          usable_scores
        ) <=
          1e-12
      ),
      "Candidate similarity scores should not increase with rank."
    )
  }
}

# ----------------------------------------------------------------------
# 7. Similarity decision thresholds
# ----------------------------------------------------------------------

score_threshold <- 0.85
margin_threshold <- 0.05

similarity_matches <- crosswalk[
  crosswalk$linkage_status ==
    "matched_similarity",
]

assert_true(
  all(
    similarity_matches$linkage_method ==
      "weighted_edit_similarity"
  ),
  "Accepted similarity matches should use weighted_edit_similarity."
)

assert_true(
  all(
    similarity_matches$top_similarity_score >=
      score_threshold
  ),
  "Accepted similarity matches must satisfy the score threshold."
)

assert_true(
  all(
    similarity_matches$similarity_margin >=
      margin_threshold
  ),
  "Accepted similarity matches must satisfy the margin threshold."
)

assert_true(
  all(
    !is.na(
      similarity_matches$canonical_firm_id
    ) &
      similarity_matches$canonical_firm_id !=
        ""
  ),
  "Accepted similarity matches require a canonical enterprise identifier."
)

ambiguous <- crosswalk[
  crosswalk$linkage_status ==
    "review_required_similarity_ambiguous",
]

if (
  nrow(
    ambiguous
  ) > 0L
) {
  assert_true(
    all(
      ambiguous$top_similarity_score >=
        score_threshold
    ),
    "Ambiguous similarity cases should satisfy the score threshold."
  )

  assert_true(
    all(
      ambiguous$similarity_margin <
        margin_threshold
    ),
    "Ambiguous similarity cases should fall below the margin threshold."
  )
}

low_similarity <- crosswalk[
  crosswalk$linkage_status ==
    "unmatched_low_similarity",
]

if (
  nrow(
    low_similarity
  ) > 0L
) {
  assert_true(
    all(
      low_similarity$top_similarity_score <
        score_threshold
    ),
    "Low-similarity cases should fall below the score threshold."
  )
}

cat(
  "test_linkage_outputs.R: PASS\n"
)
