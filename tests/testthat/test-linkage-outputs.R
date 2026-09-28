# =====================================================================
# test-linkage-outputs.R
# Integration tests for enterprise record-linkage outputs
# =====================================================================

project_path <- function(...) {
  testthat::test_path(
    "..",
    "..",
    ...
  )
}

crosswalk_path <- project_path(
  "data",
  "processed",
  "linkage_crosswalk.csv"
)

candidates_path <- project_path(
  "data",
  "processed",
  "linkage_candidates.csv"
)


testthat::test_that("required linkage outputs exist", {
  testthat::expect_true(
    file.exists(
      crosswalk_path
    )
  )

  testthat::expect_true(
    file.exists(
      candidates_path
    )
  )
})


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
  project_path(
    "data",
    "clean",
    "firms_clean.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

employment <- read.csv(
  project_path(
    "data",
    "clean",
    "employment_clean.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

turnover <- read.csv(
  project_path(
    "data",
    "clean",
    "turnover_clean.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

accounting <- read.csv(
  project_path(
    "data",
    "clean",
    "accounting_clean.csv"
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)


testthat::test_that("linkage outputs contain the required structure", {
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

  testthat::expect_true(
    all(
      required_crosswalk_columns %in%
        names(
          crosswalk
        )
    )
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

  testthat::expect_true(
    all(
      required_candidate_columns %in%
        names(
          candidates
        )
    )
  )
})


testthat::test_that("crosswalk contains one row per source entity", {
  crosswalk_key <- paste(
    crosswalk$source,
    crosswalk$source_record_id,
    sep = "::"
  )

  testthat::expect_false(
    anyDuplicated(
      crosswalk_key
    ) > 0L
  )

  expected_source_counts <- c(
    register =
      nrow(
        firms
      ),

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
      names(
        expected_source_counts
      )
  ) {
    testthat::expect_true(
      source_name %in%
        names(
          actual_source_counts
        ),
      info = paste0(
        "Crosswalk is missing source: ",
        source_name
      )
    )

    testthat::expect_equal(
      as.numeric(
        actual_source_counts[
          source_name
        ]
      ),
      as.numeric(
        expected_source_counts[
          source_name
        ]
      ),
      info = paste0(
        "Crosswalk row count is inconsistent for source ",
        source_name,
        "."
      )
    )
  }
})


testthat::test_that("register reference rows preserve canonical integrity", {
  register_rows <- crosswalk[
    crosswalk$source ==
      "register",
  ]

  testthat::expect_true(
    all(
      register_rows$linkage_status ==
        "reference"
    )
  )

  testthat::expect_true(
    all(
      register_rows$linkage_method ==
        "register_reference"
    )
  )

  testthat::expect_false(
    any(
      is.na(
        register_rows$canonical_firm_id
      ) |
        register_rows$canonical_firm_id ==
          ""
    )
  )

  testthat::expect_false(
    anyDuplicated(
      register_rows$canonical_firm_id
    ) > 0L
  )
})


testthat::test_that("deterministic linkage uses valid business identifiers", {
  deterministic <- crosswalk[
    crosswalk$linkage_status ==
      "matched_deterministic",
  ]

  testthat::expect_true(
    all(
      deterministic$linkage_method ==
        "business_id_exact"
    )
  )

  testthat::expect_true(
    all(
      !is.na(
        deterministic$business_id
      ) &
        deterministic$business_id !=
          ""
    )
  )

  testthat::expect_true(
    all(
      !is.na(
        deterministic$canonical_firm_id
      ) &
        deterministic$canonical_firm_id !=
          ""
    )
  )
})


testthat::test_that("candidate similarity scores follow the v2 construction", {
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

    testthat::expect_true(
      all(
        is.na(
          values
        ) |
          (
            values >= 0 &
              values <= 1
          )
      ),
      info = paste0(
        column_name,
        " contains values outside [0, 1]."
      )
    )
  }

  testthat::expect_true(
    all(
      is.na(
        candidates$similarity_score
      ) |
        (
          candidates$similarity_score >= 0 &
            candidates$similarity_score <= 1
        )
    )
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

  testthat::expect_true(
    all(
      score_difference <
        1e-12
    )
  )
})


testthat::test_that("candidate ranking is complete and score ordered", {
  candidate_groups <- split(
    candidates,
    paste(
      candidates$source,
      candidates$source_record_id,
      sep = "::"
    )
  )

  for (
    group_name in
      names(
        candidate_groups
      )
  ) {
    candidate_group <-
      candidate_groups[[group_name]]

    ranks <-
      candidate_group$candidate_rank

    testthat::expect_equal(
      as.numeric(
        sort(
          ranks
        )
      ),
      as.numeric(
        seq_len(
          nrow(
            candidate_group
          )
        )
      ),
      info = paste0(
        "Incomplete candidate ranks for ",
        group_name,
        "."
      )
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
      testthat::expect_true(
        all(
          diff(
            usable_scores
          ) <=
            1e-12
        ),
        info = paste0(
          "Candidate scores increase with rank for ",
          group_name,
          "."
        )
      )
    }
  }
})


testthat::test_that("similarity decisions follow the existing v2 thresholds", {
  score_threshold <- 0.85
  margin_threshold <- 0.05

  similarity_matches <- crosswalk[
    crosswalk$linkage_status ==
      "matched_similarity",
  ]

  testthat::expect_true(
    all(
      similarity_matches$linkage_method ==
        "weighted_edit_similarity"
    )
  )

  testthat::expect_true(
    all(
      similarity_matches$top_similarity_score >=
        score_threshold
    )
  )

  testthat::expect_true(
    all(
      similarity_matches$similarity_margin >=
        margin_threshold
    )
  )

  testthat::expect_true(
    all(
      !is.na(
        similarity_matches$canonical_firm_id
      ) &
        similarity_matches$canonical_firm_id !=
          ""
    )
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
    testthat::expect_true(
      all(
        ambiguous$top_similarity_score >=
          score_threshold
      )
    )

    testthat::expect_true(
      all(
        ambiguous$similarity_margin <
          margin_threshold
      )
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
    testthat::expect_true(
      all(
        low_similarity$top_similarity_score <
          score_threshold
      )
    )
  }
})
