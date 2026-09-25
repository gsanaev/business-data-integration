# =====================================================================
# linkage_decision.R
# Similarity-based linkage decisions
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Decision behavior and compatibility wrapper are intentionally
# unchanged at this stage.
# =====================================================================

decide_similarity_candidates <- function(
  ranked_candidates,
  score_threshold,
  margin_threshold
) {
  ranked_candidates %>%
    filter(
      candidate_rank <= 2
    ) %>%
    group_by(
      source_record_id
    ) %>%
    summarise(
      candidate_register_id =
        first(register_id),

      candidate_canonical_firm_id =
        first(canonical_firm_id),

      top_similarity_score =
        first(similarity_score),

      second_similarity_score =
        ifelse(
          n() >= 2,
          nth(
            similarity_score,
            2
          ),
          NA_real_
        ),

      similarity_margin =
        ifelse(
          is.na(second_similarity_score),
          top_similarity_score,
          top_similarity_score -
            second_similarity_score
        ),

      .groups = "drop"
    ) %>%
    mutate(
      similarity_status = case_when(
        top_similarity_score >=
          score_threshold &
          similarity_margin >=
            margin_threshold ~
          "matched_similarity",

        top_similarity_score >=
          score_threshold ~
          "review_required_similarity_ambiguous",

        TRUE ~
          "unmatched_low_similarity"
      )
    )
}


rank_similarity_candidates <- function(
  source_entities,
  source_id_column,
  register_entities,
  similarity_weights,
  score_threshold,
  margin_threshold
) {
  source_for_matching <-
    prepare_source_linkage_records(
      source_entities,
      source_id_column
    )

  register_for_matching <-
    prepare_register_linkage_records(
      register_entities
    )

  candidate_pairs <-
    generate_linkage_candidates(
      source_for_matching,
      register_for_matching
    ) %>%
    add_linkage_features() %>%
    score_and_rank_similarity_candidates(
      similarity_weights
    )

  decisions <-
    decide_similarity_candidates(
      candidate_pairs,
      score_threshold,
      margin_threshold
    )

  list(
    candidates = candidate_pairs,
    decisions = decisions
  )
}
