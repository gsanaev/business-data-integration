# =====================================================================
# linkage_similarity.R
# Weighted similarity scoring and candidate ranking
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Scoring and ranking behavior are intentionally unchanged.
# =====================================================================

score_and_rank_similarity_candidates <- function(
  candidate_pairs,
  similarity_weights
) {
  candidate_pairs %>%
    mutate(
      similarity_score =
        similarity_weights[["name_similarity"]] *
          name_similarity +
        similarity_weights[["street_similarity"]] *
          street_similarity +
        similarity_weights[["city_similarity"]] *
          city_similarity +
        similarity_weights[["postal_code_match"]] *
          postal_code_match +
        similarity_weights[["legal_form_match"]] *
          legal_form_match +
        similarity_weights[["nace_match"]] *
          nace_match
    ) %>%
    group_by(
      source_record_id
    ) %>%
    arrange(
      desc(similarity_score),
      register_id,
      .by_group = TRUE
    ) %>%
    mutate(
      candidate_rank =
        row_number()
    ) %>%
    ungroup()
}
