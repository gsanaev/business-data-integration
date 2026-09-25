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

link_source_entities <- function(
  source_entities,
  source_id_column,
  register_lookup,
  register_entities,
  similarity_weights,
  score_threshold,
  margin_threshold
) {
  deterministic_base <-
    source_entities %>%
    left_join(
      register_lookup,
      by = "business_id"
    ) %>%
    mutate(
      linkage_status = case_when(
        is.na(business_id) ~
          "unmatched_missing_identifier",

        !is.na(canonical_firm_id) ~
          "matched_deterministic",

        TRUE ~
          "unmatched_identifier_not_found"
      ),

      linkage_method = case_when(
        linkage_status ==
          "matched_deterministic" ~
          "business_id_exact",

        TRUE ~
          NA_character_
      )
    )

  similarity <-
    rank_similarity_candidates(
      source_entities =
        source_entities %>%
        filter(
          is.na(business_id)
        ),

      source_id_column =
        source_id_column,

      register_entities =
        register_entities,

      similarity_weights =
        similarity_weights,

      score_threshold =
        score_threshold,

      margin_threshold =
        margin_threshold
    )

  join_by <-
    setNames(
      "source_record_id",
      source_id_column
    )

  links <-
    deterministic_base %>%
    left_join(
      similarity$decisions,
      by = join_by
    ) %>%
    mutate(
      register_id = case_when(
        linkage_status ==
          "unmatched_missing_identifier" &
          similarity_status ==
            "matched_similarity" ~
          candidate_register_id,

        TRUE ~
          register_id
      ),

      canonical_firm_id = case_when(
        linkage_status ==
          "unmatched_missing_identifier" &
          similarity_status ==
            "matched_similarity" ~
          candidate_canonical_firm_id,

        TRUE ~
          canonical_firm_id
      ),

      linkage_status = case_when(
        linkage_status ==
          "unmatched_missing_identifier" &
          !is.na(similarity_status) ~
          similarity_status,

        TRUE ~
          linkage_status
      ),

      linkage_method = case_when(
        linkage_status ==
          "matched_similarity" ~
          "weighted_edit_similarity",

        TRUE ~
          linkage_method
      )
    )

  list(
    links = links,
    similarity = similarity
  )
}

build_register_linkage_crosswalk <- function(
  register_entities
) {
  register_entities %>%
    transmute(
      source = "register",
      source_record_id =
        register_id,
      business_id,
      canonical_firm_id,
      register_id,
      candidate_register_id =
        NA_character_,
      linkage_status =
        "reference",
      linkage_method =
        "register_reference",
      top_similarity_score =
        NA_real_,
      second_similarity_score =
        NA_real_,
      similarity_margin =
        NA_real_
    )
}


build_source_linkage_output <- function(
  source_links,
  similarity_candidates,
  source_name,
  source_id_column
) {
  crosswalk <-
    source_links %>%
    transmute(
      source =
        source_name,

      source_record_id =
        .data[[source_id_column]],

      business_id,
      canonical_firm_id,
      register_id,
      candidate_register_id,
      linkage_status,
      linkage_method,
      top_similarity_score,
      second_similarity_score,
      similarity_margin
    )

  candidates <-
    similarity_candidates %>%
    mutate(
      source =
        source_name
    )

  list(
    crosswalk = crosswalk,
    candidates = candidates
  )
}
