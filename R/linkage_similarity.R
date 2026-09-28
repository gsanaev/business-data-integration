# =====================================================================
# linkage_similarity.R
# Transparent Similarity Helpers for Enterprise Record Linkage
# =====================================================================

# Normalise text for comparison while preserving meaningful word-level
# differences such as "und" versus "&" or "strasse" versus "str.".
normalize_linkage_text <- function(x) {
  out <- as.character(x)

  out <- iconv(
    out,
    from = "",
    to = "ASCII//TRANSLIT",
    sub = ""
  )

  out <- tolower(out)

  out <- gsub(
    "[^a-z0-9 ]+",
    " ",
    out
  )

  out <- gsub(
    "\\s+",
    " ",
    out
  )

  trimws(out)
}


# Convert Levenshtein edit distance to a similarity measure in [0, 1].
#
# 1 means identical normalised strings.
# 0 means maximal difference relative to the longer string.
normalized_edit_similarity <- function(x, y) {
  x_norm <- normalize_linkage_text(x)
  y_norm <- normalize_linkage_text(y)

  mapply(
    function(a, b) {
      if (
        is.na(a) ||
        is.na(b) ||
        !nzchar(a) ||
        !nzchar(b)
      ) {
        return(NA_real_)
      }

      max_length <- max(
        nchar(a),
        nchar(b)
      )

      distance <- adist(
        a,
        b
      )[1, 1]

      max(
        0,
        1 - distance / max_length
      )
    },
    x_norm,
    y_norm,
    USE.NAMES = FALSE
  )
}


# Exact comparison after basic text normalisation.
normalized_exact_match <- function(x, y) {
  x_norm <- normalize_linkage_text(x)
  y_norm <- normalize_linkage_text(y)

  as.numeric(
    !is.na(x_norm) &
      !is.na(y_norm) &
      nzchar(x_norm) &
      nzchar(y_norm) &
      x_norm == y_norm
  )
}


# Weighted similarity scoring and candidate ranking.
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
