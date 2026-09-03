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
