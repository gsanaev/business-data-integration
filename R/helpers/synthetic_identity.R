# =====================================================================
# synthetic_identity.R
# Helpers for creating source-specific synthetic enterprise identities
# =====================================================================

# Create source-specific record identifiers.
make_source_id <- function(prefix, n) {
  sprintf(
    "%s%06d",
    prefix,
    seq_len(n)
  )
}


# Remove a strong business identifier from a controlled share of records.
#
# Missing strong identifiers create unresolved cases for the later
# record-linkage workflow without introducing false deterministic matches.
drop_identifier <- function(x, probability = 0.10) {
  out <- x

  missing <- runif(length(out)) < probability
  out[missing] <- NA_character_

  out
}


# Create modest source-specific variants of enterprise names.
#
# The transformations intentionally remain interpretable. Their purpose is
# to create realistic matching differences rather than difficult artificial
# corruption.
perturb_company_name <- function(x) {
  out <- x

  # Legal-form representation.
  idx <- runif(length(out)) < 0.12
  out[idx] <- gsub(
    "GmbH",
    "GMBH",
    out[idx],
    fixed = TRUE
  )

  # Ampersand representation.
  idx <- runif(length(out)) < 0.08
  out[idx] <- gsub(
    " & ",
    " und ",
    out[idx],
    fixed = TRUE
  )

  # Remove selected punctuation.
  idx <- runif(length(out)) < 0.08
  out[idx] <- gsub(
    "[.,]",
    "",
    out[idx]
  )

  out
}


# Create modest source-specific street variants.
perturb_street <- function(x) {
  out <- x

  idx <- runif(length(out)) < 0.15
  out[idx] <- gsub(
    "strasse",
    "str.",
    out[idx],
    ignore.case = TRUE
  )

  idx <- runif(length(out)) < 0.08
  out[idx] <- gsub(
    "weg",
    "W.",
    out[idx],
    ignore.case = TRUE
  )

  out
}


# Create modest source-specific city-name variants.
perturb_city <- function(x) {
  out <- x

  idx <- runif(length(out)) < 0.08
  out[idx] <- toupper(out[idx])

  out
}
