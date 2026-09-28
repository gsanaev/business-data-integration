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

# Draw a controlled corruption flag.
#
# A zero probability deliberately consumes no RNG so that the protected
# baseline realization remains unchanged when new scenario mechanisms are
# inactive.
draw_scenario_flag <- function(
  n,
  probability
) {
  if (
    length(n) != 1L ||
      !is.numeric(n) ||
      !is.finite(n) ||
      n < 0
  ) {
    stop("n must be a non-negative scalar.")
  }

  if (
    length(probability) != 1L ||
      !is.numeric(probability) ||
      !is.finite(probability) ||
      probability < 0 ||
      probability > 1
  ) {
    stop("probability must lie within [0, 1].")
  }

  n <- as.integer(n)

  if (
    n == 0L ||
      probability == 0
  ) {
    return(
      rep(
        FALSE,
        n
      )
    )
  }

  runif(n) < probability
}


make_unknown_business_id <- function(
  x,
  flag,
  source_label
) {
  out <- x

  idx <-
    which(
      flag &
        !is.na(out)
    )

  if (
    length(idx) == 0L
  ) {
    return(out)
  }

  out[idx] <-
    sprintf(
      "UNKNOWN_%s_%06d",
      toupper(source_label),
      idx
    )

  out
}


introduce_name_typo <- function(
  x,
  flag
) {
  out <- x

  idx <-
    which(
      flag &
        !is.na(out)
    )

  if (
    length(idx) == 0L
  ) {
    return(out)
  }

  out[idx] <-
    vapply(
      out[idx],
      function(value) {
        if (
          nchar(value) < 4L
        ) {
          return(value)
        }

        paste0(
          substr(
            value,
            1L,
            2L
          ),
          substr(
            value,
            4L,
            nchar(value)
          )
        )
      },
      character(1)
    )

  out
}


degrade_company_name <- function(
  x,
  flag
) {
  out <- x

  idx <-
    which(
      flag &
        !is.na(out)
    )

  if (
    length(idx) == 0L
  ) {
    return(out)
  }

  out[idx] <-
    sub(
      "^[^ ]+[ ]+",
      "",
      out[idx]
    )

  out
}


create_strong_street_discrepancy <- function(
  x,
  flag
) {
  out <- x

  idx <-
    which(
      flag &
        !is.na(out)
    )

  if (
    length(idx) == 0L
  ) {
    return(out)
  }

  out[idx] <-
    sub(
      "^[^ ]+",
      "Nebenstrasse",
      out[idx]
    )

  out
}


corrupt_postal_code <- function(
  x,
  flag
) {
  out <- x

  idx <-
    which(flag)

  if (
    length(idx) == 0L
  ) {
    return(out)
  }

  missing_idx <-
    idx[
      seq_along(idx) %% 2L == 1L
    ]

  error_idx <-
    setdiff(
      idx,
      missing_idx
    )

  out[missing_idx] <-
    NA_character_

  if (
    length(error_idx) > 0L
  ) {
    out[error_idx] <-
      vapply(
        out[error_idx],
        function(value) {
          if (
            is.na(value) ||
              !grepl(
                "^[0-9]{5}$",
                value
              )
          ) {
            return("99999")
          }

          last_digit <-
            as.integer(
              substr(
                value,
                5L,
                5L
              )
            )

          paste0(
            substr(
              value,
              1L,
              4L
            ),
            (
              last_digit + 1L
            ) %% 10L
          )
        },
        character(1)
      )
  }

  out
}


create_nace_disagreement <- function(
  x,
  flag
) {
  out <- x

  mapping <- c(
    G47 = "C10",
    C10 = "C29",
    C29 = "H49",
    H49 = "I55",
    I55 = "I56",
    I56 = "G47"
  )

  idx <-
    which(
      flag &
        !is.na(out)
    )

  if (
    length(idx) == 0L
  ) {
    return(out)
  }

  replacement <-
    unname(
      mapping[
        out[idx]
      ]
    )

  replacement[
    is.na(replacement)
  ] <- "X99"

  out[idx] <-
    replacement

  out
}


create_legal_form_disagreement <- function(
  x,
  flag
) {
  out <- x

  mapping <- c(
    AG = "GmbH",
    GmbH = "KG",
    KG = "OHG",
    OHG = "Einzelunternehmen",
    Einzelunternehmen = "AG"
  )

  idx <-
    which(
      flag &
        !is.na(out)
    )

  if (
    length(idx) == 0L
  ) {
    return(out)
  }

  replacement <-
    unname(
      mapping[
        out[idx]
      ]
    )

  replacement[
    is.na(replacement)
  ] <- "GmbH"

  out[idx] <-
    replacement

  out
}
