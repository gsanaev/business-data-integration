# =====================================================================
# evaluation.R
# Helpers for reproducible development and held-out evaluation splits
# =====================================================================

create_enterprise_split <- function(
  truth_firm_id,
  development_share = 0.70,
  seed = 202604L
) {
  if (
    length(development_share) != 1L ||
      !is.numeric(development_share) ||
      !is.finite(development_share) ||
      development_share <= 0 ||
      development_share >= 1
  ) {
    stop(
      "development_share must lie strictly between 0 and 1."
    )
  }

  if (
    length(seed) != 1L ||
      !is.numeric(seed) ||
      !is.finite(seed)
  ) {
    stop(
      "seed must be a finite numeric scalar."
    )
  }

  ids <-
    sort(
      unique(
        as.character(
          truth_firm_id
        )
      )
    )

  if (
    length(ids) == 0L ||
      anyNA(ids) ||
      any(!nzchar(ids))
  ) {
    stop(
      "truth_firm_id must contain non-missing, non-empty identifiers."
    )
  }

  n_development <-
    floor(
      length(ids) *
        development_share
    )

  had_seed <-
    exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )

  if (had_seed) {
    previous_seed <-
      get(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )
  }

  on.exit(
    {
      if (had_seed) {
        assign(
          ".Random.seed",
          previous_seed,
          envir = .GlobalEnv
        )
      } else if (
        exists(
          ".Random.seed",
          envir = .GlobalEnv,
          inherits = FALSE
        )
      ) {
        rm(
          ".Random.seed",
          envir = .GlobalEnv
        )
      }
    },
    add = TRUE
  )

  set.seed(
    as.integer(seed)
  )

  development_ids <-
    sample(
      ids,
      size = n_development,
      replace = FALSE
    )

  tibble::tibble(
    truth_firm_id = ids,
    sample_role =
      ifelse(
        ids %in% development_ids,
        "development",
        "heldout"
      )
  )
}


validate_enterprise_split <- function(
  split,
  truth_firm_id,
  development_share = 0.70
) {
  required_columns <- c(
    "truth_firm_id",
    "sample_role"
  )

  missing_columns <-
    setdiff(
      required_columns,
      names(split)
    )

  if (
    length(missing_columns) > 0L
  ) {
    stop(
      "Enterprise split is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  }

  expected_ids <-
    sort(
      unique(
        as.character(
          truth_firm_id
        )
      )
    )

  if (
    nrow(split) !=
      length(expected_ids)
  ) {
    stop(
      "Enterprise split must contain exactly one row per enterprise."
    )
  }

  if (
    anyDuplicated(
      split$truth_firm_id
    )
  ) {
    stop(
      "Enterprise split contains duplicate truth_firm_id values."
    )
  }

  if (
    !setequal(
      split$truth_firm_id,
      expected_ids
    )
  ) {
    stop(
      "Enterprise split does not match the canonical enterprise universe."
    )
  }

  allowed_roles <- c(
    "development",
    "heldout"
  )

  if (
    anyNA(
      split$sample_role
    ) ||
      any(
        !split$sample_role %in%
          allowed_roles
      )
  ) {
    stop(
      "Enterprise split contains invalid sample roles."
    )
  }

  expected_development <-
    floor(
      length(expected_ids) *
        development_share
    )

  observed_development <-
    sum(
      split$sample_role ==
        "development"
    )

  if (
    observed_development !=
      expected_development
  ) {
    stop(
      "Enterprise split has an unexpected development-sample size."
    )
  }

  invisible(TRUE)
}
