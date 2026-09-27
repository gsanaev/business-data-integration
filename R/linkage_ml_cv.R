# =====================================================================
# Grouped cross-validation
# =====================================================================

validate_grouped_cv_configuration <- function(
  n_folds,
  seed
) {
  if (
    length(
      n_folds
    ) != 1L ||
      !is.numeric(
        n_folds
      ) ||
      !is.finite(
        n_folds
      ) ||
      n_folds < 2 ||
      n_folds !=
        as.integer(
          n_folds
        )
  ) {
    stop(
      "n_folds must be an integer of at least 2."
    )
  }

  if (
    length(
      seed
    ) != 1L ||
      !is.numeric(
        seed
      ) ||
      !is.finite(
        seed
      )
  ) {
    stop(
      "seed must be a finite numeric scalar."
    )
  }

  invisible(
    TRUE
  )
}


shuffle_group_ids_preserving_rng <- function(
  group_ids,
  seed
) {
  had_seed <-
    exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )

  if (
    had_seed
  ) {
    previous_seed <-
      get(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )
  }

  on.exit(
    {
      if (
        had_seed
      ) {
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
    as.integer(
      seed
    )
  )

  sample(
    group_ids,
    size =
      length(
        group_ids
      ),
    replace = FALSE
  )
}


create_grouped_cv_folds <- function(
  candidate_pairs,
  n_folds = 5L,
  seed = 202605L
) {
  validate_grouped_cv_configuration(
    n_folds,
    seed
  )

  truth_ids <-
    sort(
      unique(
        as.character(
          candidate_pairs$truth_firm_id
        )
      )
    )

  if (
    length(
      truth_ids
    ) <
      n_folds
  ) {
    stop(
      "Number of enterprise groups must be at least n_folds."
    )
  }

  shuffled_ids <-
    shuffle_group_ids_preserving_rng(
      truth_ids,
      seed
    )

  tibble::tibble(
    truth_firm_id =
      shuffled_ids,

    cv_fold =
      rep(
        seq_len(
          as.integer(
            n_folds
          )
        ),
        length.out =
          length(
            shuffled_ids
          )
      )
  ) %>%
    dplyr::arrange(
      .data$truth_firm_id
    )
}


attach_grouped_cv_folds <- function(
  candidate_pairs,
  fold_map
) {
  result <-
    candidate_pairs %>%
    dplyr::left_join(
      fold_map,
      by =
        "truth_firm_id"
    )

  if (
    anyNA(
      result$cv_fold
    )
  ) {
    stop(
      "CV fold assignment is missing for one or more candidate pairs."
    )
  }

  result
}


validate_grouped_cv_assignment <- function(
  candidate_pairs_cv,
  n_folds = 5L
) {
  fold_counts_per_enterprise <-
    candidate_pairs_cv %>%
    dplyr::group_by(
      .data$truth_firm_id
    ) %>%
    dplyr::summarise(
      n_folds_observed =
        dplyr::n_distinct(
          .data$cv_fold
        ),
      .groups = "drop"
    )

  if (
    any(
      fold_counts_per_enterprise$n_folds_observed !=
        1L
    )
  ) {
    stop(
      "At least one enterprise spans multiple CV folds."
    )
  }

  observed_folds <-
    sort(
      unique(
        candidate_pairs_cv$cv_fold
      )
    )

  expected_folds <-
    seq_len(
      as.integer(
        n_folds
      )
    )

  if (
    !identical(
      observed_folds,
      expected_folds
    )
  ) {
    stop(
      "Observed CV folds do not match the expected fold set."
    )
  }

  enterprise_counts <-
    candidate_pairs_cv %>%
    dplyr::distinct(
      .data$truth_firm_id,
      .data$cv_fold
    ) %>%
    dplyr::count(
      .data$cv_fold,
      name =
        "enterprises"
    )

  if (
    max(
      enterprise_counts$enterprises
    ) -
      min(
        enterprise_counts$enterprises
      ) >
      1L
  ) {
    stop(
      "Enterprise groups are not balanced across CV folds."
    )
  }

  invisible(
    TRUE
  )
}


summarise_ml_cv_folds <- function(
  candidate_pairs_cv
) {
  candidate_pairs_cv %>%
    dplyr::group_by(
      .data$cv_fold
    ) %>%
    dplyr::summarise(
      enterprises =
        dplyr::n_distinct(
          .data$truth_firm_id
        ),

      source_records =
        dplyr::n_distinct(
          paste(
            .data$scenario,
            .data$source,
            .data$source_record_id,
            sep = "::"
          )
        ),

      candidate_pairs =
        dplyr::n(),

      positive_pairs =
        sum(
          .data$is_true_candidate
        ),

      negative_pairs =
        sum(
          !.data$is_true_candidate
        ),

      positive_share =
        mean(
          .data$is_true_candidate
        ),

      .groups =
        "drop"
    )
}
