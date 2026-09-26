# =====================================================================
# reporting.R
# Reporting and display helper functions
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Function behavior is intentionally unchanged at this stage.
# =====================================================================

format_millions <- function(x) {
  paste0(
    format(
      round(
        x / 1e6,
        1
      ),
      trim = TRUE,
      scientific = FALSE
    ),
    " M"
  )
}

format_thousands <- function(x) {
  paste0(
    format(
      round(
        x / 1e3,
        0
      ),
      big.mark = ",",
      trim = TRUE,
      scientific = FALSE
    ),
    "k"
  )
}

validate_sector_plot_inputs <- function(
  indicators_sector
) {
  required_sector_columns <- c(
    "year",
    "nace_code",
    "total_turnover",
    "turnover_per_employee"
  )

  missing_sector_columns <-
    setdiff(
      required_sector_columns,
      names(indicators_sector)
    )

  if (
    length(missing_sector_columns) > 0L
  ) {
    stop(
      "Missing required sector indicator columns: ",
      paste(
        missing_sector_columns,
        collapse = ", "
      )
    )
  }

  if (
    any(
      indicators_sector$total_turnover <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive annual sector turnover detected."
    )
  }

  if (
    any(
      indicators_sector$turnover_per_employee <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-positive sector turnover-per-employee detected."
    )
  }

  invisible(TRUE)
}


build_reporting_theme <- function(
  base_size = 11
) {
  theme_minimal(
    base_size = base_size
  ) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        face = "bold"
      ),
      plot.subtitle = element_text(
        margin = margin(
          b = 8
        )
      ),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}


summarise_monthly_turnover <- function(
  panel
) {
  panel %>%
    group_by(
      month
    ) %>%
    summarise(
      usable_enterprises =
        n_distinct(
          canonical_firm_id[
            !is.na(
              turnover_monthly
            )
          ]
        ),

      total_turnover =
        sum(
          turnover_monthly,
          na.rm = TRUE
        ),

      .groups = "drop"
    )
}


build_monthly_turnover_plot <- function(
  monthly_turnover,
  base_theme
) {
  ggplot(
    monthly_turnover,
    aes(
      x = month,
      y = total_turnover
    )
  ) +
    geom_line(
      linewidth = 0.8
    ) +
    labs(
      title =
        "Monthly Total Turnover",

      subtitle =
        "Integrated enterprise observations, 2023–2025",

      x =
        "Month",

      y =
        "Total turnover"
    ) +
    scale_y_continuous(
      labels =
        format_millions
    ) +
    base_theme
}


build_annual_turnover_sector_plot <- function(
  indicators_sector,
  base_theme
) {
  indicators_sector %>%
    ggplot(
      aes(
        x = year,
        y = total_turnover,
        group = nace_code,
        linetype = nace_code
      )
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_point(
      size = 2
    ) +
    scale_x_continuous(
      breaks =
        sort(
          unique(
            indicators_sector$year
          )
        )
    ) +
    scale_y_continuous(
      labels =
        format_millions
    ) +
    labs(
      title =
        "Annual Turnover by Sector",

      subtitle =
        "Annual totals use enterprise-years with complete monthly turnover coverage",

      x =
        "Year",

      y =
        "Total turnover",

      linetype =
        "NACE code"
    ) +
    base_theme
}


build_turnover_employee_sector_plot <- function(
  indicators_sector,
  base_theme
) {
  indicators_sector %>%
    ggplot(
      aes(
        x = year,
        y = turnover_per_employee,
        group = nace_code,
        linetype = nace_code
      )
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    geom_point(
      size = 2
    ) +
    scale_x_continuous(
      breaks =
        sort(
          unique(
            indicators_sector$year
          )
        )
    ) +
    scale_y_continuous(
      labels =
        format_thousands
    ) +
    labs(
      title =
        "Annual Turnover per Employee by Sector",

      subtitle =
        paste(
          "Ratio uses the common population with complete",
          "turnover and employment coverage"
        ),

      x =
        "Year",

      y =
        "Turnover per employee",

      linetype =
        "NACE code"
    ) +
    base_theme
}


prepare_coherence_plot_data <- function(
  coherence_events
) {
  coherence_plot_data <-
    coherence_events %>%
    mutate(
      outcome = case_when(
        applicability_status !=
          "applicable" ~
          "Not assessed",

        coherence_status ==
          "large_difference" ~
          "Large difference",

        coherence_status ==
          "within_expected_range" ~
          "Within expected range",

        TRUE ~
          "Other"
      )
    ) %>%
    count(
      rule_id,
      outcome,
      name = "events"
    ) %>%
    group_by(
      rule_id
    ) %>%
    mutate(
      share =
        events /
        sum(events)
    ) %>%
    ungroup()

  coherence_plot_data$outcome <-
    factor(
      coherence_plot_data$outcome,
      levels = c(
        "Within expected range",
        "Large difference",
        "Not assessed",
        "Other"
      )
    )

  coherence_plot_data
}


build_coherence_outcomes_plot <- function(
  coherence_plot_data,
  base_theme
) {
  ggplot(
    coherence_plot_data,
    aes(
      x = rule_id,
      y = share,
      fill = outcome
    )
  ) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(
      labels = function(x) {
        paste0(
          round(
            100 * x
          ),
          "%"
        )
      },
      limits = c(
        0,
        1
      )
    ) +
    labs(
      title =
        "Cross-Source Coherence Outcomes",

      subtitle =
        "Outcome shares by semantic coherence rule",

      x =
        "Coherence rule",

      y =
        "Share of events",

      fill =
        "Outcome"
    ) +
    base_theme
}


# =====================================================================
# Statistical-process quality evidence
# =====================================================================

build_quality_evidence_registry <- function() {
  tibble::tribble(
    ~evidence_id,
    ~dimension,
    ~measure,
    ~scope,
    ~reporting_level,

    "candidate_recall",
    "accuracy",
    "Candidate recall",
    "Candidate generation",
    "Overall / scenario / source",

    "top1_accuracy",
    "accuracy",
    "Top-1 candidate accuracy",
    "Candidate ranking",
    "Overall / scenario / source",

    "auto_precision",
    "accuracy",
    "Automatic-link precision",
    "Decision policy",
    "Overall / scenario / source",

    "false_auto_links",
    "accuracy",
    "False automatic links",
    "Decision policy",
    "Overall / scenario / source",

    "automation_rate",
    "operational_efficiency",
    "Automation rate",
    "Decision policy",
    "Overall / scenario / source",

    "review_rate",
    "operational_efficiency",
    "Review rate",
    "Decision policy",
    "Overall / scenario / source",

    "unmatched_rate",
    "operational_efficiency",
    "Unmatched rate",
    "Decision policy",
    "Overall / scenario / source",

    "scenario_robustness",
    "robustness",
    "Performance across identity scenarios",
    "Linkage workflow",
    "Baseline / moderate / difficult",

    "source_robustness",
    "robustness",
    "Performance across source types",
    "Linkage workflow",
    "Employment / turnover / accounting",

    "enterprise_count_error",
    "statistical_impact",
    "Enterprise-count error",
    "Downstream indicators",
    "Absolute and relative error",

    "turnover_error",
    "statistical_impact",
    "Turnover error",
    "Downstream indicators",
    "Absolute and relative error",

    "employment_error",
    "statistical_impact",
    "Employment error",
    "Downstream indicators",
    "Absolute and relative error",

    "turnover_per_employee_error",
    "statistical_impact",
    "Turnover-per-employee error",
    "Downstream indicators",
    "Absolute and relative error",

    "traceability",
    "traceability",
    "Linkage specification and thresholds",
    "Process metadata",
    "Frozen specification",

    "reproducibility",
    "reproducibility",
    "Seeds, split, CV and software workflow",
    "Process metadata",
    "Frozen specification"
  )
}


build_linkage_process_metadata <- function(
  rf_spec,
  rf_policy,
  similarity_policy,
  feature_columns,
  development_share = 0.70,
  n_cv_folds = 5L,
  split_seed = 202604L,
  cv_seed = 202605L,
  rf_tuning_seed = 202606L,
  rf_final_seed = 202607L
) {
  if (
    length(feature_columns) == 0L ||
      anyNA(feature_columns) ||
      any(!nzchar(feature_columns))
  ) {
    stop(
      "feature_columns must contain non-missing feature names."
    )
  }

  if (
    nrow(rf_spec) != 1L ||
      nrow(rf_policy) != 1L ||
      nrow(similarity_policy) != 1L
  ) {
    stop(
      "Frozen model and policy inputs must each contain exactly one row."
    )
  }

  tibble::tribble(
    ~metadata_key,
    ~metadata_value,

    "linkage_design",
    "Level 1 trusted identifier; Level 2 candidate-based linkage",

    "candidate_generation",
    "Transparent blocking using postcode OR NACE",

    "linkage_features",
    paste(
      feature_columns,
      collapse = ", "
    ),

    "development_share",
    as.character(
      development_share
    ),

    "heldout_share",
    as.character(
      1 - development_share
    ),

    "grouped_cv_folds",
    as.character(
      n_cv_folds
    ),

    "enterprise_split_seed",
    as.character(
      split_seed
    ),

    "cv_fold_seed",
    as.character(
      cv_seed
    ),

    "rf_tuning_seed",
    as.character(
      rf_tuning_seed
    ),

    "rf_final_model_seed",
    as.character(
      rf_final_seed
    ),

    "rf_engine",
    "ranger",

    "rf_num_trees",
    as.character(
      rf_spec$num_trees[[1]]
    ),

    "rf_mtry",
    as.character(
      rf_spec$mtry[[1]]
    ),

    "rf_min_node_size",
    as.character(
      rf_spec$min_node_size[[1]]
    ),

    "rf_class_weighting",
    "nonmatch = 1; match = negative / positive training pairs",

    "rf_score_threshold",
    as.character(
      rf_policy$probability_threshold[[1]]
    ),

    "rf_margin_threshold",
    as.character(
      rf_policy$margin_threshold[[1]]
    ),

    "rf_precision_target",
    as.character(
      rf_policy$precision_target[[1]]
    ),

    "similarity_score_threshold",
    as.character(
      similarity_policy$score_threshold[[1]]
    ),

    "similarity_margin_threshold",
    as.character(
      similarity_policy$margin_threshold[[1]]
    ),

    "similarity_precision_target",
    as.character(
      similarity_policy$precision_target[[1]]
    ),

    "training_scope",
    "Development enterprises only",

    "heldout_role",
    "Reserved for final evaluation; not used for model or policy selection"
  )
}
