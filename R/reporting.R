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
