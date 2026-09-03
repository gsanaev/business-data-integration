# =====================================================================
# 07_visualize_results.R
# Visualization of Integrated Enterprise Statistics
# ---------------------------------------------------------------------
# Final figures summarize:
#   1. monthly turnover development,
#   2. annual sector turnover,
#   3. annual turnover per employee,
#   4. cross-source coherence outcomes.
#
# Annual figures use the statistically defined indicators produced by
# 06_compute_indicators.R. Cross-source quality results are taken from
# 05_check_coherence.R.
#
# Output:
#   output/figures/monthly_turnover_total.png
#   output/figures/annual_turnover_by_sector.png
#   output/figures/turnover_per_employee_by_sector.png
#   output/figures/coherence_outcomes.png
# =====================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)

dir.create(
  "output/figures",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Load analytical outputs
# ----------------------------------------------------------------------

panel <- read_csv(
  "data/processed/panel_data.csv",
  show_col_types = FALSE
) %>%
  mutate(
    month = as.Date(month)
  )

indicators_sector <- read_csv(
  "output/tables/indicators_sector.csv",
  show_col_types = FALSE
)

coherence_events <- read_csv(
  "data/processed/coherence_events.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------------------
# 2. Validate required plotting inputs
# ----------------------------------------------------------------------

required_sector_columns <- c(
  "year",
  "nace_code",
  "total_turnover",
  "turnover_per_employee"
)

missing_sector_columns <- setdiff(
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

# ----------------------------------------------------------------------
# 3. Common figure settings
# ----------------------------------------------------------------------

figure_width <- 8
figure_height <- 5.5
figure_dpi <- 160

base_theme <- theme_minimal(
  base_size = 11
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

# ----------------------------------------------------------------------
# 4. Monthly total turnover
# ----------------------------------------------------------------------

monthly_turnover <- panel %>%
  group_by(month) %>%
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

p_monthly_turnover <- ggplot(
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

ggsave(
  filename =
    "output/figures/monthly_turnover_total.png",

  plot =
    p_monthly_turnover,

  width =
    figure_width,

  height =
    figure_height,

  dpi =
    figure_dpi
)

# ----------------------------------------------------------------------
# 5. Annual turnover by sector
# ----------------------------------------------------------------------

p_annual_turnover_sector <- indicators_sector %>%
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

ggsave(
  filename =
    "output/figures/annual_turnover_by_sector.png",

  plot =
    p_annual_turnover_sector,

  width =
    figure_width,

  height =
    figure_height,

  dpi =
    figure_dpi
)

# ----------------------------------------------------------------------
# 6. Annual turnover per employee by sector
# ----------------------------------------------------------------------

p_turnover_employee_sector <- indicators_sector %>%
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

ggsave(
  filename =
    "output/figures/turnover_per_employee_by_sector.png",

  plot =
    p_turnover_employee_sector,

  width =
    figure_width,

  height =
    figure_height,

  dpi =
    figure_dpi
)

# ----------------------------------------------------------------------
# 7. Cross-source coherence outcomes
# ----------------------------------------------------------------------

coherence_plot_data <- coherence_events %>%
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
  group_by(rule_id) %>%
  mutate(
    share =
      events /
        sum(events)
  ) %>%
  ungroup()

coherence_plot_data$outcome <- factor(
  coherence_plot_data$outcome,
  levels = c(
    "Within expected range",
    "Large difference",
    "Not assessed",
    "Other"
  )
)

p_coherence <- ggplot(
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

ggsave(
  filename =
    "output/figures/coherence_outcomes.png",

  plot =
    p_coherence,

  width =
    figure_width,

  height =
    figure_height,

  dpi =
    figure_dpi
)

# ----------------------------------------------------------------------
# 8. Report generated figures
# ----------------------------------------------------------------------

figure_files <- c(
  "output/figures/monthly_turnover_total.png",
  "output/figures/annual_turnover_by_sector.png",
  "output/figures/turnover_per_employee_by_sector.png",
  "output/figures/coherence_outcomes.png"
)

missing_figures <- figure_files[
  !file.exists(
    figure_files
  )
]

if (
  length(missing_figures) > 0L
) {
  stop(
    "Expected figure files were not created: ",
    paste(
      missing_figures,
      collapse = ", "
    )
  )
}

message(
  "Final figures written to 'output/figures/'."
)
