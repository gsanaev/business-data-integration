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

source("R/reporting.R")

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

validate_sector_plot_inputs(
  indicators_sector
)

# ----------------------------------------------------------------------
# 3. Common figure settings
# ----------------------------------------------------------------------

figure_width <- 8
figure_height <- 5.5
figure_dpi <- 160

base_theme <-
  build_reporting_theme()

# ----------------------------------------------------------------------
# 4. Monthly total turnover
# ----------------------------------------------------------------------

monthly_turnover <-
  summarise_monthly_turnover(
    panel
  )

p_monthly_turnover <-
  build_monthly_turnover_plot(
    monthly_turnover,
    base_theme
  )

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

p_annual_turnover_sector <-
  build_annual_turnover_sector_plot(
    indicators_sector,
    base_theme
  )

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

p_turnover_employee_sector <-
  build_turnover_employee_sector_plot(
    indicators_sector,
    base_theme
  )

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

coherence_plot_data <-
  prepare_coherence_plot_data(
    coherence_events
  )

p_coherence <-
  build_coherence_outcomes_plot(
    coherence_plot_data,
    base_theme
  )

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
