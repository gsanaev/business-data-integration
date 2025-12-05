# =====================================================================
# 05_visualize_results.R
# Visualization of Core Structural & Time-Series Indicators
# =====================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(janitor)

# Ensure output directory exists ---------------------------------------
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# ----------------------------------------------------------------------
# 1. Load data
# ----------------------------------------------------------------------

panel <- read_csv("data/processed/panel_data.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(month = as.Date(month))

indicators_sector <- read_csv("output/tables/indicators_sector.csv", show_col_types = FALSE) %>%
  clean_names()

# ----------------------------------------------------------------------
# 2. Plot: Firm size distribution (employees)
# ----------------------------------------------------------------------

p_firm_size <- panel %>%
  group_by(firm_id) %>%
  summarise(
    avg_employees = mean(employees_monthly, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = avg_employees)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Firm Size Distribution (Average Employees per Firm)",
    x = "Average Number of Employees",
    y = "Number of Firms"
  )

ggsave("output/figures/firm_size_distribution.png",
       p_firm_size, width = 7, height = 5)

# ----------------------------------------------------------------------
# 3. Plot: Average turnover per firm by sector
# ----------------------------------------------------------------------

latest_year <- max(indicators_sector$year, na.rm = TRUE)

p_turnover_sector <- indicators_sector %>%
  filter(year == latest_year) %>%
  ggplot(aes(x = nace_code, y = avg_turnover_per_firm)) +
  geom_col() +
  labs(
    title = paste("Average Turnover per Firm by Sector (", latest_year, ")", sep = ""),
    x = "NACE Code",
    y = "Average Turnover per Firm"
  ) +
  coord_flip()

ggsave("output/figures/avg_turnover_by_sector.png",
       p_turnover_sector, width = 7, height = 5)

# ----------------------------------------------------------------------
# 4. Plot: Monthly total turnover (overall)
# ----------------------------------------------------------------------

p_turnover_monthly <- panel %>%
  group_by(month) %>%
  summarise(
    total_turnover = sum(turnover_monthly, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = month, y = total_turnover)) +
  geom_line() +
  labs(
    title = "Monthly Total Turnover (All Firms)",
    x = "Month",
    y = "Total Turnover"
  )

ggsave("output/figures/monthly_turnover_total.png",
       p_turnover_monthly, width = 7, height = 5)

message("Figures written to 'output/figures/'.")
