# =====================================================================
# 04_integrate_sources.R
# Integration of Linked Enterprise Sources
# ---------------------------------------------------------------------
# Validated source records are integrated only after entity linkage.
# The operational analytical key is canonical_firm_id.
#
# Records that remain unresolved in linkage are not silently assigned
# to another enterprise.
#
# Output:
#   data/processed/panel_data.csv
# =====================================================================

library(dplyr)
library(readr)
library(lubridate)

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Load validated sources and linkage crosswalk
# ----------------------------------------------------------------------

firms <- read_csv(
  "data/clean/firms_clean.csv",
  show_col_types = FALSE
)

employment <- read_csv(
  "data/clean/employment_clean.csv",
  show_col_types = FALSE
)

turnover <- read_csv(
  "data/clean/turnover_clean.csv",
  show_col_types = FALSE
)

crosswalk <- read_csv(
  "data/processed/linkage_crosswalk.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------------------
# 2. Build source-to-canonical maps
# ----------------------------------------------------------------------

register_map <- crosswalk %>%
  filter(
    source == "register",
    !is.na(canonical_firm_id)
  ) %>%
  transmute(
    register_id = source_record_id,
    canonical_firm_id
  )

employment_map <- crosswalk %>%
  filter(
    source == "employment",
    !is.na(canonical_firm_id)
  ) %>%
  transmute(
    employment_source_id = source_record_id,
    canonical_firm_id
  )

turnover_map <- crosswalk %>%
  filter(
    source == "turnover",
    !is.na(canonical_firm_id)
  ) %>%
  transmute(
    turnover_source_id = source_record_id,
    canonical_firm_id
  )

# ----------------------------------------------------------------------
# 3. Attach canonical identifiers
# ----------------------------------------------------------------------

firms_linked <- firms %>%
  inner_join(
    register_map,
    by = "register_id"
  )

employment_linked <- employment %>%
  inner_join(
    employment_map,
    by = "employment_source_id"
  )

turnover_linked <- turnover %>%
  inner_join(
    turnover_map,
    by = "turnover_source_id"
  )

# ----------------------------------------------------------------------
# 4. Report linkage coverage entering integration
# ----------------------------------------------------------------------

common_firms <- Reduce(
  intersect,
  list(
    unique(firms_linked$canonical_firm_id),
    unique(employment_linked$canonical_firm_id),
    unique(turnover_linked$canonical_firm_id)
  )
)

message(
  "Canonical enterprises available in all three sources: ",
  length(common_firms)
)

message(
  "Employment enterprises linked: ",
  n_distinct(employment_linked$canonical_firm_id)
)

message(
  "Turnover enterprises linked: ",
  n_distinct(turnover_linked$canonical_firm_id)
)

# ----------------------------------------------------------------------
# 5. Prepare source-specific analytical columns
# ----------------------------------------------------------------------

identity_columns <- c(
  "business_id",
  "enterprise_name",
  "street",
  "postal_code",
  "city",
  "legal_form",
  "nace_code",
  "region_code"
)

employment_panel <- employment_linked %>%
  filter(
    canonical_firm_id %in% common_firms
  ) %>%
  rename(
    employees_monthly = employees
  ) %>%
  select(
    -any_of(identity_columns)
  )

turnover_panel <- turnover_linked %>%
  filter(
    canonical_firm_id %in% common_firms
  ) %>%
  rename(
    turnover_monthly = turnover
  ) %>%
  select(
    -any_of(identity_columns)
  )

firms_panel <- firms_linked %>%
  filter(
    canonical_firm_id %in% common_firms
  ) %>%
  rename(
    employees_firm = employees
  )

# ----------------------------------------------------------------------
# 6. Build unified monthly panel
# ----------------------------------------------------------------------

message("Integrating linked datasets...")

panel <- employment_panel %>%
  inner_join(
    turnover_panel,
    by = c(
      "canonical_firm_id",
      "month"
    )
  ) %>%
  left_join(
    firms_panel,
    by = "canonical_firm_id"
  ) %>%
  mutate(
    month = as.Date(month),
    employees_monthly = as.numeric(
      employees_monthly
    ),
    turnover_monthly = as.numeric(
      turnover_monthly
    )
  ) %>%
  arrange(
    canonical_firm_id,
    month
  )

# ----------------------------------------------------------------------
# 7. Derive current analytical indicators
# ----------------------------------------------------------------------

message("Constructing monthly analytical indicators...")

panel <- panel %>%
  group_by(canonical_firm_id) %>%
  arrange(
    month,
    .by_group = TRUE
  ) %>%
  mutate(
    turnover_yoy =
      (
        turnover_monthly -
          lag(turnover_monthly, 12)
      ) /
      lag(turnover_monthly, 12),

    emp_growth =
      (
        employees_monthly -
          lag(employees_monthly)
      ) /
      lag(employees_monthly),

    # This measure will be renamed more explicitly in the later
    # indicator-semantics phase.
    productivity =
      turnover_monthly /
      employees_monthly,

    month_num = month(month),

    seasonal_index =
      turnover_monthly /
      mean(
        turnover_monthly,
        na.rm = TRUE
      )
  ) %>%
  ungroup()

# ----------------------------------------------------------------------
# 8. Consistency checks
# ----------------------------------------------------------------------

implausible_prod <- panel %>%
  filter(
    productivity < 0 |
      productivity > 1e7
  )

if (nrow(implausible_prod) > 0) {
  warning(
    "Implausible turnover-per-employee values detected: ",
    nrow(implausible_prod)
  )
}

message("YoY turnover growth summary:")
print(
  summary(
    panel$turnover_yoy
  )
)

if (
  any(
    panel$employees_monthly <= 0,
    na.rm = TRUE
  )
) {
  warning(
    "Non-positive monthly employment values detected."
  )
}

# ----------------------------------------------------------------------
# 9. Write analysis-ready panel
# ----------------------------------------------------------------------

write_csv(
  panel,
  "data/processed/panel_data.csv"
)

message("Linked-source integration completed successfully.")
