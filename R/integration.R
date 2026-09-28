# =====================================================================
# integration.R
# Linked-source integration functions
# ---------------------------------------------------------------------
# Functionalized from the v2 integration workflow during v3 refactoring.
# Statistical behavior is intentionally unchanged at this stage.
# =====================================================================

build_source_maps <- function(
  crosswalk
) {
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

  accounting_map <- crosswalk %>%
    filter(
      source == "accounting",
      !is.na(canonical_firm_id)
    ) %>%
    transmute(
      accounting_source_id = source_record_id,
      canonical_firm_id
    )

  list(
    register_map = register_map,
    employment_map = employment_map,
    turnover_map = turnover_map,
    accounting_map = accounting_map
  )
}


attach_canonical_identifiers <- function(
  firms,
  employment,
  turnover,
  accounting,
  source_maps
) {
  firms_linked <- firms %>%
    inner_join(
      source_maps$register_map,
      by = "register_id"
    )

  employment_linked <- employment %>%
    inner_join(
      source_maps$employment_map,
      by = "employment_source_id"
    )

  turnover_linked <- turnover %>%
    inner_join(
      source_maps$turnover_map,
      by = "turnover_source_id"
    )

  accounting_linked <- accounting %>%
    inner_join(
      source_maps$accounting_map,
      by = "accounting_source_id"
    )

  list(
    firms_linked = firms_linked,
    employment_linked = employment_linked,
    turnover_linked = turnover_linked,
    accounting_linked = accounting_linked
  )
}


get_common_canonical_firms <- function(
  firms_linked,
  employment_linked,
  turnover_linked
) {
  Reduce(
    intersect,
    list(
      unique(
        firms_linked$canonical_firm_id
      ),
      unique(
        employment_linked$canonical_firm_id
      ),
      unique(
        turnover_linked$canonical_firm_id
      )
    )
  )
}


prepare_accounting_annual <- function(
  accounting_linked
) {
  accounting_annual <- accounting_linked %>%
    transmute(
      canonical_firm_id,
      accounting_source_id,
      reference_year,
      nace_code_accounting =
        nace_code,

      operating_revenue_raw,
      operating_revenue_status,
      operating_revenue_rule_id,
      operating_revenue,

      purchases_goods_services_raw,
      purchases_status,
      purchases_rule_id,
      purchases_goods_services,

      personnel_expense_raw,
      personnel_expense_status,
      personnel_expense_rule_id,
      personnel_expense
    ) %>%
    arrange(
      canonical_firm_id,
      reference_year
    )

  if (
    anyDuplicated(
      accounting_annual[
        c(
          "canonical_firm_id",
          "reference_year"
        )
      ]
    )
  ) {
    stop(
      "Duplicate canonical enterprise-year keys detected in accounting data."
    )
  }

  accounting_annual
}


prepare_monthly_source_panels <- function(
  firms_linked,
  employment_linked,
  turnover_linked,
  common_firms
) {
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
      canonical_firm_id %in%
        common_firms
    ) %>%
    rename(
      employees_monthly = employees
    ) %>%
    select(
      -any_of(identity_columns)
    )

  turnover_panel <- turnover_linked %>%
    filter(
      canonical_firm_id %in%
        common_firms
    ) %>%
    rename(
      turnover_monthly = turnover
    ) %>%
    select(
      -any_of(identity_columns)
    )

  firms_panel <- firms_linked %>%
    filter(
      canonical_firm_id %in%
        common_firms
    ) %>%
    rename(
      employees_firm = employees
    )

  list(
    firms_panel = firms_panel,
    employment_panel = employment_panel,
    turnover_panel = turnover_panel
  )
}


build_monthly_panel <- function(
  employment_panel,
  turnover_panel,
  firms_panel
) {
  employment_panel %>%
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
      employees_monthly =
        as.numeric(
          employees_monthly
        ),
      turnover_monthly =
        as.numeric(
          turnover_monthly
        )
    ) %>%
    arrange(
      canonical_firm_id,
      month
    )
}


derive_panel_indicators <- function(
  panel
) {
  panel %>%
    group_by(canonical_firm_id) %>%
    arrange(
      month,
      .by_group = TRUE
    ) %>%
    mutate(
      turnover_yoy =
        (
          turnover_monthly -
            lag(
              turnover_monthly,
              12
            )
        ) /
          lag(
            turnover_monthly,
            12
          ),

      emp_growth =
        (
          employees_monthly -
            lag(employees_monthly)
        ) /
          lag(employees_monthly),

      month_num =
        month(month),

      seasonal_index =
        turnover_monthly /
          mean(
            turnover_monthly,
            na.rm = TRUE
          )
    ) %>%
    ungroup()
}


build_integration_results <- function(
  firms,
  employment,
  turnover,
  accounting,
  crosswalk
) {
  source_maps <-
    build_source_maps(
      crosswalk
    )

  linked_sources <-
    attach_canonical_identifiers(
      firms,
      employment,
      turnover,
      accounting,
      source_maps
    )

  common_firms <-
    get_common_canonical_firms(
      linked_sources$firms_linked,
      linked_sources$employment_linked,
      linked_sources$turnover_linked
    )

  accounting_annual <-
    prepare_accounting_annual(
      linked_sources$accounting_linked
    )

  source_panels <-
    prepare_monthly_source_panels(
      linked_sources$firms_linked,
      linked_sources$employment_linked,
      linked_sources$turnover_linked,
      common_firms
    )

  panel <-
    build_monthly_panel(
      source_panels$employment_panel,
      source_panels$turnover_panel,
      source_panels$firms_panel
    )

  panel <-
    derive_panel_indicators(
      panel
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

  list(
    panel =
      panel,
    accounting_annual =
      accounting_annual
  )
}
