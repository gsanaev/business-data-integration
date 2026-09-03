# =====================================================================
# evaluate_methods.R
# Evaluation of Linkage and Imputation Against Synthetic Hidden Truth
# ---------------------------------------------------------------------
# This script is intentionally separate from the operational 01–07
# workflow. It reads synthetic truth only after operational processing
# has been completed.
#
# Evaluation components:
#   1. record-linkage assignment coverage and accuracy,
#   2. imputation error for variables that are actually imputed.
#
# Only aggregate evaluation metrics are written to tracked outputs.
#
# Output:
#   output/tables/linkage_evaluation.csv
#   output/tables/imputation_evaluation.csv
# =====================================================================

library(dplyr)
library(readr)

dir.create(
  "output/tables",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Required inputs
# ----------------------------------------------------------------------

required_paths <- c(
  "data/truth/linkage_truth.csv",
  "data/truth/value_truth.csv",
  "data/processed/linkage_crosswalk.csv",
  "data/clean/firms_clean.csv",
  "data/clean/employment_clean.csv",
  "data/clean/turnover_clean.csv"
)

missing_paths <- required_paths[
  !file.exists(
    required_paths
  )
]

if (
  length(missing_paths) > 0L
) {
  stop(
    "Required evaluation inputs are missing: ",
    paste(
      missing_paths,
      collapse = ", "
    )
  )
}

# ----------------------------------------------------------------------
# 2. Load hidden truth and operational outputs
# ----------------------------------------------------------------------

linkage_truth <- read_csv(
  "data/truth/linkage_truth.csv",
  show_col_types = FALSE
) %>%
  mutate(
    source =
      as.character(source),

    source_record_id =
      as.character(source_record_id),

    truth_firm_id =
      as.character(truth_firm_id)
  )

value_truth <- read_csv(
  "data/truth/value_truth.csv",
  show_col_types = FALSE
) %>%
  mutate(
    source =
      as.character(source),

    source_record_id =
      as.character(source_record_id),

    reference_period =
      as.character(reference_period),

    variable =
      as.character(variable)
  )

crosswalk <- read_csv(
  "data/processed/linkage_crosswalk.csv",
  show_col_types = FALSE
) %>%
  mutate(
    source =
      as.character(source),

    source_record_id =
      as.character(source_record_id),

    canonical_firm_id =
      as.character(canonical_firm_id),

    linkage_method =
      as.character(linkage_method)
  )

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

# ----------------------------------------------------------------------
# 3. Validate truth keys
# ----------------------------------------------------------------------

linkage_truth_key <- paste(
  linkage_truth$source,
  linkage_truth$source_record_id,
  sep = "::"
)

if (
  anyDuplicated(
    linkage_truth_key
  )
) {
  stop(
    "Duplicate source-record keys detected in linkage truth."
  )
}

value_truth_key <- paste(
  value_truth$source,
  value_truth$source_record_id,
  value_truth$reference_period,
  value_truth$variable,
  sep = "::"
)

if (
  anyDuplicated(
    value_truth_key
  )
) {
  stop(
    "Duplicate source-period-variable keys detected in value truth."
  )
}

crosswalk_key <- paste(
  crosswalk$source,
  crosswalk$source_record_id,
  sep = "::"
)

if (
  anyDuplicated(
    crosswalk_key
  )
) {
  stop(
    "Duplicate source-record keys detected in linkage crosswalk."
  )
}

# ======================================================================
# PART A. LINKAGE EVALUATION
# ======================================================================

# ----------------------------------------------------------------------
# 4. Map canonical enterprises to hidden truth
# ----------------------------------------------------------------------

register_crosswalk <- crosswalk %>%
  filter(
    source ==
      "register"
  ) %>%
  transmute(
    canonical_firm_id,

    register_source_record_id =
      source_record_id
  )

register_truth <- linkage_truth %>%
  filter(
    source ==
      "register"
  ) %>%
  transmute(
    register_source_record_id =
      source_record_id,

    assigned_truth_firm_id =
      truth_firm_id
  )

canonical_truth <- register_crosswalk %>%
  left_join(
    register_truth,
    by =
      "register_source_record_id"
  )

if (
  any(
    is.na(
      canonical_truth$assigned_truth_firm_id
    )
  )
) {
  stop(
    "Canonical register enterprises could not all be mapped to hidden truth."
  )
}

if (
  anyDuplicated(
    canonical_truth$canonical_firm_id
  )
) {
  stop(
    "Canonical enterprise identifiers are duplicated in truth mapping."
  )
}

# ----------------------------------------------------------------------
# 5. Compare operational linkage with known source identities
# ----------------------------------------------------------------------

source_truth <- linkage_truth %>%
  filter(
    source !=
      "register"
  ) %>%
  transmute(
    source,
    source_record_id,

    true_truth_firm_id =
      truth_firm_id
  )

evaluated_links <- crosswalk %>%
  filter(
    source !=
      "register"
  ) %>%
  left_join(
    source_truth,
    by = c(
      "source",
      "source_record_id"
    )
  ) %>%
  left_join(
    canonical_truth %>%
      select(
        canonical_firm_id,
        assigned_truth_firm_id
      ),
    by =
      "canonical_firm_id"
  ) %>%
  mutate(
    linkage_method =
      coalesce(
        linkage_method,
        "none"
      ),

    assigned =
      !is.na(
        canonical_firm_id
      ) &
      canonical_firm_id !=
        "",

    correct =
      assigned &
      !is.na(
        assigned_truth_firm_id
      ) &
      true_truth_firm_id ==
        assigned_truth_firm_id
  )

if (
  any(
    is.na(
      evaluated_links$true_truth_firm_id
    )
  )
) {
  stop(
    "Some operational source records are missing from linkage truth."
  )
}

if (
  any(
    evaluated_links$assigned &
      is.na(
        evaluated_links$assigned_truth_firm_id
      )
  )
) {
  stop(
    "Some assigned canonical enterprises are missing a truth mapping."
  )
}

# ----------------------------------------------------------------------
# 6. Summarise linkage performance
# ----------------------------------------------------------------------

summarise_linkage <- function(x) {
  n_records <-
    nrow(x)

  n_assigned <-
    sum(
      x$assigned
    )

  n_correct <-
    sum(
      x$correct
    )

  tibble(
    n_records =
      n_records,

    n_assigned =
      n_assigned,

    assignment_rate =
      n_assigned /
        n_records,

    n_correct =
      n_correct,

    accuracy_assigned =
      ifelse(
        n_assigned > 0,
        n_correct /
          n_assigned,
        NA_real_
      ),

    correct_assignment_rate =
      n_correct /
        n_records
  )
}

linkage_overall <- summarise_linkage(
  evaluated_links
) %>%
  mutate(
    evaluation_scope =
      "overall",

    source =
      "all",

    linkage_method =
      "all",
    .before = 1
  )

linkage_by_source <- evaluated_links %>%
  group_by(
    source
  ) %>%
  group_modify(
    ~ summarise_linkage(
      .x
    )
  ) %>%
  ungroup() %>%
  mutate(
    evaluation_scope =
      "source",

    linkage_method =
      "all",
    .before = 1
  )

linkage_by_method <- evaluated_links %>%
  group_by(
    source,
    linkage_method
  ) %>%
  group_modify(
    ~ summarise_linkage(
      .x
    )
  ) %>%
  ungroup() %>%
  mutate(
    evaluation_scope =
      "source_method",
    .before = 1
  )

linkage_evaluation <- bind_rows(
  linkage_overall,
  linkage_by_source,
  linkage_by_method
) %>%
  select(
    evaluation_scope,
    source,
    linkage_method,
    n_records,
    n_assigned,
    assignment_rate,
    n_correct,
    accuracy_assigned,
    correct_assignment_rate
  )

# ======================================================================
# PART B. IMPUTATION EVALUATION
# ======================================================================

# ----------------------------------------------------------------------
# 7. Prepare genuinely imputed operational observations
# ----------------------------------------------------------------------

register_imputations <- firms %>%
  filter(
    employees_register_imputed
  ) %>%
  transmute(
    source =
      "register",

    source_record_id =
      as.character(
        register_id
      ),

    reference_period =
      as.character(
        register_reference_year
      ),

    variable =
      "employees",

    estimate =
      employees
  )

employment_imputations <- employment %>%
  filter(
    employment_imputed
  ) %>%
  transmute(
    source =
      "employment",

    source_record_id =
      as.character(
        employment_source_id
      ),

    reference_period =
      format(
        as.Date(month),
        "%Y-%m"
      ),

    variable =
      "employees",

    estimate =
      employees
  )

turnover_imputations <- turnover %>%
  filter(
    turnover_imputed
  ) %>%
  transmute(
    source =
      "turnover",

    source_record_id =
      as.character(
        turnover_source_id
      ),

    reference_period =
      format(
        as.Date(month),
        "%Y-%m"
      ),

    variable =
      "turnover",

    estimate =
      turnover
  )

imputed_values <- bind_rows(
  register_imputations,
  employment_imputations,
  turnover_imputations
)

if (
  nrow(
    imputed_values
  ) == 0L
) {
  stop(
    "No imputed observations are available for evaluation."
  )
}

# ----------------------------------------------------------------------
# 8. Attach hidden true values
# ----------------------------------------------------------------------

evaluated_imputations <- imputed_values %>%
  left_join(
    value_truth %>%
      select(
        source,
        source_record_id,
        reference_period,
        variable,
        truth_value
      ),
    by = c(
      "source",
      "source_record_id",
      "reference_period",
      "variable"
    )
  )

if (
  any(
    is.na(
      evaluated_imputations$truth_value
    )
  )
) {
  stop(
    "Hidden truth is unavailable for one or more imputed observations."
  )
}

if (
  any(
    !is.finite(
      evaluated_imputations$estimate
    )
  )
) {
  stop(
    "Non-finite imputed estimates detected."
  )
}

if (
  any(
    !is.finite(
      evaluated_imputations$truth_value
    )
  )
) {
  stop(
    "Non-finite truth values detected for evaluated imputations."
  )
}

# ----------------------------------------------------------------------
# 9. Calculate imputation error
# ----------------------------------------------------------------------

evaluated_imputations <- evaluated_imputations %>%
  mutate(
    error =
      estimate -
        truth_value,

    absolute_error =
      abs(
        error
      ),

    absolute_percentage_error =
      if_else(
        truth_value !=
          0,

        100 *
          absolute_error /
          abs(
            truth_value
          ),

        NA_real_
      )
  )

imputation_evaluation <- evaluated_imputations %>%
  group_by(
    source,
    variable
  ) %>%
  summarise(
    n_imputed =
      n(),

    mean_truth_value =
      mean(
        truth_value
      ),

    mean_imputed_value =
      mean(
        estimate
      ),

    mean_error =
      mean(
        error
      ),

    mean_absolute_error =
      mean(
        absolute_error
      ),

    median_absolute_error =
      median(
        absolute_error
      ),

    root_mean_squared_error =
      sqrt(
        mean(
          error^2
        )
      ),

    n_nonzero_truth =
      sum(
        truth_value !=
          0
      ),

    mean_absolute_percentage_error =
      ifelse(
        any(
          truth_value !=
            0
        ),

        mean(
          absolute_percentage_error,
          na.rm = TRUE
        ),

        NA_real_
      ),

    median_absolute_percentage_error =
      ifelse(
        any(
          truth_value !=
            0
        ),

        median(
          absolute_percentage_error,
          na.rm = TRUE
        ),

        NA_real_
      ),

    .groups =
      "drop"
  )

# ----------------------------------------------------------------------
# 10. Write aggregate evaluation outputs
# ----------------------------------------------------------------------

write_csv(
  linkage_evaluation,
  "output/tables/linkage_evaluation.csv"
)

write_csv(
  imputation_evaluation,
  "output/tables/imputation_evaluation.csv"
)

message(
  "Linkage evaluation:"
)

print(
  linkage_evaluation,
  n = Inf
)

message(
  "Imputation evaluation:"
)

print(
  imputation_evaluation,
  n = Inf,
  width = Inf
)

message(
  "Synthetic-truth method evaluation completed successfully."
)
