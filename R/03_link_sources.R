# =====================================================================
# 03_link_sources.R
# Deterministic Enterprise Record Linkage
# ---------------------------------------------------------------------
# The register-style source defines the canonical analytical enterprise
# population. Other sources are linked to it using strong identifiers.
#
# Records without a usable strong identifier remain unmatched at this
# stage. Similarity-based and model-assisted linkage can later operate
# only on those unresolved cases.
#
# Important:
#   This script does not read data/truth/.
#
# Output:
#   data/processed/linkage_crosswalk.csv
# =====================================================================

library(dplyr)
library(readr)

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Load validated sources
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

# ----------------------------------------------------------------------
# 2. Validate the register reference identifier
# ----------------------------------------------------------------------

if (any(is.na(firms$business_id))) {
  stop(
    "Register reference source contains missing business_id values."
  )
}

duplicate_business_ids <- firms %>%
  count(business_id) %>%
  filter(n > 1)

if (nrow(duplicate_business_ids) > 0) {
  stop(
    "Register reference source contains duplicate business_id values: ",
    nrow(duplicate_business_ids)
  )
}

# ----------------------------------------------------------------------
# 3. Define canonical enterprises from the register source
# ----------------------------------------------------------------------

register_entities <- firms %>%
  arrange(register_id) %>%
  mutate(
    canonical_firm_id = sprintf(
      "C%06d",
      seq_len(n())
    )
  )

register_lookup <- register_entities %>%
  select(
    canonical_firm_id,
    register_id,
    business_id
  )

# ----------------------------------------------------------------------
# 4. Extract one identity record per monthly-source enterprise
# ----------------------------------------------------------------------

employment_entities <- employment %>%
  distinct(
    employment_source_id,
    business_id
  )

turnover_entities <- turnover %>%
  distinct(
    turnover_source_id,
    business_id
  )

# ----------------------------------------------------------------------
# 5. Deterministic employment linkage
# ----------------------------------------------------------------------

employment_links <- employment_entities %>%
  left_join(
    register_lookup,
    by = "business_id"
  ) %>%
  mutate(
    linkage_status = case_when(
      is.na(business_id) ~
        "unmatched_missing_identifier",

      !is.na(canonical_firm_id) ~
        "matched_deterministic",

      TRUE ~
        "unmatched_identifier_not_found"
    ),

    linkage_method = case_when(
      linkage_status == "matched_deterministic" ~
        "business_id_exact",

      TRUE ~
        NA_character_
    )
  )

# ----------------------------------------------------------------------
# 6. Deterministic turnover linkage
# ----------------------------------------------------------------------

turnover_links <- turnover_entities %>%
  left_join(
    register_lookup,
    by = "business_id"
  ) %>%
  mutate(
    linkage_status = case_when(
      is.na(business_id) ~
        "unmatched_missing_identifier",

      !is.na(canonical_firm_id) ~
        "matched_deterministic",

      TRUE ~
        "unmatched_identifier_not_found"
    ),

    linkage_method = case_when(
      linkage_status == "matched_deterministic" ~
        "business_id_exact",

      TRUE ~
        NA_character_
    )
  )

# ----------------------------------------------------------------------
# 7. Build unified linkage crosswalk
# ----------------------------------------------------------------------

register_links <- register_entities %>%
  transmute(
    source = "register",
    source_record_id = register_id,
    business_id,
    canonical_firm_id,
    register_id,
    linkage_status = "reference",
    linkage_method = "register_reference"
  )

employment_crosswalk <- employment_links %>%
  transmute(
    source = "employment",
    source_record_id = employment_source_id,
    business_id,
    canonical_firm_id,
    register_id,
    linkage_status,
    linkage_method
  )

turnover_crosswalk <- turnover_links %>%
  transmute(
    source = "turnover",
    source_record_id = turnover_source_id,
    business_id,
    canonical_firm_id,
    register_id,
    linkage_status,
    linkage_method
  )

linkage_crosswalk <- bind_rows(
  register_links,
  employment_crosswalk,
  turnover_crosswalk
)

# ----------------------------------------------------------------------
# 8. Report linkage results
# ----------------------------------------------------------------------

message("Employment deterministic linkage:")
print(
  employment_crosswalk %>%
    count(linkage_status)
)

message("Turnover deterministic linkage:")
print(
  turnover_crosswalk %>%
    count(linkage_status)
)

# ----------------------------------------------------------------------
# 9. Write crosswalk
# ----------------------------------------------------------------------

write_csv(
  linkage_crosswalk,
  "data/processed/linkage_crosswalk.csv"
)

message("Deterministic linkage completed successfully.")
