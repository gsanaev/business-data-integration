# =====================================================================
# 03_link_sources.R
# Enterprise Record Linkage
# ---------------------------------------------------------------------
# The register-style source defines the canonical analytical enterprise
# population.
#
# Linkage follows a transparent hierarchy:
#   1. exact matching on a strong business identifier,
#   2. similarity-based ranking for unresolved source entities,
#   3. review or unmatched status where evidence is insufficient.
#
# Important:
#   This operational script does not read data/truth/.
#
# Output:
#   data/processed/linkage_crosswalk.csv
#   data/processed/linkage_candidates.csv
# =====================================================================

library(dplyr)
library(readr)

source(
  "R/helpers/linkage_similarity.R"
)

source("R/config.R")

project_config <- load_project_config()

baseline_similarity_config <-
  project_config$processing$linkage$baseline_similarity

similarity_weights <-
  unlist(
    baseline_similarity_config$weights,
    use.names = TRUE
  )

dir.create(
  "data/processed",
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------------------
# 1. Linkage decision thresholds
# ----------------------------------------------------------------------

similarity_score_threshold <-
  baseline_similarity_config$score_threshold

similarity_margin_threshold <-
  baseline_similarity_config$margin_threshold

# ----------------------------------------------------------------------
# 2. Load validated sources
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

accounting <- read_csv(
  "data/clean/accounting_clean.csv",
  show_col_types = FALSE
)

# ----------------------------------------------------------------------
# 3. Validate the register reference identifier
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
# 4. Define canonical enterprises from the register source
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
# 5. Extract one identity record per source enterprise
# ----------------------------------------------------------------------

employment_entities <- employment %>%
  distinct(
    employment_source_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    legal_form,
    nace_code
  )

turnover_entities <- turnover %>%
  distinct(
    turnover_source_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    legal_form,
    nace_code
  )

accounting_entities <- accounting %>%
  distinct(
    accounting_source_id,
    business_id,
    enterprise_name,
    street,
    postal_code,
    city,
    legal_form,
    nace_code
  )

# ----------------------------------------------------------------------
# 6. Similarity-based linkage modules
# ----------------------------------------------------------------------

source("R/linkage_candidates.R")
source("R/linkage_features.R")
source("R/linkage_similarity.R")
source("R/linkage_decision.R")

# ----------------------------------------------------------------------
# 7. Link employment enterprises
# ----------------------------------------------------------------------

employment_linkage <-
  link_source_entities(
    source_entities =
      employment_entities,

    source_id_column =
      "employment_source_id",

    register_lookup =
      register_lookup,

    register_entities =
      register_entities,

    similarity_weights =
      similarity_weights,

    score_threshold =
      similarity_score_threshold,

    margin_threshold =
      similarity_margin_threshold
  )

employment_links <-
  employment_linkage$links

employment_similarity <-
  employment_linkage$similarity

# ----------------------------------------------------------------------
# 8. Link turnover enterprises
# ----------------------------------------------------------------------

turnover_linkage <-
  link_source_entities(
    source_entities =
      turnover_entities,

    source_id_column =
      "turnover_source_id",

    register_lookup =
      register_lookup,

    register_entities =
      register_entities,

    similarity_weights =
      similarity_weights,

    score_threshold =
      similarity_score_threshold,

    margin_threshold =
      similarity_margin_threshold
  )

turnover_links <-
  turnover_linkage$links

turnover_similarity <-
  turnover_linkage$similarity

# ----------------------------------------------------------------------
# 9. Link accounting enterprises
# ----------------------------------------------------------------------

accounting_linkage <-
  link_source_entities(
    source_entities =
      accounting_entities,

    source_id_column =
      "accounting_source_id",

    register_lookup =
      register_lookup,

    register_entities =
      register_entities,

    similarity_weights =
      similarity_weights,

    score_threshold =
      similarity_score_threshold,

    margin_threshold =
      similarity_margin_threshold
  )

accounting_links <-
  accounting_linkage$links

accounting_similarity <-
  accounting_linkage$similarity

# ----------------------------------------------------------------------
# 10. Prepare accounting linkage crosswalk
# ----------------------------------------------------------------------

accounting_crosswalk <- accounting_links %>%
  transmute(
    source = "accounting",
    source_record_id =
      accounting_source_id,
    business_id,
    canonical_firm_id,
    register_id,
    candidate_register_id,
    linkage_status,
    linkage_method,
    top_similarity_score,
    second_similarity_score,
    similarity_margin
  )

# ----------------------------------------------------------------------
# 11. Build unified linkage crosswalk
# ----------------------------------------------------------------------

register_links <- register_entities %>%
  transmute(
    source = "register",
    source_record_id = register_id,
    business_id,
    canonical_firm_id,
    register_id,
    candidate_register_id =
      NA_character_,
    linkage_status = "reference",
    linkage_method = "register_reference",
    top_similarity_score =
      NA_real_,
    second_similarity_score =
      NA_real_,
    similarity_margin =
      NA_real_
  )

employment_crosswalk <- employment_links %>%
  transmute(
    source = "employment",
    source_record_id =
      employment_source_id,
    business_id,
    canonical_firm_id,
    register_id,
    candidate_register_id,
    linkage_status,
    linkage_method,
    top_similarity_score,
    second_similarity_score,
    similarity_margin
  )

turnover_crosswalk <- turnover_links %>%
  transmute(
    source = "turnover",
    source_record_id =
      turnover_source_id,
    business_id,
    canonical_firm_id,
    register_id,
    candidate_register_id,
    linkage_status,
    linkage_method,
    top_similarity_score,
    second_similarity_score,
    similarity_margin
  )

linkage_crosswalk <- bind_rows(
  register_links,
  employment_crosswalk,
  turnover_crosswalk,
  accounting_crosswalk
)

# ----------------------------------------------------------------------
# 12. Preserve candidate-level evidence
# ----------------------------------------------------------------------

employment_candidates <-
  employment_similarity$candidates %>%
  mutate(
    source = "employment"
  )

turnover_candidates <-
  turnover_similarity$candidates %>%
  mutate(
    source = "turnover"
  )

accounting_candidates <-
  accounting_similarity$candidates %>%
  mutate(
    source = "accounting"
  )

linkage_candidates <- bind_rows(
  employment_candidates,
  turnover_candidates,
  accounting_candidates
) %>%
  select(
    source,
    everything()
  )

# ----------------------------------------------------------------------
# 13. Report linkage results
# ----------------------------------------------------------------------

message("Employment linkage:")
print(
  employment_crosswalk %>%
    count(linkage_status)
)

message("Turnover linkage:")
print(
  turnover_crosswalk %>%
    count(linkage_status)
)

message("Accounting linkage:")
print(
  accounting_crosswalk %>%
    count(linkage_status)
)

# ----------------------------------------------------------------------
# 14. Write linkage outputs
# ----------------------------------------------------------------------

write_csv(
  linkage_crosswalk,
  "data/processed/linkage_crosswalk.csv"
)

write_csv(
  linkage_candidates,
  "data/processed/linkage_candidates.csv"
)

message("Enterprise record linkage completed successfully.")
