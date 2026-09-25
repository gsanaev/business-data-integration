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

source("R/linkage_candidates.R")
source("R/linkage_features.R")
source("R/linkage_similarity.R")
source("R/linkage_decision.R")

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
# 3. Prepare canonical register reference
# ----------------------------------------------------------------------

register_reference <-
  prepare_register_linkage_reference(
    firms
  )

register_entities <-
  register_reference$entities

register_lookup <-
  register_reference$lookup

# ----------------------------------------------------------------------
# 4. Extract source enterprise identities
# ----------------------------------------------------------------------

employment_entities <-
  extract_source_entities(
    employment,
    "employment_source_id"
  )

turnover_entities <-
  extract_source_entities(
    turnover,
    "turnover_source_id"
  )

accounting_entities <-
  extract_source_entities(
    accounting,
    "accounting_source_id"
  )

# ----------------------------------------------------------------------
# 5. Link employment enterprises
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
# 6. Link turnover enterprises
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
# 7. Link accounting enterprises
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
# 8. Build linkage outputs
# ----------------------------------------------------------------------

register_links <-
  build_register_linkage_crosswalk(
    register_entities
  )

employment_output <-
  build_source_linkage_output(
    employment_links,
    employment_similarity$candidates,
    "employment",
    "employment_source_id"
  )

turnover_output <-
  build_source_linkage_output(
    turnover_links,
    turnover_similarity$candidates,
    "turnover",
    "turnover_source_id"
  )

accounting_output <-
  build_source_linkage_output(
    accounting_links,
    accounting_similarity$candidates,
    "accounting",
    "accounting_source_id"
  )

employment_crosswalk <-
  employment_output$crosswalk

turnover_crosswalk <-
  turnover_output$crosswalk

accounting_crosswalk <-
  accounting_output$crosswalk

linkage_crosswalk <-
  bind_rows(
    register_links,
    employment_crosswalk,
    turnover_crosswalk,
    accounting_crosswalk
  )

linkage_candidates <-
  bind_rows(
    employment_output$candidates,
    turnover_output$candidates,
    accounting_output$candidates
  ) %>%
  select(
    source,
    everything()
  )

# ----------------------------------------------------------------------
# 9. Report linkage results
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
# 10. Write linkage outputs
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
