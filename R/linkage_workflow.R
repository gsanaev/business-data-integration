# =====================================================================
# linkage_workflow.R
# End-to-end linkage workflow assembly
# =====================================================================

build_linkage_results <- function(
  firms,
  employment,
  turnover,
  accounting,
  similarity_weights,
  score_threshold,
  margin_threshold
) {
  register_reference <-
    prepare_register_linkage_reference(
      firms
    )

  register_entities <-
    register_reference$entities

  register_lookup <-
    register_reference$lookup

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
        score_threshold,
      margin_threshold =
        margin_threshold
    )

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
        score_threshold,
      margin_threshold =
        margin_threshold
    )

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
        score_threshold,
      margin_threshold =
        margin_threshold
    )

  register_links <-
    build_register_linkage_crosswalk(
      register_entities
    )

  employment_output <-
    build_source_linkage_output(
      employment_linkage$links,
      employment_linkage$similarity$candidates,
      "employment",
      "employment_source_id"
    )

  turnover_output <-
    build_source_linkage_output(
      turnover_linkage$links,
      turnover_linkage$similarity$candidates,
      "turnover",
      "turnover_source_id"
    )

  accounting_output <-
    build_source_linkage_output(
      accounting_linkage$links,
      accounting_linkage$similarity$candidates,
      "accounting",
      "accounting_source_id"
    )

  linkage_crosswalk <-
    dplyr::bind_rows(
      register_links,
      employment_output$crosswalk,
      turnover_output$crosswalk,
      accounting_output$crosswalk
    )

  linkage_candidates <-
    dplyr::bind_rows(
      employment_output$candidates,
      turnover_output$candidates,
      accounting_output$candidates
    )

  linkage_candidates <-
    dplyr::select(
      linkage_candidates,
      source,
      dplyr::everything()
    )

  list(
    crosswalk =
      linkage_crosswalk,
    candidates =
      linkage_candidates
  )
}
