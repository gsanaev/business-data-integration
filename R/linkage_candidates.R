# =====================================================================
# linkage_candidates.R
# Candidate preparation and generation for enterprise record linkage
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Candidate-generation behavior is intentionally unchanged.
# =====================================================================

prepare_source_linkage_records <- function(
  source_entities,
  source_id_column
) {
  source_entities %>%
    transmute(
      source_record_id =
        .data[[source_id_column]],

      enterprise_name_source =
        enterprise_name,

      street_source =
        street,

      postal_code_source =
        as.character(postal_code),

      city_source =
        city,

      legal_form_source =
        legal_form,

      nace_code_source =
        nace_code
    )
}


prepare_register_linkage_records <- function(
  register_entities
) {
  register_entities %>%
    transmute(
      register_id,
      canonical_firm_id,

      enterprise_name_register =
        enterprise_name,

      street_register =
        street,

      postal_code_register =
        as.character(postal_code),

      city_register =
        city,

      legal_form_register =
        legal_form,

      nace_code_register =
        nace_code
    )
}


generate_linkage_candidates <- function(
  source_records,
  register_records
) {
  # The unresolved population is deliberately small, so a complete
  # source-to-register candidate grid remains computationally modest.
  # Candidates are then retained when either geographic or industry
  # evidence agrees.
  merge(
    source_records,
    register_records,
    by = NULL
  ) %>%
    as_tibble() %>%
    filter(
      postal_code_source ==
        postal_code_register |
        nace_code_source ==
          nace_code_register
    )
}

prepare_register_linkage_reference <- function(
  firms
) {
  if (any(is.na(firms$business_id))) {
    stop(
      "Register reference source contains missing business_id values."
    )
  }

  duplicate_business_ids <-
    firms %>%
    count(
      business_id
    ) %>%
    filter(
      n > 1
    )

  if (nrow(duplicate_business_ids) > 0) {
    stop(
      "Register reference source contains duplicate business_id values: ",
      nrow(duplicate_business_ids)
    )
  }

  register_entities <-
    firms %>%
    arrange(
      register_id
    ) %>%
    mutate(
      canonical_firm_id = sprintf(
        "C%06d",
        seq_len(n())
      )
    )

  register_lookup <-
    register_entities %>%
    select(
      canonical_firm_id,
      register_id,
      business_id
    )

  list(
    entities = register_entities,
    lookup = register_lookup
  )
}


extract_source_entities <- function(
  source_data,
  source_id_column
) {
  identity_columns <- c(
    source_id_column,
    "business_id",
    "enterprise_name",
    "street",
    "postal_code",
    "city",
    "legal_form",
    "nace_code"
  )

  source_data %>%
    select(
      all_of(identity_columns)
    ) %>%
    distinct()
}

# =====================================================================
# Shared sampled linkage preparation
# =====================================================================

prepare_sampled_linkage_entities <- function(
  source_data,
  source_id_column,
  register_data,
  enterprise_split,
  sample_role = "development"
) {
  sample_role <-
    match.arg(
      sample_role,
      c(
        "development",
        "heldout"
      )
    )

  source_entities <-
    source_data %>%
    dplyr::distinct(
      .data$truth_firm_id,
      source_record_id =
        .data[[source_id_column]],
      .data$business_id,
      .data$enterprise_name,
      .data$street,
      .data$postal_code,
      .data$city,
      .data$legal_form,
      .data$nace_code
    ) %>%
    dplyr::left_join(
      enterprise_split,
      by = "truth_firm_id"
    )

  if (
    anyNA(
      source_entities$sample_role
    )
  ) {
    stop(
      "Enterprise split could not be joined to all source enterprises."
    )
  }

  source_entities <-
    source_entities %>%
    dplyr::filter(
      .data$sample_role ==
        .env$sample_role
    )

  register_entities <-
    register_data %>%
    dplyr::distinct(
      .data$truth_firm_id,
      .data$register_id,
      .data$business_id,
      .data$enterprise_name,
      .data$street,
      .data$postal_code,
      .data$city,
      .data$legal_form,
      .data$nace_code
    ) %>%
    dplyr::arrange(
      .data$register_id
    ) %>%
    dplyr::mutate(
      canonical_firm_id =
        sprintf(
          "C%06d",
          dplyr::row_number()
        )
    )

  register_lookup <-
    register_entities %>%
    dplyr::select(
      canonical_firm_id,
      register_id,
      business_id
    )

  truth_register_map <-
    register_entities %>%
    dplyr::select(
      truth_firm_id,
      true_register_id =
        register_id
    )

  unresolved <-
    source_entities %>%
    dplyr::left_join(
      register_lookup,
      by = "business_id"
    ) %>%
    dplyr::filter(
      is.na(
        .data$canonical_firm_id
      )
    ) %>%
    dplyr::left_join(
      truth_register_map,
      by = "truth_firm_id"
    )

  if (
    anyNA(
      unresolved$true_register_id
    )
  ) {
    stop(
      "True register identifiers are missing for unresolved sampled records."
    )
  }

  list(
    unresolved = unresolved,
    register_entities = register_entities
  )
}


build_unresolved_linkage_candidates <- function(
  unresolved,
  register_entities
) {
  source_for_matching <-
    prepare_source_linkage_records(
      unresolved,
      "source_record_id"
    )

  register_for_matching <-
    prepare_register_linkage_records(
      register_entities
    )

  generate_linkage_candidates(
    source_for_matching,
    register_for_matching
  )
}
