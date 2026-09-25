# =====================================================================
# linkage_features.R
# Interpretable candidate-pair features for enterprise record linkage
# ---------------------------------------------------------------------
# Extracted during v3 modularization.
# Feature definitions are intentionally unchanged.
# =====================================================================

add_linkage_features <- function(
  candidate_pairs
) {
  candidate_pairs %>%
    mutate(
      name_similarity =
        normalized_edit_similarity(
          enterprise_name_source,
          enterprise_name_register
        ),

      street_similarity =
        normalized_edit_similarity(
          street_source,
          street_register
        ),

      city_similarity =
        normalized_edit_similarity(
          city_source,
          city_register
        ),

      postal_code_match =
        as.numeric(
          postal_code_source ==
            postal_code_register
        ),

      legal_form_match =
        normalized_exact_match(
          legal_form_source,
          legal_form_register
        ),

      nace_match =
        as.numeric(
          nace_code_source ==
            nace_code_register
        )
    )
}
