library(targets)

tar_option_set(
  packages = c(
    "dplyr",
    "tidyr",
    "readr",
    "janitor",
    "lubridate",
    "ggplot2"
  )
)

tar_source(
  files = c(
    "R/helpers/plausibility.R",
    "R/helpers/synthetic_identity.R",
    "R/helpers/linkage_similarity.R",
    "R/config.R",
    "R/synthetic.R",
    "R/validation.R",
    "R/linkage_candidates.R",
    "R/linkage_features.R",
    "R/linkage_similarity.R",
    "R/linkage_ml.R",
    "R/linkage_decision.R",
    "R/integration.R",
    "R/coherence.R",
    "R/enterprise_year.R",
    "R/indicators.R",
    "R/evaluation.R",
    "R/reporting.R"
  ),
  change_directory = FALSE
)

list(
  tar_target(
    config_files,
    c(
      "config/source_contracts.csv",
      "config/scenarios.yml",
      "config/processing.yml",
      "config/coherence_rules.csv"
    ),
    format = "file"
  ),

  tar_target(
    project_config,
    {
      config_files
      load_project_config()
    }
  ),

  tar_target(
    synthetic_baseline,
    {
      # Preserve the verified v2/v3 baseline RNG sequence exactly.
      set.seed(2025)

      reference_structures <-
        create_synthetic_reference_structures()

      regions <-
        reference_structures$regions

      industry_params <-
        reference_structures$industry_params

      legal_forms <-
        reference_structures$legal_forms

      n_firms <- 1500L

      baseline_scenario <-
        project_config$scenarios$scenarios$baseline

      firm_truth <-
        generate_latent_enterprises(
          regions,
          industry_params,
          legal_forms,
          n_firms
        )

      years <- 2023:2025

      annual_truth <-
        generate_annual_latent_states(
          firm_truth,
          years
        )

      firms_inconsistent <-
        generate_register_source(
          annual_truth
        )

      monthly_reference <-
        create_monthly_reference_profiles()

      months <-
        monthly_reference$months

      employment_seasonality <-
        monthly_reference$employment_seasonality

      turnover_seasonality <-
        monthly_reference$turnover_seasonality

      employment <-
        generate_monthly_employment(
          firm_truth,
          annual_truth,
          months,
          employment_seasonality
        )

      turnover <-
        generate_monthly_turnover(
          firm_truth,
          annual_truth,
          months,
          turnover_seasonality
        )

      identity_truth <-
        create_enterprise_identity_truth(
          firm_truth,
          regions
        )

      primary_identities <-
        generate_primary_source_identities(
          identity_truth,
          n_firms,
          baseline_scenario$missing_business_id
        )

      register_identity <-
        primary_identities$register

      employment_identity <-
        primary_identities$employment

      turnover_identity <-
        primary_identities$turnover

      accounting <-
        generate_accounting_source(
          annual_truth
        )

      accounting_identity <-
        generate_accounting_identity(
          identity_truth,
          n_firms,
          baseline_scenario$missing_business_id
        )

      attached_sources <-
        attach_synthetic_source_identities(
          firms_inconsistent,
          employment,
          turnover,
          accounting,
          register_identity,
          employment_identity,
          turnover_identity,
          accounting_identity
        )

      operational_sources <-
        build_operational_synthetic_sources(
          attached_sources
        )

      truth_outputs <-
        build_synthetic_truth_outputs(
          identity_truth,
          register_identity,
          employment_identity,
          turnover_identity,
          accounting_identity,
          attached_sources
        )

      list(
        operational = operational_sources,
        truth = truth_outputs,
        attached_sources = attached_sources
      )
    }
  ),

  tar_target(
    enterprise_split,
    {
      split_seed <- 202604L

      canonical_ids <-
        synthetic_baseline$truth$enterprise$truth_firm_id

      split <-
        create_enterprise_split(
          canonical_ids,
          development_share = 0.70,
          seed = split_seed
        )

      validate_enterprise_split(
        split,
        canonical_ids,
        development_share = 0.70
      )

      split
    }
  ),

  tar_target(
    identity_scenarios,
    {
      scenarios <-
        project_config$scenarios$scenarios

      baseline <-
        apply_identity_scenario_to_sources(
          synthetic_baseline$attached_sources,
          "baseline",
          scenarios$baseline,
          scenarios$baseline
        )

      scenario_seed <- 202603L

      set.seed(
        scenario_seed
      )

      moderate <-
        apply_identity_scenario_to_sources(
          synthetic_baseline$attached_sources,
          "moderate",
          scenarios$moderate,
          scenarios$baseline
        )

      # Reuse the same random-number stream so difficulty levels
      # are compared under common random draws where possible.
      set.seed(
        scenario_seed
      )

      difficult <-
        apply_identity_scenario_to_sources(
          synthetic_baseline$attached_sources,
          "difficult",
          scenarios$difficult,
          scenarios$baseline
        )

      list(
        baseline = baseline,
        moderate = moderate,
        difficult = difficult
      )
    }
  ),

  tar_target(
    scenario_operational_sources,
    list(
      baseline =
        build_operational_synthetic_sources(
          identity_scenarios$baseline$sources
        ),
      moderate =
        build_operational_synthetic_sources(
          identity_scenarios$moderate$sources
        ),
      difficult =
        build_operational_synthetic_sources(
          identity_scenarios$difficult$sources
        )
    )
  ),

  tar_target(
    scenario_corruption_log,
    bind_rows(
      identity_scenarios$baseline$corruption_log,
      identity_scenarios$moderate$corruption_log,
      identity_scenarios$difficult$corruption_log
    ) %>%
      left_join(
        enterprise_split,
        by = "truth_firm_id"
      )
  ),

  tar_target(
    scenario_calibration_rates,
    summarise_scenario_corruption_rates(
      scenario_corruption_log
    )
  ),

  tar_target(
    scenario_calibration_counts,
    summarise_scenario_corruption_counts(
      scenario_corruption_log
    )
  ),

  tar_target(
    scenario_candidate_records,
    {
      source_specs <-
        list(
          employment =
            "employment_source_id",
          turnover =
            "turnover_source_id",
          accounting =
            "accounting_source_id"
        )

      scenario_names <-
        c(
          "baseline",
          "moderate",
          "difficult"
        )

      results <-
        list()

      result_index <- 1L

      for (
        scenario_name in
          scenario_names
      ) {
        scenario_sources <-
          identity_scenarios[[scenario_name]]$sources

        for (
          source_name in
            names(
              source_specs
            )
        ) {
          results[[result_index]] <-
            evaluate_candidate_generation(
              source_data =
                scenario_sources[[source_name]],
              source_id_column =
                source_specs[[source_name]],
              register_data =
                scenario_sources$firms,
              enterprise_split =
                enterprise_split,
              scenario_name =
                scenario_name,
              source_name =
                source_name
            )

          result_index <-
            result_index + 1L
        }
      }

      bind_rows(
        results
      )
    }
  ),

  tar_target(
    scenario_candidate_summary,
    summarise_candidate_generation(
      scenario_candidate_records
    )
  ),

  tar_target(
    similarity_benchmark_records,
    {
      similarity_weights <-
        unlist(
          project_config$processing$linkage$baseline_similarity$weights,
          use.names = TRUE
        )

      source_specs <-
        list(
          employment =
            "employment_source_id",
          turnover =
            "turnover_source_id",
          accounting =
            "accounting_source_id"
        )

      scenario_names <-
        c(
          "baseline",
          "moderate",
          "difficult"
        )

      results <-
        list()

      result_index <- 1L

      for (
        scenario_name in
          scenario_names
      ) {
        scenario_sources <-
          identity_scenarios[[scenario_name]]$sources

        for (
          source_name in
            names(
              source_specs
            )
        ) {
          results[[result_index]] <-
            build_similarity_benchmark_records(
              source_data =
                scenario_sources[[source_name]],
              source_id_column =
                source_specs[[source_name]],
              register_data =
                scenario_sources$firms,
              enterprise_split =
                enterprise_split,
              scenario_name =
                scenario_name,
              source_name =
                source_name,
              similarity_weights =
                similarity_weights
            )

          result_index <-
            result_index + 1L
        }
      }

      bind_rows(
        results
      )
    }
  ),

  tar_target(
    similarity_benchmark_summary,
    summarise_similarity_benchmark(
      similarity_benchmark_records
    )
  ),

  tar_target(
    ml_candidate_pairs_development,
    {
      source_specs <-
        list(
          employment =
            "employment_source_id",
          turnover =
            "turnover_source_id",
          accounting =
            "accounting_source_id"
        )

      scenario_names <-
        c(
          "baseline",
          "moderate",
          "difficult"
        )

      results <-
        list()

      result_index <-
        1L

      for (
        scenario_name in
          scenario_names
      ) {
        scenario_sources <-
          identity_scenarios[[scenario_name]]$sources

        for (
          source_name in
            names(
              source_specs
            )
        ) {
          results[[result_index]] <-
            build_ml_candidate_pairs(
              source_data =
                scenario_sources[[source_name]],
              source_id_column =
                source_specs[[source_name]],
              register_data =
                scenario_sources$firms,
              enterprise_split =
                enterprise_split,
              scenario_name =
                scenario_name,
              source_name =
                source_name
            )

          result_index <-
            result_index + 1L
        }
      }

      bind_rows(
        results
      )
    }
  ),

  tar_target(
    ml_cv_folds,
    {
      validate_ml_candidate_pairs(
        ml_candidate_pairs_development,
        enterprise_split
      )

      create_grouped_cv_folds(
        ml_candidate_pairs_development,
        n_folds =
          5L,
        seed =
          202605L
      )
    }
  ),

  tar_target(
    ml_candidate_pairs_cv,
    {
      candidate_pairs_cv <-
        attach_grouped_cv_folds(
          ml_candidate_pairs_development,
          ml_cv_folds
        )

      validate_grouped_cv_assignment(
        candidate_pairs_cv,
        n_folds =
          5L
      )

      candidate_pairs_cv
    }
  ),

  tar_target(
    ml_cv_summary,
    summarise_ml_cv_folds(
      ml_candidate_pairs_cv
    )
  ),

  tar_target(
    rf_tuning_grid,
    rf_linkage_tuning_grid()
  ),

  tar_target(
    rf_cv_tuning,
    run_rf_cv_tuning(
      ml_candidate_pairs_cv,
      tuning_grid =
        rf_tuning_grid,
      seed =
        202606L,
      num_threads =
        2L
    )
  ),

  tar_target(
    rf_selected_spec,
    select_rf_configuration(
      rf_cv_tuning$summary
    )
  ),

  tar_target(
    rf_oof_records,
    {
      selected_config_id <-
        rf_selected_spec$config_id[[1]]

      selected_records <-
        rf_cv_tuning$record_metrics %>%
        dplyr::filter(
          .data$config_id ==
            selected_config_id
        )

      if (
        nrow(
          selected_records
        ) !=
          nrow(
            similarity_benchmark_records
          )
      ) {
        stop(
          "Selected RF OOF records do not match the development benchmark size."
        )
      }

      selected_records
    }
  ),

  tar_target(
    rf_policy_grid,
    search_rf_policy_grid(
      rf_oof_records,
      precision_target =
        0.99
    )
  ),

  tar_target(
    rf_selected_policy,
    select_rf_policy(
      rf_policy_grid,
      precision_target =
        0.99
    )
  ),

  tar_target(
    rf_final_development_model,
    fit_rf_candidate_model(
      training_pairs =
        ml_candidate_pairs_development,
      mtry =
        rf_selected_spec$mtry[[1]],
      min_node_size =
        rf_selected_spec$min_node_size[[1]],
      num_trees =
        rf_selected_spec$num_trees[[1]],
      seed =
        202607L,
      num_threads =
        2L
    )
  ),

  tar_target(
    development_method_comparison,
    build_development_method_comparison(
      similarity_records =
        similarity_benchmark_records,
      similarity_policy =
        similarity_policy_selected,
      rf_records =
        rf_oof_records,
      rf_policy =
        rf_selected_policy
    )
  ),

  tar_target(
    quality_evidence_registry,
    build_quality_evidence_registry()
  ),

  tar_target(
    linkage_process_metadata,
    build_linkage_process_metadata(
      rf_spec =
        rf_selected_spec,
      rf_policy =
        rf_selected_policy,
      similarity_policy =
        similarity_policy_selected,
      feature_columns =
        ml_linkage_feature_columns(),
      development_share =
        0.70,
      n_cv_folds =
        5L,
      split_seed =
        202604L,
      cv_seed =
        202605L,
      rf_tuning_seed =
        202606L,
      rf_final_seed =
        202607L
    )
  ),

  tar_target(
    similarity_policy_grid,
    search_similarity_policy_grid(
      similarity_benchmark_records,
      precision_target =
        0.99
    )
  ),

  tar_target(
    similarity_policy_selected,
    select_similarity_policy(
      similarity_policy_grid,
      precision_target =
        0.99
    )
  ),

  tar_target(
    raw_files,
    {
      dir.create(
        "data/raw",
        showWarnings = FALSE,
        recursive = TRUE
      )

      dir.create(
        "data/truth",
        showWarnings = FALSE,
        recursive = TRUE
      )

      write_csv(
        synthetic_baseline$operational$firms,
        "data/raw/firms.csv"
      )

      write_csv(
        synthetic_baseline$operational$employment,
        "data/raw/employment.csv"
      )

      write_csv(
        synthetic_baseline$operational$turnover,
        "data/raw/turnover.csv"
      )

      write_csv(
        synthetic_baseline$operational$accounting,
        "data/raw/accounting.csv"
      )

      write_csv(
        synthetic_baseline$truth$enterprise,
        "data/truth/enterprise_truth.csv"
      )

      write_csv(
        synthetic_baseline$truth$linkage,
        "data/truth/linkage_truth.csv"
      )

      write_csv(
        synthetic_baseline$truth$value,
        "data/truth/value_truth.csv"
      )

      c(
        "data/raw/firms.csv",
        "data/raw/employment.csv",
        "data/raw/turnover.csv",
        "data/raw/accounting.csv",
        "data/truth/enterprise_truth.csv",
        "data/truth/linkage_truth.csv",
        "data/truth/value_truth.csv"
      )
    },
    format = "file"
  ),

  tar_target(
    validated_sources,
    {
      raw_files

      validation_config <-
        project_config$processing$validation

      foundation_year_min <-
        validation_config$foundation_year_min

      employment_spike_multiplier <-
        validation_config$employment_spike_multiplier

      firms_raw <-
        read_csv(
          "data/raw/firms.csv",
          show_col_types = FALSE
        )

      employment_raw <-
        read_csv(
          "data/raw/employment.csv",
          show_col_types = FALSE
        )

      turnover_raw <-
        read_csv(
          "data/raw/turnover.csv",
          show_col_types = FALSE
        )

      accounting_raw <-
        read_csv(
          "data/raw/accounting.csv",
          show_col_types = FALSE
        )

      validate_source_structures(
        firms_raw,
        employment_raw,
        turnover_raw,
        accounting_raw
      )

      firms_clean <-
        validate_register_source(
          firms_raw,
          foundation_year_min
        )

      employment_clean <-
        validate_employment_source(
          employment_raw,
          employment_spike_multiplier
        )

      turnover_clean <-
        validate_turnover_source(
          turnover_raw
        )

      accounting_clean <-
        validate_accounting_source(
          accounting_raw
        )

      assert_validated_sources(
        firms_clean,
        employment_clean,
        turnover_clean,
        accounting_clean
      )

      list(
        firms = firms_clean,
        employment = employment_clean,
        turnover = turnover_clean,
        accounting = accounting_clean
      )
    }
  ),

  tar_target(
    clean_files,
    {
      dir.create(
        "data/clean",
        showWarnings = FALSE,
        recursive = TRUE
      )

      write_csv(
        validated_sources$firms,
        "data/clean/firms_clean.csv"
      )

      write_csv(
        validated_sources$employment,
        "data/clean/employment_clean.csv"
      )

      write_csv(
        validated_sources$turnover,
        "data/clean/turnover_clean.csv"
      )

      write_csv(
        validated_sources$accounting,
        "data/clean/accounting_clean.csv"
      )

      c(
        "data/clean/firms_clean.csv",
        "data/clean/employment_clean.csv",
        "data/clean/turnover_clean.csv",
        "data/clean/accounting_clean.csv"
      )
    },
    format = "file"
  ),

  tar_target(
    linkage_results,
    {
      clean_files

      baseline_similarity_config <-
        project_config$processing$linkage$baseline_similarity

      similarity_weights <-
        unlist(
          baseline_similarity_config$weights,
          use.names = TRUE
        )

      similarity_score_threshold <-
        similarity_policy_selected$score_threshold[[1]]

      similarity_margin_threshold <-
        similarity_policy_selected$margin_threshold[[1]]

      firms <-
        read_csv(
          "data/clean/firms_clean.csv",
          show_col_types = FALSE
        )

      employment <-
        read_csv(
          "data/clean/employment_clean.csv",
          show_col_types = FALSE
        )

      turnover <-
        read_csv(
          "data/clean/turnover_clean.csv",
          show_col_types = FALSE
        )

      accounting <-
        read_csv(
          "data/clean/accounting_clean.csv",
          show_col_types = FALSE
        )

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
          source_entities = employment_entities,
          source_id_column = "employment_source_id",
          register_lookup = register_lookup,
          register_entities = register_entities,
          similarity_weights = similarity_weights,
          score_threshold = similarity_score_threshold,
          margin_threshold = similarity_margin_threshold
        )

      turnover_linkage <-
        link_source_entities(
          source_entities = turnover_entities,
          source_id_column = "turnover_source_id",
          register_lookup = register_lookup,
          register_entities = register_entities,
          similarity_weights = similarity_weights,
          score_threshold = similarity_score_threshold,
          margin_threshold = similarity_margin_threshold
        )

      accounting_linkage <-
        link_source_entities(
          source_entities = accounting_entities,
          source_id_column = "accounting_source_id",
          register_lookup = register_lookup,
          register_entities = register_entities,
          similarity_weights = similarity_weights,
          score_threshold = similarity_score_threshold,
          margin_threshold = similarity_margin_threshold
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
        bind_rows(
          register_links,
          employment_output$crosswalk,
          turnover_output$crosswalk,
          accounting_output$crosswalk
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

      list(
        crosswalk = linkage_crosswalk,
        candidates = linkage_candidates
      )
    }
  ),

  tar_target(
    linkage_files,
    {
      dir.create(
        "data/processed",
        showWarnings = FALSE,
        recursive = TRUE
      )

      write_csv(
        linkage_results$crosswalk,
        "data/processed/linkage_crosswalk.csv"
      )

      write_csv(
        linkage_results$candidates,
        "data/processed/linkage_candidates.csv"
      )

      c(
        "data/processed/linkage_crosswalk.csv",
        "data/processed/linkage_candidates.csv"
      )
    },
    format = "file"
  ),

  tar_target(
    integration_results,
    {
      clean_files
      linkage_files

      firms <-
        read_csv(
          "data/clean/firms_clean.csv",
          show_col_types = FALSE
        )

      employment <-
        read_csv(
          "data/clean/employment_clean.csv",
          show_col_types = FALSE
        )

      turnover <-
        read_csv(
          "data/clean/turnover_clean.csv",
          show_col_types = FALSE
        )

      accounting <-
        read_csv(
          "data/clean/accounting_clean.csv",
          show_col_types = FALSE
        )

      crosswalk <-
        read_csv(
          "data/processed/linkage_crosswalk.csv",
          show_col_types = FALSE
        )

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
        panel = panel,
        accounting_annual = accounting_annual
      )
    }
  ),

  tar_target(
    integration_files,
    {
      write_csv(
        integration_results$panel,
        "data/processed/panel_data.csv"
      )

      write_csv(
        integration_results$accounting_annual,
        "data/processed/accounting_annual.csv"
      )

      c(
        "data/processed/panel_data.csv",
        "data/processed/accounting_annual.csv"
      )
    },
    format = "file"
  ),

  tar_target(
    coherence_results,
    {
      integration_files
      config_files

      coherence_rules_config <-
        project_config$coherence_rules

      materiality_config <-
        project_config$processing$coherence$materiality

      threshold_revenue_accounting <-
        get_coherence_threshold(
          "COH_REV_ACCOUNTING",
          coherence_rules_config
        )

      threshold_revenue_register <-
        get_coherence_threshold(
          "COH_REV_REGISTER",
          coherence_rules_config
        )

      threshold_employment_register <-
        get_coherence_threshold(
          "COH_EMP_REGISTER",
          coherence_rules_config
        )

      required_months <- 12L

      materiality_medium_percentile <-
        materiality_config$medium_percentile

      materiality_high_percentile <-
        materiality_config$high_percentile

      panel <-
        read_csv(
          "data/processed/panel_data.csv",
          show_col_types = FALSE
        ) %>%
        mutate(
          month = as.Date(month),
          year = year(month)
        )

      accounting <-
        read_csv(
          "data/processed/accounting_annual.csv",
          show_col_types = FALSE
        )

      contracts <-
        read_csv(
          "config/source_contracts.csv",
          show_col_types = FALSE,
          col_types = cols(
            .default = col_character()
          )
        )

      assert_comparable_group(
        "turnover",
        "turnover",
        "accounting",
        "operating_revenue",
        "annual_revenue_related",
        contracts
      )

      assert_comparable_group(
        "turnover",
        "turnover",
        "register",
        "revenue_last_year",
        "annual_revenue_related",
        contracts
      )

      assert_comparable_group(
        "employment",
        "employees",
        "register",
        "employees",
        "employment",
        contracts
      )

      annual_panel <-
        build_coherence_annual_panel(
          panel,
          required_months
        )

      validate_coherence_annual_panel(
        annual_panel
      )

      revenue_accounting_events <-
        build_revenue_accounting_events(
          annual_panel,
          accounting,
          required_months,
          threshold_revenue_accounting,
          materiality_medium_percentile,
          materiality_high_percentile
        )

      revenue_register_events <-
        build_revenue_register_events(
          annual_panel,
          required_months,
          threshold_revenue_register,
          materiality_medium_percentile,
          materiality_high_percentile
        )

      employment_register_events <-
        build_employment_register_events(
          annual_panel,
          required_months,
          threshold_employment_register,
          materiality_medium_percentile,
          materiality_high_percentile
        )

      coherence_events <-
        combine_coherence_events(
          revenue_accounting_events,
          revenue_register_events,
          employment_register_events,
          accounting,
          annual_panel
        )

      review_queue <-
        build_coherence_review_queue(
          coherence_events
        )

      list(
        events = coherence_events,
        review_queue = review_queue
      )
    }
  ),

  tar_target(
    coherence_files,
    {
      write_csv(
        coherence_results$events,
        "data/processed/coherence_events.csv"
      )

      write_csv(
        coherence_results$review_queue,
        "data/processed/review_queue.csv"
      )

      c(
        "data/processed/coherence_events.csv",
        "data/processed/review_queue.csv"
      )
    },
    format = "file"
  ),

  tar_target(
    indicator_results,
    {
      integration_files

      required_months <- 12L

      panel <-
        read_csv(
          "data/processed/panel_data.csv",
          show_col_types = FALSE
        ) %>%
        mutate(
          month = as.Date(month),
          year = year(month)
        )

      validate_monthly_panel_structure(
        panel,
        required_months
      )

      enterprise_year <-
        build_enterprise_year(
          panel,
          required_months
        )

      validate_enterprise_year(
        enterprise_year
      )

      indicators_sector_region <-
        aggregate_indicators(
          enterprise_year,
          c(
            "year",
            "nace_code",
            "region_code"
          )
        )

      indicators_sector <-
        aggregate_indicators(
          enterprise_year,
          c(
            "year",
            "nace_code"
          )
        )

      indicators_region <-
        aggregate_indicators(
          enterprise_year,
          c(
            "year",
            "region_code"
          )
        )

      indicator_tables <-
        list(
          sector_region =
            indicators_sector_region,
          sector =
            indicators_sector,
          region =
            indicators_region
        )

      validate_indicator_tables(
        indicator_tables
      )

      list(
        enterprise_year =
          enterprise_year,
        sector_region =
          indicators_sector_region,
        sector =
          indicators_sector,
        region =
          indicators_region
      )
    }
  ),

  tar_target(
    indicator_files,
    {
      dir.create(
        "output/tables",
        showWarnings = FALSE,
        recursive = TRUE
      )

      write_csv(
        indicator_results$enterprise_year,
        "data/processed/enterprise_year_indicators.csv"
      )

      write_csv(
        indicator_results$sector_region,
        "output/tables/indicators_sector_region.csv"
      )

      write_csv(
        indicator_results$sector,
        "output/tables/indicators_sector.csv"
      )

      write_csv(
        indicator_results$region,
        "output/tables/indicators_region.csv"
      )

      c(
        "data/processed/enterprise_year_indicators.csv",
        "output/tables/indicators_sector_region.csv",
        "output/tables/indicators_sector.csv",
        "output/tables/indicators_region.csv"
      )
    },
    format = "file"
  ),

  tar_target(
    figure_files,
    {
      integration_files
      indicator_files
      coherence_files

      dir.create(
        "output/figures",
        showWarnings = FALSE,
        recursive = TRUE
      )

      panel <-
        read_csv(
          "data/processed/panel_data.csv",
          show_col_types = FALSE
        ) %>%
        mutate(
          month = as.Date(month)
        )

      indicators_sector <-
        read_csv(
          "output/tables/indicators_sector.csv",
          show_col_types = FALSE
        )

      coherence_events <-
        read_csv(
          "data/processed/coherence_events.csv",
          show_col_types = FALSE
        )

      validate_sector_plot_inputs(
        indicators_sector
      )

      figure_width <- 8
      figure_height <- 5.5
      figure_dpi <- 160

      base_theme <-
        build_reporting_theme()

      monthly_turnover <-
        summarise_monthly_turnover(
          panel
        )

      p_monthly_turnover <-
        build_monthly_turnover_plot(
          monthly_turnover,
          base_theme
        )

      ggsave(
        filename =
          "output/figures/monthly_turnover_total.png",
        plot =
          p_monthly_turnover,
        width =
          figure_width,
        height =
          figure_height,
        dpi =
          figure_dpi
      )

      p_annual_turnover_sector <-
        build_annual_turnover_sector_plot(
          indicators_sector,
          base_theme
        )

      ggsave(
        filename =
          "output/figures/annual_turnover_by_sector.png",
        plot =
          p_annual_turnover_sector,
        width =
          figure_width,
        height =
          figure_height,
        dpi =
          figure_dpi
      )

      p_turnover_employee_sector <-
        build_turnover_employee_sector_plot(
          indicators_sector,
          base_theme
        )

      ggsave(
        filename =
          "output/figures/turnover_per_employee_by_sector.png",
        plot =
          p_turnover_employee_sector,
        width =
          figure_width,
        height =
          figure_height,
        dpi =
          figure_dpi
      )

      coherence_plot_data <-
        prepare_coherence_plot_data(
          coherence_events
        )

      p_coherence <-
        build_coherence_outcomes_plot(
          coherence_plot_data,
          base_theme
        )

      ggsave(
        filename =
          "output/figures/coherence_outcomes.png",
        plot =
          p_coherence,
        width =
          figure_width,
        height =
          figure_height,
        dpi =
          figure_dpi
      )

      figure_files <- c(
        "output/figures/monthly_turnover_total.png",
        "output/figures/annual_turnover_by_sector.png",
        "output/figures/turnover_per_employee_by_sector.png",
        "output/figures/coherence_outcomes.png"
      )

      missing_figures <-
        figure_files[
          !file.exists(
            figure_files
          )
        ]

      if (
        length(
          missing_figures
        ) > 0L
      ) {
        stop(
          "Expected figure files were not created: ",
          paste(
            missing_figures,
            collapse = ", "
          )
        )
      }

      figure_files
    },
    format = "file"
  )
)
