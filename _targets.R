library(targets)

tar_option_set(
  packages = c(
    "dplyr",
    "tidyr",
    "readr",
    "janitor",
    "lubridate",
    "ggplot2",
    "ranger"
  )
)

tar_source(
  files = c(
    "R/plausibility.R",
    "R/synthetic_identity.R",
    "R/linkage_similarity.R",
    "R/config.R",
    "R/synthetic.R",
    "R/validation.R",
    "R/linkage_candidates.R",
    "R/linkage_features.R",
    "R/linkage_ml_candidates.R",
    "R/linkage_ml_cv.R",
    "R/linkage_ml_rf.R",
    "R/linkage_ml_tuning.R",
    "R/linkage_decision.R",
    "R/linkage_workflow.R",
    "R/integration.R",
    "R/coherence.R",
    "R/enterprise_year.R",
    "R/indicators.R",
    "R/downstream_evaluation.R",
    "R/evaluation_split.R",
    "R/evaluation_benchmark.R",
    "R/evaluation_policy.R",
    "R/evaluation_heldout.R",
    "R/evaluation_orchestration.R",
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
    build_synthetic_baseline(
      project_config
    )
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
    build_scenario_candidate_records(
      identity_scenarios =
        identity_scenarios,
      enterprise_split =
        enterprise_split
    )
  ),

  tar_target(
    scenario_candidate_summary,
    summarise_candidate_generation(
      scenario_candidate_records
    )
  ),

  tar_target(
    similarity_benchmark_records,
    build_scenario_similarity_records(
      identity_scenarios =
        identity_scenarios,
      enterprise_split =
        enterprise_split,
      similarity_weights =
        unlist(
          project_config$processing$linkage$baseline_similarity$weights,
          use.names = TRUE
        )
    )
  ),

  tar_target(
    similarity_benchmark_summary,
    summarise_similarity_benchmark(
      similarity_benchmark_records
    )
  ),

  tar_target(
    ml_candidate_pairs_development,
    build_scenario_ml_candidate_pairs(
      identity_scenarios =
        identity_scenarios,
      enterprise_split =
        enterprise_split
    )
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

  # -------------------------------------------------------------------
  # Stage 10B: final held-out linkage evaluation
  # -------------------------------------------------------------------

  tar_target(
    heldout_candidate_records,
    build_scenario_candidate_records(
      identity_scenarios =
        identity_scenarios,
      enterprise_split =
        enterprise_split,
      sample_role =
        "heldout"
    )
  ),

  tar_target(
    heldout_candidate_summary,
    summarise_candidate_generation(
      heldout_candidate_records
    )
  ),

  tar_target(
    heldout_similarity_records,
    build_scenario_similarity_records(
      identity_scenarios =
        identity_scenarios,
      enterprise_split =
        enterprise_split,
      similarity_weights =
        unlist(
          project_config$processing$linkage$baseline_similarity$weights,
          use.names = TRUE
        ),
      sample_role =
        "heldout"
    )
  ),

  tar_target(
    heldout_similarity_summary,
    summarise_similarity_benchmark(
      heldout_similarity_records
    )
  ),

  tar_target(
    ml_candidate_pairs_heldout,
    {
      candidate_pairs <-
        build_scenario_ml_candidate_pairs(
          identity_scenarios =
            identity_scenarios,
          enterprise_split =
            enterprise_split,
          sample_role =
            "heldout"
        )

      validate_ml_candidate_pairs(
        candidate_pairs,
        enterprise_split,
        sample_role =
          "heldout"
      )

      candidate_pairs
    }
  ),

  tar_target(
    rf_heldout_records,
    {
      match_score <-
        predict_rf_match_probability(
          rf_final_development_model$model,
          ml_candidate_pairs_heldout,
          num_threads =
            2L
        )

      scored_records <-
        score_rf_candidate_records(
          ml_candidate_pairs_heldout,
          match_score
        )

      complete_rf_evaluation_records(
        scored_records,
        heldout_similarity_records
      )
    }
  ),

  tar_target(
    rf_heldout_assignments,
    build_rf_top_candidate_assignments(
      fitted_model =
        rf_final_development_model$model,
      candidate_pairs =
        ml_candidate_pairs_heldout,
      num_threads =
        2L
    )
  ),

  tar_target(
    heldout_rf_summary,
    summarise_rf_benchmark(
      rf_heldout_records
    )
  ),

  tar_target(
    heldout_method_comparison,
    build_linkage_method_comparison(
      similarity_records =
        heldout_similarity_records,
      similarity_policy =
        similarity_policy_selected,
      rf_records =
        rf_heldout_records,
      rf_policy =
        rf_selected_policy
    )
  ),

  tar_target(
    heldout_method_comparison_by_scenario,
    build_linkage_method_comparison_by_group(
      similarity_records =
        heldout_similarity_records,
      similarity_policy =
        similarity_policy_selected,
      rf_records =
        rf_heldout_records,
      rf_policy =
        rf_selected_policy,
      group_column =
        "scenario"
    )
  ),

  tar_target(
    heldout_method_comparison_by_source,
    build_linkage_method_comparison_by_group(
      similarity_records =
        heldout_similarity_records,
      similarity_policy =
        similarity_policy_selected,
      rf_records =
        rf_heldout_records,
      rf_policy =
        rf_selected_policy,
      group_column =
        "source"
    )
  ),

  tar_target(
    heldout_complete_linkage_records,
    build_scenario_complete_linkage_records(
      identity_scenarios =
        identity_scenarios,
      enterprise_split =
        enterprise_split,
      similarity_records =
        heldout_similarity_records,
      similarity_policy =
        similarity_policy_selected,
      rf_records =
        rf_heldout_records,
      rf_assignments =
        rf_heldout_assignments,
      rf_policy =
        rf_selected_policy,
      sample_role =
        "heldout"
    )
  ),

  tar_target(
    heldout_complete_linkage_summary,
    summarise_complete_linkage_evaluation(
      heldout_complete_linkage_records
    )
  ),

  tar_target(
    heldout_complete_linkage_by_scenario,
    summarise_complete_linkage_evaluation(
      heldout_complete_linkage_records,
      group_columns =
        "scenario"
    )
  ),

  tar_target(
    heldout_complete_linkage_by_source,
    summarise_complete_linkage_evaluation(
      heldout_complete_linkage_records,
      group_columns =
        "source"
    )
  ),

  tar_target(
    heldout_downstream_scenario_results,
    {
      scenario_names <-
        c(
          "baseline",
          "moderate",
          "difficult"
        )

      validation_config <-
        project_config$processing$validation

      results <-
        lapply(
          scenario_names,
          function(
            scenario_name
          ) {
            run_downstream_scenario_evaluation(
              operational_sources =
                scenario_operational_sources[[scenario_name]],
              linkage_records =
                heldout_complete_linkage_records,
              validation_config =
                validation_config,
              scenario_name =
                scenario_name
            )
          }
        )

      names(results) <-
        scenario_names

      results
    }
  ),

  tar_target(
    heldout_downstream_error_records,
    dplyr::bind_rows(
      lapply(
        heldout_downstream_scenario_results,
        `[[`,
        "error_records"
      )
    )
  ),

  tar_target(
    heldout_downstream_error_summary,
    summarise_downstream_indicator_errors(
      heldout_downstream_error_records
    )
  ),

  tar_target(
    heldout_downstream_collision_records,
    dplyr::bind_rows(
      lapply(
        heldout_downstream_scenario_results,
        `[[`,
        "collision_records"
      )
    )
  ),

  tar_target(
    heldout_downstream_coverage,
    dplyr::bind_rows(
      lapply(
        heldout_downstream_scenario_results,
        `[[`,
        "coverage"
      )
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

      build_validated_sources(
        firms_raw =
          firms_raw,
        employment_raw =
          employment_raw,
        turnover_raw =
          turnover_raw,
        accounting_raw =
          accounting_raw,
        validation_config =
          project_config$processing$validation
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

      similarity_weights <-
        unlist(
          project_config$processing$linkage$baseline_similarity$weights,
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

      build_linkage_results(
        firms =
          firms,
        employment =
          employment,
        turnover =
          turnover,
        accounting =
          accounting,
        similarity_weights =
          similarity_weights,
        score_threshold =
          similarity_score_threshold,
        margin_threshold =
          similarity_margin_threshold
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

      build_integration_results(
        firms =
          firms,
        employment =
          employment,
        turnover =
          turnover,
        accounting =
          accounting,
        crosswalk =
          crosswalk
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
