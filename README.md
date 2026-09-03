# 📊 Multisource Enterprise Statistics Integration Workflow in R

*A Reproducible Methodological Demonstration of Validation, Record Linkage, Multisource Integration, Coherence Assessment, and Structural Indicator Production Using Synthetic Enterprise Data*

![made-with-R](https://img.shields.io/badge/Made%20with-R-276DC2.svg)
![license](https://img.shields.io/badge/license-MIT-green.svg)

## 🇩🇪 Kurzbeschreibung

Dieses Projekt demonstriert einen reproduzierbaren Workflow zur Aufbereitung, Verknüpfung, Integration und Auswertung mehrerer statistischer Unternehmensdatenquellen auf Basis vollständig synthetischer Daten.

Der Schwerpunkt liegt auf methodischen Fragestellungen der Unternehmensstatistik, insbesondere auf:

- statistischer Datenvalidierung und Plausibilisierung,
- nachvollziehbarer Behandlung fehlender und auffälliger Werte,
- deterministischer und ähnlichkeitsbasierter Datensatzverknüpfung,
- Harmonisierung heterogener Datenquellen,
- Integration monatlicher und jährlicher Unternehmensdaten,
- quellenübergreifenden Kohärenzprüfungen,
- Priorisierung prüfungsrelevanter Fälle,
- sowie der Erstellung sektoraler und regionaler Kennzahlen.

Das Projekt orientiert sich konzeptionell an Herausforderungen moderner Unternehmens- und amtlicher Statistik. Es bildet jedoch keine internen Verfahren oder Produktionssysteme einer statistischen Institution nach und dient ausschließlich als transparente methodische Demonstration.

Alle verwendeten Unternehmensdaten sind vollständig synthetisch.

---

## 🇬🇧 Overview

This repository demonstrates a reproducible workflow for integrating, validating, linking, and analyzing heterogeneous enterprise-statistics data sources using fully synthetic data.

The project is conceptually inspired by methodological challenges commonly encountered in modern enterprise statistics and official-statistics production environments, including:

- multisource data integration,
- statistical editing and plausibility validation,
- source-specific enterprise identifiers,
- deterministic and similarity-based record linkage,
- harmonization of classifications and concepts,
- integration of monthly and annual information,
- cross-source coherence assessment,
- handling of imperfect and incomplete observations,
- review prioritization,
- and reproducible statistical production workflows.

The repository illustrates selected stages of a simplified enterprise-statistics production process while emphasizing:

- methodological transparency,
- traceability,
- reproducibility,
- explicit statistical concepts,
- and interpretable processing rules.

All datasets are fully synthetic and are used exclusively for methodological demonstration purposes.

---

## 🚀 1. Project Overview

Modern enterprise statistics increasingly rely on combinations of multiple data sources such as:

- statistical business registers,
- surveys,
- administrative sources,
- and accounting-based information.

These sources may differ with respect to:

- identifiers,
- statistical concepts,
- reporting frequency,
- reference periods,
- classification structures,
- completeness,
- and statistical quality.

This repository provides a simplified but reproducible methodological workflow showing how heterogeneous enterprise data can be:

1. generated,
2. validated,
3. statistically edited,
4. linked across source-specific identities,
5. integrated into canonical enterprise structures,
6. assessed for cross-source coherence,
7. transformed into annual enterprise indicators,
8. aggregated into sectoral and regional statistics,
9. tested automatically,
10. and evaluated against hidden synthetic truth.

The project intentionally favors transparent and explainable procedures over unnecessary methodological complexity.

---

## 🗂 2. Repository Structure

```text
business-data-integration/
├── config/
│   └── source_contracts.csv
│
├── data/
│   ├── raw/              # generated synthetic source data
│   ├── clean/            # validated source-specific data
│   ├── processed/        # linked and analysis-ready data
│   └── truth/            # generated hidden truth for evaluation
│
├── evaluation/
│   └── evaluate_methods.R
│
├── output/
│   ├── figures/          # selected reproducible figures
│   └── tables/           # selected aggregate outputs
│
├── R/
│   ├── 01_generate_synthetic_data.R
│   ├── 02_clean_and_validate_data.R
│   ├── 03_link_sources.R
│   ├── 04_integrate_sources.R
│   ├── 05_check_coherence.R
│   ├── 06_compute_indicators.R
│   ├── 07_visualize_results.R
│   └── helpers/
│       ├── linkage_similarity.R
│       ├── plausibility.R
│       └── synthetic_identity.R
│
├── tests/
│   ├── helpers/
│   │   └── assertions.R
│   ├── run_tests.R
│   ├── test_coherence_outputs.R
│   ├── test_indicator_outputs.R
│   ├── test_linkage_outputs.R
│   ├── test_linkage_similarity.R
│   └── test_plausibility.R
│
├── .Rprofile
├── LICENSE
├── README.md
├── renv.lock
└── renv/
```

Generated enterprise-level CSV files are intentionally excluded from version control.

Selected aggregated tables and figures are committed as reproducible examples.

The project uses `renv` for dependency management and reproducible package versions.

---

## 🧪 3. Synthetic Statistical Source Data

The repository uses fully synthetic enterprise-level data to illustrate selected methodological aspects of multisource enterprise-statistics integration and statistical production workflows.

The generator uses a fixed random seed:

```r
set.seed(2025)
```

The synthetic environment contains:

- **1,500 enterprises**
- observations covering **2023–2025**
- monthly employment and turnover information,
- annual accounting information,
- register-style structural information,
- controlled missingness,
- plausibility issues,
- and source-specific identity inconsistencies.

No real enterprise, survey, accounting, administrative, or confidential information is used.

---

### A) Synthetic Register-Style Enterprise Source

This source represents a simplified structural reference source.

Selected variables include:

- `register_id`
- `business_id`
- `enterprise_name`
- `street`
- `postal_code`
- `city`
- `region_code`
- `nace_code`
- `legal_form`
- `employees`
- `foundation_year`
- `revenue_last_year`

The source intentionally contains selected imperfections such as:

- missing employment values,
- invalid or implausible revenue values,
- and source-specific reporting irregularities.

The register-style source functions as the canonical structural reference for enterprise integration.

---

### B) Synthetic Monthly Employment Source

The employment source contains monthly enterprise-level observations from January 2023 through December 2025.

Selected variables include:

- `employment_source_id`
- `business_id`
- enterprise identity attributes,
- `month`
- `nace_code`
- `region_code`
- `employees`

The source incorporates:

- synthetic missing observations,
- controlled irregularities,
- enterprise heterogeneity,
- sector-specific seasonal structures,
- and source-specific identity variations.

#### ⭐ Simplified Sector-Specific Seasonal Structures

Selected sectors include simplified seasonal employment patterns:

- **Retail trade (G47)** — stronger year-end activity,
- **Accommodation and food services (I55, I56)** — seasonal peaks,
- **Manufacturing (C10, C29)** — moderate seasonal development,
- **Transport (H49)** — comparatively stable development.

The objective is methodological illustration rather than empirical replication of real sector dynamics.

---

### C) Synthetic Monthly Turnover Source

The turnover source contains monthly enterprise-level turnover observations for 2023–2025.

Selected variables include:

- `turnover_source_id`
- `business_id`
- enterprise identity attributes,
- `month`
- `nace_code`
- `region_code`
- `turnover`

Turnover is generated in relation to employment and persistent enterprise-specific turnover-per-employee characteristics.

The source additionally contains:

- missing observations,
- controlled reporting inconsistencies,
- seasonal structures,
- and source-specific identity variation.

---

### D) Synthetic Annual Accounting Source

The accounting-style source provides annual enterprise information for 2023–2025.

Selected variables include:

- `accounting_source_id`
- `business_id`
- enterprise identity attributes,
- `reference_year`
- `nace_code`
- `operating_revenue`
- `purchases_goods_services`
- `personnel_expense`

Accounting operating revenue is intentionally related to statistical turnover but is **not assumed to be definitionally identical**.

This distinction is important when assessing cross-source coherence.

---

## 🔧 4. Statistical Production Workflow & Methods

### Workflow Structure

The repository is organized as a modular seven-stage operational workflow:

```text
                 ┌──────────────────────────────────┐
                 │ 01_generate_synthetic_data       │
                 └────────────────┬─────────────────┘
                                  ▼
                 ┌──────────────────────────────────┐
                 │ 02_clean_and_validate_data       │
                 └────────────────┬─────────────────┘
                                  ▼
                 ┌──────────────────────────────────┐
                 │ 03_link_sources                  │
                 └────────────────┬─────────────────┘
                                  ▼
                 ┌──────────────────────────────────┐
                 │ 04_integrate_sources             │
                 └────────────────┬─────────────────┘
                                  ▼
                 ┌──────────────────────────────────┐
                 │ 05_check_coherence               │
                 └────────────────┬─────────────────┘
                                  ▼
                 ┌──────────────────────────────────┐
                 │ 06_compute_indicators            │
                 └────────────────┬─────────────────┘
                                  ▼
                 ┌──────────────────────────────────┐
                 │ 07_visualize_results             │
                 └──────────────────────────────────┘
```

A separate evaluation layer is intentionally kept outside the operational sequence:

```text
Operational workflow 01–07
          │
          ▼
evaluation/evaluate_methods.R
          │
          ├── linkage evaluation
          └── imputation evaluation
```

This separation ensures that hidden synthetic truth is used for method evaluation rather than operational processing.

---

### Step 1 — Generation of Synthetic Statistical Sources

The first stage generates:

- register-style data,
- monthly employment data,
- monthly turnover data,
- annual accounting data,
- source-specific identities,
- and hidden synthetic truth.

Operational source files do not expose the hidden enterprise identifier used by the generator.

This allows record-linkage methods to be evaluated independently afterward.

---

### Step 2 — Statistical Editing & Plausibility Validation

`R/02_clean_and_validate_data.R` performs source-specific statistical editing and validation.

Quality statuses include:

- `accepted`
- `imputed`
- `review_required`
- `rejected`

Where relevant, the workflow retains:

- original raw values,
- analytical values,
- rule identifiers,
- quality statuses,
- and imputation flags.

This supports traceability between original and processed observations.

#### Bounded Monthly Interpolation

Missing monthly employment and turnover values may be interpolated only when valid observations exist on both sides of an internal gap.

Example:

```text
observed ─ missing ─ observed
          ↓
     interpolation
```

Boundary gaps are not extrapolated:

```text
missing ─ observed ─ observed
   ↓
review_required
```

This prevents synthetic extension beyond the available temporal information.

#### Register Employment

Missing register employment values are not replaced with a cross-sectional peer median.

Because enterprises remain heterogeneous even within the same NACE × region groups, missing register employment is conservatively classified as:

```text
review_required
```

rather than assigning a weak proxy value.

---

## 🔗 5. Enterprise Record Linkage

Each operational source uses its own record identifier:

- `register_id`
- `employment_source_id`
- `turnover_source_id`
- `accounting_source_id`

The linkage workflow therefore does not rely on hidden synthetic truth.

It follows a transparent hierarchy.

### Stage 1 — Deterministic Linkage

Records with an available common `business_id` are linked directly.

### Stage 2 — Similarity-Based Linkage

Records without a usable common identifier are evaluated using observable enterprise information.

The weighted similarity score combines:

| Component | Weight |
|---|---:|
| Enterprise-name similarity | 40.0% |
| Street similarity | 30.0% |
| City similarity | 5.0% |
| Postal-code agreement | 10.0% |
| Legal-form agreement | 7.5% |
| NACE agreement | 7.5% |

Candidate records are restricted using observable source information before scoring.

The workflow also retains:

- candidate scores,
- score components,
- candidate ranks,
- best-match margins,
- and linkage method.

Current linkage results are:

| Source | Deterministic | Similarity | Total |
|---|---:|---:|---:|
| Employment | 1,344 | 156 | 1,500 |
| Turnover | 1,354 | 146 | 1,500 |
| Accounting | 1,363 | 137 | 1,500 |

The approach deliberately favors transparent similarity logic over a more complex predictive model where the controlled synthetic linkage problem does not require one.

---

## 🗂 6. Metadata, Source Contracts & Concept Alignment

Metadata plays an important role in multisource statistical production because variables with similar names are not necessarily statistically identical.

The repository therefore includes:

```text
config/source_contracts.csv
```

The source contracts document:

- source,
- variable,
- statistical unit,
- grain,
- frequency,
- reference period,
- statistical concept,
- source role,
- comparability group,
- and known limitations.

### Example Concept Relationships

| Source | Variable | Frequency | Concept |
|---|---|---|---|
| Register | employees | annual snapshot | register-style employment count |
| Employment | employees | monthly | monthly employment count |
| Register | revenue_last_year | annual | prior-year register-style revenue |
| Turnover | turnover | monthly | monthly statistical turnover |
| Accounting | operating_revenue | annual | accounting operating revenue |

Examples of important distinctions:

- monthly employment is a monthly stock measure,
- register employment is an annual snapshot,
- monthly turnover must be aggregated before annual comparison,
- accounting operating revenue is related to turnover but not assumed to be definitionally equivalent.

The source-contract layer helps prevent mechanically comparing variables that are numerically similar but conceptually different.

---

## 🔄 7. Multisource Integration

`R/04_integrate_sources.R` combines linked source records using canonical enterprise identifiers.

The integrated monthly panel contains:

- **1,500 canonical enterprises**
- **54,000 enterprise-month observations**
- monthly employment,
- monthly turnover,
- structural enterprise information,
- linkage information,
- and selected analytical variables.

Annual accounting information remains at its appropriate annual grain rather than being artificially repeated as if it were monthly information.

This separation helps preserve statistical meaning across heterogeneous source frequencies.

---

## ✅ 8. Cross-Source Coherence & Review Prioritization

`R/05_check_coherence.R` performs explicit cross-source coherence checks after concept alignment.

Three coherence rules are implemented:

1. annual monthly turnover vs. annual accounting operating revenue,
2. 2024 annual turnover vs. register prior-year revenue,
3. 2025 annual-average employment vs. register employment snapshot.

Comparisons are made only where coverage and source availability permit a meaningful assessment.

### Applicability States

Possible applicability outcomes include:

- `applicable`
- `insufficient_monthly_coverage`
- `right_value_unavailable`

### Coherence Outcomes

Applicable comparisons are classified as:

- `within_expected_range`
- `large_difference`

The thresholds are prototype parameters calibrated to the controlled synthetic environment.

They are **not official statistical thresholds**.

### Materiality-Based Review Prioritization

Large differences are further evaluated according to within-rule materiality.

This produces a review queue that prioritizes more consequential discrepancies rather than treating all differences equally.

The current synthetic run produces:

```text
145 materiality-prioritized review cases
```

This demonstrates how statistical quality review can be made more focused and traceable.

---

## 📊 9. Annual Enterprise Indicators

`R/06_compute_indicators.R` transforms monthly observations into annual enterprise-year measures.

Annual measures require complete monthly coverage.

For each enterprise-year, the workflow derives:

- annual turnover,
- annual-average employment,
- turnover per employee,
- turnover completeness,
- employment completeness,
- and joint completeness.

Current annual coverage is:

| Year | Enterprises | Complete Turnover | Complete Employment | Complete Both |
|---:|---:|---:|---:|---:|
| 2023 | 1,500 | 1,442 | 1,452 | 1,395 |
| 2024 | 1,500 | 1,468 | 1,462 | 1,430 |
| 2025 | 1,500 | 1,449 | 1,446 | 1,396 |

Aggregate indicators are produced for:

- sectors,
- regions,
- and sector × region combinations.

Examples include:

- total turnover,
- average turnover per enterprise,
- total annual-average employment,
- average employment per enterprise,
- enterprise counts,
- completeness counts,
- and turnover per employee.

For turnover per employee, numerator and denominator are calculated over the **same complete-observation population**.

---

## 📈 10. Analytical Outputs & Visualization

The final stage produces selected aggregated analytical outputs and visualizations.

Current figures include:

- monthly total turnover,
- annual turnover by sector,
- turnover per employee by sector,
- coherence outcomes.

Generated figures are stored in:

```text
output/figures/
```

Selected committed figures:

```text
annual_turnover_by_sector.png
coherence_outcomes.png
monthly_turnover_total.png
turnover_per_employee_by_sector.png
```

Selected aggregate tables are stored in:

```text
output/tables/
```

including:

```text
indicators_region.csv
indicators_sector.csv
indicators_sector_region.csv
linkage_evaluation.csv
imputation_evaluation.csv
```

Enterprise-level generated datasets remain untracked.

---

## 🧪 11. Automated Tests

The repository contains a lightweight automated test suite based on Base R assertions.

Run:

```bash
Rscript tests/run_tests.R
```

Current test files are:

```text
test_coherence_outputs.R
test_indicator_outputs.R
test_linkage_outputs.R
test_linkage_similarity.R
test_plausibility.R
```

The tests cover:

- bounded interpolation behavior,
- linkage text normalization,
- similarity calculations,
- linkage crosswalk integrity,
- deterministic and similarity linkage rules,
- coherence applicability,
- coherence classification,
- materiality-based review selection,
- annual completeness,
- enterprise-level indicator semantics,
- and aggregate common-population calculations.

All automated tests pass on the current workflow.

---

## 🔍 12. Synthetic-Truth Method Evaluation

The data generator creates hidden synthetic truth under:

```text
data/truth/
```

The generated truth CSV files are ignored by Git.

Operational scripts `02`–`07` do not use hidden truth.

Method evaluation is performed separately:

```bash
Rscript evaluation/evaluate_methods.R
```

### Linkage Evaluation

The evaluation distinguishes:

- assignment coverage,
- accuracy among assigned records,
- and overall correct-assignment rate.

For the current controlled synthetic dataset:

```text
4,500 / 4,500 non-register source records assigned
4,500 / 4,500 assignments correct
```

This includes both deterministic and weighted similarity matches.

The result should be interpreted only within the controlled synthetic design and does not imply equivalent performance on real enterprise or administrative data.

### Imputation Evaluation

Only observations actually imputed by the operational workflow are evaluated.

Current results are:

| Source | Variable | Imputed | MAE | Median Absolute Error | MAPE |
|---|---|---:|---:|---:|---:|
| Employment | employees | 476 | 1.47 | 1.00 | 3.79% |
| Turnover | turnover | 495 | 44,633 | 12,433 | 6.80% |

Missing register employment is not part of the imputation evaluation because these observations are routed to review rather than imputed.

Aggregate evaluation outputs are stored in:

```text
output/tables/linkage_evaluation.csv
output/tables/imputation_evaluation.csv
```

---

## 🔄 13. Reproducibility With `renv`

The project uses `renv` to provide a reproducible R package environment.

The current lockfile targets:

```text
R 4.4.2
```

After cloning the repository, open R from the project root.

Check the environment:

```r
renv::status()
```

If the declared package environment needs to be restored:

```r
renv::restore()
```

The repository has been tested through a clean end-to-end rebuild starting without generated intermediate CSV files.

Using the fixed synthetic-data seed, the complete workflow, tests, evaluation tables, and committed figures reproduce deterministically.

The currently committed aggregate tables and figures were reproduced **byte-for-byte** during the final clean reproducibility check.

---

## ▶️ 14. How to Run the Pipeline

### Step 1 — Restore the R Environment if Required

From R:

```r
renv::restore()
renv::status()
```

### Step 2 — Run the Operational Pipeline

From the project root:

```bash
Rscript R/01_generate_synthetic_data.R
Rscript R/02_clean_and_validate_data.R
Rscript R/03_link_sources.R
Rscript R/04_integrate_sources.R
Rscript R/05_check_coherence.R
Rscript R/06_compute_indicators.R
Rscript R/07_visualize_results.R
```

### Step 3 — Run Automated Tests

```bash
Rscript tests/run_tests.R
```

### Step 4 — Run Synthetic-Truth Evaluation

```bash
Rscript evaluation/evaluate_methods.R
```

Generated data appear under:

```text
data/raw/
data/clean/
data/processed/
data/truth/
```

Selected reproducible outputs appear under:

```text
output/tables/
output/figures/
```

---

## ⚠️ 15. Methodological Scope & Limitations

This repository is intended as a methodological demonstration of selected enterprise-statistics integration concepts using fully synthetic data.

It does not aim to reproduce internal official-statistics production systems, institutional infrastructures, or real enterprise-statistics workflows in their full methodological complexity.

### Synthetic Data Environment

The repository contains:

- no real enterprise data,
- no administrative records,
- no confidential statistical information,
- and no internal institutional data.

The synthetic environment is constructed solely to demonstrate methodological concepts.

### Simplified Statistical Units

The current workflow primarily uses a simplified enterprise-level statistical unit.

It does not explicitly model:

- enterprise groups,
- legal-unit hierarchies,
- local units,
- multi-entity reporting structures,
- or consolidated enterprise-group accounting.

### Simplified Statistical Methodology

The project does not implement:

- formal survey sampling,
- survey weighting,
- calibration,
- benchmarking frameworks,
- official revision procedures,
- formal seasonal adjustment,
- or disclosure-control systems.

### Prototype Coherence Rules

Cross-source coherence thresholds are prototype parameters developed for the controlled synthetic environment.

They are not official thresholds and should not be interpreted as institutional production rules.

### Record-Linkage Evaluation

The linkage procedure performs perfectly on the current synthetic truth.

This reflects the controlled design of the generated data and should not be generalized to real administrative or enterprise datasets.

### Editing & Imputation

Interpolation is intentionally conservative and limited to internal monthly gaps with temporal support on both sides.

Cross-sectional register employment gaps are routed to review rather than being filled with weak peer-based substitutes.

### Institutional Infrastructure

The repository does not attempt to emulate:

- secure statistical production platforms,
- internal administrative-data architectures,
- institutional metadata systems,
- official publication infrastructures,
- or confidential-data environments.

The project is best understood as a transparent methodological and portfolio demonstration.

---

## 🔭 16. Possible Methodological Extensions

Possible future extensions could include:

- more complex statistical-unit relationships,
- enterprise-group and legal-unit structures,
- formal sampling and weighting procedures,
- benchmarking and revision frameworks,
- seasonal adjustment,
- more difficult ambiguous linkage scenarios,
- probabilistic or model-assisted linkage comparisons,
- additional source-quality indicators,
- longitudinal revision histories,
- formal disclosure-control demonstrations,
- and richer metadata-governance structures.

The repository intentionally prioritizes methodological transparency and reproducibility over implementation complexity.

Future extensions should therefore add methodological value rather than complexity for its own sake.

---

## 📘 17. License

**MIT License**

---

## 👤 18. Author

**Golib Sanaev**

Data Analyst & Applied Data Scientist<br>
Econometrics • Statistical Modelling • Enterprise Statistics

**GitHub:** [@gsanaev](https://github.com/gsanaev)<br>
**Email:** gsanaev80@gmail.com<br>
**LinkedIn:** [golib-sanaev](https://linkedin.com/in/golib-sanaev)

---

## 📚 Citation

> Sanaev, G. (2026). *Multisource Enterprise Statistics Integration Workflow in R.*<br>
> GitHub Repository: [https://github.com/gsanaev/business-data-integration](https://github.com/gsanaev/business-data-integration)
