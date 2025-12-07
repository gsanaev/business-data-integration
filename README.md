# 📊 Business Data Integration & Economic Indicators Pipeline in R

*A Reproducible Workflow for Multi-Source Firm-Level Statistics*

![made-with-R](https://img.shields.io/badge/Made%20with-R-276DC2.svg)
![license](https://img.shields.io/badge/license-MIT-green.svg)

This project demonstrates how to integrate heterogeneous business datasets into a coherent analytical framework for economic statistics.
It reflects challenges commonly encountered in official statistics (e.g., Destatis, Eurostat): inconsistent identifiers, divergent variable definitions, missing values, measurement errors, and discrepancies across data sources.

The project uses fully synthetic data to illustrate transparent, reproducible methods for:
- data generation
- cleaning & validation
- harmonization across sources
- statistical integration
- computation of structural indicators
- visualization of economic patterns

All code is implemented in R using a modular pipeline suitable for production-like environments.


## 🚀 1. Project Overview  

Modern economic statistics increasingly rely on **multi-source integration**:
administrative business registers, structural business surveys, short-term indicators, and accounting extracts.

Such sources differ in:
- reporting frequency
- timeliness
- variable definitions
- quality and completeness
- industry and regional classification detail

This project provides a compact but realistic framework to:
1. Generate synthetic firm-level source datasets
2. Clean and validate each dataset
3 Link identifiers and harmonize classifications
4. Integrate monthly panels across sources
5. Compute economic indicators at firm, sector, and regional level
6. Produce reproducible tables and visualizations


## 2. Repository Structure

```text
business-data-integration/
├── data
│   ├── clean               # cleaned intermediate data
│   ├── processed           # unified firm-level panel (analysis-ready)
│   └── raw                 # synthetic raw datasets (generated)
├── LICENSE
├── output
│   ├── figures             # visualizations
│   └── tables              # aggregated indicators
├── R
│   ├── 01_generate_synthetic_data.R
│   ├── 02_clean_and_validate_data.R
│   ├── 03_integrate_sources.R
│   ├── 04_compute_indicators.R
│   └── 05_visualize_results.R
├── README.md
├── renv
│   ├── activate.R
│   ├── library
│   ├── settings.json
│   └── staging
└── renv.lock
```
**Reproducibility**: The project uses `renv` for a full dependency snapshot.

**🔄 Reproducibility With** `renv`

This project uses **renv** to ensure that anyone who clones the repository obtains exactly the same R package environment.

Before running the pipeline for the first time, start R inside the project directory and check the environment:
```r
renv::status()
```
If packages need to be restored, run:
```r
renv::restore()
```
This guarantees that all scripts operate identically across machines.

## 🧪 3. Synthetic Data Sources

Three realistic (but fully artificial) firm-level datasets are generated:

### A) Administrative Business Register

Variables:
- `firm_id`
- `region_code`
- `nace_code`
- `legal_form`
- `employees`
- `revenue_last_year`

Intentionally includes:
- missing values
- negative values
- inconsistent reporting patterns

### B) Monthly Employment Survey

Panel data for Jan–Dec 2023:
- `firm_id`
- `month`
- `employees`
- synthetic missingness for interpolation
- regional & sector attributes copied from the register

**⭐ Industry-Specific Seasonal Patterns (Added Realism)**

The monthly employment dataset includes **sector-specific seasonal variation**, reflecting realistic trends observed in economic statistics:
- **Retail (G47)** — strong December activity
- **Accommodation & Food (I55, I56)** — summer employment peaks
- **Manufacturing (C10, C29)** — mild seasonal movement
- **Transport (H49)** — steady with slight autumn increases

Seasonality is introduced using multiplicative adjustment factors, producing more realistic monthly employment curves.

### C) Monthly Turnover Survey

Variables:
- `firm_id`
- `month`
- `turnover`
- missing values for interpolation
- moderate log-normal variability


## 🔧 4. Methods & Workflow

### Pipeline Diagram
```markdown
                 ┌────────────────────────────┐
                 │ 01_generate_synthetic_data │
                 └──────────────┬─────────────┘
                                ▼
                 ┌────────────────────────────┐
                 │ 02_clean_and_validate_data │
                 └──────────────┬─────────────┘
                                ▼
                 ┌────────────────────────────┐
                 │   03_integrate_sources     │
                 └──────────────┬─────────────┘
                                ▼
                 ┌────────────────────────────┐
                 │   04_compute_indicators    │
                 └──────────────┬─────────────┘
                                ▼
                 ┌────────────────────────────┐
                 │   05_visualize_results     │
                 └────────────────────────────┘
```

### Step 1 — Data Cleaning & Validation
- structural key validation
- detection of inconsistencies (negative values, missingness)
- industry/region-based imputation
- time-series interpolation (approximation)

### Step 2 — Harmonization & Identifier Mapping
- standardization of variable names
- controlled join logic between datasets
- date harmonization
- preparation of unified firm-month records

### Step 3 — Multi-Source Integration
For each firm × month:
- turnover precedence rules
- employment precedence rules
- consistency checks
- generation of firm-level indicators

### Step 4 — Derived Indicators
Computed at firm level:
- turnover YoY growth
- monthly employment growth
- labor productivity
- simple seasonal index

Computed at sector/region level:
- total turnover
- average turnover per firm
- total employees
- productivity aggregates
- firm counts

### Step 5 — Visualization
- firm size distribution
- sectoral turnover profiles
- monthly aggregate turnover trends
Visual outputs stored in `output/figures/`.

## 🛠 5. Technologies Used

- **R**
    -`dplyr`, `tidyr` — reshaping, joins, aggregation
    - `lubridate` — date manipulation
    - `ggplot2` — visualization
    - `readr` — data I/O
    - `janitor` — column cleaning
    - `purrr` — functional utilities
- **renv** for reproducibility
- Supports **VS Code, RStudio**, and command-line R

## ▶️ 6. How to Run the Pipeline

**🔧 Before Running the Pipeline**

Start R in the project root and ensure the correct environment is active:
```r
renv::status()
```
If packages are missing:
```r
renv::restore()
```
Then proceed with the pipeline steps below.
```r
# 1. Generate synthetic data
source("R/01_generate_synthetic_data.R")

# 2. Clean & validate
source("R/02_clean_and_validate_data.R")

# 3. Integrate sources & build indicators
source("R/03_integrate_sources.R")

# 4. Compute sectoral and regional aggregates
source("R/04_compute_indicators.R")

# 5. Produce visualizations
source("R/05_visualize_results.R")
```

Outputs appear in:
- `data/clean/`
- `data/processed/`
- `output/tables/`
- `output/figures/`

## 🔭 7. Possible Extensions

Future enhancements might include:
- probabilistic record linkage
- multiple-year panels
- Monte Carlo simulations
- machine learning–based imputation
- benchmarking algorithms for multi-source coherence (e.g. Denton, Chow-Lin)
- firm-level microdata anonymization techniques

## 📘 8. License

**MIT License**

## 👤 9. Author

**Golib Sanaev**
Applied Data Scientist & Analyst | ML • Data Analysis • Forecasting • Python • SQL • Econometrics

**GitHub:** [@gsanaev](https://github.com/gsanaev)  
**Email:** gsanaev80@gmail.com  
**LinkedIn:** [golib-sanaev](https://linkedin.com/in/golib-sanaev)


## 📚 Citation
> Sanaev, G. (2025). *Business Data Integration & Economic Indicators Pipeline in R.*  
> GitHub Repository: [https://github.com/gsanaev/business-data-integration](https://github.com/gsanaev/business-data-integration)

