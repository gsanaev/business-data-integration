# 📊 Business Data Integration & Economic Indicators Pipeline in R

This project demonstrates how multiple firm-level data sources can be cleaned, harmonized, linked, and integrated into a coherent analytical dataset for producing structural economic indicators.

It reflects typical workflows in **business statistics, structural reporting**, and **multi-source statistical production**, such as those used in official statistics (e.g., SysdU, structural and short-term statistics, administrative data integration).

All data used here are **synthetic**, but generated to mimic realistic structures, inconsistencies, and integration challenges.


## 1. Project Overview  

Modern statistical production frequently relies on multiple heterogeneous data sources—administrative registers, business surveys, and accounting data. These sources differ in:

- variable definitions
- identifier structures
- classification systems
- degrees of completeness
- reporting accuracy

This project provides a reproducible pipeline in **R** that:
- cleans and validates source datasets
- harmonizes sector/classification variables
- links identifiers across sources
- resolves discrepancies using precedence rules
- evaluates data quality
- produces sectoral and regional economic indicators

The workflow mirrors real-world processes in statistical offices preparing multi-source structural business statistics.


## 2. Repository Structure  
```text
project/
├── R/
│   ├── 01_generate_synthetic_data.R
│   ├── 02_clean_and_validate_data.R
│   ├── 03_integrate_sources.R
│   ├── 04_compute_indicators.R
│   └── 05_visualize_results.R
│
├── data/
│   ├── raw/        # synthetic source datasets
│   └── processed/  # cleaned & integrated datasets
│
├── output/
│   ├── figures/
│   └── tables/
│
└── README.md
```

## 3. Synthetic Datasets

The project uses three synthetic input datasets reflecting typical structures of business and administrative statistics.

## Dataset A – Administrative Data (Business Register-like)  

Variables include:  
- `firm_id`  
- `nace2` (sector classification)  
- `region`  
- `employees`  
- `turnover_admin`  
Includes realistic imperfections: missing values, mild outliers, and coding inconsistencies.

## Dataset B – Survey Data (Sample of Firms)  

Variables include:  
- `firm_id` (subset of A + some survey-only firms)  
- `turnover_survey`  
- `employees_survey`  
- `investment`  
- `it_spending`  
Typical patterns: rounding, under-reporting, selective non-response.  

## Dataset C – Accounting Extract  

Variables include:
- `internal_id` → requires mapping to `firm_id`  
- `branch_code` → must be harmonized to `nace2`  
- `revenue`  
- `wage_bill`  
- `costs_total`  
Includes: implausible ratios, classification inconsistencies, and ID mismatches.    


## 4. Methods & Workflow

### Step 1 — Data Cleaning
- Missing-value treatment
- Outlier detection
- Standardization of variable formats
- Removal of implausible observations
- Structural validation checks

### Step 2 — Classification Harmonization
- Map `branch_code` → `nace2` using a synthetic lookup table
- Resolve minor inconsistencies
- Flag ambiguous cases

### Step 3 — Identifier Linking
- Convert `internal_id` to `firm_id` using a crosswalk
- Flag unmatched or conflicting identifiers
- Create a unified `firm_id` key across all datasets

### Step 4 — Multi-Source Integration
A rule-based integration replicates typical statistical office procedures:
- **Turnover**: administrative → accounting → survey
- **Employees**: administrative → survey
- **Costs / wage bill**: accounting preferred
- **Investment / IT spending**: survey only

All conflicts are recorded in diagnostic tables.

### Step 5 — Data Quality Checks
- Missingness summaries
- Range and distribution comparisons
- Cross-source coherence checks
- Internal consistency ratios (e.g., costs/turnover)

### Step 6 — Indicator Computation
Indicators are produced by **sector** and **region**:
- average & median turnover
- employment levels
- investment intensity
- IT adoption rate
- wage share (wage_bill / revenue)

### Step 7 — Visualization
Using `ggplot2`:
- firm size distribution
- turnover distributions across sources
- sector-level indicator plots

## 5. Integration Pipeline — Harmonization & Economic Indicators

This section provides a concise, recruiter-friendly overview of the core integration logic implemented in the project.

### 5.1 Integration Logic

The integration pipeline mirrors common procedures in multi-source business statistics:

**1. Standardization** of keys and classifications
**2. Matching** accounting → administrative identifiers
**3. Consolidation** of variable definitions
**4. Precedence rules** to resolve contradictory information
**5. Documentation** of decisions and discrepancies

Every integrated variable includes metadata on:
- original source
- reliability / completeness
- whether conflicts were resolved

### 5.2 Harmonization

- All sector variables converted to *NACE Rev.2 style* codes
- Branch codes mapped using a synthetic lookup
- Region codes normalized to consistent formats

### 5.3 Economic Indicator Framework

The integrated dataset supports robust statistical indicators:

| **Indicator**	| Description |
|---------------|-------------|
| **Structural turnover** |	Sectoral & regional aggregates |
| **Employment structure** | Workforce distribution, mean/median values |
| **Investment intensity** | Investment / turnover |
| **IT adoption** | Share of firms with reported IT spending |
| **Wage share** | wage_bill / revenue |

The goal is to replicate analytical outputs found in structural business statistics or enterprise statistics programs.

## 6. Technologies Used

**R (>=4.4)**
- `dplyr`, `tidyr` — data wrangling
- `readr` — I/O
- `stringr` — cleaning
- `purrr` — applied pipelines
- `ggplot2` — visualization
- `janitor` — data standardization

The project runs identically in:
- VS Code
- RStudio
- Terminal R sessions

## 7. How to Run the Pipeline
```r
# 1. Generate synthetic datasets
source("R/01_generate_synthetic_data.R")

# 2. Clean datasets
source("R/02_clean_and_validate_data.R")

# 3. Integrate data sources
source("R/03_integration_pipeline.R")

# 4. Compute indicators
source("R/04_compute_indicators.R")

# 5. Visualize results
source("R/05_visualize_results.R")
```

Outputs are written to:
- `data/processed/`
- `output/tables/`
- `output/figures/`

## 8. Possible Extensions

- Probabilistic record linkage
- Time-series extension to multiple years
- Advanced imputation strategies
- Consistency benchmarking algorithms
- Regionalization models
- Alternative classification systems

## 9. License

MIT License

## 10. Author

**Golib Sanaev**

## 📚 Citation
> Sanaev, G. (2025). *Business Data Integration & Economic Indicators Pipeline in R.*  
> GitHub Repository: [https://github.com/gsanaev/business-data-integration](https://github.com/gsanaev/business-data-integration)

---

## 📞 Contact

**GitHub:** [@gsanaev](https://github.com/gsanaev)  
**Email:** gsanaev80@gmail.com  
**LinkedIn:** [golib-sanaev](https://linkedin.com/in/golib-sanaev)