<div align="center">

<img src="man/figures/logo.svg" width="400px"/>

# ancovEE

### EE · ANCOVA · NORMALIZATION

**An R package for energy expenditure analysis in mouse metabolic phenotyping studies**

[![R](https://img.shields.io/badge/R-%3E%3D4.0-blue)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![MMPC](https://img.shields.io/badge/Framework-MMPC-green)](https://www.mmpc.org)

</div>

---

## Overview

`ancovEE` provides a complete, reproducible pipeline for energy expenditure (EE) analysis in mouse metabolic phenotyping studies. It follows the statistical framework established by the [NIDDK Mouse Metabolic Phenotyping Centers (MMPC)](https://www.mmpc.org) and is designed for use with data from:

- **Bruker minispec NMR** body composition analyzer
- **Promethion** indirect calorimetry system (Sable Systems)

The package implements ANCOVA-based EE adjustment for body mass/composition, which is statistically superior to traditional ratio normalization (EE/mass). See [Module 1](https://www.mmpc.org) for an explanation of why ratio normalization is invalid.

---

## Workflow

The full pipeline runs in 6 steps:

```
Step 1  →  Parse NMR body composition file        → NMR_Lean_Output.xlsx
Step 2  →  Parse Promethion IC file               → Promethion_Output.xlsx
Step 3  →  EE averages + cage assignment          → EE_Summary.xlsx
Step 4  →  Prepare ANCOVA input                   → ANCOVA_Input.csv
Step 5  →  Run ANCOVA analysis + plots            → ANCOVA_Results.xlsx + ANCOVA_Plots.pdf
Step 6  →  Normalize for PRISM                    → Normalized_Output.xlsx
```

---

## Installation

### Prerequisites

```r
install.packages(c("devtools", "readxl", "openxlsx", "ggplot2", "gridExtra"))
```

### Install from GitHub

```r
devtools::install_github("akumar901/ancovEE")
```

### Install from local folder

```r
devtools::install("/path/to/ancovEE")
```

---

## Quick Start

The easiest way to run the full pipeline is using the provided R Markdown template (`ancovEE_analysis.Rmd`). Open it in RStudio, update the file paths, and run each chunk in order.

```r
library(ancovEE)

# Step 1 - Parse NMR body composition
lean_data <- run_step1(
  input_file  = "path/to/NMR_INPUT.xlsx",
  output_file = "path/to/NMR_Lean_Output.xlsx"
)

# Step 2 - Parse Promethion indirect calorimetry
result <- run_step2(
  input_file  = "path/to/Promethion.xlsx",
  output_file = "path/to/Promethion_Output.xlsx"
)

# Step 3 - EE summary + cage assignment
EE <- run_step3(
  promethion_file = "path/to/Promethion_Output.xlsx",
  nmr_file        = "path/to/NMR_Lean_Output.xlsx",
  output_xlsx     = "path/to/EE_Summary.xlsx"
)

# Step 4 - Prepare ANCOVA input
ancova_data <- run_step4(
  ee_file    = "path/to/EE_Summary.xlsx",
  output_csv = "path/to/ANCOVA_Input.csv"
)

# Step 5 - Run ANCOVA analysis
results <- run_step5(
  input_file  = "path/to/ANCOVA_Input.csv",
  output_xlsx = "path/to/ANCOVA_Results.xlsx",
  output_pdf  = "path/to/ANCOVA_Plots.pdf"
)

# Step 6 - Normalize for PRISM
norm <- run_step6(
  promethion_file   = "path/to/Promethion_Output.xlsx",
  ee_summary_file   = "path/to/EE_Summary.xlsx",
  ancova_input_file = "path/to/ANCOVA_Input.csv",
  output_xlsx       = "path/to/Normalized_Output.xlsx"
)
```

---

## Step-by-Step Guide

### Step 1 — NMR Body Composition

Reads a Bruker minispec NMR Excel export and dynamically locates the **Lean** compound block regardless of column position. Extracts `Sample_Name`, `Compound`, `Mass` and `Unit`.

```r
lean_data <- run_step1(
  input_file  = "NMR_INPUT.xlsx",
  output_file = "NMR_Lean_Output.xlsx"
)
```

**Output:** `NMR_Lean_Output.xlsx` with columns `Sample_Name`, `Compound`, `Mass`, `Unit`

---

### Step 2 — Promethion Indirect Calorimetry

Reads the **Macro13** sheet from a Promethion/Sable Systems Excel export. Prompts for a datetime range and extracts VO2, VCO2, RER and kcal_hr for all chambers.

```r
result <- run_step2(
  input_file  = "Promethion.xlsx",
  output_file = "Promethion_Output.xlsx"
)
```

**Output:** `Promethion_Output.xlsx` with 4 sheets: `VO2`, `VCO2`, `RER`, `kcal_hr`

---

### Step 3 — EE Summary and Cage Assignment

Calculates `Avg_kcal_hr` and `Avg_kcal_day` per cage. Interactively maps each cage position to an animal ID from the NMR file. Includes a confirmation step to catch assignment errors.

```r
EE <- run_step3(
  promethion_file = "Promethion_Output.xlsx",
  nmr_file        = "NMR_Lean_Output.xlsx",
  output_xlsx     = "EE_Summary.xlsx"
)
```

**Output:** `EE_Summary.xlsx` with 2 sheets: `EE_Summary`, `Cage_Assignment`

---

### Step 4 — Prepare ANCOVA Input

Extracts `Mouse_ID`, `Avg_kcal_day` and `Lean_Mass_g` from `Cage_Assignment`, renames them to `Group`, `EE` and `LBM`. Uses a **persistent diet directory** (`~/.ancovEE/diet_directory.json`) to automatically recognize group names (HFD, CHOW, HFLP, HFLAA etc.).

```r
ancova_data <- run_step4(
  ee_file    = "EE_Summary.xlsx",
  output_csv = "ANCOVA_Input.csv"
)
```

**Output:** `ANCOVA_Input` sheet added to `EE_Summary.xlsx` + `ANCOVA_Input.csv`

#### Diet Directory

The package maintains a persistent diet directory that remembers group name mappings across sessions:

```r
# View current directory
view_diet_directory()

# Reset to defaults (HFD, HFLAA, HFLP, CHOW, CHO, CHW, CWO)
reset_diet_directory()
```

---

### Step 5 — ANCOVA Analysis

Runs the full ANCOVA following the MMPC framework:

1. Tests whether slopes of EE on LBM differ between groups (interaction test)
2. Fits **standard ANCOVA** (parallel slopes) if interaction p > 0.05
3. Fits **interaction model** (separate slopes) if interaction p ≤ 0.05
4. Reports adjusted means, group differences, p-values and residual diagnostics
5. Generates regression plot and dfbeta diagnostic plots

```r
results <- run_step5(
  input_file  = "ANCOVA_Input.csv",
  output_xlsx = "ANCOVA_Results.xlsx",
  output_pdf  = "ANCOVA_Plots.pdf"
)
```

**Output:**
- `ANCOVA_Results.xlsx` with 3 sheets:
  - `ANCOVA_Output` — full results matching MMPC output format
  - `Residual_Diagnostics` — dfbeta values per animal
  - `Interpretation` — plain-English explanation of every result
- `ANCOVA_Plots.pdf` — regression plot (top) + dfbeta LBM and Group plots (side by side)

---

### Step 6 — Normalize for PRISM

Normalizes VCO2, VO2 and kcal_hr by lean body mass (LBM) for every individual timepoint. RER is copied as-is (already a ratio). Columns are named using Mouse_ID and ordered by group for direct copy-paste into GraphPad PRISM.

```r
norm <- run_step6(
  promethion_file   = "Promethion_Output.xlsx",
  ee_summary_file   = "EE_Summary.xlsx",
  ancova_input_file = "ANCOVA_Input.csv",
  output_xlsx       = "Normalized_Output.xlsx"
)
```

**Output:** `Normalized_Output.xlsx` with 4 sheets:
- `VCO2_norm` — columns named `VCO2_MouseID_per_LBM`, grouped by diet
- `VO2_norm` — columns named `VO2_MouseID_per_LBM`, grouped by diet
- `kcal_hr_norm` — columns named `kcal_hr_MouseID_per_LBM`, grouped by diet
- `RER` — copied as-is, reordered by group

---

## Statistical Background

`ancovEE` implements the ANCOVA framework described in the MMPC Energy Expenditure Analysis modules:

- **Module 1:** Why ratio normalization (EE/mass) is statistically invalid
- **Module 2:** Introduction to ANCOVA for EE analysis
- **Module 3:** Standard ANCOVA (homogeneous slopes)
- **Module 4:** Interaction model (heterogeneous slopes) + Johnson-Neyman technique
- **Module 5:** Statistical formulas

### Key assumptions of ANCOVA
1. Regression slopes of EE on body mass covariate are the same for both groups (tested automatically)
2. Residuals are normally distributed
3. Variances within treatment groups are similar
4. Linear relationship between EE and the covariate within the measured range

---

## Acknowledgement

If you use `ancovEE` in your research, please acknowledge:

> *"The EE ANCOVA analysis was performed using the ancovEE R package, following the NIDDK Mouse Metabolic Phenotyping Centers (MMPC) Energy Expenditure Analysis framework (www.mmpc.org), supported by grants DK076169 and DK115255."*

### MMPC References

- Kaiyala et al. (2010) *Diabetes* 59:1657-1666
- Kaiyala & Schwartz (2011) *Diabetes* 60:17-23
- Kaiyala (2014) *PLOS ONE* 9(7)

---

## Dependencies

| Package | Use |
|---------|-----|
| `readxl` | Read Excel input files |
| `openxlsx` | Write formatted Excel output files |
| `ggplot2` | Regression and diagnostic plots |
| `gridExtra` | Multi-panel plot layout |

---

## License

MIT © ancovEE authors

---

<div align="center">
<sub>Built for mouse metabolic phenotyping research · Follows NIDDK MMPC framework</sub>
</div>
