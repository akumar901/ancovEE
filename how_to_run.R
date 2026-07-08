# =============================================================================
# ancovEE Package — How to Install and Run Step 1
# =============================================================================
#
# STEP 1: Install dependencies (run once)
# =============================================================================
install.packages("devtools")
install.packages(c("readxl", "openxlsx"))

# =============================================================================
# STEP 2: Install ancovEE (run once, re-run when package is updated)
# =============================================================================

# Unzip ancovEE.zip first, then point to the folder:
devtools::install("/path/to/ancovEE")   # <- change this to your actual path
# e.g. on Mac:     devtools::install("/Users/AMARMAC/Downloads/ancovEE")
# e.g. on Windows: devtools::install("C:/Users/YourName/Downloads/ancovEE")

# =============================================================================
# STEP 3: Every future R session — just these two lines
# =============================================================================
library(ancovEE)

lean_data <- run_step1(
  input_file  = "/path/to/NMR_INPUT.xlsx",    # <- your NMR file
  output_file = "/path/to/NMR_Lean_Output.xlsx"  # <- where to save
)

# =============================================================================
# Inspect results
# =============================================================================

# View the full table
print(lean_data)

# How many animals per group?
table(lean_data$Group)

# Mean lean mass by group
tapply(lean_data$Mass, lean_data$Group, mean)
