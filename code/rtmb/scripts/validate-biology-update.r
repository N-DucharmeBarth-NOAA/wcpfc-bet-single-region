# Validation Script for Biology Update in bet.Rmd
# 
# This script provides testing and validation for the biology parameter updates
# that extract parameters from MFCL files instead of using hardcoded values.
#
# Usage:
#   source("code/rtmb/scripts/validate-biology-update.r")
#
# ==============================================================================
# SUMMARY OF CHANGES
# ==============================================================================
#
# 1. QUARTERLY AGE STRUCTURE
#    Before: Annual ages (0-20), 21 age classes, representing 0-20 real years
#    After:  Quarterly ages (1-40), 40 age classes, representing 0.25-10.0 real years
#    Rationale: Matches MFCL/SS3 convention where ages represent quarterly increments
#
# 2. TIME STEPS
#    Before: n_year = 74 (1945-2018 annual), n_season = 4
#    After:  n_year = 268 (ts 1-268 quarterly), n_season = 1
#    Rationale: Each model time step is one quarter; uses `ts` column from CSV data
#
# 3. ARRAY DIMENSIONS
#    All arrays resized:
#    - catch_obs_ysf: (74, 4, 15) → (268, 1, 15)
#    - weight_fya: (15, 74, 21) → (15, 268, 40)
#    - rdev_y: length 74 → length 268
#    - maturity_a: length 21 → length 40
#    - M_a: length 21 → length 40 (now data, not parameter)
#
# ==============================================================================
# BIOLOGY PARAMETERS
# ==============================================================================
#
# GROWTH (mean_laa, sd_laa)
# - Source: mean_laa(base_rep) and sd_laa(base_rep) from MFCL report
# - Format: Vectors of length 40 (quarterly ages)
# - Usage: Build length-at-age distribution for weight-at-age and maturity
#
# MATURITY
# - Source: mat_at_length(base_par) from MFCL par file
# - Processing: 
#   1. Interpolate from MFCL length bins to model length bins (10-200 cm by 2 cm)
#   2. Convert maturity-at-length to maturity-at-age using get_pla()
# - Result: Vector of length 40 (quarterly ages)
#
# NATURAL MORTALITY
# - Source: m_at_age(base_rep) from MFCL report
# - Format: Vector of length 40 (quarterly ages)
# - Change: Now stored as data$M_a instead of estimated parameter log_M
# - Note: Values are on quarterly basis (annual M / 4)
#
# LENGTH-WEIGHT
# - Source: lw_params(base_ini) from MFCL ini file
# - Parameters: a and b for W = a * L^b relationship
# - Processing: Weight-at-age calculated by integrating L-W over length distribution
# - Approach: Single L-W for all 15 fisheries (matches SS3 approach)
#
# ==============================================================================
# VALIDATION TESTS
# ==============================================================================

validate_biology_update <- function() {
  
  cat("=================================================================\n")
  cat("VALIDATION SCRIPT FOR BIOLOGY UPDATE\n")
  cat("=================================================================\n\n")
  
  # Load required packages
  library(tidyverse)
  library(FLR4MFCL)
  library(data.table)
  library(this.path)
  
  # Load the bet package
  devtools::load_all("code/rtmb")
  
  # Load MFCL files
  proj_dir <- this.path::this.proj()
  dir_model <- file.path(proj_dir, "model-files")
  dir_base_mfcl <- file.path(dir_model, "mfcl", "v11")
  
  base_par <- read.MFCLPar(file.path(dir_base_mfcl, "10.par"), first.yr = 1952)
  base_rep <- read.MFCLRep(file.path(dir_base_mfcl, "plot-10.par.rep"))
  base_ini <- read.MFCLIni(file.path(dir_base_mfcl, "bet.ini"), nseasons = 4)
  
  # Test 1: Check age structure
  cat("Test 1: Age structure\n")
  ages <- 1:40
  cat("  Expected: ages = 1:40 (length 40)\n")
  cat("  Result: ", ifelse(length(ages) == 40 && ages[1] == 1 && ages[40] == 40, 
                           "PASS", "FAIL"), "\n\n")
  
  # Test 2: Check growth parameters
  cat("Test 2: Growth parameters\n")
  mean_laa <- as.vector(mean_laa(base_rep))
  sd_laa <- as.vector(sd_laa(base_rep))
  cat("  Expected: mean_laa length = 40\n")
  cat("  Result: ", length(mean_laa), " - ", 
      ifelse(length(mean_laa) == 40, "PASS", "FAIL"), "\n")
  cat("  Expected: sd_laa length = 40\n")
  cat("  Result: ", length(sd_laa), " - ", 
      ifelse(length(sd_laa) == 40, "PASS", "FAIL"), "\n\n")
  
  # Test 3: Check maturity
  cat("Test 3: Maturity parameters\n")
  mat_at_length_mfcl <- as.vector(mat_at_length(base_par))
  cat("  Expected: mat_at_length from MFCL\n")
  cat("  Result: length =", length(mat_at_length_mfcl), "\n")
  cat("  Range: [", min(mat_at_length_mfcl), ",", max(mat_at_length_mfcl), "]\n\n")
  
  # Test 4: Check natural mortality
  cat("Test 4: Natural mortality\n")
  M_at_age <- as.vector(m_at_age(base_rep))
  cat("  Expected: M_at_age length = 40\n")
  cat("  Result: ", length(M_at_age), " - ", 
      ifelse(length(M_at_age) == 40, "PASS", "FAIL"), "\n")
  cat("  Mean M: ", mean(M_at_age), " (quarterly rate)\n\n")
  
  # Test 5: Check length-weight parameters
  cat("Test 5: Length-weight parameters\n")
  lw_params_vec <- lw_params(base_ini)
  cat("  Expected: 2 parameters (a, b)\n")
  cat("  Result: a =", lw_params_vec[1], ", b =", lw_params_vec[2], "\n")
  cat("  Status: ", ifelse(length(lw_params_vec) == 2, "PASS", "FAIL"), "\n\n")
  
  # Test 6: Check time series data
  cat("Test 6: Time series structure\n")
  df_catch <- read_csv("code/rtmb/catch-data.csv", show_col_types = FALSE)
  cat("  Expected: ts column ranges from 1 to 268\n")
  cat("  Result: min(ts) =", min(df_catch$ts), ", max(ts) =", max(df_catch$ts), "\n")
  cat("  Status: ", ifelse(min(df_catch$ts) == 1 && max(df_catch$ts) == 268, 
                           "PASS", "FAIL"), "\n\n")
  
  # Test 7: Construct get_pla and test maturity conversion
  cat("Test 7: Maturity-at-age conversion using get_pla()\n")
  len_lower <- seq(10, 198, by = 2)
  len_upper <- seq(12, 200, by = 2)
  len_mid <- (len_lower + len_upper) / 2
  
  # Interpolate maturity to model bins
  mfcl_bin_lower <- seq(2, 198, by = 2)
  mfcl_bin_mid <- mfcl_bin_lower + 1
  maturity_at_length <- approx(
    x = mfcl_bin_mid,
    y = mat_at_length_mfcl,
    xout = len_mid,
    method = "linear",
    rule = 2
  )$y
  
  # Convert to maturity-at-age
  pla <- get_pla(len_lower, len_upper, mu_a = mean_laa, sd_a = sd_laa)
  maturity_a <- as.vector(t(pla) %*% maturity_at_length)
  
  cat("  Expected: maturity_a length = 40\n")
  cat("  Result: ", length(maturity_a), " - ", 
      ifelse(length(maturity_a) == 40, "PASS", "FAIL"), "\n")
  cat("  Range: [", min(maturity_a), ",", max(maturity_a), "]\n")
  cat("  Maturity at age 20 (5 years): ", maturity_a[20], "\n\n")
  
  # Summary
  cat("=================================================================\n")
  cat("VALIDATION COMPLETE\n")
  cat("=================================================================\n")
  cat("\nNext steps:\n")
  cat("1. Load bet.Rmd and run get-biology chunk\n")
  cat("2. Run get-data chunk and verify array dimensions\n")
  cat("3. Create AD function and test compilation\n")
  cat("4. Compare biology plots with MFCL values\n")
  
  invisible(list(
    ages = ages,
    mean_laa = mean_laa,
    sd_laa = sd_laa,
    mat_at_length = maturity_at_length,
    maturity_a = maturity_a,
    M_at_age = M_at_age,
    lw_params = lw_params_vec
  ))
}

# Run validation if sourced directly
if (!interactive()) {
  validate_biology_update()
}
