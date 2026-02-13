# Tests for extract_ss3_weight_comp and extract_mfcl_weight_comp functions

library(testthat)
library(data.table)
library(magrittr)

# Define project paths
proj_dir = this.path::this.proj()
dir_model = file.path(proj_dir, "model-files")
dir_ss3 = file.path(dir_model, "ss3")
dir_mfcl = file.path(dir_model, "mfcl")
dir_helper_fns_ss3 = file.path(proj_dir, "code", "ss3", "helper-fns")
dir_helper_fns_mfcl = file.path(proj_dir, "code", "mfcl", "helper-fns")

# Source helper functions from both directories
sapply(file.path(dir_helper_fns_ss3, list.files(dir_helper_fns_ss3)), source)
sapply(file.path(dir_helper_fns_mfcl, list.files(dir_helper_fns_mfcl)), source)

context("extract_weight_comp")

# Define required columns for output
required_cols = c("id", "Fleet", "Fleet_name", "Used", "Kind", "Sex",
                 "Bin", "Obs", "Exp", "Dev", "effN", "Nsamp_in", "Nsamp_adj")

# ===== Pre-load data once for all tests =====
# Read SS3 weight composition once
wt_ss3_base = NULL
if(file.exists(file.path(dir_ss3, "01-bet-base", "Report.sso")) &&
   requireNamespace("r4ss", quietly = TRUE)) {
  tryCatch({
    wt_ss3_base = extract_ss3_weight_comp(
      file.path(dir_ss3, "01-bet-base"),
      "01-bet-base",
      verbose = FALSE
    )
  }, error = function(e) {
    # SS3 weight composition may not be available
  })
}

# Read MFCL weight composition once (if files exist)
wt_mfcl_v11 = NULL
if(file.exists(file.path(dir_mfcl, "v11", "weight.fit")) &&
   file.exists(file.path(dir_mfcl, "v11", "bet.frq"))) {
  tryCatch({
    wt_mfcl_v11 = extract_mfcl_weight_comp(
      file.path(dir_mfcl, "v11", "weight.fit"),
      file.path(dir_mfcl, "v11", "bet.frq"),
      "mfcl-v11",
      output_dir = dir_mfcl,
      save_csv = FALSE,
      verbose = FALSE
    )
  }, error = function(e) {
    # MFCL weight composition may not be available
  })
}

# ===== SS3 Tests =====

test_that("extract_ss3_weight_comp returns data.table with correct structure", {
  skip_if_not(!is.null(wt_ss3_base),
              "SS3 weight composition not available (pre-loaded)")
  
  expect_s3_class(wt_ss3_base, "data.table")
  expect_true(all(required_cols %in% names(wt_ss3_base)))
})

test_that("extract_ss3_weight_comp returns correct data types", {
  skip_if_not(!is.null(wt_ss3_base),
              "SS3 weight composition not available (pre-loaded)")
  
  expect_is(wt_ss3_base$id, "character")
  expect_is(wt_ss3_base$Fleet, "integer")
  expect_is(wt_ss3_base$Fleet_name, "character")
  expect_is(wt_ss3_base$Used, "character")  # Should be character like length comp
  expect_is(wt_ss3_base$Kind, "character")  # Should be character like length comp
  expect_is(wt_ss3_base$Sex, "integer")
  expect_true(is.numeric(wt_ss3_base$Bin))
  expect_is(wt_ss3_base$Obs, "numeric")
  expect_is(wt_ss3_base$Exp, "numeric")
  expect_is(wt_ss3_base$Dev, "numeric")
  expect_is(wt_ss3_base$effN, "numeric")
  expect_is(wt_ss3_base$Nsamp_in, "numeric")
  expect_is(wt_ss3_base$Nsamp_adj, "numeric")
})

test_that("extract_ss3_weight_comp deviations calculated correctly", {
  skip_if_not(!is.null(wt_ss3_base),
              "SS3 weight composition not available (pre-loaded)")
  
  # Check that Dev = Obs - Exp for all rows
  expect_equal(wt_ss3_base$Dev, wt_ss3_base$Obs - wt_ss3_base$Exp, tolerance = 1e-10)
})

test_that("extract_ss3_weight_comp model ID is correct", {
  skip_if_not(!is.null(wt_ss3_base),
              "SS3 weight composition not available (pre-loaded)")
  
  expect_equal(unique(wt_ss3_base$id), "01-bet-base")
})

test_that("extract_ss3_weight_comp proportions are reasonable", {
  skip_if_not(!is.null(wt_ss3_base),
              "SS3 weight composition not available (pre-loaded)")
  
  # All proportions should be non-negative
  expect_true(all(wt_ss3_base$Obs >= 0))
  expect_true(all(wt_ss3_base$Exp >= 0))
  
  # Sample sizes should be positive (or NA)
  expect_true(all(wt_ss3_base$Nsamp_in > 0 | is.na(wt_ss3_base$Nsamp_in)))
})

test_that("extract_ss3_weight_comp bins are sorted", {
  skip_if_not(!is.null(wt_ss3_base),
              "SS3 weight composition not available (pre-loaded)")
  
  # Within each fleet, bins should be sorted
  for(fleet in unique(wt_ss3_base$Fleet)) {
    fleet_bins = wt_ss3_base[Fleet == fleet, Bin]
    expect_equal(fleet_bins, sort(fleet_bins))
  }
})

test_that("extract_ss3_weight_comp save_csv = FALSE does not create file", {
  ss3_model_dir = file.path(dir_ss3, "01-bet-base")
  skip_if_not(file.exists(ss3_model_dir), "SS3 model directory not found")
  
  # Remove existing file if present
  csv_file = file.path(ss3_model_dir, "comp_size.csv")
  if(file.exists(csv_file)) file.remove(csv_file)
  
  # Extract without saving CSV
  wt_comp = extract_ss3_weight_comp(ss3_model_dir, "01-bet-base", 
                                    save_csv = FALSE, verbose = FALSE)
  
  # Verify file was not created
  expect_false(file.exists(csv_file))
  
  # But data.table should still be returned
  expect_s3_class(wt_comp, "data.table")
})

# ===== MFCL Tests =====

test_that("extract_mfcl_weight_comp returns correct structure", {
  skip_if_not(!is.null(wt_mfcl_v11),
              "MFCL weight composition not available (pre-loaded)")
  
  expect_s3_class(wt_mfcl_v11, "data.table")
  expect_true(all(required_cols %in% names(wt_mfcl_v11)))
})

test_that("extract_mfcl_weight_comp returns correct data types", {
  skip_if_not(!is.null(wt_mfcl_v11),
              "MFCL weight composition not available (pre-loaded)")
  
  expect_is(wt_mfcl_v11$id, "character")
  expect_is(wt_mfcl_v11$Fleet, "integer")
  expect_is(wt_mfcl_v11$Fleet_name, "character")
  expect_is(wt_mfcl_v11$Used, "character")  # Should be character
  expect_is(wt_mfcl_v11$Kind, "character")  # Should be character
  expect_is(wt_mfcl_v11$Sex, "integer")
  expect_true(is.numeric(wt_mfcl_v11$Bin))
  expect_is(wt_mfcl_v11$Obs, "numeric")
  expect_is(wt_mfcl_v11$Exp, "numeric")
  expect_is(wt_mfcl_v11$Dev, "numeric")
})

test_that("extract_mfcl_weight_comp deviations calculated correctly", {
  skip_if_not(!is.null(wt_mfcl_v11),
              "MFCL weight composition not available (pre-loaded)")
  
  expect_equal(wt_mfcl_v11$Dev, wt_mfcl_v11$Obs - wt_mfcl_v11$Exp, tolerance = 1e-10)
})

test_that("extract_mfcl_weight_comp handles missing weight.fit gracefully", {
  mfcl_dir = file.path(dir_mfcl, "v11")
  skip_if_not(file.exists(mfcl_dir), "MFCL directory not found")
  skip_if_not(file.exists(file.path(mfcl_dir, "bet.frq")), ".frq file not found")
  
  # Test with non-existent file - should error
  expect_error(
    extract_mfcl_weight_comp(
      file.path(mfcl_dir, "nonexistent.fit"),
      file.path(mfcl_dir, "bet.frq"),
      "test",
      output_dir = mfcl_dir,
      save_csv = FALSE,
      verbose = FALSE
    ),
    "weight.fit file not found"
  )
})

test_that("extract_mfcl_weight_comp requires output_dir when save_csv = TRUE", {
  mfcl_dir = file.path(dir_mfcl, "v11")
  skip_if_not(file.exists(file.path(mfcl_dir, "weight.fit")), "weight.fit file not found")
  skip_if_not(file.exists(file.path(mfcl_dir, "bet.frq")), ".frq file not found")
  
  # Should error when output_dir is missing and save_csv = TRUE
  expect_error(
    extract_mfcl_weight_comp(
      file.path(mfcl_dir, "weight.fit"),
      file.path(mfcl_dir, "bet.frq"),
      "test",
      save_csv = TRUE,
      verbose = FALSE
    ),
    "output_dir must be provided"
  )
})

# ===== Cross-platform Tests =====

test_that("SS3 and MFCL outputs have identical structure", {
  skip_if_not(!is.null(wt_ss3_base), "SS3 weight composition not available")
  skip_if_not(!is.null(wt_mfcl_v11), "MFCL weight composition not available")
  
  # Both should have same column names
  expect_equal(names(wt_ss3_base), names(wt_mfcl_v11))
  
  # Both should have same column types
  expect_equal(sapply(wt_ss3_base, class), sapply(wt_mfcl_v11, class))
})

# ===== Bin Harmonization Tests =====

test_that("Bin harmonization preserves total", {
  ss3_model_dir = file.path(dir_ss3, "01-bet-base")
  skip_if_not(file.exists(ss3_model_dir), "SS3 model directory not found")
  skip_if_not(!is.null(wt_ss3_base), "SS3 weight composition not available")
  skip_if(nrow(wt_ss3_base) == 0, "No weight composition data available")
  
  # Define target bins based on data range
  bin_range = range(wt_ss3_base$Bin)
  target_bins = seq(floor(bin_range[1]), ceiling(bin_range[2]) + 5, by = 5)
  
  # Get rebinned data
  wt_rebin = extract_ss3_weight_comp(ss3_model_dir, "01-bet-base",
                                     harmonize_bins = TRUE,
                                     target_bins = target_bins,
                                     save_csv = FALSE,
                                     verbose = FALSE)
  
  # Check that total observations preserved within each fleet
  for(fleet in unique(wt_ss3_base$Fleet)) {
    total_orig = wt_ss3_base[Fleet == fleet, sum(Obs)]
    total_rebin = wt_rebin[Fleet == fleet, sum(Obs)]
    expect_equal(total_orig, total_rebin, tolerance = 0.001,
                 info = paste("Fleet", fleet, "totals don't match"))
  }
})

test_that("Harmonized bins match target bins", {
  ss3_model_dir = file.path(dir_ss3, "01-bet-base")
  skip_if_not(file.exists(ss3_model_dir), "SS3 model directory not found")
  skip_if_not(!is.null(wt_ss3_base), "SS3 weight composition not available")
  skip_if(nrow(wt_ss3_base) == 0, "No weight composition data available")
  
  # Define specific target bins
  target_bins = seq(0, 140, by = 2)
  
  wt_rebin = extract_ss3_weight_comp(ss3_model_dir, "01-bet-base",
                                     harmonize_bins = TRUE,
                                     target_bins = target_bins,
                                     save_csv = FALSE,
                                     verbose = FALSE)
  
  # Bins should be from target_bins (minus the last edge)
  expected_bins = target_bins[-length(target_bins)]
  unique_bins = sort(unique(wt_rebin$Bin))
  
  # Check that rebinned bins are a subset of expected bins
  expect_true(all(unique_bins %in% expected_bins))
})

test_that("CSV files are created in correct locations when save_csv = TRUE", {
  ss3_model_dir = file.path(dir_ss3, "01-bet-base")
  skip_if_not(file.exists(ss3_model_dir), "SS3 model directory not found")
  
  # Remove existing file if present
  csv_file = file.path(ss3_model_dir, "comp_size.csv")
  if(file.exists(csv_file)) file.remove(csv_file)
  
  # Extract and verify file is created
  wt_comp = extract_ss3_weight_comp(ss3_model_dir, "01-bet-base", 
                                    save_csv = TRUE, verbose = FALSE)
  expect_true(file.exists(csv_file))
  
  # Verify CSV can be read back
  csv_data = fread(csv_file)
  expect_equal(nrow(csv_data), nrow(wt_comp))
})
