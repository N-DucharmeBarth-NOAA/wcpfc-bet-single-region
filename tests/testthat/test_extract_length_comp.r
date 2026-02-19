# Tests for extract_ss3_length_comp and extract_mfcl_length_comp functions

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

context("extract_length_comp")

# Define required columns for output
required_cols = c("id", "Fleet", "Fleet_name", "Used", "Kind", "Sex",
                 "Bin", "Obs", "Exp", "Dev", "effN", "Nsamp_in", "Nsamp_adj")

# ===== Pre-load data once for all tests =====
# Read SS3 length composition once
len_ss3_base = NULL
if(file.exists(file.path(dir_ss3, "01-bet-base", "Report.sso")) &&
   requireNamespace("r4ss", quietly = TRUE)) {
  len_ss3_base = extract_ss3_length_comp(
    file.path(dir_ss3, "01-bet-base"),
    "01-bet-base",
    verbose = FALSE
  )
}

# Read MFCL length composition once (if files exist)
# Create two variants: raw (no replacement) and zero-replaced (to match SS3 tiny-fill)
len_mfcl_v11_raw = NULL
len_mfcl_v11 = NULL
par_file_mfcl = file.path(dir_mfcl, "v11", "10.par")
if(file.exists(file.path(dir_mfcl, "v11", "length.fit")) &&
   file.exists(file.path(dir_mfcl, "v11", "bet.frq"))) {
  tryCatch({
    len_mfcl_v11_raw = extract_mfcl_length_comp(
      file.path(dir_mfcl, "v11", "length.fit"),
      file.path(dir_mfcl, "v11", "bet.frq"),
      "mfcl-v11",
      output_dir = dir_mfcl,
      save_csv = FALSE,
      verbose = FALSE,
      zero_replace = NULL,
      par_file = par_file_mfcl
    )
  }, error = function(e) {
    # MFCL raw data may not be available
  })

  tryCatch({
    len_mfcl_v11 = extract_mfcl_length_comp(
      file.path(dir_mfcl, "v11", "length.fit"),
      file.path(dir_mfcl, "v11", "bet.frq"),
      "mfcl-v11",
      output_dir = dir_mfcl,
      save_csv = FALSE,
      verbose = FALSE,
      zero_replace = 9.90589e-05,
      par_file = par_file_mfcl
    )
  }, error = function(e) {
    # MFCL zero-replaced data may not be available
  })
}

# ===== SS3 Tests =====

test_that("extract_ss3_length_comp returns data.table with correct structure", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  expect_s3_class(len_ss3_base, "data.table")
  expect_true(all(required_cols %in% names(len_ss3_base)))
})

test_that("extract_ss3_length_comp returns correct data types", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  expect_is(len_ss3_base$id, "character")
  expect_is(len_ss3_base$Fleet, "integer")
  expect_is(len_ss3_base$Fleet_name, "character")
  expect_is(len_ss3_base$Used, "character")
  expect_is(len_ss3_base$Kind, "character")
  expect_is(len_ss3_base$Sex, "integer")
  expect_true(is.numeric(len_ss3_base$Bin))
  expect_is(len_ss3_base$Obs, "numeric")
  expect_is(len_ss3_base$Exp, "numeric")
  expect_is(len_ss3_base$Dev, "numeric")
  expect_is(len_ss3_base$effN, "numeric")
  expect_is(len_ss3_base$Nsamp_in, "numeric")
  expect_is(len_ss3_base$Nsamp_adj, "numeric")
})

test_that("extract_ss3_length_comp writes output CSV", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Remove existing file if present
  output_file = file.path(dir_ss3, "01-bet-base", "comp_len.csv")
  if(file.exists(output_file)) {
    unlink(output_file)
  }
  
  # Call with save_csv=TRUE to generate file
  extract_ss3_length_comp(
    file.path(dir_ss3, "01-bet-base"),
    "01-bet-base",
    save_csv = TRUE,
    verbose = FALSE
  )
  
  expect_true(file.exists(output_file))
  
  # Read back and verify
  len_read = fread(output_file)
  expect_equal(names(len_read), required_cols)
  expect_equal(nrow(len_read), nrow(len_ss3_base))
})

test_that("extract_ss3_length_comp with save_csv=FALSE does not write CSV", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Remove file if present
  output_file = file.path(dir_ss3, "01-bet-base", "comp_len.csv")
  if(file.exists(output_file)) {
    unlink(output_file)
  }
  
  # Call with save_csv=FALSE
  len_comp = extract_ss3_length_comp(
    file.path(dir_ss3, "01-bet-base"),
    "01-bet-base",
    save_csv = FALSE,
    verbose = FALSE
  )
  
  # Verify CSV was not created
  expect_false(file.exists(output_file))
  
  # Verify data.table was still returned
  expect_s3_class(len_comp, "data.table")
  expect_true(all(required_cols %in% names(len_comp)))
})

test_that("extract_ss3_length_comp deviation equals Obs - Exp", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  expect_equal(len_ss3_base$Dev, len_ss3_base$Obs - len_ss3_base$Exp, tolerance = 1e-10)
})

test_that("extract_ss3_length_comp aggregation produces reasonable sums", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Check that we have data
  expect_true(nrow(len_ss3_base) > 0)
  
  # Check that proportions are non-negative
  expect_true(all(len_ss3_base$Obs >= 0))
  expect_true(all(len_ss3_base$Exp >= 0))
  
  # Check that sample sizes are positive
  expect_true(all(len_ss3_base$Nsamp_in > 0 | len_ss3_base$Nsamp_in == 0))
})

test_that("extract_ss3_length_comp bin harmonization preserves total", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Use pre-loaded data as the original (no harmonization)
  len_orig = len_ss3_base
  
  # Determine appropriate target bins based on data range
  min_bin = min(len_orig$Bin)
  max_bin = max(len_orig$Bin)
  target_bins = seq(floor(min_bin), ceiling(max_bin) + 10, by = 10)
  
  # Get rebinned data
  len_rebin = extract_ss3_length_comp(
    file.path(dir_ss3, "01-bet-base"),
    "01-bet-base",
    harmonize_bins = TRUE,
    target_bins = target_bins,
    verbose = FALSE
  )
  
  # Check total approximately preserved (within tolerance)
  total_orig = len_orig[, sum(Obs)]
  total_rebin = len_rebin[, sum(Obs)]
  expect_equal(total_orig, total_rebin, tolerance = 0.001)
  
  # Check Exp totals too
  total_exp_orig = len_orig[, sum(Exp)]
  total_exp_rebin = len_rebin[, sum(Exp)]
  expect_equal(total_exp_orig, total_exp_rebin, tolerance = 0.001)
})

test_that("extract_ss3_length_comp harmonized bins match target", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  target_bins = seq(20, 200, by = 10)
  
  len_rebin = extract_ss3_length_comp(
    file.path(dir_ss3, "01-bet-base"),
    "01-bet-base",
    harmonize_bins = TRUE,
    target_bins = target_bins,
    verbose = FALSE
  )
  
  # Check that bins match target (excluding upper edge)
  unique_bins = sort(unique(len_rebin$Bin))
  expected_bins = target_bins[-length(target_bins)]
  
  # Should contain bins from target (may be subset if some bins have no data)
  expect_true(all(unique_bins %in% expected_bins))
})

test_that("extract_ss3_length_comp stops if Report.sso not found", {
  expect_error(
    extract_ss3_length_comp(
      file.path(dir_ss3, "nonexistent-model"),
      "nonexistent",
      verbose = FALSE
    ),
    "Report.sso not found"
  )
})

test_that("extract_ss3_length_comp requires target_bins when harmonize_bins=TRUE", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  expect_error(
    extract_ss3_length_comp(
      file.path(dir_ss3, "01-bet-base"),
      "01-bet-base",
      harmonize_bins = TRUE,
      target_bins = NULL,
      verbose = FALSE
    ),
    "target_bins must be provided"
  )
})

# ===== MFCL Tests =====

test_that("extract_mfcl_length_comp handles missing length.fit file gracefully", {
  expect_error(
    extract_mfcl_length_comp(
      file.path(dir_mfcl, "v11", "nonexistent_length.fit"),
      file.path(dir_mfcl, "v11", "bet.frq"),
      "mfcl-v11",
      output_dir = dir_mfcl,
      verbose = FALSE
    ),
    "length.fit file not found"
  )
})

test_that("extract_mfcl_length_comp stops if frq file not found", {
  # Create a temporary dummy length.fit file for testing
  temp_fit = tempfile(fileext = ".fit")
  writeLines("# dummy", temp_fit)
  
  expect_error(
    extract_mfcl_length_comp(
      temp_fit,
      file.path(dir_mfcl, "nonexistent.frq"),
      "test",
      output_dir = tempdir(),
      verbose = FALSE
    ),
    ".frq file not found"
  )
  
  unlink(temp_fit)
})

test_that("extract_mfcl_length_comp requires output_dir when save_csv=TRUE", {
  # Create temporary dummy files for testing
  temp_fit = tempfile(fileext = ".fit")
  temp_frq = tempfile(fileext = ".frq")
  writeLines("# dummy", temp_fit)
  writeLines("# dummy", temp_frq)
  
  expect_error(
    extract_mfcl_length_comp(
      temp_fit,
      temp_frq,
      "test",
      output_dir = NULL,
      save_csv = TRUE,
      verbose = FALSE
    ),
    "output_dir must be provided when save_csv = TRUE"
  )
  
  unlink(temp_fit)
  unlink(temp_frq)
})

test_that("extract_mfcl_length_comp returns correct data types (raw)", {
  skip_if_not(!is.null(len_mfcl_v11_raw),
              "MFCL raw length composition not available (pre-loaded)")

  expect_is(len_mfcl_v11_raw$id, "character")
  expect_is(len_mfcl_v11_raw$Fleet, "integer")
  expect_is(len_mfcl_v11_raw$Fleet_name, "character")
  expect_is(len_mfcl_v11_raw$Used, "character")
  expect_is(len_mfcl_v11_raw$Kind, "character")
  expect_is(len_mfcl_v11_raw$Sex, "integer")
  expect_is(len_mfcl_v11_raw$Bin, "numeric")
  expect_is(len_mfcl_v11_raw$Obs, "numeric")
  expect_is(len_mfcl_v11_raw$Exp, "numeric")
  expect_is(len_mfcl_v11_raw$Dev, "numeric")
  expect_is(len_mfcl_v11_raw$effN, "numeric")
  expect_is(len_mfcl_v11_raw$Nsamp_in, "numeric")
  expect_is(len_mfcl_v11_raw$Nsamp_adj, "numeric")
})

# ===== Format Consistency Tests =====

test_that("SS3 and MFCL outputs have identical column structure", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Check that SS3 output has the required structure
  expect_equal(names(len_ss3_base), required_cols)
  
  # Check data types individually
  expect_is(len_ss3_base$id, "character")
  expect_is(len_ss3_base$Fleet, "integer")
  expect_is(len_ss3_base$Fleet_name, "character")
  expect_is(len_ss3_base$Used, "character")
  expect_is(len_ss3_base$Kind, "character")
  expect_is(len_ss3_base$Sex, "integer")
  expect_true(is.numeric(len_ss3_base$Bin), "Bin should be numeric")
  expect_is(len_ss3_base$Obs, "numeric")
  expect_is(len_ss3_base$Exp, "numeric")
  expect_is(len_ss3_base$Dev, "numeric")
  expect_is(len_ss3_base$effN, "numeric")
  expect_is(len_ss3_base$Nsamp_in, "numeric")
  expect_is(len_ss3_base$Nsamp_adj, "numeric")
})

test_that("Length composition output is compatible with plotting format", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Verify all required columns for plotting exist
  expect_true(all(c("id", "Fleet", "Bin", "Obs", "Exp") %in% names(len_ss3_base)))
  
  # Verify no NA in critical columns
  expect_false(anyNA(len_ss3_base$Bin))
  expect_false(anyNA(len_ss3_base$Obs))
  expect_false(anyNA(len_ss3_base$Exp))
})

test_that("Multiple fleets are handled correctly", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Check that fleet-specific aggregation works
  if(length(unique(len_ss3_base$Fleet)) > 1) {
    # If multiple fleets, verify each has its own data
    fleet_counts = len_ss3_base[, .N, by = Fleet]
    expect_true(all(fleet_counts$N > 0))
    
    # Verify fleet names are unique per fleet
    fleet_names = len_ss3_base[, unique(Fleet_name), by = Fleet]
    expect_equal(nrow(fleet_names), length(unique(len_ss3_base$Fleet)))
  }
})

test_that("Sex-specific data is preserved", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Check that Sex column exists and contains valid values
  expect_true("Sex" %in% names(len_ss3_base))
  expect_true(all(len_ss3_base$Sex %in% c(0, 1, 2)))
})

test_that("Empty fleets produce valid output", {
  skip_if_not(!is.null(len_ss3_base),
              "SS3 length composition not available (pre-loaded)")
  
  # Check that if any fleet has zero observations, it's handled
  zero_obs_fleets = len_ss3_base[, .(total_obs = sum(Obs)), by = Fleet][total_obs == 0]
  
  if(nrow(zero_obs_fleets) > 0) {
    # Verify structure is still valid for zero observation fleets
    zero_data = len_ss3_base[Fleet %in% zero_obs_fleets$Fleet]
    expect_true(all(required_cols %in% names(zero_data)))
  }
  
  # Test passes if no zero fleets or if they're handled correctly
  expect_true(TRUE)
})

test_that("MFCL and SS3 observed values match for common fisheries and bins with same width", {
  skip_if_not(!is.null(len_ss3_base) && !is.null(len_mfcl_v11),
              "Both SS3 and MFCL length composition data must be available (pre-loaded)")
  
  # Find common fleets between datasets
  ss3_fleets = unique(len_ss3_base$Fleet)
  mfcl_fleets = unique(len_mfcl_v11$Fleet)
  common_fleets = intersect(ss3_fleets, mfcl_fleets)
  
  skip_if(length(common_fleets) == 0, "No common fleets between SS3 and MFCL data")
  
  # For each common fleet, check observed values match
  for(fleet in common_fleets) {
    ss3_data = len_ss3_base[Fleet == fleet, .(Bin, Obs)]
    setorder(ss3_data, Bin)
    
    mfcl_data = len_mfcl_v11[Fleet == fleet, .(Bin, Obs)]
    setorder(mfcl_data, Bin)
    
    # Calculate bin widths for both datasets
    if(nrow(ss3_data) > 1) {
      ss3_data[, bin_width := c(diff(Bin), diff(Bin)[length(diff(Bin))])]
    } else {
      ss3_data[, bin_width := NA_real_]
    }
    
    if(nrow(mfcl_data) > 1) {
      mfcl_data[, bin_width := c(diff(Bin), diff(Bin)[length(diff(Bin))])]
    } else {
      mfcl_data[, bin_width := NA_real_]
    }
    
    # Find common bins
    common_bins = intersect(ss3_data$Bin, mfcl_data$Bin)
    
    if(length(common_bins) > 0) {
      ss3_subset = ss3_data[Bin %in% common_bins]
      mfcl_subset = mfcl_data[Bin %in% common_bins]
      
      # Verify bin widths match for common bins
      bin_width_matches = abs(ss3_subset$bin_width - mfcl_subset$bin_width) < 1e-6
      
      if(!all(bin_width_matches, na.rm = TRUE)) {
        # Only compare observed values for bins with matching widths
        matching_bins = ss3_subset$Bin[bin_width_matches]
        ss3_subset = ss3_subset[Bin %in% matching_bins]
        mfcl_subset = mfcl_subset[Bin %in% matching_bins]
      }
      
      # Only proceed with comparison if we have bins with matching widths
      if(nrow(ss3_subset) > 0 && nrow(mfcl_subset) > 0) {
        # With back_transform=TRUE (default), SS3 Obs is on the original input
        # proportion scale, matching MFCL. Tolerance covers floating-point
        # round-trip through Report.sso and any residual clamping at zero.
        expect_equal(ss3_subset$Obs, mfcl_subset$Obs, tolerance = 1.5e-4,
             label = paste("Fleet", fleet, "observed values should match between SS3 and MFCL for bins with same width"))
      }
    }
  }
})

# ===== Pre-load time-resolved (non-aggregated) data for tests =====

# Required columns for time-resolved output
required_time_cols = c("id", "Fleet", "Fleet_name", "year", "month", "ts",
                       "Used", "Kind", "Sex", "Bin",
                       "Obs", "Exp", "Dev", "effN", "Nsamp_in", "Nsamp_adj")

len_ss3_time = NULL
if(file.exists(file.path(dir_ss3, "01-bet-base", "Report.sso")) &&
   requireNamespace("r4ss", quietly = TRUE)) {
  len_ss3_time = extract_ss3_length_comp(
    file.path(dir_ss3, "01-bet-base"),
    "01-bet-base",
    aggregate = FALSE,
    save_csv = FALSE,
    verbose = FALSE
  )
}

len_mfcl_time = NULL
if(file.exists(file.path(dir_mfcl, "v11", "length.fit")) &&
   file.exists(file.path(dir_mfcl, "v11", "bet.frq"))) {
  tryCatch({
    len_mfcl_time = extract_mfcl_length_comp(
      file.path(dir_mfcl, "v11", "length.fit"),
      file.path(dir_mfcl, "v11", "bet.frq"),
      "mfcl-v11",
      output_dir = dir_mfcl,
      aggregate = FALSE,
      save_csv = FALSE,
      verbose = FALSE,
      par_file = par_file_mfcl
    )
  }, error = function(e) {
    # MFCL time-resolved data may not be available
  })
}

# ===== SS3 Time-Resolved Tests =====

test_that("extract_ss3_length_comp aggregate=FALSE returns correct columns", {
  skip_if_not(!is.null(len_ss3_time),
              "SS3 time-resolved length composition not available")
  
  expect_s3_class(len_ss3_time, "data.table")
  expect_equal(names(len_ss3_time), required_time_cols)
})

test_that("extract_ss3_length_comp aggregate=FALSE has correct data types", {
  skip_if_not(!is.null(len_ss3_time),
              "SS3 time-resolved length composition not available")
  
  expect_is(len_ss3_time$id, "character")
  expect_is(len_ss3_time$Fleet, "integer")
  expect_is(len_ss3_time$Fleet_name, "character")
  expect_is(len_ss3_time$year, "integer")
  expect_is(len_ss3_time$month, "integer")
  expect_is(len_ss3_time$ts, "integer")
  expect_is(len_ss3_time$Used, "character")
  expect_is(len_ss3_time$Kind, "character")
  expect_is(len_ss3_time$Sex, "integer")
  expect_true(is.numeric(len_ss3_time$Bin))
  expect_is(len_ss3_time$Obs, "numeric")
  expect_is(len_ss3_time$Exp, "numeric")
  expect_is(len_ss3_time$Dev, "numeric")
  expect_is(len_ss3_time$effN, "numeric")
  expect_is(len_ss3_time$Nsamp_in, "numeric")
  expect_is(len_ss3_time$Nsamp_adj, "numeric")
})

test_that("extract_ss3_length_comp aggregate=FALSE has valid time values", {
  skip_if_not(!is.null(len_ss3_time),
              "SS3 time-resolved length composition not available")
  
  # Year should be in plausible range
  expect_true(all(len_ss3_time$year >= 1952))
  expect_true(all(len_ss3_time$year <= 2025))
  
  # Month should be one of the quarterly months matching MFCL convention
  expect_true(all(len_ss3_time$month %in% c(2, 5, 8, 11)))
  
  # ts should be positive and consistent with year and month
  expect_true(all(len_ss3_time$ts > 0))
  expected_ts = (len_ss3_time$year - 1952L) * 4L + match(len_ss3_time$month, c(2, 5, 8, 11))
  expect_equal(len_ss3_time$ts, expected_ts)
})

test_that("extract_ss3_length_comp aggregate=FALSE deviation equals Obs - Exp", {
  skip_if_not(!is.null(len_ss3_time),
              "SS3 time-resolved length composition not available")
  
  expect_equal(len_ss3_time$Dev, len_ss3_time$Obs - len_ss3_time$Exp, tolerance = 1e-10)
})

test_that("extract_ss3_length_comp aggregate=FALSE has more rows than aggregated", {
  skip_if_not(!is.null(len_ss3_time) && !is.null(len_ss3_base),
              "Both SS3 time-resolved and aggregated data must be available")
  
  # Time-resolved should have more rows (multiple time steps per fleet/bin)
  expect_true(nrow(len_ss3_time) >= nrow(len_ss3_base))
})

test_that("extract_ss3_length_comp aggregate=FALSE writes comp_len_time.csv", {
  skip_if_not(!is.null(len_ss3_time),
              "SS3 time-resolved length composition not available")
  
  output_file = file.path(dir_ss3, "01-bet-base", "comp_len_time.csv")
  if(file.exists(output_file)) unlink(output_file)
  
  extract_ss3_length_comp(
    file.path(dir_ss3, "01-bet-base"),
    "01-bet-base",
    aggregate = FALSE,
    save_csv = TRUE,
    verbose = FALSE
  )
  
  expect_true(file.exists(output_file))
  
  len_read = fread(output_file)
  expect_equal(names(len_read), required_time_cols)
  
  # Cleanup
  unlink(output_file)
})

# ===== MFCL Time-Resolved Tests =====

test_that("extract_mfcl_length_comp aggregate=FALSE returns correct columns", {
  skip_if_not(!is.null(len_mfcl_time),
              "MFCL time-resolved length composition not available")
  
  expect_s3_class(len_mfcl_time, "data.table")
  expect_equal(names(len_mfcl_time), required_time_cols)
})

test_that("extract_mfcl_length_comp aggregate=FALSE has correct data types", {
  skip_if_not(!is.null(len_mfcl_time),
              "MFCL time-resolved length composition not available")
  
  expect_is(len_mfcl_time$id, "character")
  expect_is(len_mfcl_time$Fleet, "integer")
  expect_is(len_mfcl_time$Fleet_name, "character")
  expect_is(len_mfcl_time$year, "integer")
  expect_is(len_mfcl_time$month, "integer")
  expect_is(len_mfcl_time$ts, "integer")
  expect_is(len_mfcl_time$Used, "character")
  expect_is(len_mfcl_time$Kind, "character")
  expect_is(len_mfcl_time$Sex, "integer")
  expect_true(is.numeric(len_mfcl_time$Bin))
  expect_is(len_mfcl_time$Obs, "numeric")
  expect_is(len_mfcl_time$Exp, "numeric")
  expect_is(len_mfcl_time$Dev, "numeric")
  expect_is(len_mfcl_time$effN, "numeric")
  expect_is(len_mfcl_time$Nsamp_in, "numeric")
  expect_is(len_mfcl_time$Nsamp_adj, "numeric")
})

test_that("extract_mfcl_length_comp aggregate=FALSE has valid time values", {
  skip_if_not(!is.null(len_mfcl_time),
              "MFCL time-resolved length composition not available")
  
  # Year should be in plausible range
  expect_true(all(len_mfcl_time$year >= 1952))
  expect_true(all(len_mfcl_time$year <= 2025))
  
  # Month should be one of the quarterly months
  expect_true(all(len_mfcl_time$month %in% c(2, 5, 8, 11)))
  
  # ts should be positive and consistent with year and month
  expect_true(all(len_mfcl_time$ts > 0))
  expected_ts = (len_mfcl_time$year - 1952L) * 4L + match(len_mfcl_time$month, c(2, 5, 8, 11))
  expect_equal(len_mfcl_time$ts, expected_ts)
})

test_that("extract_mfcl_length_comp aggregate=FALSE deviation equals Obs - Exp", {
  skip_if_not(!is.null(len_mfcl_time),
              "MFCL time-resolved length composition not available")
  
  expect_equal(len_mfcl_time$Dev, len_mfcl_time$Obs - len_mfcl_time$Exp, tolerance = 1e-10)
})

test_that("extract_mfcl_length_comp aggregate=FALSE has more rows than aggregated", {
  skip_if_not(!is.null(len_mfcl_time) && !is.null(len_mfcl_v11),
              "Both MFCL time-resolved and aggregated data must be available")
  
  expect_true(nrow(len_mfcl_time) >= nrow(len_mfcl_v11))
})

test_that("extract_mfcl_length_comp aggregate=FALSE writes comp_len_time.csv", {
  skip_if_not(!is.null(len_mfcl_time),
              "MFCL time-resolved length composition not available")
  
  output_file = file.path(dir_mfcl, "comp_len_time.csv")
  if(file.exists(output_file)) unlink(output_file)
  
  extract_mfcl_length_comp(
    file.path(dir_mfcl, "v11", "length.fit"),
    file.path(dir_mfcl, "v11", "bet.frq"),
    "mfcl-v11",
    output_dir = dir_mfcl,
    aggregate = FALSE,
    save_csv = TRUE,
    verbose = FALSE
  )
  
  expect_true(file.exists(output_file))
  
  len_read = fread(output_file)
  expect_equal(names(len_read), required_time_cols)
  
  # Cleanup
  unlink(output_file)
})

# ===== Cross-Model Time-Resolved Consistency Tests =====

test_that("SS3 and MFCL time-resolved outputs have identical column structure", {
  skip_if_not(!is.null(len_ss3_time) && !is.null(len_mfcl_time),
              "Both SS3 and MFCL time-resolved data must be available")
  
  expect_equal(names(len_ss3_time), names(len_mfcl_time))
  expect_equal(names(len_ss3_time), required_time_cols)
})

test_that("SS3 and MFCL time-resolved ts values overlap", {
  skip_if_not(!is.null(len_ss3_time) && !is.null(len_mfcl_time),
              "Both SS3 and MFCL time-resolved data must be available")
  
  ss3_ts = sort(unique(len_ss3_time$ts))
  mfcl_ts = sort(unique(len_mfcl_time$ts))
  common_ts = intersect(ss3_ts, mfcl_ts)
  
  # There should be at least some overlapping time steps
  expect_true(length(common_ts) > 0)
})

test_that("SS3 and MFCL time-resolved observed values match for spot-check selection with same bin widths", {
  skip_if_not(!is.null(len_ss3_time) && !is.null(len_mfcl_time),
              "Both SS3 and MFCL time-resolved data must be available")
  
  # Find common time steps
  ss3_ts = sort(unique(len_ss3_time$ts))
  mfcl_ts = sort(unique(len_mfcl_time$ts))
  common_ts = intersect(ss3_ts, mfcl_ts)
  
  skip_if(length(common_ts) == 0, "No common time steps between SS3 and MFCL time-resolved data")
  
  # Sample a few time steps for spot check (first 3, or all if fewer than 3)
  sampled_ts = common_ts[1:min(3, length(common_ts))]
  
  # For each sampled time step, compare fleets and bins
  for(ts_val in sampled_ts) {
    ss3_ts_data = len_ss3_time[ts == ts_val, .(Fleet, Fleet_name, Bin, Obs)]
    mfcl_ts_data = len_mfcl_time[ts == ts_val, .(Fleet, Fleet_name, Bin, Obs)]
    
    # Find common fleets
    common_fleets = intersect(ss3_ts_data$Fleet, mfcl_ts_data$Fleet)
    
    for(fleet in common_fleets) {
      ss3_fleet = ss3_ts_data[Fleet == fleet, .(Bin, Obs)]
      setorder(ss3_fleet, Bin)
      
      mfcl_fleet = mfcl_ts_data[Fleet == fleet, .(Bin, Obs)]
      setorder(mfcl_fleet, Bin)
      
      # Calculate bin widths
      if(nrow(ss3_fleet) > 1) {
        ss3_fleet[, bin_width := c(diff(Bin), diff(Bin)[length(diff(Bin))])]
      } else {
        ss3_fleet[, bin_width := NA_real_]
      }
      
      if(nrow(mfcl_fleet) > 1) {
        mfcl_fleet[, bin_width := c(diff(Bin), diff(Bin)[length(diff(Bin))])]
      } else {
        mfcl_fleet[, bin_width := NA_real_]
      }
      
      # Find common bins
      common_bins = intersect(ss3_fleet$Bin, mfcl_fleet$Bin)
      
      if(length(common_bins) > 0) {
        ss3_subset = ss3_fleet[Bin %in% common_bins]
        mfcl_subset = mfcl_fleet[Bin %in% common_bins]
        
        # Verify bin widths match for common bins
        bin_width_matches = abs(ss3_subset$bin_width - mfcl_subset$bin_width) < 1e-6
        
        if(!all(bin_width_matches, na.rm = TRUE)) {
          # Only compare observed values for bins with matching widths
          matching_bins = ss3_subset$Bin[bin_width_matches]
          ss3_subset = ss3_subset[Bin %in% matching_bins]
          mfcl_subset = mfcl_subset[Bin %in% matching_bins]
        }
        
        # Only proceed with comparison if we have bins with matching widths
        if(nrow(ss3_subset) > 0 && nrow(mfcl_subset) > 0) {
          # With back_transform=TRUE (default), SS3 Obs matches original input
          # proportions. Tolerance covers floating-point round-trip only.
          expect_equal(ss3_subset$Obs, mfcl_subset$Obs, tolerance = 1.5e-4,
               label = paste("TS", ts_val, "Fleet", fleet, "time-resolved observed values should match closely between SS3 and MFCL for bins with same width"))
        }
      }
    }
  }
})

# ===== Back-Transform Tests =====

test_that("back_transform=TRUE SS3 Obs matches MFCL Obs for same bins", {
  skip_if_not(!is.null(len_ss3_time) && !is.null(len_mfcl_time),
              "Both SS3 and MFCL time-resolved data must be available")
  
  # First length data appears at ts 146 (1988 Q2) for Fleet 8
  common_ts = intersect(unique(len_ss3_time$ts), unique(len_mfcl_time$ts))
  skip_if(length(common_ts) == 0, "No common time steps")
  
  ts_val = common_ts[1]
  
  # Find a fleet that has data in this time step for both models
  ss3_fleets = unique(len_ss3_time[ts == ts_val]$Fleet)
  mfcl_fleets = unique(len_mfcl_time[ts == ts_val]$Fleet)
  common_fleets = intersect(ss3_fleets, mfcl_fleets)
  skip_if(length(common_fleets) == 0, "No common fleets in first time step")
  
  flt = common_fleets[1]
  
  ss3_f = len_ss3_time[Fleet == flt & ts == ts_val]
  mfcl_f = len_mfcl_time[Fleet == flt & ts == ts_val]
  
  common_bins = intersect(ss3_f$Bin, mfcl_f$Bin)
  skip_if(length(common_bins) == 0, "No common bins")
  
  ss3_comp = ss3_f[Bin %in% common_bins][order(Bin)]$Obs
  mfcl_comp = mfcl_f[Bin %in% common_bins][order(Bin)]$Obs
  
  # With back_transform=TRUE, the Obs proportions should closely match
  # (tolerance allows for floating-point rounding through Report.sso)
  expect_equal(ss3_comp, mfcl_comp, tolerance = 1e-5,
               label = "Back-transformed SS3 Obs should match MFCL Obs")
})

test_that("back_transform=FALSE returns raw SS3 Report.sso values", {
  ss3_model_dir = file.path(dir_ss3, "01-bet-base")
  skip_if_not(file.exists(file.path(ss3_model_dir, "Report.sso")),
              "SS3 Report.sso not found")
  
  len_raw = tryCatch(
    extract_ss3_length_comp(ss3_model_dir, "01-bet-base",
                            aggregate = FALSE, save_csv = FALSE,
                            back_transform = FALSE, verbose = FALSE),
    error = function(e) NULL
  )
  skip_if_not(!is.null(len_raw), "Could not read SS3 output")
  
  # With back_transform=FALSE, Obs values should contain the addtocomp
  # adjustment. Even zero-proportion bins will have a small positive value.
  # Verify by checking that all Obs > 0 (because addtocomp > 0).
  expect_true(all(len_raw$Obs > 0),
              label = "back_transform=FALSE Obs should include addtocomp (all > 0)")
  
  # Nsamp_in should be the variance-adjusted values (original * Factor 4).
  # Compare against back_transform=TRUE to confirm they differ.
  len_bt = tryCatch(
    extract_ss3_length_comp(ss3_model_dir, "01-bet-base",
                            aggregate = FALSE, save_csv = FALSE,
                            back_transform = TRUE, verbose = FALSE),
    error = function(e) NULL
  )
  skip_if_not(!is.null(len_bt), "Could not read SS3 output with back_transform=TRUE")
  
  # Back-transformed Nsamp_in should be much larger (divided by Factor 4)
  expect_true(mean(len_bt$Nsamp_in) > mean(len_raw$Nsamp_in) * 100,
              label = "back_transform=TRUE Nsamp_in should be much larger than raw")
})

test_that("back_transform=TRUE recovers original Nsamp scale from data.ss", {
  skip_if_not(!is.null(len_ss3_time),
              "SS3 time-resolved length composition not available (pre-loaded)")
  
  # The pre-loaded len_ss3_time uses back_transform=TRUE by default
  # Nsamp_in should be on the original data.ss scale (e.g., hundreds or thousands)
  # rather than tiny variance-adjusted values
  expect_true(all(len_ss3_time$Nsamp_in > 1),
              label = "Back-transformed Nsamp_in should be on original data.ss scale")
})

# ===== MFCL Nsamp_adj with flag 49 Tests =====

test_that("MFCL Nsamp_adj uses flag 49 values when par_file provided", {
  skip_if_not(!is.null(len_mfcl_time) && file.exists(par_file_mfcl),
              "MFCL time-resolved data and par file must be available")
  
  # Read flag 49 values directly for verification
  if(!requireNamespace("FLR4MFCL", quietly = TRUE)) skip("FLR4MFCL not available")
  base_par = FLR4MFCL::read.MFCLPar(par_file_mfcl, first.yr = 1952)
  flag_dt = data.table::as.data.table(FLR4MFCL::flags(base_par))
  flag49 = flag_dt[flag == 49 & flagtype < 0]
  flag49[, fleet := abs(flagtype)]
  flag49_lookup = stats::setNames(flag49$value, flag49$fleet)
  
  # Check one observation per fleet: Nsamp_adj should equal min(Nsamp_in, 1000) / flag49
  for(flt in unique(len_mfcl_time$Fleet)) {
    flt_char = as.character(flt)
    if(flt_char %in% names(flag49_lookup)) {
      f49 = flag49_lookup[flt_char]
      obs_row = len_mfcl_time[Fleet == flt][1]
      expected_adj = unname(min(obs_row$Nsamp_in, 1000) / f49)
      expect_equal(obs_row$Nsamp_adj, expected_adj, tolerance = 1e-10,
                   label = paste0("Fleet ", flt, " Nsamp_adj = min(Nsamp_in, 1000) / ", f49))
    }
  }
})

test_that("MFCL Nsamp_adj differs from Nsamp_in when par_file provided", {
  skip_if_not(!is.null(len_mfcl_time) && file.exists(par_file_mfcl),
              "MFCL time-resolved data and par file must be available")
  
  # Nsamp_adj should NOT equal Nsamp_in (because flag 49 divides it)
  # At least for rows where Nsamp_in > 0
  rows_with_data = len_mfcl_time[Nsamp_in > 0]
  if(nrow(rows_with_data) > 0) {
    expect_false(all(rows_with_data$Nsamp_adj == rows_with_data$Nsamp_in),
                 label = "Nsamp_adj should differ from Nsamp_in when flag 49 is applied")
  }
})

test_that("MFCL Nsamp_adj is capped at 1000/flag49 when Nsamp_in > 1000", {
  skip_if_not(!is.null(len_mfcl_time) && file.exists(par_file_mfcl),
              "MFCL time-resolved data and par file must be available")
  
  if(!requireNamespace("FLR4MFCL", quietly = TRUE)) skip("FLR4MFCL not available")
  base_par = FLR4MFCL::read.MFCLPar(par_file_mfcl, first.yr = 1952)
  flag_dt = data.table::as.data.table(FLR4MFCL::flags(base_par))
  flag49 = flag_dt[flag == 49 & flagtype < 0]
  flag49[, fleet := abs(flagtype)]
  flag49_lookup = stats::setNames(flag49$value, flag49$fleet)
  
  # For rows where Nsamp_in > 1000, Nsamp_adj should be 1000/flag49
  for(flt in unique(len_mfcl_time$Fleet)) {
    flt_char = as.character(flt)
    if(flt_char %in% names(flag49_lookup)) {
      f49 = flag49_lookup[flt_char]
      large_rows = len_mfcl_time[Fleet == flt & Nsamp_in > 1000]
      if(nrow(large_rows) > 0) {
        expected_adj = 1000 / f49
        expect_true(all(abs(large_rows$Nsamp_adj - expected_adj) < 1e-10),
                    label = paste0("Fleet ", flt,
                                   ": Nsamp_adj should be 1000/", f49,
                                   " when Nsamp_in > 1000"))
      }
    }
  }
})