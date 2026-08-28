
# Nicholas Ducharme-Barth
# 2026/07/15
# R code to prepare baseline data for RTMB model
# Indian Ocean Yellowfin tuna simulated data from IATTC `sample_tree/4` model
#
# Copyright (c) 2026 Nicholas Ducharme-Barth
# GPL-3.0

# =============================================================================
# 0. Setup
# =============================================================================

# renv::install("n-ducharmebarth-noaa/opal@dev")
library(opal)
library(data.table)
library(magrittr)
library(tibble)

dir_ss3 <- file.path(getwd(), "model-files", "ss3", "xx-io-yft-sim-iattc-sample-tree-4")
dir_out <- file.path(getwd(), "opal_data", "io-yft-ss3")
dir.create(dir_out,recursive=TRUE)

last_hist_yr <- 256

# =============================================================================
# 1. Model dimensions
# =============================================================================

data_lines <- readLines(file.path(dir_ss3, "test_data.ss"))

styr    <- 1
endyr   <- last_hist_yr
n_year  <- endyr - styr + 1   # 75
n_seas  <- 1
n_age   <- 28                 # SS3 ages 0-43 → opal ages 1:44
age_a   <- 1:n_age            # opal convention: internal ages always 1:n_age
n_extract <- 16
n_cpue  <- 4
n_index <- n_cpue
n_fleet <- n_extract + n_cpue

fleet_names <- c("fishing_gi_1","fishing_gi_4","fishing_hd_1","fishing_ll_1","fishing_ll_2","fishing_ll_3","fishing_ll_4","fishing_other_1","fishing_other_4","fishing_bb_1","fishing_ps_1","fishing_ps_2","fishing_ps_4","fishing_trol_1","fishing_trol_2","fishing_trol_4","llcpue_1","llcpue_2","llcpue_3","llcpue_4")
fleet_types <- c(rep(1,n_extract), rep(3, n_cpue))  # 1=extraction, 3=index
catch_units <- c(rep(2,n_extract), rep(1, n_cpue))    # numbers, biomass

# Data length bins
len_bins      <- c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180, 185, 190, 195, 200) #_lbin_vector
n_len         <- length(len_bins)
len_bin_width <- 5
len_bin_start <- 10

# =============================================================================
# 2. Extract catch data
# =============================================================================

catch_header_idx <- grep("N_lines_of_catch_to_read", data_lines)[1]
n_catch_lines <- as.integer(strsplit(trimws(data_lines[catch_header_idx]), "\\s+")[[1]][1])

catch_rows <- list()
row_count <- 0
for (i in seq(catch_header_idx + 2, length(data_lines))) {
  line <- trimws(data_lines[i])
  if (grepl("^#", line) || line == "") next
  line_clean <- sub("#.*$", "", line)
  vals <- as.numeric(strsplit(trimws(line_clean), "\\s+")[[1]])
  if (length(vals) < 18 || is.na(vals[1]) || vals[1] == -9999) next
  catch_rows[[length(catch_rows) + 1]] <- vals[1:18]
  row_count <- row_count + 1
  if (row_count >= n_catch_lines) break
}

catch_mat <- do.call(rbind, catch_rows)
catch_dt <- as.data.table(catch_mat)
setnames(catch_dt, c(paste0("fleet_", seq_len(n_extract)), "year", "seas"))
catch_dt <- catch_dt[year >= styr & year <= endyr]

catch_obs_ysf <- array(0, dim = c(n_year, n_seas, n_fleet))
for (r in seq_len(nrow(catch_dt))) {
  yr_idx <- catch_dt$year[r] - styr + 1
  for (fl in seq_len(n_extract)) {
    catch_obs_ysf[yr_idx, 1, fl] <- catch_dt[[paste0("fleet_", fl)]][r]
  }
}

# =============================================================================
# 3. Extract CPUE data
# =============================================================================

cpue_header <- grep("^#_Year", data_lines)[1]

cpue_rows <- list()
i <- cpue_header + 1
while (i <= length(data_lines)) {
  line <- trimws(data_lines[i])
  if (grepl("^#", line) || line == "") { i <- i + 1; next }
  line_clean <- sub("#.*$", "", line)
  vals <- as.numeric(strsplit(trimws(line_clean), "\\s+")[[1]])
  if (length(vals) < 5 || is.na(vals[1]) || vals[1] == -9999) break
  cpue_rows[[length(cpue_rows) + 1]] <- vals[1:5]
  i <- i + 1
}

cpue_dt <- as.data.table(do.call(rbind, cpue_rows))
setnames(cpue_dt, c("year", "unit", "fleet", "obs", "se"))
cpue_dt <- cpue_dt[year >= styr & year <= endyr]
cpue_dt[, ts := year - styr + 1]

# Split the original fleet 17 CPUE series into four indices by timestep pattern
cpue_dt[fleet == 17, fleet := 17 + ((ts - 81) %% 4)]

# Build CPUE tibble matching required format
fish_levels <- sort(unique(cpue_dt$fleet))
cpue_data <- cpue_dt[, .(
  year    = year,
  month   = 1,
  ts      = ts,
  fishery = fleet,
  index   = match(fleet, fish_levels),
  metric  = "cpue",
  units   = catch_units[fleet],
  value   = obs,
  se      = se
)]
# scale to mean 1 per index for numerical stability
cpue_data[, value := value / mean(value), by = fishery]
# ensure column order and convert to tibble
cpue_data <- tibble::as_tibble(cpue_data[, .(year, month, ts, fishery, index, metric, units, value, se)])
cpue_data$month <- c(2,5,8,11)[as.integer(as.factor(cpue_data$fishery))]


# =============================================================================
# 4a. Extract length composition data
# =============================================================================

lencomp_header_idx <- grep("^#_Yr\\s+Seas\\s+FltSvy", data_lines)[1]
lencomp_nobs <- as.integer(strsplit(trimws(data_lines[grep("^1114 #_N_Length_comp_observations", data_lines)[1]]), "\\s+")[[1]][1])

lf_rows <- list()
for (i in seq(lencomp_header_idx + 1, lencomp_header_idx + lencomp_nobs)) {
  line <- trimws(data_lines[i])
  if (grepl("^#", line) || line == "") next
  vals <- as.numeric(strsplit(trimws(line), "\\s+")[[1]])
  if (length(vals) < 6 || is.na(vals[1]) || vals[1] == -9999) next
  lf_rows[[length(lf_rows) + 1]] <- vals
}

lf_mat <- do.call(rbind, lf_rows)
lf_meta <- data.table(
  year  = lf_mat[, 1],
  month = lf_mat[, 2],
  fleet = lf_mat[, 3],
  Nsamp = lf_mat[, 6]
)

lf_props <- lf_mat[, 7:(6 + n_len), drop = FALSE]
colnames(lf_props) <- as.character(len_bins)

lf_wide <- cbind(lf_meta[, .(year, month, fleet, Nsamp)], as.data.table(lf_props))
setnames(lf_wide, "fleet", "fishery")
lf_wide <- lf_wide[year >= styr & year <= endyr]

ioyft_lf <- melt(lf_wide,
                 id.vars = c("year", "month", "fishery", "Nsamp"),
                 variable.name = "bin",
                 value.name = "value")
ioyft_lf[, bin := as.numeric(as.character(bin))]
ioyft_lf[, ts := year - styr + 1]
ioyft_lf[, week := 1]
ioyft_lf[, value := value * Nsamp]
ioyft_lf[, Nsamp := NULL]
ioyft_lf <- ioyft_lf[, .(year, month, ts, fishery, bin, value, week)]
setorder(ioyft_lf, fishery, year, month, bin)

# =============================================================================
# 4b. Extract aggregated length composition data
# =============================================================================

agg_lencomp_count_idx <- grep("^127$", data_lines)[1]
agg_lencomp_bin_idx <- agg_lencomp_count_idx + 1
agg_lencomp_nobs <- as.integer(strsplit(trimws(data_lines[agg_lencomp_count_idx]), "\\s+")[[1]][1])
agg_len_bins <- abs(as.numeric(strsplit(trimws(data_lines[agg_lencomp_bin_idx]), "\\s+")[[1]]))
agg_len_nbins <- length(agg_len_bins)

agg_lf_rows <- list()
for (i in seq(agg_lencomp_bin_idx + 1, agg_lencomp_bin_idx + agg_lencomp_nobs)) {
  line <- trimws(data_lines[i])
  if (grepl("^#", line) || line == "") next
  vals <- as.numeric(strsplit(trimws(line), "\\s+")[[1]])
  if (length(vals) <= agg_len_nbins) next

  prop_vals <- vals[(length(vals) - agg_len_nbins + 1):length(vals)]
  meta_vals <- vals[1:(length(vals) - agg_len_nbins)]

  year <- meta_vals[2]
  month <- meta_vals[3]
  fishery <- meta_vals[4]
  Nsamp <- meta_vals[7]

  if (!is.finite(year) || !is.finite(month) || !is.finite(fishery) || !is.finite(Nsamp)) next
  if (year < styr || year > endyr) next
  if (!(fishery %in% 17:20)) next
  if (!is.finite(Nsamp) || Nsamp <= 0) Nsamp <- 1

  ts <- as.integer(year - styr + 1)
  fishery <- 17 + ((ts - 81) %% 4)

  row_vals <- rep(0, length(len_bins))
  names(row_vals) <- as.character(len_bins)

  for (j in seq_along(prop_vals)) {
    prop <- prop_vals[j]
    if (is.na(prop) || prop <= 0) next

    agg_bin <- agg_len_bins[j]
    split_bins <- if (agg_bin < 0) c(10, 15) else c(agg_bin, agg_bin + 5)
    split_bins <- split_bins[split_bins %in% len_bins]

    if (length(split_bins) > 0) {
      row_vals[as.character(split_bins)] <- row_vals[as.character(split_bins)] + prop / length(split_bins)
    }
  }

  row_vals <- row_vals * Nsamp

  for (bin_val in len_bins) {
    agg_lf_rows[[length(agg_lf_rows) + 1]] <- data.table(
      year = year,
      month = month,
      ts = ts,
      fishery = fishery,
      bin = bin_val,
      value = row_vals[as.character(bin_val)],
      week = 1
    )
  }
}

if (length(agg_lf_rows) > 0) {
  agg_lf_dt <- rbindlist(agg_lf_rows, use.names = TRUE, fill = TRUE)
  agg_lf_dt <- agg_lf_dt[, .(
    year = as.numeric(year),
    month = as.numeric(month),
    ts = as.numeric(ts),
    fishery = as.numeric(fishery),
    bin = as.numeric(bin),
    value = as.numeric(value),
    week = as.numeric(week)
  )]
  ioyft_lf <- rbindlist(list(ioyft_lf, agg_lf_dt[, .(year, month, ts, fishery, bin, value, week)]), use.names = TRUE)
  setorder(ioyft_lf, fishery, year, month, bin)
}

# =============================================================================
# 5. Extract biological parameters from control.ss_new
# =============================================================================

ctl_lines <- readLines(file.path(dir_ss3, "control.ss_new"))

get_init <- function(pattern) {
  pline <- grep(pattern, ctl_lines, value = TRUE, fixed = TRUE)[1]
  as.numeric(strsplit(trimws(pline), "\\s+")[[1]][3])
}

# --- Natural mortality ---
M     <- c(0.3358, 0.2955, 0.2552, 0.2149, 0.1746, 0.1343, 0.1343, 0.1343, 0.1343, 0.1343, 0.1410, 0.1606, 0.1780, 0.1915, 0.1994, 0.2009, 0.1962, 0.1865, 0.1743, 0.1623, 0.1522, 0.1449, 0.1401, 0.1373, 0.1357, 0.1349, 0.1346, 0.1344)

# --- SS3 growth parameters (standard VB) ---
L_at_Amin <- 22   # L at SS3 age 0
Linf <- 145   # Linf (since Growth_Age_for_L2 = 999)
k_ss3     <- 0.455
CV_young  <- 0.1
CV_old    <- 0.1

# --- Convert to Schnute parameterization: A1=1, A2=n_age ---
# opal internal age i maps to SS3 age (i-1)
# L1 = VB(SS3 age 0) = L_at_Amin → assigned to opal age 1
# L2 = VB(SS3 age n_age-1) → assigned to opal age n_age
#
# Standard VB: L(a) = Linf - (Linf - L0) * exp(-k * a)
A1 <- 1L
A2 <- n_age  
L1 <- L_at_Amin  # VB at SS3 age 0 = opal age 1
L2 <- L1 + (Linf - L1) * (1 - exp(-k_ss3 * (A2 - A1)))

# --- Weight-length ---
lw_a <- 2.459e-5
lw_b <- 2.9667

# --- Maturity (length logistic) ---
mat50     <- get_init("Mat50%_Fem_GP_1")
mat_slope <- get_init("Mat_slope_Fem_GP_1")

len_mid         <- len_bins + len_bin_width / 2
maturity_at_age <- c(0, 0, 0, 0, 0.1, 0.15, 0.2, 0.3, 0.5, 0.7, 0.9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# --- Fecundity: eggs = Wt * (a + b*Wt); a=1, b=0 → eggs = weight ---
fec_a <- get_init("Eggs/kg_inter_Fem_GP_1")
fec_b <- get_init("Eggs/kg_slope_wt_Fem_GP_1")

wt_at_len        <- lw_a * len_mid^lw_b
fecundity_at_len <- wt_at_len 

# --- Stock-recruitment ---
SR_lnR0   <- as.numeric(strsplit(trimws(grep("SR_LN\\(R0\\)", ctl_lines, value = TRUE)[1]), "\\s+")[[1]][3])
SR_steep   <- get_init("SR_BH_steep")
SR_sigmaR  <- get_init("SR_sigmaR")

# --- Catchability ---
Q_vals  <- rep(1,n_index)

# --- Selectivity ---
sel_type_f <- c(rep(24,n_extract), rep(1, n_index))  # 1=logistic, 24=double-normal

# =============================================================================
# 6. Extract estimated parameters from ss.par
# =============================================================================

par_lines <- readLines(file.path(dir_ss3, "ss.par"))

get_par <- function(tag) {
  idx <- grep(tag, par_lines, fixed = TRUE)
  as.numeric(trimws(par_lines[idx + 1]))
}

# Selectivity parameters
sel_parm_vals <- sapply(1:10, function(j) get_par(paste0("# selparm[", j, "]:")))

par_sel <- matrix(0, nrow = n_fleet, ncol = 6)
par_sel[1, 1:2] <- sel_parm_vals[1:2]   # Comm: logistic
par_sel[2, 1:2] <- sel_parm_vals[3:4]   # Non_comm: logistic (fixed)
par_sel[3, 1:6] <- sel_parm_vals[5:10]  # ResFish: double-normal

# Recruitment deviations (historical only)
recdev_early <- as.numeric(strsplit(trimws(
  par_lines[grep("recdev_early:", par_lines, fixed = TRUE) + 1]
), "\\s+")[[1]])

recdev2 <- as.numeric(strsplit(trimws(
  par_lines[grep("recdev2:", par_lines, fixed = TRUE) + 1]
), "\\s+")[[1]])

early_yrs <- 1906:(1906 + length(recdev_early) - 1)
main_yrs  <- 1928:(1928 + length(recdev2) - 1)

all_dev_yrs <- c(early_yrs, main_yrs)
all_devs    <- c(recdev_early, recdev2)

dev_idx <- which(all_dev_yrs >= styr & all_dev_yrs <= endyr)
rdev_y  <- all_devs[dev_idx]

# =============================================================================
# 7. Assemble opaka_data (matching wcpo_bet_data structure)
# =============================================================================

opaka_data <- list(
  # Dimensions
  age_a        = age_a,         # 1:44 (opal convention)
  n_age        = n_age,         # 44
  n_season     = n_seas,        # 1
  n_fishery    = n_fleet,       # 3
  n_year       = n_year,        # 75
  first_yr     = 1L,
  last_yr      = n_year,
  years        = seq_len(n_year),

  # Length structure
  len_bin_start = len_bin_start,   # 5
  len_bin_width = len_bin_width,   # 5
  n_len         = n_len,           # 17

  # Catch
  first_yr_catch = 1L,
  catch_units_f  = catch_units,
  catch_obs_ysf  = catch_obs_ysf,

  # CPUE
  cpue_switch = 1L,
  n_index     = n_index,
  cpue_data   = cpue_data,

  # Biology
  lw_a      = lw_a,
  lw_b      = lw_b,
  maturity  = maturity_at_len,     # n_len vector
  fecundity = fecundity_at_len,    # n_len vector
  M         = M,                   # n_age vector (constant)

  # Growth reference ages (Schnute parameterization)
  A1 = A1,   # 1
  A2 = A2,   # 44 (= n_age)

  # Selectivity
  sel_type_f = sel_type_f,

  # Priors (placeholder)
  priors = list(
    log_B0     = list(type = "normal", par1 = 0, par2 = 0.2, index = 1),
    log_cpue_q = list(type = "normal", par1 = 0, par2 = 0.2, index = 2),
    par_sel    = list(type = "normal", par1 = 0, par2 = 0.2, index = 3),
    log_L1     = list(type = "normal", par1 = 0, par2 = 0.2, index = 4),
    log_L2     = list(type = "normal", par1 = 0, par2 = 0.2, index = 5),
    log_k      = list(type = "normal", par1 = 0, par2 = 0.2, index = 6),
    log_CV1    = list(type = "normal", par1 = 0, par2 = 0.2, index = 7),
    log_CV2    = list(type = "normal", par1 = 0, par2 = 0.2, index = 8)
  )
)

# =============================================================================
# 8. Assemble opaka_parameters (matching wcpo_bet_parameters structure)
# =============================================================================

opaka_parameters <- list(
  # Stock parameters
  log_B0         = SR_lnR0,           # 5.656 (SS3's LN(R0))
  log_h          = log(SR_steep),      # log(0.76)
  log_sigma_r    = log(SR_sigmaR),     # log(0.52)

  # Observation model
  log_cpue_q     = as.numeric(Q_vals), # [-3.773, -6.253]
  cpue_creep     = 0,
  log_cpue_tau   = -Inf,
  log_cpue_omega = 0,

  # Recruitment deviations
  rdev_y         = rdev_y,             # 75 values (1949-2023)

  # Selectivity
  par_sel        = par_sel,            # [3 × 6] matrix

  # Growth (Schnute parameterization, log-scale)
  log_L1         = log(L1),            # log(6.0)
  log_L2         = log(L2),            # log(67.498)
  log_k          = log(k_ss3),         # log(0.242)
  log_CV1        = log(CV_young),      # log(0.085)
  log_CV2        = log(CV_old)         # log(0.085)
)

# =============================================================================
# 9. Save
# =============================================================================

opaka_lf <- ioyft_lf

save(opaka_data, file = file.path(dir_out, "opaka_data.rda"))
save(opaka_lf,   file = file.path(dir_out, "opaka_lf.rda"))
save(opaka_parameters, file = file.path(dir_out, "opaka_parameters.rda"))

# =============================================================================
# 10. Summary
# =============================================================================

cat("\n=== Extraction complete (historical only: 1949-2023) ===\n\n")

cat("opaka_data.rda:\n")
cat(sprintf("  Dimensions: %d years × %d ages × %d fleets × %d length bins\n",
            n_year, n_age, n_fleet, n_len))
cat(sprintf("  age_a: %d:%d (opal internal), maps to SS3 ages %d-%d\n",
            min(age_a), max(age_a), 0, n_age - 1))
cat(sprintf("  Growth ref ages: A1=%d (L1=%.2f cm), A2=%d (L2=%.2f cm), k=%.3f\n",
            A1, L1, A2, L2, k_ss3))
cat(sprintf("  M = %.3f (constant)\n", M_val))
cat(sprintf("  LW: a=%.2e, b=%.2f\n", lw_a, lw_b))
cat(sprintf("  Maturity: logistic, Mat50=%.1f cm, slope=%.2f\n", mat50, mat_slope))
cat(sprintf("  Catch: %d non-zero entries (fleets 1-2)\n", sum(catch_obs_ysf > 0)))
n_cpue <- nrow(cpue_data)
n_cpue_f1 <- sum(cpue_data$fishery == 1)
n_cpue_f3 <- sum(cpue_data$fishery == 3)
cat(sprintf("  CPUE: %d obs (fleet 1: %d, fleet 3: %d)\n",
            n_cpue,
            n_cpue_f1,
            n_cpue_f3))
cat(sprintf("  Selectivity types: %s\n", paste(sel_type_f, collapse = ", ")))

cat("\nopaka_lf.rda:\n")
cat(sprintf("  %d rows (fleet 1: %d obs, fleet 3: %d obs)\n",
            nrow(opaka_lf),
            opaka_lf[, uniqueN(paste(year, month)), by = fishery][fishery == 1, V1],
            opaka_lf[, uniqueN(paste(year, month)), by = fishery][fishery == 3, V1]))

cat("\nopaka_parameters.rda:\n")
cat(sprintf("  SR: ln(R0)=%.3f, h=%.2f, sigmaR=%.2f\n", SR_lnR0, SR_steep, SR_sigmaR))
cat(sprintf("  Growth: log_L1=%.4f, log_L2=%.4f, log_k=%.4f\n",
            log(L1), log(L2), log(k_ss3)))
cat(sprintf("  CV: log_CV1=%.4f, log_CV2=%.4f (constant CV=%.3f)\n",
            log(CV_young), log(CV_old), CV_young))
cat(sprintf("  Q: %s\n", paste(sprintf("%.3f", Q_vals), collapse = ", ")))
cat(sprintf("  Rec devs: %d values (%d-%d)\n", length(rdev_y), styr, endyr))
cat(sprintf("  Selectivity: %d fleets × %d params\n", nrow(par_sel), ncol(par_sel)))
