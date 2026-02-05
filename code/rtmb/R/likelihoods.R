
#' Length Composition Likelihood
#'
#' Computes likelihood for observed length compositions using ALKs.
#'
#' @param lf_switch Vectors of indices for observations.
#' @param removal_switch_f Vectors of indices for observations.
#' @param lf_year,lf_season,lf_fishery Vectors of indices for observations.
#' @param lf_minbin Minimum size bin to be aggregated.
#' @param lf_obs Matrix of observed length proportions.
#' @param lf_n Vector of effective sample sizes.
#' @param par_log_lf_alpha Vector of effective sample sizes.
#' @param catch_pred_fya 3D array of predicted catch.
#' @param alk_ysal 4D array year, season, age, length_bin of ALKs.
#' @return List with predicted compositions and likelihood contributions.
#' @importFrom RTMB dmultinom
#' @importFrom RTMBdist ddirichlet ddirmult
#' @export
#' 
get_length_like <- function(lf_switch = 1, removal_switch_f, lf_year, lf_season, lf_fishery, lf_minbin, lf_obs, 
                            lf_n, par_log_lf_alpha, catch_pred_fya, alk_ysal) {
  "[<-" <- ADoverload("[<-")
  "c" <- ADoverload("c")
  n_lf <- nrow(lf_obs)
  n_bins <- 25
  n_age <- dim(catch_pred_fya)[3]
  lp <- numeric(n_lf)
  lf_pred <- matrix(0, n_lf, n_bins)
  for (i in seq_len(n_lf)) {
    y <- lf_year[i]
    s <- lf_season[i]
    f <- lf_fishery[i]
    catch_a <- catch_pred_fya[f, y,]
    pred <- catch_a %*% alk_ysal[y, s,, 1:n_bins]
    pred <- pred[1,] / sum(pred)
    obs <- lf_obs[i,]
    mbin <- lf_minbin[f]
    if (mbin > 1) {
      pred[mbin] <- sum(pred[1:mbin])
      obs[mbin] <- sum(obs[1:mbin])
      obs <- obs[mbin:n_bins]
      pred <- pred[mbin:n_bins]
    }
    lf_pred[i, mbin:n_bins] <- pred
    if (removal_switch_f[f] == 0 & lf_n[i] > 0) {
      if (lf_switch == 1) {
        # obs <- obs + 1e-6
        obs <- obs * lf_n[i]
        pred <- pred + 1e-6
        pred <- pred / sum(pred)
        lp[i] <- -dmultinom(x = obs, prob = pred, log = TRUE)
      }
      if (lf_switch == 2) {
        obs <- obs + 1e-6
        obs <- obs / sum(obs)
        pred <- pred + 1e-6
        pred <- pred / sum(pred) * lf_n[i] * exp(par_log_lf_alpha[f])
        lp[i] <- -ddirichlet(x = obs, alpha = pred, log = TRUE)
      }
      if (lf_switch == 3) {
        obs <- obs * lf_n[i]
        pred <- pred + 1e-6
        pred <- pred / sum(pred) * exp(par_log_lf_alpha[f])
        lp[i] <- -ddirmult(x = obs, size = lf_n[i], alpha = pred, log = TRUE)
      }
      if (lf_switch == 9) {
        pred <- pred + 1e-6 # I added this is as I was having Inf issues with pred
        lp[i] <- -lf_n[i] * sum(obs * log(pred))
        obs <- obs + 1e-6
        lp[i] <- lp[i] + lf_n[i] * sum(obs * log(obs))
      }
    }
  }
  return(list(pred = lf_pred, lp = lp))
}

#' CPUE length composition likelihood
#'
#' Computes likelihood for observed length compositions using ALKs.
#'
#' @param lf_switch Vectors of indices for observations.
#' @param cpue_years Vectors of indices for observations.
#' @param cpue_lfs Matrix of observed length proportions.
#' @param cpue_n Vector of effective sample sizes.
#' @param par_log_lf_alpha Vectors of indices for observations.
#' @param number_ysa 3D array of predicted catch.
#' @param sel_fya 3D array of predicted catch.
#' @param alk_ysal 4D array year, season, age, length_bin of ALKs.
#' @return List with predicted compositions and likelihood contributions.
#' @importFrom RTMB ADoverload dmultinom
#' @importFrom RTMBdist ddirichlet ddirmult
#' @export
#' 
get_cpue_length_like <- function(lf_switch = 1, cpue_years, cpue_lfs, cpue_n, par_log_lf_alpha, number_ysa, sel_fya, alk_ysal) {
  "[<-" <- ADoverload("[<-")
  "c" <- ADoverload("c")
  n_lf <- nrow(cpue_lfs)
  n_age <- dim(sel_fya)[3]
  n_bins <- 25
  lp <- numeric(n_lf)
  lf_pred <- matrix(0, n_lf, n_bins)
  for (i in seq_len(n_lf)) {
    y <- cpue_years[i]
    catch_a <- number_ysa[y, 2,] * sel_fya[7, y,]
    pred <- catch_a %*% alk_ysal[y, 2,, 1:n_bins]
    pred <- pred[1,] / sum(pred)
    lf_pred[i,] <- pred
    obs <- cpue_lfs[i,]
    if (lf_switch == 1) {
      obs <- obs * cpue_n[i]
      pred <- pred + 1e-6
      pred <- pred / sum(pred)
      lp[i] <- -dmultinom(x = obs, prob = pred, log = TRUE)
    }
    if (lf_switch == 2) {
      obs <- obs + 1e-6
      obs <- obs / sum(obs)
      pred <- pred + 1e-6
      pred <- pred / sum(pred) * cpue_n[i] * exp(par_log_lf_alpha[5])
      lp[i] <- -ddirichlet(x = obs, alpha = pred, log = TRUE)
    }
    if (lf_switch == 3) {
      obs <- obs * cpue_n[i]
      pred <- pred + 1e-6
      pred <- pred / sum(pred) * exp(par_log_lf_alpha[5])
      lp[i] <- -ddirmult(x = obs, size = cpue_n[i], alpha = pred, log = TRUE)
    }
    if (lf_switch == 9) {
      lp[i] <- -cpue_n[i] * sum(obs * log(pred))
      obs <- obs + 1e-6
      lp[i] <- lp[i] + cpue_n[i] * sum(obs * log(obs))
    }
  }
  return(list(pred = lf_pred, lp = lp))
}

#' CPUE index likelihood
#'
#' Computes the likelihood for a standardized CPUE index using a log-linear model.
#'
#' @param cpue_switch Integer switch to activate the likelihood.
#' @param cpue_a1,cpue_a2 Minimum and maximum CPUE age indices.
#' @param cpue_years a \code{vector} of year indices for CPUE observations.
#' @param cpue_obs a \code{vector} of observed CPUE values.
#' @param cpue_sd a \code{vector} of SDs associated with the observed CPUE values.
#' @param cpue_sigma Process error standard deviation.
#' @param cpue_omega Power parameter for scaling to total numbers.
#' @param log_cpue_q Logarithm of catchability coefficient.
#' @param cpue_creep Logarithm of catchability coefficient.
#' @param number_ysa a 3D \code{array} year, season, age of numbers-at-age.
#' @param sel_fya a 3D \code{array} of selectivity by fishery, year, and age.
#' @return a \code{list} with predicted CPUE, residuals, and likelihood vector.
#' @importFrom RTMB ADoverload dnorm
#' @export
#' 
get_cpue_like <- function(cpue_switch = 1, cpue_a1 = 5, cpue_a2 = 17, 
                          cpue_years, cpue_obs, cpue_sd, 
                          cpue_sigma = 0.2, cpue_omega = 1, log_cpue_q = -0.02040606, 
                          creep_init = 1, cpue_creep = 0.005, 
                          number_ysa, sel_fya) {
  "[<-" <- ADoverload("[<-")
  "c" <- ADoverload("c")
  n_cpue <- length(cpue_obs)
  n_age <- dim(sel_fya)[3]
  cpue_adjust <- cpue_log_pred <- lp <- numeric(n_cpue)
  cpue_adjust[1] <- creep_init
  for (i in 2:n_cpue) cpue_adjust[i] <- cpue_adjust[i - 1] + cpue_creep
  cpue_sig <- sqrt(cpue_sd^2 + cpue_sigma^2)
  for (i in seq_len(n_cpue)) {
    y <- cpue_years[i]
    cpue_sel <- sel_fya[7, y, 5:n_age] # age 4+
    cpue_n <- number_ysa[y, 2, 5:n_age] # season 2
    cpue_selm <- sel_fya[7, y, (cpue_a1 + 1):(cpue_a2 + 1)]
    tmpN <- sum(cpue_sel * cpue_n) / mean(cpue_selm)
    cpue_log_pred[i] <- log(cpue_adjust[i]) + cpue_omega * log(tmpN)
  }
  unscaled_pred <- cpue_log_pred
  cpue_log_pred <- cpue_log_pred - log(mean(exp(cpue_log_pred))) + log_cpue_q
  cpue_log_obs <- log(cpue_obs)
  cpue_log_obs <- OBS(cpue_log_obs)
  # cpue_resid <- (log(cpue_obs) - cpue_log_pred) / cpue_sig
  if (cpue_switch > 0) {
    lp[] <- -dnorm(x = cpue_log_obs, mean = cpue_log_pred, sd = cpue_sig, log = TRUE)
  }
  return(list(pred = exp(cpue_log_pred), 
              # resid = cpue_resid, # drop resid?
              # sigma = cpue_sig, 
              cpue_adjust = cpue_adjust, 
              unscaled_pred = exp(unscaled_pred), 
              lp = lp))
}
