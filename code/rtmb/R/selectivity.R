#' Compute selectivity-at-age by year
#'
#' Constructs the selectivity-at-age array by fishery and year, incorporating initial values and changes.
#'
#' @param n_age Total number of ages.
#' @param max_age Maximum model age.
#' @param first_yr Model start year.
#' @param first_yr_catch Vector of first catch years per fishery.
#' @param sel_min_age_f,sel_max_age_f,sel_end_f Logical vector indicating whether to extend the final selectivity across remaining ages.
#' @param sel_change_year_fy Matrix fishery, year indicating change years.
#' @param par_log_sel Vector of changes in selectivity (log-space).
#' @return 3D array fishery, year, age of selectivity values.
#' @export
#' 
get_selectivity <- function(n_age, max_age, first_yr, first_yr_catch, 
                            sel_min_age_f, sel_max_age_f, sel_end_f, sel_change_year_fy,
                            par_log_sel) {
  "[<-" <- ADoverload("[<-")
  "c" <- ADoverload("c")
  n_sel <- nrow(sel_change_year_fy)
  n_year <- ncol(sel_change_year_fy)
  ymin <- first_yr_catch - first_yr + 1
  sel_fya <- array(0, dim = c(n_sel, n_year, n_age))
  for (f in seq_len(n_sel)) {
    amin <- sel_min_age_f[f] + 1
    amax <- sel_max_age_f[f] + 1
    ipar <- 1
    for (y in ymin:n_year) {
      if (sel_change_year_fy[f, y] != 0) {
        sel_tmp <- exp(par_log_sel[[f]][ipar,])
        ipar <- ipar + 1
        sel_fya[f, y, amin:amax] <- sel_tmp / mean(sel_tmp)
        if (as.logical(sel_end_f[f]) && amax < max_age) {
          for (a in (amax + 1):n_age) {
            sel_fya[f, y, a] <- sel_fya[f, y, amax]
          }
        }
      } else {
        sel_fya[f, y, ] <- sel_fya[f, y - 1, ]
      }
    }
  }
  return(sel_fya)
}

#' Plot selectivity
#' 
#' Plot selectivity by fishery, year, and age.
#' 
#' @param data a \code{list} containing the data that was passed to \code{MakeADFun}.
#' @param object a \code{list} specifying the AD object created using \code{MakeADFun}.
#' @param posterior an \code{rstan} objected created using the \code{tmbstan} function.
#' @param probs a numeric vector of probabilities with values in \code{[0, 1]} for plotting quantiles of the posterior distribution.
#' @param years the years to show on the plot.
#' @param fisheries the fisheries to show on the plot.
#' @param ... options passed on to \code{geom_density_ridges}.
#' @return a \code{ggplot2} object.
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom stats median quantile
#' @importFrom reshape2 melt
#' @importFrom ggridges geom_density_ridges
#' @importFrom scales pretty_breaks
#' @export
#' 
plot_selectivity <- function(data, object, posterior = NULL, probs = c(0.025, 0.975), 
                             years = 2013:2022, 
                             fisheries = c("LL1", "LL2", "LL3", "LL4", "Indonesian", "Australian", "CPUE"), ...) {
  
  ages <- data$min_age:data$max_age
  yrs <- data$first_yr:data$last_yr
  fsh <- c("LL1", "LL2", "LL3", "LL4", "Indonesian", "Australian", "CPUE")
  rem <- c(data$removal_switch_f, 0)
  fyr <- c(data$first_yr_catch_f, 1969)
  # data$catch_obs_ysf[,,3]
  # data$catch_obs_ysf[,,4]
  # cut out years with no catch, drop LF crosses in these years too?
  
  # Range of estimated ages for selectivity
  
  range_ages <- data.frame(fishery = fsh, min = data$sel_min_age_f, max = data$sel_max_age_f, removal = rem) %>% 
    filter(fishery %in% fisheries) %>%
    filter(removal == 0) %>%
    mutate(fishery = factor(fishery, levels = fsh))
  
  # Selectivity change years
  
  dimnames(data$sel_change_year_fy) <- NULL
  df_change <- melt(data$sel_change_year_fy) %>% 
    mutate(fishery = fsh[Var1], year = yrs[Var2]) %>%
    select(-Var1, -Var2) %>%
    pivot_wider(names_from = fishery) %>%
    mutate_at(fsh, function(x){1 + cumsum(x > 0)}) %>%
    pivot_longer(LL1:Australian, names_to = "fishery", values_to = "change") %>%
    mutate(change = ifelse(change %% 2, 1, 2))
  
  # if (is.null(posterior)) {
  # df_sel <- get_array(object$report()$sel_fya) %>%
  df1 <- object$report()$sel_fya
  # df2 <- proj
  
  df_sel <- df1 %>%
    melt() %>%
    mutate(fishery = fsh[Var1], removal = rem[Var1], 
           first_yr_catch = fyr[Var1], year = yrs[Var2], 
           age = ages[Var3]) %>%
    left_join(df_change, by = join_by("year", "fishery")) %>% 
    filter(fishery %in% fisheries, 
           year %in% years, 
           year >= first_yr_catch,
           removal == 0)
  
  # df_hrate <- get_array(object$report()$hrate_fya) %>%
  # df_hrate <- object$report()$hrate_fya %>%
  #   melt() %>%
  #   mutate(fishery = fsh[Var1], removal = rem[Var1], 
  #          first_yr_catch = fyr[Var1], year = yrs[Var2], 
  #          age = ages[Var3]) %>%
  #   filter(fishery %in% fisheries, year %in% years, 
  #          year >= first_yr_catch, removal == 1) %>%
  #   group_by(fishery, year) %>%
  #   mutate(value = value / sum(value, na.rm = TRUE)) %>%
  #   mutate(change = ifelse(year %% 2, 1, 2))
  # } else {
  #   # df0 <- get_posterior(object = object, posterior = posterior, pars = "sel_fya", iters = 1)
  #   post <- extract(object = posterior, pars = "lp__", permuted = FALSE, include = FALSE)
  #   chains <- dim(post)[2]
  #   iters <- nrow(post)
  #   # iters <- 5
  #   r1 <- object$report(par = post[1, 1, ])
  #   sel_jifya <- array(data = NA, dim = c(chains, iters, dim(get_array(r1$sel_fya))))
  #   hrate_jifya <- array(data = NA, dim = c(chains, iters, dim(get_array(r1$hrate_fya))))
  #   for (j in 1:chains) {
  #     for (i in 1:iters) {
  #       r1 <- object$report(par = post[i, j, ])
  #       sel_jifya[j,i,,,] <- get_array(r1$sel_fya)
  #       hrate_jifya[j,i,,,] <- get_array(r1$hrate_fya)
  #     }
  #   }
  #   
  #   df_sel <- sel_jifya %>%
  #     melt() %>%
  #     rename(chain = Var1, iter = Var2) %>%
  #     mutate(fishery = fsh[Var3], removal = rem[Var3], year = yrs[Var4], age = ages[Var5]) %>%
  #     group_by(fishery, removal, year, age) %>%
  #     mutate(value = median(value)) %>%
  #     left_join(df_change, by = join_by("year", "fishery")) %>% 
  #     filter(fishery %in% fisheries, year %in% years, year >= data$first_yr_catch) %>%
  #     filter(removal == 0) %>%
  #     mutate(fishery = factor(fishery, levels = fsh))
  #   
  #   df_hrate <- hrate_jifya %>%
  #     melt() %>%
  #     rename(chain = Var1, iter = Var2) %>%
  #     mutate(fishery = fsh[Var3], removal = rem[Var3], year = yrs[Var4], age = ages[Var5]) %>%
  #     group_by(fishery, removal, year, age) %>%
  #     mutate(value = median(value)) %>%
  #     filter(fishery %in% fisheries, year %in% years, year >= data$first_yr_catch) %>%
  #     filter(removal == 1) %>%
  #     mutate(fishery = factor(fishery, levels = fsh)) %>%
  #     group_by(fishery, year) %>%
  #     mutate(value = value / sum(value, na.rm = TRUE)) %>%
  #     mutate(change = ifelse(year %% 2, 1, 2))
  # }
  
  # df <- bind_rows(df_sel, df_hrate) %>%
  df <- df_sel %>%
    mutate(fishery = factor(fishery, levels = fsh))
  
  # Age and length composition specifications
  
  specs_lf <- data.frame(year = data$lf_year + data$first_yr, fishery = fsh[data$lf_fishery])
  specs_af <- data.frame(year = data$af_year + data$first_yr, fishery = fsh[data$af_fishery])
  specs <- bind_rows(specs_lf, specs_af) %>%
    inner_join(df, by = join_by(year, fishery)) %>%
    select(year, fishery) %>%
    distinct() %>%
    mutate(value = NA) %>%
    mutate(fishery = factor(fishery, levels = fsh))
  
  ggplot(data = df, aes(x = age, y = year, height = value, group = year)) +
    geom_vline(data = range_ages, aes(xintercept = min), linetype = "dashed") +
    geom_vline(data = range_ages, aes(xintercept = max), linetype = "dashed") +
    geom_density_ridges(aes(fill = factor(change), colour = factor(change)), stat = "identity", alpha = 0.5, rel_min_height = 0) +
    geom_point(data = specs, aes(x = 0.25, y = year), shape = 4) +
    facet_wrap(fishery ~ .) +
    labs(x = "Age", y = "Year") +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_y_reverse(breaks = pretty_breaks())
}
