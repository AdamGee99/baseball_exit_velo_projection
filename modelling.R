library(tidyverse)
library(here)
library(GGally)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggdist)
library(sn)
library(scattermore)


############################################## Modelling ############################################## 

mlb_full = read.csv(here("data", "mlb_2024_2025.csv"))

#train on 2024, test on 2025
train = mlb_full %>% filter(game_year == 2024)
test = mlb_full %>% filter(game_year == 2025)

#true player means in 2025 season - target
true_vals = test %>%
  select(stan_batter_id, exit_velo) %>%
  group_by(stan_batter_id) %>%
  summarise(true_mean_exit_velo = mean(exit_velo)) %>%
  ungroup()




### Baseline Model ### 

#for quick testing
#train = train %>% filter(stan_batter_id %in% 1:50)

#input to stan
stan_data = list(N = nrow(train),
                 J = train$stan_batter_id %>% unique() %>% length(),
                 y = train$exit_velo,
                 id = train$stan_batter_id)

stan_file = here("stan", "baseline.stan")
#model
mod = cmdstan_model(stan_file)

#sample from model
fit = mod$sample(data = stan_data,
                 seed = 123,
                 chains = 4,
                 parallel_chains = 4,
                 refresh = 100)



#save baseline fit
#fit$save_object(file = here("stan fits", "baseline.RDS"))

#read in fit
fit = readRDS(file = here("stan fits", "baseline.RDS"))

fit$summary()
#all chains show good convergence - rhat close to 1

#posteriors
mcmc_areas(fit$draws(c("mu"))) #locations
mcmc_areas(fit$draws(c("sigma"))) #scales
mcmc_areas(fit$draws(c("alpha"))) #skew


#function that gets batters mean given their estimated location, scale and skew parameters
#this is the exact mean from the skewed normal distribution given the location, scale and skew parameters 
#https://en.wikipedia.org/wiki/Skew_normal_distribution
get_skew_mean = function(location, scale, skew) {
  location + scale*(skew/sqrt(1 + skew^2))*sqrt(2/pi)
}

#player and global pars inputted as characters
get_player_pars = function(fit, player_pars, global_pars) {
  #player-speciic pars
  player_pars_sum = fit$summary(player_pars) %>% 
    select(variable, mean) %>%
    mutate(stan_batter_id = as.numeric(gsub(".*\\[|\\]", "", variable)),
           param = gsub("\\[.*", "", variable)) %>%
    select(-variable) %>%
    pivot_wider(names_from = param, values_from = mean)
  
  #global (shared) pars
  global_pars_sum = fit$summary(global_pars) %>% 
    select(variable, mean) %>%
    pivot_wider(names_from = variable, values_from = mean)

  #join
  cbind(player_pars_sum, global_pars_sum)
}

#function that takes in fitted pars and outputs df with true values and predicted values
get_results = function(fitted_pars, true_vals) {
  full_join(fitted_pars, true_vals, by = "stan_batter_id") %>%
    mutate(pred_mean_exit_velo = get_skew_mean(location = mu, scale = sigma, skew = alpha)) 
}

#function that takes in results and plot true vs predicted
plot_results = function(results) {
  ggplot(results, mapping = aes(x = true_mean_exit_velo, y = pred_mean_exit_velo)) + 
    geom_point() + 
    geom_abline(slope = 1, intercept = 0, colour = "orange", size = 1) +
    scale_x_continuous(limits = c(80, 98), n.breaks = 10) + 
    scale_y_continuous(limits = c(80, 98), n.breaks = 10) + 
    labs(x = "True Mean Exit Velocity (mph)", y = "Predicted Mean Exit Velocity (mph)") +
    theme_bw()
}

#function that takes in results and outputs rmse
get_rmse = function(results) {
  results %>% 
    summarise(rmse = sqrt(mean((true_mean_exit_velo - pred_mean_exit_velo)^2))) %>%
    pull(rmse)
}

#fitted pars from baseline fit
baseline_fitted_pars_2024 = get_player_pars(fit, c("mu"), c("sigma", "alpha"))

#results (true vals vs predicted vals)
baseline_results = get_results(baseline_fitted_pars_2024, true_vals)

#plot results
plot_results(baseline_results)


#rmse
baseline_rmse = get_rmse(baseline_results)
baseline_rmse
#1.59
#on avg model has 2025 seasonal mean error of 1.59 mph 





### Advanced Model ### 


#' building on baseline model one iteration at a time
#' if new iteration improves predictions, adopt it, if not scrap it


#' Iterations:
#' 1. add player-specific scales (sigma)
#'    - improvement 1.59 --> 1.44 rmse
#'    - but increases computation time
#' 
#' 2. add player-specific skew (alpha)


stan_file = here("stan", "advanced.stan")
#model
mod = cmdstan_model(stan_file)

#sample from model
fit = mod$sample(data = stan_data,
                 seed = 123,
                 chains = 4,
                 parallel_chains = 4,
                 refresh = 100)


#save advanced fit
#fit$save_object(file = here("stan fits", "advanced.RDS"))

#read in fit
fit = readRDS(file = here("stan fits", "advanced.RDS"))

fit$summary()
#good convergence - rhat close to 1

#posteriors
mcmc_areas(fit$draws(c("mu"))) #locations
mcmc_areas(fit$draws(c("sigma"))) #scales
mcmc_areas(fit$draws(c("alpha"))) #skew

#get fitted pars
advanced_fitted_pars_2024 = get_player_pars(fit, player_pars = c("mu", "sigma"), global_pars = c("alpha"))

#results (true vals vs predicted vals)
advanced_results = get_results(advanced_fitted_pars_2024, true_vals)

#plot results
plot_results(advanced_results)

#rmse
advanced_rmse = get_rmse(advanced_results)
advanced_rmse
#1.44


