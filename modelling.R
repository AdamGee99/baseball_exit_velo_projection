library(tidyverse)
library(here)
library(GGally)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggdist)
library(sn)
library(scattermore)
source(here("helper.R"))


############################################## Modelling ############################################## 

mlb_full = read.csv(here("data", "mlb_2024_2025.csv"))

#need to scale heights and weights for identifiability issues
mlb_full = mlb_full %>% mutate(height_scaled = (height - mean(height))/sd(height),
                               weight_scaled = (weight - mean(weight))/sd(weight),
                               age_scaled = (age_bat - mean(age_bat))/sd(age_bat))

#mapping of batter ids to stan batter ids
id_map = mlb_full %>% select(batter, stan_batter_id) %>% distinct()

#height weight covariates
height_weight = mlb_full %>% 
  group_by(stan_batter_id) %>% 
  select(stan_batter_id, starts_with("height"), starts_with("weight")) %>%
  distinct()

#train on 2024, test on 2025
train = mlb_full %>% filter(game_year == 2024)
test = mlb_full %>% filter(game_year == 2025)

#true player means in 2025 season (target)
true_vals = test %>%
  select(stan_batter_id, exit_velo) %>%
  group_by(stan_batter_id) %>%
  summarise(true_mean_exit_velo = mean(exit_velo)) %>%
  ungroup()



#naive rmse - mean of batters last season pred as mean of next season
naive = mlb_full %>%
  group_by(stan_batter_id, game_year) %>%
  summarise(mean_exit_velo = mean(exit_velo)) %>%
  ungroup() %>%
  pivot_wider(names_from = game_year, values_from = mean_exit_velo) %>%
  rename(pred_mean_exit_velo = `2024`,
         true_mean_exit_velo = `2025`)
naive_rmse = get_rmse(naive)
naive_rmse
#1.38





### Baseline Model ### 

#input to stan
stan_data = list(N = nrow(train),
                 J = train$stan_batter_id %>% unique() %>% length(),
                 y = train$exit_velo,
                 id = train$stan_batter_id,
                 weight = train$weight_scaled)

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
mcmc_areas(fit$draws(paste0("zeta[", rep(1:10), "]"))) #locations of first 10 
mcmc_areas(fit$draws(c("omega"))) #scales
mcmc_areas(fit$draws(c("alpha"))) #skew


#fitted pars from baseline fit
baseline_fitted_pars_2024 = get_player_pars(fit, c("zeta"), c("omega", "alpha"))

#results (true vals vs predicted vals)
baseline_results = baseline_fitted_pars_2024  %>%
  mutate(pred_mean_exit_velo = get_skew_mean(location = zeta,
                                             scale = omega,
                                             skew = alpha))

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
#' 
#' 2. adding hierarchical structure to zeta, omega
#'    - improvement 1.44 --> 1.41 rmse
#'    - increases computation time by quite a bit though
#' 
#' 3. adding player-specific skew
#'    - no improvement 1.41 --> 1.42 rmse
#'    - also increases model complexity and computation time
#'    - don't adopt
#' 
#' 4. adding global weight effect
#'    - slight improvement 1.4098 --> 1.4066
#'    - expected a bigger improvement, look into this
#'    - adopt
#'    
#' 5. adding global height effect
#'    -no improvement 1.4066 --> 1.4066
#'    -height and weight are pretty correlated
#'    -do not adopt
#'    
#' 6. adding global age effect
#'    -expect very small change here
#'    -no improvement 1.4066 --> 1.413
#'    -don't adopt


stan_file = here("stan", "advanced.stan")
#model
mod = cmdstan_model(stan_file)

#sample from model
fit = mod$sample(data = stan_data,
                 #init = 0,
                 seed = 123,
                 chains = 4,
                 parallel_chains = 4,
                 refresh = 100)


#save advanced fit
#fit$save_object(file = here("stan fits", "advanced_age_weight_scaled.RDS"))

#read in fit
#fit = readRDS(file = here("stan fits", "advanced_weight_scaled.RDS"))

fit$summary() %>% print(n = 30)
fit$summary("gamma")
#good convergence - rhat close to 1

#posteriors
mcmc_areas(fit$draws(paste0("zeta[", rep(1:10), "]"))) #locations of first 10 
mcmc_areas(fit$draws(c("delta"))) #weight effect
mcmc_areas(fit$draws(paste0("omega[", rep(1:10), "]"))) #scales
mcmc_areas(fit$draws(c("alpha"))) #skew
mcmc_areas(fit$draws(c("mu_zeta"))) #locations global mean
mcmc_areas(fit$draws(c("mu_omega"))) #scales global meanff
mcmc_areas(fit$draws(c("sigma_zeta", "sigma_omega"))) #location, scale, skew global variance

#get fitted pars
advanced_fitted_pars_2024 = get_player_pars(fit, player_pars = c("zeta", "omega"), global_pars = c("alpha", "delta", "gamma"))

#results (true vals vs predicted vals)
advanced_results = advanced_fitted_pars_2024 %>%
  mutate(pred_mean_exit_velo = get_skew_mean(location = zeta + delta*weight_scaled,
                                             scale = omega,
                                             skew = alpha))

#plot results
plot_results(advanced_results)

#rmse
advanced_rmse = get_rmse(advanced_results)
advanced_rmse
#1.4098 - no height/weight effect
#1.4077 - height, no weight
#1.4066 - weight, no height
#1.4066 - weight & height



mcmc_pairs(fit$draws(c("delta", "zeta[4]"))) #scaled height is 0 for this guy, so no ridge
mcmc_pairs(fit$draws(c("delta", "zeta[46]"))) #the worst rhat is for the heaviest guy
#so bad convergence for the extreme weight values, good when its 0
#this is an identifiability issue

mcmc_pairs(fit$draws(c("delta", "zeta[61]"))) #ridge is in the reverse direction for lighter players 





