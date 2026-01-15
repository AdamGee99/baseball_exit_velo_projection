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
mlb_full = mlb_full %>% mutate(height_centered = (height - mean(height)),
                               weight_centered = (weight - mean(weight)),
                               height_scaled = height_centered/sd(height),
                               weight_scaled = weight_centered/sd(weight))

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





### Baseline Model ### 

#for quick testing
train_subset = train %>% filter(stan_batter_id %in% 1:10)
stan_data_subset = list(N = nrow(train_subset),
                        J = train_subset$stan_batter_id %>% unique() %>% length(),
                        y = train_subset$exit_velo,
                        id = train_subset$stan_batter_id,
                        height = train_subset$height_scaled)
#input to stan
stan_data = list(N = nrow(train),
                 J = train$stan_batter_id %>% unique() %>% length(),
                 y = train$exit_velo,
                 id = train$stan_batter_id,
                 height = train$height_scaled)

stan_file = here("stan", "baseline.stan")
#model
mod = cmdstan_model(stan_file)

#sample from model
fit = mod$sample(data = stan_data_subset,
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
mcmc_areas(fit$draws(c("zeta"))) #locations
mcmc_areas(fit$draws(c("omega"))) #scales
mcmc_areas(fit$draws(c("alpha"))) #skew


#fitted pars from baseline fit
baseline_fitted_pars_2024 = get_player_pars(fit, c("zeta"), c("omega", "alpha"))

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
#' 4. adding global hieght effect
#'    - no improvement 1.41 --> 1.69
#'    - this should be an improvement so need to look into it
#'    - heights were centered and scaled
#'    
#' 5. adding global weight effect
#'
#' 
#' 



### problem is the model is non-identifiable 

#switches between a global height/weight and player-specifici intercept AND global intercetp and player-specific weight/height???




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
fit$save_object(file = here("stan fits", "advanced_height_scaled.RDS"))

#read in fit
#fit = readRDS(file = here("stan fits", "advanced_height_scaled.RDS"))

fit$summary() %>% print(n = 30)
#good convergence - rhat close to 1

#posteriors
mcmc_areas(fit$draws(paste0("zeta[", rep(1:10), "]"))) #locations of first 10 
mcmc_areas(fit$draws(c("delta"))) #height effect
mcmc_areas(fit$draws(paste0("omega[", rep(1:10), "]"))) #scales
mcmc_areas(fit$draws(c("alpha"))) #skew
mcmc_areas(fit$draws(c("mu_zeta"))) #locations global mean
mcmc_areas(fit$draws(c("mu_omega"))) #scales global meanff
mcmc_areas(fit$draws(c("sigma_zeta", "sigma_omega"))) #location, scale, skew global variance

#get fitted pars
advanced_fitted_pars_2024 = get_player_pars(fit, player_pars = c("zeta", "omega"), global_pars = c("alpha", "delta"))

#results (true vals vs predicted vals)
advanced_results = advanced_fitted_pars_2024 %>%
  mutate(pred_mean_exit_velo = get_skew_mean(location = zeta + delta*height_scaled,
                                             scale = omega,
                                             skew = alpha))

#plot results
plot_results(advanced_results)

#rmse
advanced_rmse = get_rmse(advanced_results)
advanced_rmse
#1.41







