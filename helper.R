#helper functions


### Plotting functions ###


#function that plots exit velo for a df, could be a single player or multiple players
plot_exit_velo_dist = function(df) {
  if(length(unique(df$batter)) == 1) {
    player_name = unique(df$player_name)
  } else {
    player_name = "All Players"
  }
  
  ggplot(df, mapping = aes(x = exit_velo)) +
    geom_histogram(colour = "black", fill = "orange", bins = 30) +
    labs(x = "Exit Velocity (mph)", y = "Count", title = player_name) +
    scale_x_continuous(limits = c(0, 130), n.breaks = 12) +
    theme_bw()
}




### MCMC functions ###



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
  df = if(is.character(global_pars)) {
    global_pars_sum = fit$summary(global_pars) %>% 
      select(variable, mean) %>%
      pivot_wider(names_from = variable, values_from = mean)
    
    #join
    cbind(player_pars_sum, global_pars_sum)
  } else {
    player_pars_sum
  }
  
  #join covariates and true values and return
  df %>% left_join(true_vals, by = "stan_batter_id") %>%
    left_join(height_weight, by = "stan_batter_id") 
}

#function that takes in fitted pars and outputs df with true values and predicted values
get_results = function(fitted_pars, true_vals, covs) {
  
  #join true values, covs, and get predictions
  left_join(fitted_pars, true_vals, by = "stan_batter_id") %>%
    left_join(height_weight, by = "stan_batter_id") %>%
    mutate(pred_mean_exit_velo = get_skew_mean(location = zeta + delta*height_scaled, scale = omega, skew = alpha)) 
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


