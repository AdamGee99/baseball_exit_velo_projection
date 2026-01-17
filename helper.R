#helper functions


### EDA functions ###


#function that plots exit velo for a df, could be a single player or multiple players
plot_exit_velo_dist = function(df, title = NULL) {
  if(length(unique(df$batter)) == 1) {
    player_name = unique(df$player_name)
  } else if(is.character(title)) {
    player_name = title
    } else {
    player_name = "All Players"
  }
  
  ggplot(df, mapping = aes(x = exit_velo, y = after_stat(density))) +
    geom_histogram(colour = "black", fill = "orange", bins = 30) +
    labs(x = "Exit Velocity (mph)", y = "Density", title = player_name) +
    scale_x_continuous(limits = c(0, 130), n.breaks = 12) +
    scale_y_continuous(n.breaks = 4) +
    theme_bw()
}



### MCMC functions ###


#function that gets batters mean given their estimated location, scale and skew parameters
#this is the exact mean from the skewed normal distribution given the location, scale and skew parameters 
#https://en.wikipedia.org/wiki/Skew_normal_distribution
get_skew_mean = function(location, scale, skew) {
  location + scale*(skew/sqrt(1 + skew^2))*sqrt(2/pi)
}

#function that gets all 4000 draws of parameter sets for each player
get_par_draws = function(fit, player_pars, global_pars) {
  player_specific_par_draws = fit$draws(player_pars) %>% 
    as_draws_df() %>%
    select(-c(".chain", ".iteration")) %>%
    pivot_longer(-.draw) %>%
    mutate(stan_batter_id = as.numeric(gsub(".*\\[|\\]", "", name)), 
           param = gsub("\\[.*", "", name)) %>%
    select(-name) %>%
    pivot_wider(names_from = param, values_from = value)
  
  if(is.character(global_pars)) {
    global_par_draws = fit$draws(global_pars) %>%
      as_draws_df() %>%
      select(-c(.chain, .iteration))
  }
  
  player_specific_par_draws %>% 
    full_join(global_par_draws, by = ".draw") %>%
    left_join(height_weight, by = "stan_batter_id") 
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
    scale_x_continuous(limits = c(78, 98), n.breaks = 11) + 
    scale_y_continuous(limits = c(78, 98), n.breaks = 11) + 
    labs(x = "True Mean Exit Velocity (mph)", y = "Predicted Mean Exit Velocity (mph)") +
    theme_bw()
}

#function that takes in results and outputs rmse
get_rmse = function(results) {
  results %>% 
    summarise(rmse = sqrt(mean((true_mean_exit_velo - pred_mean_exit_velo)^2))) %>%
    pull(rmse)
}




### Plotting functions ###


#function that plots the predicted mean exit velo of 2025 distributions for certain players against truth 
#players is the stan_batter_id map to player name df
#draws is the 4000 posterior draws df for each player 
plot_pred_mean_dists = function(players, draws) {
  #for geom_segment below
  geom_seg_vals = players_vis %>%
    left_join(true_vals) %>%
    mutate(player_name = factor(player_name, levels = players_vis$player_name),
           ymin = row_number() - 0.3,
           ymax = row_number() + 0.3)
  
  #player predictive distributions vs true mean exit velo in 2025
  pred_mean_dists = draws %>%
    ggplot(mapping = aes(x = pred_mean_exit_velo, y = factor(player_name, levels = players_vis$player_name))) +
    stat_histinterval(fill = "#00A3E0") +
    geom_segment(data = geom_seg_vals,
                 aes(x = true_mean_exit_velo, xend = true_mean_exit_velo, y = ymin, yend = ymax,
                     color = "True Mean Exit\nVelocity in 2025"), linewidth = 1.1, 
                 key_glyph = 'vline') +
    labs(x = "Estimated 2025 Mean Exit Velocity (mph)", y = "Batter") +
    scale_x_continuous(limits = c(81.5, 93), n.breaks = 10) +
    scale_color_manual(name = "", values=c("True Mean Exit\nVelocity in 2025" = "red"))+
    theme_bw()
  pred_mean_dists
}


#function that plots player predictive distribution against their observed exit velo dist in 2025
#id is stan_batter_id
plot_player_predictive_dist = function(player_id, player_name, fit_results) {
  player_results = fit_results %>% filter(stan_batter_id == player_id)
  
  mlb_full %>% 
    filter(game_year == 2025, stan_batter_id == player_id) %>%
    ggplot(mapping = aes(x = exit_velo)) +
    #observed distribution in 2025
    geom_histogram(aes(y = after_stat(density), fill = "Observed 2025\nExit Velocities"), colour = "black") + 
    #predictive distribution from 2024
    stat_function(fun = dsn, 
                  args = list(xi = player_results$zeta + player_results$delta*player_results$height_scaled,
                              omega = player_results$omega,
                              alpha = player_results$alpha),
                  aes(col = "Predictive\nDistribution"),
                  size = 2) +
    scale_x_continuous(limits = c(0, 120)) + 
    labs(x = "Exit Velocity (mph)", y = "Density") +
    annotate("text", x = 15, y = 0.05, label = player_name, size = 6) + 
    scale_fill_manual(name = "", values = c("Observed 2025\nExit Velocities" = "#00A3E0")) + 
    scale_color_manual(name = "", values = c("Predictive\nDistribution" = "black")) +
    guides(fill = guide_legend(order = 2, override.aes = list(color = NA)), 
           color = guide_legend(order = 1)) +
    theme_bw()
}




