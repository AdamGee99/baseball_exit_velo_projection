// advanced model 

// iterating upon baseline model 
//
// Iterations:
// 1. add player-specific scales (sigma)
//

data {
  int<lower=0> N;                      // number of batted balls
  int<lower=0> J;                      // number of batters
  array[N] real <lower=0> y;           // exit velocity of each batted ball
  array[N] int <lower=1, upper=J> id;  // id of focal batter involved
}

parameters {
  array[J] real mu;                    // batter-specific location
  array[J] real <lower=0> sigma;       // batter-specific scale 
  real alpha;                          // common skew across all players
}

model {
  mu ~ normal(100, 10);                // prior for batter-specific intercept
  sigma ~ normal(20, 5);               // prior for batter-specific scale
  alpha ~ normal(-5, 5);               // prior for common skew
  
  for (i in 1:N){
    //skew_normal(location, scale, skew)
    y[i] ~ skew_normal(mu[id[i]], sigma[id[i]], alpha);
  }
}

