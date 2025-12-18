// advanced model 

// iterating upon baseline model 
//
// Iterations:
// 1. add player-specific scales (sigma)
//    - adopt
//
// 2. add hierarchical structure
//    - adopt

data {
  int<lower=0> N;                        // number of batted balls
  int<lower=0> J;                        // number of batters
  array[N] real <lower=0> y;             // exit velocity of each batted ball
  array[N] int <lower=1, upper=J> id;    // id of focal batter involved - mapped to 1,2,3...J
}

parameters {
  real mu_zeta;                          // mean of batter locations
  real mu_omega;                         // mean of batter scales
  real <lower=0> sigma_zeta;             // variance of batter locations
  real <lower=0> sigma_omega;            // variance of batter scales
  
  array[J] real zeta;                    // batter-specific location
  array[J] real <lower=0> omega;         // batter-specific scale 
  real alpha;                            // common skew 
}

model {
  mu_zeta ~ normal(110, 5);              // prior for mean of batter locations
  mu_omega ~ normal(25, 2);              // prior for mean of batter scales
 
  sigma_zeta ~ normal(0, 5);             // prior for variance of batter locations
  sigma_omega ~ normal(0, 5);            // prior for variance of batter locations
  
  
  zeta ~ normal(mu_zeta, sigma_zeta);    // prior for batter-specific intercept
  omega ~ normal(mu_omega, sigma_omega); // prior for batter-specific scale
  alpha ~ normal(-10, 2);                // prior for common skew
  
  for (i in 1:N){
    //skew_normal(location, scale, skew)
    y[i] ~ skew_normal(zeta[id[i]], omega[id[i]], alpha);
  }
}

