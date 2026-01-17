// advanced model 

// iterating upon baseline model 
//
// Iterations:
// 1. add player-specific scales (sigma)
//    - adopt
//
// 2. add hierarchical structure to zeta, omega
//    - adopt
//
// 3. add player-specific skew
//    - don't adopt
//
// 4. add global weight effect
//    - adopt
//
// 5. add global height effect
//    - don't adopt
//
// 6. add global age effect
//    - don't adopt

data {
  int<lower=0> N;                        // number of batted balls
  int<lower=0> J;                        // number of batters
  array[N] real <lower=0> y;             // exit velocity of each batted ball
  array[N] int <lower=1, upper=J> id;    // id of focal batter involved - mapped to 1,2,3...J
  
  vector[N] weight;                      // scaled weight of focal batter
}

parameters {
  real mu_zeta;                          // mean of batter locations
  real mu_omega;                         // mean of batter scales
  real <lower=0> sigma_zeta;             // variance of batter locations
  real <lower=0> sigma_omega;            // variance of batter scales
  
  vector[J] zeta;                        // batter-specific location
  vector<lower=0>[J] omega;              // batter-specific scale 
  real alpha;                            // common skew 
  
  real delta;                            // global weight effect
}

model {
  mu_zeta ~ normal(110, 5);              // prior for mean of batter locations
  mu_omega ~ normal(25, 2);              // prior for mean of batter scales
  sigma_zeta ~ normal(0, 2);             // prior for variance of batter locations
  sigma_omega ~ normal(0, 2);            // prior for variance of batter scales

  zeta ~ normal(mu_zeta, sigma_zeta);    // prior for batter-specific intercept
  omega ~ normal(mu_omega, sigma_omega); // prior for batter-specific scale
  alpha ~ normal(0, 1);                  // prior for common skew
  
  delta ~ normal(0, 1);                  // prior for global weight effect
  
  for (i in 1:N){
    //skew_normal(location, scale, skew)
    y[i] ~ skew_normal(zeta[id[i]] + delta*weight[i], omega[id[i]], alpha);
  }
}

