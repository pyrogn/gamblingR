data {
  int<lower=0> N; // number of observations
  real y[N]; // observed values
}

parameters {
  real mu; // mean of the normal distribution
  real<lower=0> sigma; // standard deviation of the normal distribution
}

model {
  // prior distributions
  mu ~ normal(10, 5); // normal prior with mean 10 and standard deviation 5
  sigma ~ cauchy(0, 5); // cauchy prior with location 0 and scale 5

  // likelihood function
  y ~ normal(mu, sigma); // normal likelihood with parameters mu and sigma
}
