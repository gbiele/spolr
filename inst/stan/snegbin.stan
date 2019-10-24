// modified from code generated with brms 2.9.0
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  real sd_prior_b;
}
parameters {
  vector[K] b;  // population-level effects
  real Intercept;
  real<lower = 0> phi;
}
model {
  vector[N] eta = Intercept + X * b;
  // priors including all constants
  target += student_t_lpdf(Intercept | 3, 0, 10);
  target += normal_lpdf(b | 0, sd_prior_b);
  target += normal_lpdf(phi | 0, 200);
  // likelihood including all constants
  target += neg_binomial_2_log_lpmf(Y | eta, phi);
}
