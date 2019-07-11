// modified from code generated with brms 2.9.0
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  real sd_prior_b;
}
parameters {
  vector[K] b;  // population-level effects
  real Intercept;  // intercept
  real<lower=0> sigma;
}
model {
  // priors including all constants
  target += student_t_lpdf(Intercept | 3, 0, 10);
  target += normal_lpdf(b | 0, sd_prior_b);
  target += student_t_lpdf(sigma | 3, 0, 10);
  // likelihood including all constants
  target += normal_id_glm_lpdf(Y | X, Intercept, b, sigma);
}
