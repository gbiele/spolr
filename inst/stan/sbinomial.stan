// modified from code generated with brms 2.9.0
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  real sd_prior_b;
  int<lower=2> trials;
}
parameters {
  vector[K] b;  // population-level effects
  real Intercept;
}
model {
  vector[N] alpha = Intercept + X * b;
  // priors including all constants
  target += student_t_lpdf(Intercept | 3, 0, 10);
  target += normal_lpdf(b | 0, sd_prior_b);
  // likelihood including all constants
  target += binomial_logit_lpmf(Y | trials, alpha);

}
