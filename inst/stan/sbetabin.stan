// modified from code generated with brms 2.9.0

functions {
  real beta_binomial2_lpmf(int[] y, vector mu, real phi, int T) {
    return beta_binomial_lpmf(y | T, mu * phi, (1 - mu) * phi);
  }
}

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
  real phi;
}
model {
  vector[N] alpha = inv_logit(Intercept + X * b);
  // priors including all constants
  target += student_t_lpdf(Intercept | 3, 0, 10);
  target += normal_lpdf(b | 0, sd_prior_b);
  target += normal_lpdf(phi | 0, 10);
  // likelihood including all constants
  target += beta_binomial2_lpmf(Y | alpha, phi, trials);

}
