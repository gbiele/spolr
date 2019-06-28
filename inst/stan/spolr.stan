// modified from code generated with brms 2.9.0
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  real sd_prior_b;
  int<lower=2> ncat;  // number of categories
}
parameters {
  vector[K] b;  // population-level effects
  ordered[ncat - 1] Intercept;  // temporary thresholds
}
model {
  vector[N] mu = X * b;
  // priors including all constants
  target += student_t_lpdf(Intercept | 3, 0, 10);
  target += normal_lpdf(b | 0, sd_prior_b);
  // likelihood including all constants
    for (n in 1:N) {
      target += ordered_logistic_lpmf(Y[n] | mu[n], Intercept);
    }
}
