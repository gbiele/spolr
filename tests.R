library(boot)
N = 50000
K = 10

set.seed(123)
X = apply(matrix(rnorm(N*K),ncol = K),2,scale)
beta = seq(-2,2,length = K)
Intercept = 2

trials = 25
zeta = seq(-2,2,length = 4)

eta = Intercept + X %*% beta

Y_logitstic = inv.logit(eta) > runif(N)
glm(Y_logitstic ~ X, family = "binomial")

Y_binomial = rbinom(N,trials,inv.logit(eta))
glm(Y_binomial/trials ~ X, family = "binomial")
