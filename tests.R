#devtools::install(local=FALSE)

library(boot)
library(spolr)
library(mice)
library(extraDistr)

set.seed(123)
make_data = function(N,K,beta) {
  X = apply(matrix(rnorm(N*K),ncol = K),2,scale)

  Intercept = 2

  trials = 25
  zeta = seq(-2,2,length = 4)

  eta = 1 + X %*% beta
  etanb = Intercept + X %*% beta/(K/2.5)
  etabb = Intercept + X %*% beta#/(K/2.5)


  cumpr = matrix(plogis(matrix(zeta, N, length(zeta), byrow=TRUE) - drop(eta)),
                 ncol =  length(zeta))
  Y_oridnal = t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
  Y_oridnal = ordered(apply(Y_oridnal,1,function(x) which(cumsum(x) > runif(1))[1]))

  data  = data.frame(Y_binomial = rbinom(N,trials,inv.logit(eta)),
                     Y_logistic = factor(inv.logit(eta) > runif(N)),
                     Y_ordinal = Y_oridnal,
                     Y_negbin = rnbinom(N,size = 20,mu = exp(etanb)),
                     Y_betabin = rbbinom(N,25, 25*inv.logit(etabb), 25*(1-inv.logit(etabb))),
                     X)
  return(data)

}

N = 10000
K = 25
beta = seq(-1.5,1.5,length = K)
data = make_data(N,K,beta)


Y = data[1:min(N,10000),grep("Y_",names(data))]
Y$Y_ordinal = as.numeric(Y$Y_ordinal)
Y$Y_logistic = as.numeric(Y$Y_logistic)
round(cor(Y),digits = 2)
pairs(Y, panel = function(...) smoothScatter(..., nrpoints = 0, add = TRUE),
      gap = 0.2)

f_logistic = as.formula(paste("Y_logistic ~ ", paste(names(data)[-c(1:5)],collapse = " + ")))
f_binomial = as.formula(paste("Y_binomial ~ ", paste(names(data)[-c(1:5)],collapse = " + ")))
f_ordinal = as.formula(paste("Y_ordinal ~ ", paste(names(data)[-c(1:5)],collapse = " + ")))
f_negbin = as.formula(paste("Y_negbin ~ ", paste(names(data)[-c(1:5)],collapse = " + ")))
f_betabin = as.formula(paste("Y_betabin ~ ", paste(names(data)[-c(1:5)],collapse = " + ")))


round(slogreg(f_logistic, data = data)$beta - beta,digits = 4)
round(spolr(f_ordinal, data = data)$beta - beta, digits = 4)
round(sbinomial(f_binomial, data = data)$beta - beta, digits = 4)
round(snebgin(f_negbin, data = data)$beta - beta/3, digits = 4)
round(sbetabin(f_betabin, data = data)$beta - beta/2, digits = 4)




library(mice)
N = 100
K = 50
beta = seq(-1.5,1.5,length = K)

data = make_data(N,K, beta)
NA_idx = cbind(sample(nrow(data),150, replace = T),sample(ncol(data),150,replace = T))
for (k in 1:nrow(NA_idx))
  data[NA_idx[k,1],NA_idx[k,2]] = NA

method = rep("sgauss",ncol(data))
names(method) = names(data)
method[1:5] = c("sbinomial","slogreg","spolr","sbetabin","sbetabin")

imp = mice(data,
           method = method,
           m =  5)

complete.data = complete(imp)
for (v in names(imp$method)) {
  missing_rows = attr(imp$imp[[v]],"row.names")
  f = imp$formulas[v][[1]]
  X = model.matrix(f,complete.data[missing_rows,])
}

tmp1 = mice.mids(imp,maxit = 1)
tmp2 = mice.mids(tmp1,maxit = 1)

tmp = do.call(f, args = args)
imputes[cc] = tmp
attr(imputes,"model.parameters") = attr(tmp,"model.parameters")
