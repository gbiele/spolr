rstan_create_package("spolr",
                     stan_files = c("H:/misc/spolr/inst/stan/slogreg.stan",
                                    "H:/misc/spolr/inst/stan/spolr.stan",
                                    "H:/misc/spolr/inst/stan/spolrn.stan",
                                    "H:/misc/spolr/inst/stan/sbinomial.stan"),
                     travis = F)
pkgbuild::compile_dll()
roxygen2::roxygenize()
devtools::install(local=FALSE)

library(spolr)
library(mice)
library(ordinal)
library(rstanarm)
library(MASS)
library(brms)
library(rstan)
mydata = wine
mydata$tmp = ((as.numeric(mydata$rating) + rnorm(72,3)) > 6)


fit_spolr = spolr(rating ~ bottle, mydata)
fit_polr = polr(rating ~ bottle, mydata)
fit_stan_polr = stan_polr(rating ~ bottle, mydata, prior = R2(0.1,"mean"), chains = 1)

fit_slogreg = slogreg(tmp ~ bottle, mydata, scale = F, sd_prior_b = 3)$beta
fit_gml = glm(tmp ~ bottle, mydata, family = "binomial")

brms_stan_model_code = make_stancode(rating ~ bottle, mydata, family = "cumulative")
brms_stan_model = stan_model(model_code = brms_stan_model_code)
brms_standata = make_standata(rating ~ bottle, mydata, family = "cumulative")
brms_fit_mcmc = sampling(brms_stan_model, data = brms_standata, chains = 4)
brms_fit_optim = optimizing(brms_stan_model, data = brms_standata)
brms_fit_direct = brm(rating ~ bottle, mydata, family = "cumulative")

brms_stan_model_reduced = stan_model("brms.stan")
brms_fit_mcmc_reduced = sampling(brms_stan_model_reduced, data = brms_standata, chains = 4)


brms_stan_model_code = make_stancode(tmp ~ bottle, mydata, family = "bernoulli")
brms_stan_model = stan_model(model_code = brms_stan_model_code)
brms_standata = make_standata(tmp ~ bottle, mydata, family = "bernoulli")
brms_fit_mcmc = sampling(brms_stan_model, data = brms_standata, chains = 4)
brms_fit_optim = optimizing(brms_stan_model, data = brms_standata)
brms_fit_direct = brm(tmp ~ bottle, mydata, family = "bernoulli")

brms_stan_model_reduced = stan_model("lrbrms.stan")
brms_fit_mcmc_reduced = sampling(brms_stan_model_reduced, data = brms_standata, chains = 4)



mydata$rating[sample(72,10)] = NA
mydata$tmp[sample(72,10)] = NA


trace(mice.impute.spolr,browser, at = 1)
trace(spolr,browser, at = 1)

imp = mice(mydata,
           m = 1,
           maxit=10,
           defaultMethod = c("pmm","slogreg","polyreg","spolr"))
