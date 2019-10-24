#' Bayesian regularized beta-binomial regression with Stan
#'
#' @export
#' @param formula Formula.
#' @param data data.frame.
#' @param scale.X logical value indicating if predictors are scaled
#' @param sd_prior_b standard deviation for the prior for regression weights
#' @return An object of class `spolr` with the slots
#' `par` and `value` (taken from `stan::optimizing`),
#' `standata` (data for stan model fit),
#' `b` (regression weights),
#' `phi` overdisperion parameters, and
#' `formula` (R formula for regression model).
#'
#' Regularization is achieved by scaling predictors to mean zeros and
#' standard deviation of one and setting the standard deviation for the
#' prior of regression weights to two.
#'
#' Optimization is initialized with Interecpt and regression coeffcients at 0.
sbetabin <- function(formula,data, scale.X = T, sd_prior_b = 2) {
  standata = make_standata.spolr(formula, data = data, family = "betabin",
                                 scale.X = scale.X, sd_prior_b = sd_prior_b)
  inits = list(Intercept = 0, phi = 1, b = rep(0,ncol(standata$X)))
  out <- rstan::optimizing(stanmodels$sbetabin, data = standata, init = inits)
  out$beta = head(out$par,standata$K)
  out$Intercept = out$par[standata$K+1]
  out$phi = out$par[standata$K+2]
  names(out$beta) = colnames(standata$X)
  out$formula = formula
  out[["standata"]] = standata
  out$scale.X = scale.X
  class(out) = "sbetabin"
  return(out)
}

#' Prediction for snegbin
#'
#' @export
#' @param object Object of class `spolr` (returned from function `spolr`).
#' @param newdata data.frame with new data (optional).
#' @param type kind of predictions ( `response` or `linear`).
#' @return predicted reponses
#'
predict.sbetabin = function(object, newdata, type= c("response","linear"), method = "logistic") {
  type <- match.arg(type)
  if(missing(newdata)) {
    standata = object$standata
  } else {
    standata = make_standata.spolr(object$formula,
                                   data = newdata,
                                   family = "betabin",
                                   scale.X = object$scale.X,
                                   X.means = object$standata$X.means,
                                   X.sds = object$standata$X.sds,
                                   sd_prior_b = object$standata$sd_prior_b)
  }

  theta <- object$Intercept + drop(standata$X %*% object$beta)

  if(type == "response") {
    mu = boot::inv.logit(theta)
    alpha = mu * object$phi
    beta = (1-mu) * object$phi
    return(extraDistr::rbbinom(standata$N,standata$trials, alpha, beta))
  } else {
    return(theta)
  }

}

