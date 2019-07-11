#' Bayesian regularized gaussian regression with Stan
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
#' `sigma` error variance, and
#' `formula` (R formula for regression model).
#'
#' Regularization is achieved by scaling predictors to mean zeros and
#' standard deviation of one and setting the standard deviation for the
#' prior of regression weights to two.
#'
sgauss <- function(formula,data, scale.X = T, sd_prior_b = 2) {
  standata = make_standata.spolr(formula, data = data, family = "gauss",
                                 scale.X = scale.X, sd_prior_b = sd_prior_b)
  out <- rstan::optimizing(stanmodels$sgauss, data = standata)
  out$beta = head(out$par,standata$K)
  out$Intercept = out$par[standata$K+1]
  out$sigma = out$par[standata$K+2]
  names(out$beta) = colnames(standata$X)
  out$formula = formula
  out[["standata"]] = standata
  out$scale.X = scale.X
  class(out) = "sgauss"
  return(out)
}

#' Prediction for sgauss
#'
#' @export
#' @param object Object of class `sgauss` (returned from function `sgauss`).
#' @param newdata data.frame with new data (optional).
#' @return predicted reponses
#'
predict.sgauss = function(object, newdata) {
  if(missing(newdata)) {
    standata = object$standata
  } else {
    standata = make_standata.spolr(object$formula,
                                   data = newdata,
                                   family = "sgauss",
                                   scale.X = object$scale.X,
                                   X.means = object$standata$X.means,
                                   X.sds = object$standata$X.sds,
                                   sd_prior_b = object$standata$sd_prior_b)
  }

  mu <- object$Intercept + drop(standata$X %*% object$beta)
  return(rnorm(standata$N,mu,object$sigma))

}

