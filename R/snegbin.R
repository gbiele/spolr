#' Bayesian regularized negative-binomial regression with Stan
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
#' `phi` overdispersion, and
#' `formula` (R formula for regression model).
#'
#' Regularization is achieved by scaling predictors to mean zeros and
#' standard deviation of one and setting the standard deviation for the
#' prior of regression weights to two.
#'
snebgin <- function(formula,data, scale.X = T, sd_prior_b = 2) {
  standata = make_standata.spolr(formula, data = data, family = "negbin",
                                 scale.X = scale.X, sd_prior_b = sd_prior_b)
  out <- rstan::optimizing(stanmodels$snegbin, data = standata)
  out$beta = head(out$par,standata$K)
  out$Intercept = out$par[standata$K+1]
  out$phi = out$par[standata$K+2]
  names(out$beta) = colnames(standata$X)
  out$formula = formula
  out[["standata"]] = standata
  out$scale.X = scale.X
  class(out) = "snegbin"
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
predict.snegbin = function(object, newdata, type= c("response","linear"), method = "logistic") {
  type <- match.arg(type)
  if(missing(newdata)) {
    standata = object$standata
  } else {
    standata = make_standata.spolr(object$formula,
                                   data = newdata,
                                   family = "negbin",
                                   scale.X = object$scale.X,
                                   X.means = object$standata$X.means,
                                   X.sds = object$standata$X.sds,
                                   sd_prior_b = object$standata$sd_prior_b)
  }

  theta = object$Intercept + drop(standata$X %*% object$beta)

  if(type == "response") {
    return(rnbinom(standata$N, size = object$phi, mu = exp(theta)))
  } else {
    return(theta)
  }

}

