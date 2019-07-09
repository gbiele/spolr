#' Bayesian logistic regression with Stan
#'
#' @export
#' @param formula Formula.
#' @param data data.frame.
#' @param scale.X logical value indicating if predictors are scaled
#' @param sd_prior_b standard deviation for the prior for regression weights
#' @return An object of class `spolr` with the slots
#' `par` and  `value` (taken from `stan::optimizing`),
#' `standata` (data for stan model fit),
#' `b` (regression weights),
#' `Intercept` (interecept for regression model), and
#' `formula` (R formula for regression model).
#'
#' Regularization is achieved by scaling predictors to mean zeros and
#' standard deviation of one and setting the standard deviation for the
#' prior of regression weights to two.
#'
slogreg <- function(formula,data,scale.X = T, sd_prior_b = 2) {
  standata = make_standata.spolr(formula, data = data, ordinal = F,
                                 scale.X = scale.X, sd_prior_b = sd_prior_b)
  out <- rstan::optimizing(stanmodels$slogreg, data = standata)
  out$beta = head(out$par,standata$K)
  names(out$beta) = colnames(standata$X)
  out$Intercept = out$par["Intercept"]
  out$formula = formula
  out[["standata"]] = standata
  out$scale.X = scale.X
  class(out) = "slogreg"
  return(out)
}

#' Prediction for slogreg
#'
#' @export
#' @param object Object of class `spolr` (returned from function `spolr`).
#' @param newdata data.frame with new data (optional).
#' @param type kind of predictions (probabilities, `probs` - or highest probability response, `class`).
#' @return predicted reponses
#'
predict.slogreg = function(object, newdata, type=c("probs", "class")) {
  type <- match.arg(type)
  if(missing(newdata)) {
    standata = object$standata
  } else {
    standata = make_standata.spolr(object$formula,
                                   data = newdata,
                                   ordinal = F,
                                   scale.X = object$scale.X,
                                   X.means = object$standata$X.means,
                                   X.sds = object$standata$X.sds,
                                   sd_prior_b = object$standata$sd_prior_b)
  }
  Y <- boot::inv.logit(object$Intercept + standata$X %*% c(object$beta))
  if(type == "class")
    Y = (Y > .5)*1
  return(Y)
}

