#' Bayesian regularized ordered logistic regression with Stan
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
#' `zeta` (cut points for ordinal regression model), and
#' `formula` (R formula for regression model).
#'
#' Regularization is achieved by scaling predictors to mean zeros and
#' standard deviation of one and setting the standard deviation for the
#' prior of regression weights to two.
#'
spolrn <- function(formula,data, scale.X = T, sd_prior_b = 2) {
  standata = make_standata.spolr(formula, data = data, family = "cumulative",
                                 scale.X = scale.X, sd_prior_b = sd_prior_b)
  out <- rstan::optimizing(stanmodels$spolrn, data = standata)
  out$beta = head(out$par,standata$K)
  names(out$beta) = colnames(standata$X)
  out$zeta = tail(out$par,standata$ncat-1)
  out$formula = formula
  out[["standata"]] = standata
  out$scale.X = scale.X
  class(out) = "spolr"
  return(out)
}

#' Prediction for spolrn
#'
#' @export
#' @param object Object of class `spolr` (returned from function `spolr`).
#' @param newdata data.frame with new data (optional).
#' @param type kind of predictions (probabilities, `probs` - or highest probability response, `class`).
#' @return predicted reponses
#'
predict.spolrn = function(object, newdata, type= c("probs","class"), method = "logistic") {
  type <- match.arg(type)
  if(missing(newdata)) {
    standata = object$standata
  } else {
    standata = make_standata.spolr(object$formula,
                                   data = newdata,
                                   family = "cumulative",
                                   scale.X = object$scale.X,
                                   X.means = object$standata$X.means,
                                   X.sds = object$standata$X.sds,
                                   sd_prior_b = object$standata$sd_prior_b)
  }

  n <- nrow(standata$X)
  q <- length(object$zeta)
  eta <- drop(standata$X %*% object$beta)
  cumpr <- matrix(plogis(matrix(object$zeta, n, q, byrow=TRUE) - eta), ncol =  q)
  Y <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))

  levels = as.character(1:object$standata$ncat)
  if(type == "class")
    Y = factor(max.col(Y), levels=seq_along(levels), labels=levels)
  return(Y)
}

