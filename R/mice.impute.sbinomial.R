#'Imputation using bayesian regularized regression
#'
#'Imputes missing data in a count variable using binomial regression
#'@aliases mice.impute.sbinomial
#'@return Vector with imputed data, same type as \code{y}, and of length
#'\code{sum(wy)}
#'@details
#'The function \code{mice.impute.sbinomial()} imputes for binomial response
#'variables by using the binomial regression with a logit link function.
#'The model is fit in Stan and uses weakly informative (shrinkage) priors to
#'regularize regression coefficients.
#'
#'
#'@author Guido Biele
#'@family univariate imputation functions
#'@keywords datagen
#'@export
#'
mice.impute.sbinomial <- function(y, ry, x, wy = NULL, ...)
{
  if (is.null(wy)) wy <- !ry

  x <- as.matrix(x)
  xy <- cbind.data.frame(y = y, x = x)

  fit = suppressWarnings(sbinomial(formula(xy), data = xy[ry, , drop = FALSE]))
  imputations = predict(fit, xy[wy, , drop = FALSE], type = "response")
  fit$standata$X = NULL
  fit$standata$Y = NULL
  attr(fit$formula,".Environment") = NULL
  attr(imputations,"spolr.fit") = fit
  return(imputations)
}
