#'Imputation of ordered data by bayesian ordered logistic regression
#'
#'Imputes missing data in a categorical variable using polytomous regression
#'@aliases mice.impute.spolr
#'@return Vector with imputed data, same type as \code{y}, and of length
#'\code{sum(wy)}
#'@details
#'The function \code{mice.impute.spolr()} imputes for ordered categorical response
#'variables by using the proportional odds logistic regression (polr) model. The
#'model is also known as the cumulative link model. The model is fit in Stan and
#'uses weakly informative (shrinkage) priors to regularize
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
  attr(imputations,"spolr.fit") = fit
  return(imputations)
}
