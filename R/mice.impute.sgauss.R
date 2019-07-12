#'Imputation of ordered data by bayesian ordered logistic regression
#'
#'Imputes missing data in a normally distributed variable
#'@aliases mice.impute.sgauss
#'@return Vector with imputed data, same type as \code{y}, and of length
#'\code{sum(wy)}
#'@details
#'The function \code{mice.impute.gauss()} imputes for normal
#'variables by using the gaussian regression model. The model is fit in Stan and
#'uses weakly informative (shrinkage) priors to regularize.
#'
#'
#'@author Guido Biele
#'@family univariate imputation functions
#'@keywords datagen
#'@export
#'
mice.impute.sgauss <- function(y, ry, x, wy = NULL, ...)
{
  if (is.null(wy)) wy <- !ry

  x <- as.matrix(x)
  xy <- cbind.data.frame(y = y, x = x)

  fit = suppressWarnings(sgauss(formula(xy), data = xy[ry, , drop = FALSE]))
  imputations <- predict(fit, xy[wy, , drop = FALSE])
  fit$standata$X = NULL
  fit$standata$Y = NULL
  attr(fit$formula,".Environment") = NULL
  attr(imputations,"spolr.fit") = fit
  return(imputations)
}
