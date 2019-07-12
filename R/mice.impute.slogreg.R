#'Imputation of ordered data by bayesian ordered logistic regression
#'
#'Imputes missing data in a categorical variable using polytomous regression
#'@aliases mice.impute.slogreg
#'@return Vector with imputed data, same type as \code{y}, and of length
#'\code{sum(wy)}
#'@details
#'The function \code{mice.impute.slogreg()} imputes for binary
#'variables by using the logistic regression model. The model is fit in Stan and
#'uses weakly informative (shrinkage) priors to regularize.
#'
#'
#'@author Guido Biele
#'@family univariate imputation functions
#'@keywords datagen
#'@export
#'
mice.impute.slogreg <- function(y, ry, x, wy = NULL, ...)
{
  if (is.null(wy)) wy <- !ry

  x <- as.matrix(x)
  xy <- cbind.data.frame(y = y, x = x)

  fit = suppressWarnings(slogreg(formula(xy), data = xy[ry, , drop = FALSE]))
  post <- predict(fit, xy[wy, , drop = FALSE], type = "probs")
  draws <- post > runif(length(post))
  if (class(y) == "factor") {
    idx <- 1 + draws
    imputations = levels(y)[idx]
  } else {
    imputations = draws
  }
  fit$standata$X = NULL
  fit$standata$Y = NULL
  attr(fit$formula,".Environment") = NULL
  attr(imputations,"spolr.fit") = fit
  return(imputations)
}
