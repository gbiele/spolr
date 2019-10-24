#'Imputation using bayesian regularized regression
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
mice.impute.spolr <- function(y, ry, x, wy = NULL, ...)
{
  if (is.null(wy)) wy <- !ry

  x <- as.matrix(x)
  xy <- cbind.data.frame(y = y, x = x)

  fit = suppressWarnings(spolr(formula(xy), data = xy[ry, , drop = FALSE]))
  post <- predict(fit, xy[wy, , drop = FALSE], type = "probs")
  if (sum(wy) == 1)
    post <- matrix(post, nrow = 1, ncol = length(post))
  fy <- as.factor(y)
  nc <- length(levels(fy))
  un <- rep(runif(sum(wy)), each = nc)
  if (is.vector(post))
    post <- matrix(c(1 - post, post), ncol = 2)
  draws <- un > apply(post, 1, cumsum)
  idx <- 1 + apply(draws, 2, sum)
  imputations = levels(fy)[idx]
  fit$standata$X = NULL
  fit$standata$Y = NULL
  attr(fit$formula,".Environment") = NULL
  attr(imputations,"spolr.fit") = fit
  return(imputations)
}
