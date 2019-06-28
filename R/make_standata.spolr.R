#' Make standata for ordered logistic regression in stan
#'
#' @export
#' @param formula Formula.
#' @param data data.frame.
#' @param scale.X logical value indicating if predictors are scaled
#' @param sd_prior_b standard deviation for the prior for regression weights
#' @param ordinal logical value indicating if data for an ordinal model is prepared (adds ncat and removes interecept)
#' @param X.means mean of the unscaled variables.
#' @param X.sds mean of the unscaled variables. When X.means and X.sds are given, standardization is based on these values.
#' @return a named list with
#' N (number of rows),
#' Y (int vector with outcomes),
#' K (number of predictors)
#' X (scaled design matrix)
#' sd_prior_b (standard deviation for the prior of regression weights)
#' ncat (only for ordinal models number of levels of ordinal variable)
#'
#' The design matrix X is generated from formula and data.
#' It does not include an interecept, which is instead implemented in the Stan program.
#' By default, each column in X is scaled to mean zero and sd 1.
#'
make_standata.spolr = function(formula, data, ordinal = F, 
                               sd_prior_b = 2, scale.X = T, X.means, X.sds) {
  if( sum(is.na(data)) > 0 )
    stop('Expecting data set without missing values')
  resp = strsplit(deparse(formula)[1],split = " ~ ")[[1]][1]
  Y = as.numeric(data[[resp]])
  if (ordinal == F)
    Y = Y-1
  X = model.matrix(formula,data)
  X = X[,-1, drop = F]
  if (scale.X == T) {
    if (missing(X.means)) {
      X.means = colMeans(X)
      X.sds = apply(X,2,sd) 
    }
    for (k in 1:ncol(X))
      X[,k] = (X[,k]-X.means[k])/X.sds[k]
  }
    
  standata = list(
    N = length(Y),
    Y = Y,
    K = ncol(X),
    X = X,
    sd_prior_b = sd_prior_b
  )
  
  if (scale.X == T) {
    standata$X.means = X.means
    standata$X.sds = X.sds
  }
  
  if (ordinal == T)
    standata$ncat = length(unique(Y))
  return(standata)
}
