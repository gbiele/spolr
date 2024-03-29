% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sbetabin.R
\name{sbetabin}
\alias{sbetabin}
\title{Bayesian regularized beta-binomial regression with Stan}
\usage{
sbetabin(formula, data, scale.X = T, sd_prior_b = 2)
}
\arguments{
\item{formula}{Formula.}

\item{data}{data.frame.}

\item{scale.X}{logical value indicating if predictors are scaled}

\item{sd_prior_b}{standard deviation for the prior for regression weights}
}
\value{
An object of class `spolr` with the slots
`par` and `value` (taken from `stan::optimizing`),
`standata` (data for stan model fit),
`b` (regression weights),
`phi` overdisperion parameters, and
`formula` (R formula for regression model).

Regularization is achieved by scaling predictors to mean zeros and
standard deviation of one and setting the standard deviation for the
prior of regression weights to two.

Optimization is initialized with Interecpt and regression coeffcients at 0.
}
\description{
Bayesian regularized beta-binomial regression with Stan
}
