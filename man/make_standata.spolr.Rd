% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_standata.spolr.R
\name{make_standata.spolr}
\alias{make_standata.spolr}
\title{Make standata for regularized regressions in stan}
\usage{
make_standata.spolr(formula, data, family = "cumulative",
  sd_prior_b = 2, scale.X = T, X.means, X.sds)
}
\arguments{
\item{formula}{Formula.}

\item{data}{data.frame.}

\item{family}{character indicating preparation for data for
a logistic model ("bernoulli"), an
ordinal model ("cumulative", adds ncat) or a
binomial model ("binomial", adds number of trials)}

\item{sd_prior_b}{standard deviation for the prior for regression weights}

\item{scale.X}{logical value indicating if predictors are scaled}

\item{X.means}{mean of the unscaled variables.}

\item{X.sds}{mean of the unscaled variables. When X.means and X.sds are given, standardization is based on these values.}
}
\value{
a named list with
N (number of rows),
Y (int vector with outcomes),
K (number of predictors)
X (scaled design matrix), generally without intercept column
sd_prior_b (standard deviation for the prior of regression weights)
ncat (only for ordinal models number of levels of ordinal variable)
trials (only for binomial models number of trials)

The design matrix X is generated from formula and data.
It does not include an interecept, which is instead implemented in the Stan program.
By default, each column in X is scaled to mean zero and sd 1.
}
\description{
Make standata for regularized regressions in stan
}
