% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_opsr_ate.R
\name{summary.opsr.ate}
\alias{summary.opsr.ate}
\title{Summarizing OPSR ATE Objects}
\usage{
\method{summary}{opsr.ate}(object, ...)
}
\arguments{
\item{object}{an object of class \code{"opsr.ate"}.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
An object of class \code{"summary.opsr.ate"} containing among others:
\itemize{
\item \code{ate}: An object of class \code{"ate"}. See also \code{\link{print.ate}}.
\item \code{te}: An object of class \code{"te"}. See also \code{\link{print.te}}.
}

The p-values of the weighted paired t-test are attached as attributes.
}
\description{
This function computes weighted treatment effects and corresponding weighted
paired t-tests.
}
