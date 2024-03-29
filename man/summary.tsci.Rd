% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary-method.R
\name{summary.tsci}
\alias{summary.tsci}
\title{Summarizing Two Stage Curvature Identification Fits}
\usage{
\method{summary}{tsci}(object, extended_output = FALSE, ...)
}
\arguments{
\item{object}{an object of class 'tsci'.}

\item{extended_output}{logical. If \code{TRUE} are more detailed summary is returned.}

\item{...}{arguments to be passed to or from other methods.}
}
\value{
an object of class 'summary.tsci' containing the following elements:
\describe{
\item{\code{coefficient}}{a data frame with columns for the estimated treatment coefficient, its standard error, confidence interval and (two-sided) p-value.}
\item{\code{invalidity}}{a vector containing the number of times the instrumental variable(s) were considered valid, invalid or too weak to perform the test.}
\item{\code{viospace_selection}}{a data frame with columns for the number of times each of the violation space candidate was selected by comparison,
the conservative method and as the largest violation space candidate for which the instrumental variable was considered to be strong enough.}
\item{\code{treatment_model}}{a data frame with information about the method used to fit the treatment model.}
\item{\code{sample_size_A1}}{the number of observations in the subset used to fit the outcome model.}
\item{\code{sample_size_A2}}{the number of observations in the subset used to train the parameters for fitting the treatment model.}
\item{\code{n_splits}}{the number of sample splits performed.}
\item{\code{mult_split_method}}{the method used to calculate the standard errors and p-values if \code{n_splits} is larger than 1.}
\item{\code{alpha}}{the significance level used.}
\item{\code{iv_strength}}{a data frame with columns containing the estimated instrumental variable strength and the estimated instrumental variable strength threshold
for each violation space candidate. Will only be returned if \code{extended_output} is true.}
\item{\code{coefficients_all}}{a data frame with columns for the estimated treatment coefficients, its standard errors, confidence intervals and (two-sided) p-values for
each violation space candidate.}
}
}
\description{
Summarizing Two Stage Curvature Identification Fits
}
