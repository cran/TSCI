% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_monomials.R
\name{create_monomials}
\alias{create_monomials}
\title{Monomials as Violation Space Candidates}
\usage{
create_monomials(Z, degree, type = c("monomials_main", "monomials_full"))
}
\arguments{
\item{Z}{observations of the instrumental variable(s). Either a numeric vector of length n
or a numeric matrix with dimension n by s.}

\item{degree}{The degree up to which monomials should be created. Either a single positive integer or a vector of length s containing positive integers.}

\item{type}{One out of \code{monomials_main} or \code{monomials_full}. \cr
\code{monomials_main} creates the monomials for the polynomials of each instrumental variable up to degree \code{degree}. \cr
\code{monomials_full} creates the monomials for the polynomials of a combination of all instrumental variables up to degree \code{degree}. \cr
Default is \code{monomials_full}.}
}
\value{
A list. Each element is a matrix consisting of the monomials to be added to
the next violation space candidate.
}
\description{
Monomials as Violation Space Candidates
}
\details{
assuming there are 3 instrumental variables Z1, Z2, and Z3 and \code{degree} = c(d1, d2, d3) with d1 < d2 < d3,
\code{monomials_main} creates the monomials of the polynomials (Z1 + 1)^d1, (Z2 + 1)^d2, (Z3 + 1)^d3 without the constants and
\code{monomials_full} creates the monomials (Z1 + Z2 + Z3), (Z1 + Z2 + Z3)^2, ..., (Z1 + Z2 + Z3)^d3 without the constants and excluding
monomials that are products of Z1^d or Z2^d with d > d1 resp. d > d2.
Thus \code{type} = \code{monomials_main} does not include interactions between the instrumental variables.
}
\examples{
Z <- matrix(rnorm(100 * 3), nrow = 100, ncol = 3)
vio_space <- create_monomials(Z = Z, degree = 4, type = "monomials_full")
}
