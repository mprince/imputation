% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{dist_q_matrix}
\alias{dist_q_matrix}
\title{Calculate \eqn{L_q} distance}
\usage{
dist_q_matrix(x_ref, x_rest, q)
}
\arguments{
\item{q}{An integer specifying the which norm to take the L-q distance of.}

\item{x}{A numeric matrix Missing values are allowed.}

\item{ref}{An integer specifying the reference row.}
}
\value{
a numeric vector of length \code{nrow(x) - 1}
}
\description{
Calculate \eqn{L_q} distance of all vectors in a matrix to a reference
vector.
}

