\name{coef.crosspred}
\alias{coef.crosspred}
\alias{coef.crossreduce}
\alias{vcov.crosspred}
\alias{vcov.crossreduce}

\title{ Model Coefficients and their (Co)Variance Matrix of a DLNM }

\description{
These method functions extract the estimated model coefficients and their (co)variance matrix from a DLNM from objects of class \code{"crosspred"} and \code{"crossreduce"}.
}

\usage{

\method{coef}{crosspred}(object, ...)

\method{vcov}{crosspred}(object, ...)

\method{coef}{crossreduce}(object, ...)

\method{vcov}{crossreduce}(object, ...)
}

\arguments{
  \item{object}{ an object of class \code{"crosspred"} or \code{"crossreduce"}.}
  \item{\dots}{ further arguments passed to or from other methods.}
}

\author{Antonio Gasparrini, \email{antonio.gasparrini@lshtm.ac.uk}}

\seealso{
See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\keyword{methods}
