\name{cbPen}
\alias{cbPen}

\title{ Generate Penalty Matrices for a DLNM }

\description{
This function generates penalty matrices for the two dimensions of predictor and lags, given the functions selected to model the relationship in each space. It can also be used for generating the single penalty matrix for the predictor space of a uni-dimensional function.
}

\usage{
cbPen(cb, sp=-1, addSlag=NULL)
}

\arguments{
  \item{cb }{ an object of class \code{"onebasis"} or \code{"crossbasis"}.}
  \item{sp }{ supplied smoothing parameters. See Details below.}
  \item{addSlag }{ matrix or vector (or list of matrices and/or vectors) defining additional penalties on the lag structure. See Details below.}
}

\details{
This function is used to perform penalized regression models using the \emph{external} method. This involves generating the transformation using \code{\link{onebasis}} or \code{\link{crossbasis}} with functions for penalized spline (either \code{\link{ps}} or \code{\link{cr}}). The function \code{cbPen} is then called to generate a list of the related penalty matrices. Then, the model is performed using the \code{\link[mgcv]{gam}} function in \pkg{mgcv}, including the basis or cross-basis matrix in the regression formula and the list of penalty matrices in the \code{paraPen} argument. 

The vector \code{sp} must have the same length as the number of penalties, including additional penalties on the lags, and it is replicated accordingly if of length 1. Positive or zero elements are taken as fixed smoothing parameters. Negative elements signal that these parameters need to be estimated.
}

\value{
...
}

\author{Antonio Gasparrini, \email{antonio.gasparrini@lshtm.ac.uk}}

\note{
... 
}

\seealso{
...

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
rnorm(1)
}

\keyword{utilities}
