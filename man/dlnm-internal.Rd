\name{dlnm-internal}
\alias{fci}
\alias{findrank}
\alias{getcoef}
\alias{getlink}
\alias{getvcov}
\alias{mkaddSlag}
\alias{mkat}
\alias{mkcen}
\alias{mklag}
\alias{mkXpred}
\alias{seqlag}


\title{ Internal Functions for Package dlnm }

\description{
These functions are called internally by other functions and are not meant to be directly run by the users.
}

\usage{
fci(ci, x, high, low, ci.arg, plot.arg, noeff=NULL)

findrank(X)

getcoef(model, class)

getlink(model, class, model.link=NULL)

getvcov(model, class)

mkaddSlag(addSlag, d)

mkat(at, from, to, by, range, lag, bylag)

mkcen(cen, type, basis, range)

mklag(lag)

mkXpred(type, basis, at, predvar, predlag, cen)

seqlag(lag, by=1)
}

\arguments{
  \item{ci }{ type of confidence intervals representation.}
  \item{x }{ the coordinates of the x axis.}
  \item{high, low }{ the coordinates of the y axis for the interval.}
  \item{ci.arg }{ list of arguments to draw the confidence intervals.}
  \item{plot.arg }{ list of arguments of the main plot.}
  \item{noeff }{ reference value of the null effect.}
  \item{X }{ a matrix.}
  \item{model }{ a regression model object.}
  \item{class }{ a regression model class.}
  \item{addSlag }{ matrix or vector (or list of matrices and/or vectors) defining additional penalties on the lag structure.}
  \item{d }{ numeric vector of length 2 providing the cross-basis dimensions.}
  \item{at }{ either a numeric vector representing the values of a constant exposure throughout the lag period defined by \code{lag}, or a matrix of exposure histories over the same lag period used for estimation.}
  \item{from, to}{ range of predictor values used for prediction.}
  \item{by, bylag}{ increment of the sequences of predictor and lag values used for prediction.}
  \item{range}{ range of values used for prediction.}
  \item{lag }{ either an integer scalar or vector of length 2, defining the the maximum lag or the lag range, respectively.}
  \item{cen }{ logical or a numeric scalar. It specifies the centering value, then used as a reference for predictions.}
  \item{type, basis}{ type of model and related basis object from which predictions are needed. See \code{\link{crosspred}}.}
  \item{predvar }{ vector or matrix of predictor values used for prediction}
  \item{predlag }{ vector or matrix of lag values used for prediction}
}

\details{
The function \code{fci} provides different options for representing confidence intervals, and it is called internally in plotting functions.

The function \code{findrank} returns the rank of a matrix.

The functions \code{getcoef}, \code{getlink}, and \code{getvcov} extract coefficients, the model link, and (co)variance matrix, respectively, depending on the class of the model, and returns a message error if the process fails. They are used internally in \code{\link{crosspred}} and \code{\link{crossreduce}}.

The function \code{mkaddSlag} returns a list of rescaled penalty matrices defining additional penalties on the lag structure. It is used intenally in functions for performing penalized models.

The function \code{mkat} is used internally in \code{\link{crosspred}} to define the values used for predictions.

The function \code{mkcen} is used internally in \code{\link{crosspred}} to define the centering value for computing predictions.

The functions \code{mklag} is used internally in several other functions to check/define the vector of length 2 representing the lag interval.

The function \code{mkXpred} is used internally in \code{\link{crosspred}} to define the basis or cross-basis matrix for computing predictions.

The function \code{seqlag} is used internally in several other functions to create the vector with the sequence of lags given the range provided.
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>}

\note{
These and other undocumented internal functions are not exported in the namespace of the package \pkg{dlnm}. The user can visualize them through the use of the triple colon operator '\code{:::}' or through the function \code{\link{getAnywhere}} (see the examples below).  
}

\seealso{
See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
### display internal functions
dlnm:::getcoef
getAnywhere(getcoef)

### display other undocumented functions
dlnm:::fci
getAnywhere(fci)
}

\keyword{internal}
