\name{cbPen}
\alias{cbPen}

\title{ Generate Penalty Matrices for a DLNM }

\description{
This function generates penalty matrices for the two dimensions of predictor and lags, given the functions selected to model the relationship in each space. It can also be used for generating the single penalty matrix for the predictor space of a uni-dimensional basis not accouning for lags.
}

\usage{
cbPen(cb, sp=-1, addSlag=NULL)
}

\arguments{
  \item{cb }{ an object of class \code{"crossbasis"} or \code{"onebasis"}.}
  \item{sp }{ supplied smoothing parameters. See Details below.}
  \item{addSlag }{ matrix or vector (or list of matrices and/or vectors) defining additional penalties on the lag structure. See Details below.}
}

\details{
This function is used to perform penalized regression models using the \emph{external} method. This involves generating the transformation using \code{\link{crossbasis}} or \code{\link{onebasis}} with functions for penalized splines (either \code{\link{ps}} or \code{\link{cr}}). The function \code{cbPen} is then called to generate a list of the related penalty matrices. The model is performed by penalizing so-called parametric terms in the \code{\link[mgcv]{gam}} function of \pkg{mgcv}, by including the basis or cross-basis matrix in the regression formula and the list of penalty matrices in its \code{paraPen} argument.

When \code{cb} is a cross-basis object, the penalty matrices for the two spaces of predictor and lags are rescaled and expanded accordingly to its tensor product-type structure. A penalty matrix is not defined when using a function different than \code{\link{ps}} or \code{\link{cr}}, thus keeping one of the two dimensions unpenalized. 

Additional penalties on the lag dimension can be added through the argument \code{addSlag}, either as a single matrix or a list of matrices. If provided as a vector, this is taken as the diagonal of the penalty matrix and expanded accordingly. These objects must have appropriate dimensions in accordance with the basis matrix for the lag space. 

All the penalty matrices are also appropriately rescaled to improve the estimation process.

The vector \code{sp} must have the same length as the number of penalties, including additional penalties on the lags, and it is replicated accordingly if of length 1. Positive or zero elements are taken as fixed smoothing parameters. Negative elements signal that these parameters need to be estimated.
}

\value{
A list including penalty matrices plus two vectors \code{rank} and \code{sp} defining their rank and the smoothing parameters. This list is consistent with the argument \code{paraPen} in the regression function \code{\link[mgcv]{gam}} function of \pkg{mgcv}.
}

\references{
Gasparrini A, Scheipl F, Armstrong B, Kenward MG. A penalized framework for distributed lag non-linear models. *Biometrics*. 2017; in press. [freely available \href{http://www.ag-myresearch.com/2017_gasparrini_biomet.html}{here}]

Wood S. N. Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC Press, 2006.
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>}

\seealso{
\code{\link{ps}} and \code{\link{cr}} for penalized spline functions. The \code{\link[=smooth.construct.cb.smooth.spec]{cb smooth constructor}} for cross-basis penalized spline smooths.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information, in particular the vignette \strong{dlnmPenalized}.
}

\examples{
# to be added soon
}

\keyword{utilities}
