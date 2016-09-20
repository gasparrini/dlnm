\name{cr}
\alias{cr}

\title{ Generate a Basis Matrix for Penalized Cubic Regression Splines }

\description{
Generate the basis matrix for cubic regression splines with penalties on the second derivatives.
}

\usage{
cr(x, df=10, knots=NULL, intercept=FALSE, fx= FALSE, S=NULL)
}

\arguments{
  \item{x }{ the predictor variable. Missing values are allowed.}
  \item{df }{ degrees of freedom, basically the dimension of the basis matrix. If supplied in the absence of \code{knots}, it automatically selects \code{df+1-intercept} knots at equally-spaced quantiles of \code{x}. The minimum allowed is \code{df=3}.}
  \item{knots }{ breakpoints that define the spline. These are generally automatically selected, and not defined by the user. See Details below.}
  \item{intercept }{ logical. If \code{TRUE}, an intercept is included in the basis matrix. See Details below.}
  \item{fx }{ logical. If \code{TRUE}, it removes the penalization. See Details below.}
  \item{S }{ penalty matrix, usually internally defined if \code{NULL} (default).}
}

\details{
The function has a usage similar to \code{\link{bs}} and \code{\link{ns}} in the \pkg{splines} package. It produces spline transformations, however using a \emph{cardinal} parameterization that represents the splines in terms of values at the knots. A penalty matrix is also defined. The same results are returned by the related \code{\link[=smooth.construct.cr.smooth.spec]{smooth constructor}} in the package \pkg{mgcv}, which is in fact called internally.

The argument \code{knots} defines a vector of knots within the range of the predictor \code{x}, by default at equally-spaced quantiles. The penalization is defined on the second derivative of the function through a penalty matrix \code{S}.

Similarly to \code{\link{bs}} and \code{\link{ns}}, setting \code{intercept=FALSE} (default) determines the exclusion of the first transformed variables, and the corresponding first row and column in \code{S}, thus avoiding identifiability issues during the model fitting. Note how the procedure of imposing identifiability constraints is different from that adopted by \code{\link{smoothCon}} in the package \pkg{mgcv}, where a more complex reparameterization is produced. 
}

\value{
A matrix object of class \code{"cr"}. It contains the attributes \code{df}, \code{knots}, \code{intercept}, \code{fx}, and \code{S}, with values which can be different than the arguments provided due to internal reset.
}

\references{
Gasparrini A. A penalized framework for distributed lag non-linear models. \emph{Biometrics}. 2016; in press.

Eilers P. H. C. and Marx B. D. Flexible smoothing with B-splines and penalties. \emph{Statistical Science}. 1996; \bold{11}(2):89-121.

Wood S. N. Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC Press, 2006.
}

\author{Antonio Gasparrini, \email{antonio.gasparrini@lshtm.ac.uk}, using functions included in the package \pkg{mgcv} by Simon N. Wood.}

\note{
While the function can be used to fit penalized models with the \code{\link[mgcv]{gam}} regression function in \pkg{mgcv} including the penalty matrix in the argument \code{paraPen}, the use of the \code{\link[=smooth.construct.cr.smooth.spec]{smooth constructor}} is to be preferred. The function is primarily added here to specify penalized DLMs and DLNMs using the sa-called \emph{external} method (see \code{\link{cbPen}}). 
}

\seealso{
\code{\link{ps}} for P-splines. \code{\link{bs}} and \code{\link{ns}} for B-splines and natural cubic splines, respectively. \code{\link{cbPen}} for defining tensor-type bi-dimensional penalties in DLNMs. The \code{\link[=smooth.construct.ps.smooth.spec]{smooth constructor}} for cross-basis spline smooths.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
rnorm(1)
}

\keyword{smooth}

