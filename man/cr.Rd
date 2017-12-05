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
The function has a usage similar to \code{\link{bs}} and \code{\link{ns}} in the \pkg{splines} package. It produces spline transformations, however using a parameterization that represents the splines fit in terms of values at the knots. A penalty matrix is also defined. The same results are returned by the related \code{\link[=smooth.construct.cr.smooth.spec]{smooth constructor}} in the package \pkg{mgcv}, which is in fact called internally.

The argument \code{knots} defines a vector of knots within the range of the predictor \code{x}, by default at equally-spaced quantiles. The penalization is defined on the second derivative of the function through a penalty matrix \code{S}.

Similarly to \code{\link{bs}} and \code{\link{ns}}, setting \code{intercept=FALSE} (default) determines the exclusion of the first transformed variables, and the corresponding first row and column in \code{S}, thus avoiding identifiability issues during the model fitting. Note how the procedure of imposing identifiability constraints is different from that adopted by \code{\link[mgcv]{smoothCon}} in the package \pkg{mgcv}, where a more complex reparameterization is produced. 
}

\value{
A matrix object of class \code{"cr"}. It contains the attributes \code{df}, \code{knots}, \code{intercept}, \code{fx}, and \code{S}, with values that can be different than the arguments provided due to internal reset.
}

\references{
Gasparrini A, Scheipl F, Armstrong B, Kenward MG. A penalized framework for distributed lag non-linear models. \emph{Biometrics}. 2017;\bold{73}(3):938-948. [freely available \href{http://www.ag-myresearch.com/2017_gasparrini_biomet.html}{here}]

Wood S. N. Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC Press, 2006.
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>, with internall calls to functions included in the package \pkg{mgcv} by Simon N. Wood.}

\note{
The function is primarily added here to specify penalized DLMs and DLNMs using the so-called \emph{external} method, \emph{i.e.} by including the penalty matrix in the argument \code{paraPen} of the \code{\link[mgcv]{gam}} regression function in \pkg{mgcv}  (see \code{\link{cbPen}}). However, this approach can be also used to fit standard uni-dimensional penalized cubic spline models as an alternative to the use of specific \code{\link[=smooth.construct.cr.smooth.spec]{smooth constructor}}, as it takes advantage of the use of prediction and plotting functions in \pkg{dlnm}.
}

\seealso{
\code{\link{ps}} for P-splines. \code{\link{bs}} and \code{\link{ns}} for B-splines and natural cubic splines, respectively. \code{\link{cbPen}} for defining tensor-type bi-dimensional penalties in DLNMs. The related \code{\link[=smooth.construct.cr.smooth.spec]{smooth constructor}} for cubic regression spline smooths in \pkg{mgcv}. The \code{\link[=smooth.construct.cb.smooth.spec]{cb smooth constructor}} for cross-basis penalized spline smooths.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
# to be added soon
}

\keyword{smooth}

