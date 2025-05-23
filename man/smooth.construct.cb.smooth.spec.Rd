\name{smooth.construct.cb.smooth.spec}
\alias{smooth.construct.cb.smooth.spec}
\alias{Predict.matrix.cb.smooth}

\title{ Cross-Basis Spline Smooths for a DLNM }

\description{
These are method functions for a smooth class defining bi-dimensional cross-basis splines for penalized distributed lag linear (DLMs) and non-linear (DLNMs) models. The functions are not supposed to be called directly, and the class is usually specified via terms like \code{s(X,L,bs="cb",...)} in the formula of the \code{\link[mgcv]{gam}} function of the package \pkg{mgcv}.
}

\usage{
\method{smooth.construct}{cb.smooth.spec}(object, data, knots)

\method{Predict.matrix}{cb.smooth}(object, data)
}

\arguments{
  \item{object }{ for \code{smooth.construct}, an object of class \code{"cb.smooth.spec"} usually generated by a call \code{s(X,L,bs="cb",...)}. For \code{Predict.matrix}, an object of class \code{cb.smooth} generated by \code{smooth.construct}.}
  \item{data }{ a list containing just the data (including any by variable) required by this term, with names corresponding to \code{object$term}.}
  \item{knots }{ a list containing any knots supplied for basis setup --- in same order and with same names as data. It is usually \code{NULL}.}
}

\details{
These method functions embed tools available in the packages \pkg{dlnm} and \pkg{mgcv} to perform penalized DLMs and DLNMs. This represent the \emph{internal} approach to perform such models (see Notes below). Specifically, the models are fitted by including a term \code{s(X,L,bs="cb",...)}, defining a basis \code{"cb"} for bi-dimensional cross-basis splines, in the formula of the \code{\link[mgcv]{gam}} function. The constructor function for this class turns this smooth terms into a smooth specification object, which includes the cross-basis matrix (see \code{\link{crossbasis}}) and the penalty matrices for the two spaces of predictor and lags used in model fitting. Then, \code{\link{crosspred}} uses the predict matrix function for the \code{"cb"} basis to obtain predictions, and a graphical representation can be obtained by standard \code{\link[=plot.crosspred]{plotting functions}}, similary to unpenalized models.

The first two arguments \code{X} and \code{L} in \code{\link[mgcv]{s}} represent a matrix of exposure histories and a matrix of lags. The former, also used in \code{\link{crossbasis}}, needs to be defined directly even with time series data by lagging the exposure series. The matrix \code{L} must have the same dimensions of \code{X}, with identical rows representing the sequence of lags. The other arguments of \code{\link[mgcv]{s}} have the same meaning: in particular, \code{k} (default to 10), \code{fx} (default to \code{FALSE}) and \code{sp} (default to \code{NULL}) can be provided for each marginal basis as vectors of length 2, and similarly \code{m} can be provided as a list (see also \code{\link[mgcv]{te}}). No \code{by} argument is allowed.

Extra information can be included in the argument \code{xt} of \code{\link[mgcv]{s}}, which accepts a single object or a list of objects. First, an object \code{bs} (a vector of length 1 or 2) can be used to specify the smoother for each marginal dimension, with current options restricted to \code{"ps"} for P-splines (see \code{\link[mgcv]{smooth.construct.ps.smooth.spec}}, used by default) and/or \code{"cr"} for cubic regression splines (\code{\link[mgcv]{smooth.construct.cr.smooth.spec}}). Second, list objects \code{argvar} and \code{arglag} can be used to build the marginal bases for predictor and lags by calling other functions (see the same arguments in \code{\link{crossbasis}}). In particular, these can be used for a more flexible specification of penalized functions (using \code{\link{ps}} or \code{\link{cr}}) or for using unpenalized functions for one marginal basis, thus limiting the penalization to one of the two dimensions. Third, the object \code{addSlag} can contain a matrix or vector (or list of matrices and/or vectors) defining additional penalties on the lag structure (see \code{\link{cbPen}}).
}

\value{
The smooth constructor function returns an object of classes \code{"cb.smooth"} and \code{"tensor.smooth"}. Specifically, a list with a similar structure of that returned by the smooth constructor for tensor product smooths \code{\link[mgcv]{smooth.construct.tensor.smooth.spec}} (see also \code{\link[mgcv]{te}}).

The \code{Predict.matrix} function return a cross-basis matrix evaluated at specific values used for prediction.
}

\references{
Gasparrini A, Scheipl F, Armstrong B, Kenward MG. A penalized framework for distributed lag non-linear models. \emph{Biometrics}. 2017;\bold{73}(3):938-948. [freely available \href{http://www.ag-myresearch.com/2017_gasparrini_biomet.html}{here}]

Wood S. N. Generalized Additive Models: An Introduction with R. Chapman and Hall/CRC Press, 2006.
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}> and Fabian Scheipl <\email{fabian.scheipl@stat.uni-muenchen.de}>}

\note{
Identifiability constraints are applied to marginal basis for predictor (see \code{\link[mgcv]{smoothCon}}), while the marginal basis for the lag dimension is left untransformed. This involves a re-parameterization with the absorption of constraints into the basis that causes its dimension to decrease by 1. Note that this procedure is similar to that in \code{\link{crossbasis}}, while it is different than in standard tensor product smooths (see \code{\link[mgcv]{te}}), where identifiability constraints are not applied to the marginal bases.

Using the default specification with \code{k=10} in the smooth terms defined by \code{\link[mgcv]{s}}, the dimension of the cross-basis matrix will be \eqn{(10-1) \times 10 = 90}{(10-1)x10=90} (accounting for identifiability constraints). This is consistent with the rationale that this choice is not important as far as the upper limit for the degrees of freedom in each marginal basis is large enough to represent the underlying relationship (see \code{\link[mgcv]{choose.k}}). Smaller values of \code{k} can be used for speeding up the computation, as long as the underlying relationship can be assumed to be smooth enough.

These method functions provide an \emph{internal} method for performing penalized DLMs and DLNMs, with the cross-basis spline smoother defined directly in the model formula of \code{\link[mgcv]{gam}} through a smooth term specified by \code{\link[mgcv]{s}}. The alternative \emph{external} method relies on the standard use of \code{\link{crossbasis}} and on the penalization of so-called parametric terms through the argument \code{paraPen} of \code{\link[mgcv]{gam}} (see \code{\link{cbPen}} for details). The two methods are expected to returns almost identical results in most cases. However, while the internal method takes advantage of the full machinery of \pkg{mgcv} and plausibly more stable procedures, the external method allows more flexibility and the optional use of user-defined smoothers.
}

\seealso{
Smooth constructors \code{\link[mgcv]{smooth.construct.ps.smooth.spec}} and \code{\link[mgcv]{smooth.construct.cr.smooth.spec}} in \pkg{mgcv}. \code{\link{ps}} and \code{\link{cr}} for the same functions available in \pkg{dlnm}. \code{\link{cbPen}} for defining tensor-type bi-dimensional penalties in DLNMs.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
# to be added soon
}

\keyword{smooth} \keyword{models} \keyword{regression}
