\name{exphist}
\alias{exphist}

\title{ Define Exposure Histories from an Exposure Profile }

\description{
This function builds a matrix of exposure histories given an exposure profile, the time points at which each exposure history is evaluated, and a lag period.
}

\usage{
exphist(exp, time, lag)
}

\arguments{
  \item{exp }{ an exposure profile defined at equally-spaced time units, from time 1 on.}
  \item{time }{ either a numeric scalar or vector of positive integer numbers specifying the time points at which each exposure history is evaluated. By default, all the time points of \code{exp}.}
  \item{lag }{ either an integer scalar or vector of length 2, defining the the maximum lag or the lag range, respectively. Only non-negative lags allowed. By default, the lag period from 0 to \code{length(exp)-1}.}
}

\details{
This function is used to define matrices of exposure histories (backward in time) given an exposure profile (forward in time). Among other uses, this can be applied to define specific exposure histories for obtaining predictions in \code{\link{crosspred}}.

The exposure profile in \code{exp} is assumed to represent a series of exposure events defined forward in time, starting from time 1 and on. An exposure history is then evaluated backward in time for each point defined by \code{time} (rounded to integers) on the lag period defined by \code{lag}.

If the values in \code{time} are higher than the length of \code{exp}, or if the lag period extends backward before the beginning of the exposure profile, the exposure history is padded with 0's.
}

\value{
A numeric matrix of exposure histories, with rows corresponding to the values in \code{time} and columns corresponding to the lag period in \code{lag}.
}

\references{
Gasparrini A. Modeling exposure-lag-response associations with distributed lag non-linear models. \emph{Statistics in Medicine}. 2014; \bold{33}(5):881-899. [freely available \href{http://www.ag-myresearch.com/statmed2014}{here}]
}

\author{Antonio Gasparrini, \email{antonio.gasparrini@lshtm.ac.uk}}

\seealso{
\code{\link{crosspred}} to obtain predictions after model fitting.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\examples{
### an exposure history evaluated at a single time
(exp <- sample(1:10))
exphist(exp, 5, 3)
exphist(exp, 5, 12)
exphist(exp, 15, 3)

### use of argument lag
exphist(exp, 15, c(3,7))

### exposure histories evaluated at multiple times
exphist(exp, 3:5, 12)
exphist(exp, lag=12)

### see help(drug) and help(nested) for further examples
}

\keyword{smooth}

