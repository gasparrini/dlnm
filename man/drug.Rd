\name{drug}
\alias{drug}
\docType{data}

\title{ A Trial on the Effect of Time-Varying Doses of a Drug }

\description{
The data set contains simulated data from an hypothetical randomized controlled trial on the effect of time-varying doses of a drug. The study include records for 200 randomized subjects, each receiving doses of a drug randomly allocated in two out of four weeks, with daily doses varying each week. The daily doses are reported on 7-day intervals corresponding to each week.
}

\usage{data(drug)}

\format{
  A data frame with 200 observations on the following 7 variables.
  \itemize{
    \item{\code{id}: subject ID.}
    \item{\code{out}: the outcome level measured at day 28.}
    \item{\code{sex}: the sex of the subject.}
    \item{\code{day1.7}: daily dose for the first week.}
    \item{\code{day8.14}: daily dose for the second week.}
    \item{\code{day15.21}: daily dose for the third week.}
    \item{\code{day22.28}: daily dose for the fourth week.}
  }
}

\details{
The exposure history for each subject (series of daily doses from day 28 to 1) can be recovered by expanding the values given in \code{day1.7}-\code{day22.28}.
}

\source{
This data set only contains simulated data.
}

\author{Antonio Gasparrini, \email{antonio.gasparrini@lshtm.ac.uk}}

\seealso{
\code{\link{nested}} for an example of nested case-control study data. \code{\link{chicagoNMMAPS}} for an example of time series data.

The application of DLNMs to these data with detailed examples are given in vignette \strong{dlnmExtended}.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\keyword{datasets}