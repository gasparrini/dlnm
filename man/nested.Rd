\name{nested}
\alias{nested}
\docType{data}

\title{ Nested Case-Control Study with a Time-Varying Exposure and a Cancer Outcome }

\description{
The data set contains simulated data from an hypothetical nested case-control study on the association between a time-varying occupational exposure and a cancer outcome. The study includes 300 risk sets, each with a case and a control matched by age year. The data on the exposure is collected on 5-year age intervals between 15 and 65 years.
}

\usage{data(nested)}

\format{
  A data frame with 600 observations on the following 14 variables.
  \itemize{
    \item{\code{id}: subject ID.}
    \item{\code{case}: indicator for case (1) or control (0).}
    \item{\code{age}: age of each subject.}
    \item{\code{riskset}: risk set id.}
    \item{\code{exp15}: yearly exposure in the age period 15-19 year.}
    \item{\code{exp20}: yearly exposure in the age period 20-24 year.}
    \item{\code{...}}
    \item{\code{exp60}: yearly exposure in the age period 60-64 year.}
  }
}

\details{
The exposure history for each subject (series of yearly exposures) can be recovered by expanding the values given in \code{exp15}-\code{exp60}, and then selecting the values backward from the age of the subject for a given lag period.
}

\source{
These nested case-control data were extracted from a simulated cohort with 300 cases of cancer and a time-varying exposure.
}

\author{Antonio Gasparrini <\email{antonio.gasparrini@lshtm.ac.uk}>}

\seealso{
\code{\link{drug}} for an example of randomized controlled trial data. \code{\link{chicagoNMMAPS}} for an example of time series data.

The application of DLNMs to these data with detailed examples are provided in the vignette \strong{dlnmExtended}.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}


\keyword{datasets}