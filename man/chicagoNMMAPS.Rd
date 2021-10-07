\name{chicagoNMMAPS}
\alias{chicagoNMMAPS}
\docType{data}

\title{ Daily Mortality Weather and Pollution Data for Chicago }

\description{
The data set contains daily mortality (all causes, CVD, respiratory), weather (temperature, dew point temperature, relative humidity) and pollution data (PM10 and ozone) for Chicago in the period 1987-2000 from the National Morbidity, Mortality and Air Pollution Study (NMMAPS)
}

\usage{data(chicagoNMMAPS)}

\format{
  A data frame with 5114 observations on the following 14 variables.
  \itemize{
    \item{\code{date}: Date in the period 1987-2000.}
    \item{\code{time}: The sequence of observations}
    \item{\code{year}: Year}
    \item{\code{month}: Month (numeric)}
    \item{\code{doy}: Day of the year}
    \item{\code{dow}: Day of the week (factor)}
    \item{\code{death}: Counts of all cause mortality excluding accident}
    \item{\code{cvd}: Cardiovascular Deaths }
    \item{\code{resp}: Respiratory Deaths}
    \item{\code{temp}: Mean temperature (in Celsius degrees)}
    \item{\code{dptp}: Dew point temperature}
    \item{\code{rhum}: Mean relative humidity}
    \item{\code{pm10}: PM10}
    \item{\code{o3}: Ozone}
  }
}

\details{
These data represents a subsample of the variables included in the NMMAPS dataset for Chicago.

The variable \code{temp} is derived from the original \code{tmpd} after a transformation from Fahrenheit to Celsius. The variables \code{pm10} and \code{o3} are an approximated reconstruction of the original series, adding the de-trended values and the median of the long term trend. This is the reason they include negative values.
}

\source{
The complete dataset used to be available at the Internet-based Health and Air Pollution Surveillance System (iHAPSS) website, or through the packages \pkg{NMMAPSdata} or \pkg{NMMAPSlite}. Currently, the data are not available any more and the two packages have been archived.
}

\seealso{
\code{\link{nested}} for an example of analysing exposure-lag-response associations in a nested case-control study. \code{\link{drug}} for an example of analysing exposure-lag-response associations in a randomized controlled trial.

The application of DLNMs to this data with more detailed examples are given in vignette \strong{dlnmExtended}.

See \code{\link{dlnm-package}} for an introduction to the package and for links to package vignettes providing more detailed information.
}

\keyword{datasets}