### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016


#' Generate a Cross-Basis Matrix for a DLNM
#' 
#' The function generates the basis matrices for the two dimensions of
#' predictor and lags, given the functions selected to model the relationship
#' in each space. Then, these one-dimensions basis matrices are combined in
#' order to create the related cross-basis matrix, which can be included in a
#' model formula to fit a distributed lag non-linear model (DLNM).
#' 
#' The argument \code{x} defines the type of data. If a \eqn{n}-dimensional
#' vector, the data are interpreted as a time series of equally-spaced and
#' complete observations. If a \eqn{n \times (L-\ell_0+1)}{n x (L-L0+1)}
#' matrix, the data are interpreted as a set of complete exposure histories at
#' equally-spaced lags over the same lag period from \eqn{\ell_0}{L0} to
#' \eqn{L} for each observation. The latter is general and can be used for
#' applying distributed lag linear and non-linear models in different study
#' designs. Lags are usually positive integers: if not provided, by default the
#' minimum lag \eqn{L0} is set to 0, and the maximum lag \eqn{L} is set to 0 if
#' \code{x} is a vector or to \code{ncol(x)-1} otherwise. Negative lags are
#' rarely needed but allowed.
#' 
#' The lists in \code{argvar} and \code{arglag} are passed to
#' \code{\link{onebasis}}, which calls existing or user-defined functions to
#' build the related basis matrices. The two lists should contain the argument
#' \code{fun} defining the chosen function, and a set of additional arguments
#' of the function. The \code{argvar} list is applied to \code{x}, in order to
#' generate the matrix for the space of the predictor. The \code{arglag} list
#' is applied to a new vector given by the sequence obtained by \code{lag}, in
#' order to generate the matrix for the space of lags. By default, the basis
#' functions for lags are defined with an intercept (if not otherwise stated).
#' Some arguments can be automatically re-set by \code{\link{onebasis}}. Then,
#' the two set of basis matrices are combined in order to create the related
#' cross-basis matrix.
#' 
#' Common choices for \code{fun} are represented by \code{\link[splines]{ns}}
#' and \code{\link[splines]{bs}} from package \pkg{splines} or by the internal
#' functions of the package \pkg{dlnm}, namely \code{\link{poly}},
#' \code{\link{strata}}, \code{\link{thr}}, \code{\link{integer}} and
#' \code{\link{lin}}. See \code{help(onebasis)} and the help pages of these
#' functions for information on the additional arguments to be specified. Also,
#' other existing or user-defined functions can be applied.
#' 
#' The argument \code{group}, only used for time series data, defines groups of
#' observations representing independent series. Each series must be
#' consecutive, complete and ordered.
#' 
#' @aliases crossbasis summary.crossbasis
#' @param x either a numeric vector representing a complete series of ordered
#' observations (for time series data), or a matrix of exposure histories over
#' the same lag period for each observation. See Details below.
#' @param lag either an integer scalar or vector of length 2, defining the the
#' maximum lag or the lag range, respectively.
#' @param argvar,arglag lists of arguments to be passed to the function
#' \code{\link{onebasis}} for generating the two basis matrices for predictor
#' and lags, respectively. See Details below.
#' @param group a factor or a list of factors defining groups of observations.
#' Only for time series data.
#' @param \dots additional arguments. See Details below.
#' @return A matrix object of class \code{'crossbasis'} which can be included
#' in a model formula in order to fit a DLNM. It contains the attributes
#' \code{df} (vector of length 2 with the df for each dimension), \code{range}
#' (range of the original vector of observations), \code{lag} (lag range),
#' \code{argvar} and \code{arglag} (lists of arguments defining the basis
#' functions in each space, which can be modified if compared to lists used in
#' the call). The method summary.crossbasis returns a summary of the
#' cross-basis matrix and the related attributes, and can be used to check the
#' options for the basis functions chosen for the two dimensions.
#' @note Missing values in \code{x} are allowed, but this causes the
#' observation (for non-time series data with \code{x} as a matrix) or the
#' following observations corresponding to the lag period (for time series data
#' with \code{x} as a vector series) to be set to \code{NA}. Although correct,
#' this could generate computational problems in the presence of a high number
#' of missing observations.
#' 
#' The name of the crossbasis object will be used by \code{\link{crosspred}} in
#' order to extract the related estimated parameters. If more than one variable
#' is transformed through cross-basis functions in the same model, different
#' names must be specified.
#' 
#' Before version 2.2.0 of \pkg{dlnm}, the \code{argvar} list could include a
#' \code{cen} argument to be passed internally to \code{\link{onebasis}} for
#' centering the basis. This step is now moved to the prediction stage, with a
#' \code{cen} argument in \code{\link{crosspred}} or \code{\link{crossreduce}}
#' (see the related help pages). For backward compatibility, the use of
#' \code{cen} in \code{crossbasis} is still allowed (with a warning), but may
#' be discontinued in future versions.
#' @section Warnings: In previous versions of the package the function adopted
#' a different usage. In particular, the \code{argvar} list should not include
#' a \code{cen} argument any more (see Note in this help page or
#' \code{\link{onebasis}}). Users are strongly suggested to comply with the
#' current usage, as backward compatibility may be discontinued in future
#' versions of the package.
#' 
#' Meaningless combinations of arguments in \code{argvar} and \code{arglag}
#' passed to \code{\link{onebasis}} could lead to collinear variables, with
#' identifiability problems in the model and the exclusion of some of them.
#' 
#' It is strongly recommended to avoid the inclusion of an intercept in the
#' basis for \code{x} (\code{intercept} in \code{argvar} should be
#' \code{FALSE}, as default), otherwise a rank-deficient cross-basis matrix
#' will be specified, causing some of the cross-variables to be excluded in the
#' regression model. Conversely, an intercept is included by default in the
#' basis for the space of lags.
#' @author Antonio Gasparrini <\email{antonio.gasparrini@@lshtm.ac.uk}>
#' @seealso \code{\link{onebasis}} to generate one-dimensional basis matrices.
#' \code{\link{crosspred}} to obtain predictions after model fitting. The
#' method function \code{\link[=plot.crosspred]{plot}} to plot several type of
#' graphs.
#' 
#' See \code{\link{dlnm-package}} for an introduction to the package and for
#' links to package vignettes providing more detailed information.
#' @references Gasparrini A. Distributed lag linear and non-linear models in R:
#' the package dlnm. \emph{Journal of Statistical Software}. 2011;
#' \bold{43}(8):1-20, freely available at
#' \url{http://www.ag-myresearch.com/jss2011}.
#' 
#' Gasparrini A. Modeling exposure-lag-response associations with distributed
#' lag non-linear models. \emph{Statistics in Medicine}. 2014;
#' \bold{33}(5):881-899, freely available at
#' \url{http://www.ag-myresearch.com/statmed2014}.
#' 
#' Gasparrini A., Armstrong, B.,Kenward M. G. Distributed lag non-linear
#' models. \emph{Statistics in Medicine}. 2010; \bold{29}(21):2224-2234,
#' freely available at \url{http://www.ag-myresearch.com/statmed2010}.
#' @keywords smooth ts
#' @importFrom utils modifyList
#' @importFrom tsModel Lag
#' @export
#' @examples
#' 
#' ### example of application in time series analysis - see vignette('dlnmTS')
#' 
#' # create the crossbasis objects and summarize their contents
#' cb1.pm <- crossbasis(chicagoNMMAPS$pm10, lag=15, argvar=list(fun='lin'),
#'   arglag=list(fun='poly',degree=4))
#' cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag=3, argvar=list(df=5),
#'   arglag=list(fun='strata',breaks=1))
#' summary(cb1.pm)
#' summary(cb1.temp)
#' 
#' # run the model and get the predictions for pm10
#' library(splines)
#' model1 <- glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow,
#'   family=quasipoisson(), chicagoNMMAPS)
#' pred1.pm <- crosspred(cb1.pm, model1, at=0:20, bylag=0.2, cumul=TRUE)
#' 
#' # plot the lag-response curves for specific and incremental cumulative effects
#' plot(pred1.pm, 'slices', var=10, col=3, ylab='RR', ci.arg=list(density=15,lwd=2),
#'   main='Lag-response curve for a 10-unit increase in PM10')
#' plot(pred1.pm, 'slices', var=10, col=2, cumul=TRUE, ylab='Cumulative RR',
#'   main='Lag-response curve of incremental cumulative effects')
#' 
#' ### example of application beyond time series - see vignette('dlnmExtended')
#' 
#' # generate the matrix of exposure histories from the 5-year periods
#' Qnest <- t(apply(nested, 1, function(sub) exphist(rep(c(0,0,0,sub[5:14]), 
#'   each=5), sub['age'], lag=c(3,40))))
#' 
#' # define the cross-basis
#' cbnest <- crossbasis(Qnest, lag=c(3,40), argvar=list('bs',degree=2,df=3),
#'   arglag=list(fun='ns',knots=c(10,30),intercept=FALSE))
#' summary(cbnest)
#' 
#' # run the model and predict
#' library(survival)
#' mnest <- clogit(case~cbnest+strata(riskset), nested)
#' pnest <- crosspred(cbnest,mnest, cen=0, at=0:20*5)
#' 
#' # bi-dimensional exposure-lag-response association
#' plot(pnest, zlab='OR', xlab='Exposure', ylab='Lag (years)')
#' # lag-response curve for dose 60
#' plot(pnest, var=50, ylab='OR for exposure 50', xlab='Lag (years)', xlim=c(0,40))
#' # exposure-response curve for lag 10
#' plot(pnest, lag=5, ylab='OR at lag 5', xlab='Exposure', ylim=c(0.95,1.15))
#' 
crossbasis <- function(x, lag, argvar = list(), arglag = list(), group = NULL, ...) {
  # COHERENCE CHECKS CHECK OLD USAGE
  checkcrossbasis(argvar, arglag, list(...))
  # lag MUST BE A POSITIVE INTEGER VECTOR
  lag <- if (missing(lag)) 
    c(0, NCOL(x) - 1) else mklag(lag)
  # CREATE THE BASIS FOR THE PREDICTOR SPACE x MUST BE A VECTOR OR MATRIX WITH NUMBER OF COLUMNS COMPATIBLE
  # WITH lag IF A VECTOR, x IS TREATED AS A TIME SERIES OTHERWISE, x IS TREATED AS A MATRIX OF LAGGED
  # OCCURRENCES
  x <- as.matrix(x)
  dim <- dim(x)
  if (!dim[2] %in% c(1L, diff(lag) + 1L)) 
    stop("NCOL(x) must be equal to 1 (if x is ", "a time series vector), otherwise to the lag period (for x as a matrix of ", 
      "lagged occurrences)")
  # THE BASIS TRANSFORMATION CREATES DIFFERENT MATRICES DEPENDING THE DATA : IF TIME SERIES, EACH COLUMN
  # CONTAINS THE UNLAGGED TRANSFORMATION IF NOT, EACH COLUMN CONTAINS THE TRANFORMATION FOR ALL THE LAGGED
  # VALUES
  basisvar <- do.call("onebasis", modifyList(argvar, list(x = as.numeric(x))))
  # CREATE THE BASIS FOR THE LAG SPACE SET FUN='STRATA' AND DF=1 UNDER SPECIFIC CIRCUMSTANCES
  if (length(arglag) == 0L || diff(lag) == 0L) 
    arglag <- list(fun = "strata", df = 1, intercept = TRUE)
  # IF NOT SPECIFIED AND AN ARGUMENT, INCLUDE AN INTERCEPT BY DEFAULT
  if ((is.null(arglag$fun) || "intercept" %in% names(formals(arglag$fun))) && sum(pmatch(names(arglag), 
    "intercept", nomatch = 0)) == 0) 
    arglag$intercept <- TRUE
  # FORCE UNCENTERED TRANSFORMATIONS
  arglag$cen <- NULL
  # THE BASIS TRANSFORMATIONS ARE ONLY APPLIED TO THE LAG VECTOR DIMENSIONS ACCOUNTED FOR IN CROSS-BASIS
  # COMPUTATIONS BELOW
  basislag <- do.call("onebasis", modifyList(arglag, list(x = seqlag(lag))))
  # CROSSBASIS COMPUTATION GROUP
  if (!is.null(group)) 
    checkgroup(group, x, basisvar, lag)
  # COMPUTE CROSS-BASIS: FOR TIME SERIES DATA, COMPUTE THE MATRIX OF LAGGED OCCURRENCES FIRST IF x WAS
  # ALREADY A MATRIX, JUST RECOMPUTE THE APPROPRIATE DIMENSIONS NB: ORDER OF TRANSFORMATION IN THE TENSOR
  # CHANGED SINCE VERSION 2.2.4
  crossbasis <- matrix(0, nrow = dim[1], ncol = ncol(basisvar) * ncol(basislag))
  for (v in seq(length = ncol(basisvar))) {
    if (dim[2] == 1L) {
      mat <- as.matrix(Lag(basisvar[, v], seqlag(lag), group = group))
    } else mat <- matrix(basisvar[, v], ncol = diff(lag) + 1)
    for (l in seq(length = ncol(basislag))) {
      crossbasis[, ncol(basislag) * (v - 1) + l] <- mat %*% (basislag[, l])
    }
  }
  # ATTRIBUTES AND NAMES NAMES NB: ORDER CHANGED SINCE VERSION 2.2.4
  
  cn <- paste0("v", rep(seq(ncol(basisvar)), each = ncol(basislag)), ".l", rep(seq(ncol(basislag)), ncol(basisvar)))
  dimnames(crossbasis) <- list(rownames(x), cn)
  # REDEFINE ARGUMENTS FOR BASES, THEY MIGHT HAVE BEEN CHANGED BY onebasis FIRST VAR
  ind <- match(names(formals(attributes(basisvar)$fun)), names(attributes(basisvar)), nomatch = 0)
  argvar <- c(attributes(basisvar)["fun"], attributes(basisvar)[ind])
  # THEN LAG
  ind <- match(names(formals(attributes(basislag)$fun)), names(attributes(basislag)), nomatch = 0)
  arglag <- c(attributes(basislag)["fun"], attributes(basislag)[ind])
  # THEN ADD CENTERING FOR VAR, IF PROVIDED (OTHERWISE NULL)
  argvar$cen <- attributes(basisvar)$cen
  # ATTRIBUTES
  attributes(crossbasis) <- c(attributes(crossbasis), list(df = c(ncol(basisvar), ncol(basislag)), range = range(x, 
    na.rm = T), lag = lag, argvar = argvar, arglag = arglag))
  if (!is.null(group)) 
    attributes(crossbasis)$group <- length(unique(group))
  # 
  class(crossbasis) <- c("crossbasis", "matrix")
  # 
  return(crossbasis)
}

