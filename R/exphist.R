###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
exphistint <-
function(time1, exp, lag) {
#
################################################################################
#
  # EXTEND exp
  exp <- c(exp,rep(0,max(0,time1-length(exp))))
  # REVERSE exp
  exphist <- rev(exp[seq(time1)])
#
  # DEFINE EXPOSURE HISTORY
  exphist <- c(exphist,rep(0,lag[2]))[seq(lag[1],lag[2])+1]
#
  # NAMES
  names(exphist) <- paste("lag",seq(lag[1],lag[2]),sep="")
#
  return(exphist)
}
#
#
exphist <-
function(exp, time, lag) {
#
################################################################################
#
  # CHECKS
  exp <- as.vector(exp)
  lag <- if(missing(lag)) c(0,length(exp)-1) else mklag(lag)
  if(any(lag<0)) stop("only non-negative lags allowed")
  time <- if(missing(time)) seq(length(exp)) else round(time)
  if(any(time<1)) stop("time must composed by positive integer numbers")
#
  # GENERATE EXPOSURE HISTORIES FOR EACH time
  hist <- do.call(rbind,lapply(time,exphistint,exp,lag))
  rownames(hist) <- time
#
  return(hist)
}

