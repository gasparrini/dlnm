###
### R routines for the R package dlnm (c) Antonio Gasparrini 2013-2016
#
exphist <-
function(exp, times, lag, fill=0) {
#
################################################################################
#
  # CHECKS
  exp <- as.vector(exp)
  lag <- if(missing(lag)) c(0,length(exp)-1) else mklag(lag)
  if(any(lag<0)) stop("only non-negative lags allowed")
  times <- if(missing(times)) seq(length(exp)) else round(times)
  if(any(times<1)) stop("'times' must composed by positive integer numbers")
#
  # DEFINE THE FUNCTION TO COMPUTE EXPOSURE HISTORY FOR A SINGLE TIME
  fexphist1 <- function(time1, exp, lag, fill) {
    # EXTEND exp
    exp <- c(exp,rep(fill,max(0,time1-length(exp))))
    # DEFINE EXPOSURE HISTORY
    exphist <- rev(exp[seq(time1)])
    # FILL
    exphist <- c(exphist,rep(fill,lag[2]))[seq(lag[1],lag[2])+1]
    # NAMES
    names(exphist) <- paste("lag",seq(lag[1],lag[2]),sep="")
    return(exphist)
  }
#
  # GENERATE EXPOSURE HISTORIES FOR EACH OF times
  hist <- do.call(rbind,lapply(times,fexphist1,exp,lag,fill))
  rownames(hist) <- times
#
  return(hist)
}

