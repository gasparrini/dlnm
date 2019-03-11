###
### R routines for the R package dlnm (c)
#
exphist <-
function(exp, times, lag, fill=0) {
#
################################################################################
#
  # CHECKS
  exp <- as.vector(exp)
  lag <- if(missing(lag)) c(0,length(exp)-1) else mklag(lag)
  times <- if(missing(times)) seq(length(exp)) else round(times)
#
  # DEFINE THE EXTENSION OF exp IN BOTH SIDES, DEPENDING ON timees AND lag[2]
  left <- max(0, lag[2L] + 1 - min(times))
  right <- max(0, max(times) - length(exp) - lag[1L])
  
#
  # EXTEND exp
  exp <- c(rep(fill,left),exp,rep(fill,right))
#
  # DEFINE THE LIST OF SEQUENCES
  seqlist <- lapply(times,"-",lag-left)
#
  # GENERATE EXPOSURE HISTORIES FOR EACH OF times
  hist <- do.call(rbind, lapply(seqlist, function(x) exp[seq(x[1L],x[2L])]))
  dimnames(hist) <- list(times,paste0("lag",seqlag(lag)))
#
  return(hist)
  }
