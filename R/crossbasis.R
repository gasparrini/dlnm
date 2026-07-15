###
### R routines for the R package dlnm (c)
#
crossbasis <-
function(x, lag, argvar=list(), arglag=list(), group=NULL, ...) {         
  
################################################################################
# RESET ARGUMENTS AND COHERENCE CHECKS
  
  # CHECK THE FORMAT OF x AND TRANSFORM IT IN A VECTOR
  # NB: TRANSPOSE IF A MATRIX OF EXPHIST, SO TO HAVE APPENDED LAG SERIES
  xncol <- NCOL(x)
  if(xncol>1L) x <- t(x)
  x <- as.numeric(x)
  
  #  lag MUST BE A POSITIVE INTEGER VECTOR 
  lag <- if(missing(lag)) c(0,NCOL(x)-1) else mklag(lag)
  
  # CHECK IF DIMENSIONS OF x COMPATIBLE WITH lag
  if(!xncol %in% c(1L,diff(lag)+1L))
    stop("NCOL(x) must be equal to 1 (if x is a time series vector), ",
      "otherwise to the lag period (for x as a matrix of lagged occurrences)")
  
  # CHECK LIST FORMAT
  if(!is.list(argvar)) stop("'argvar' must be a list")
  if(!is.list(arglag)) stop("'arglag' must be a list")
  
  # ERROR IF group INCLUDED WITH EXPOSURE HISTORY DATA
  if(xncol>1L && !is.null(group))
    stop("'group' allowed only for time series data")  
  
############################################################################
# CREATE THE BASIS FOR THE PREDICTOR SPACE
  
  # THE BASIS DIMENSIONS DEPEND ON THE ORIGINAL FORMAT OF x
  # IF TIME SERIES, STANDARD DIMENSIONS
  # IF A MATRIX OF EXPHIST, ROWS EXPANDED TO INCLUDE ALL THE LAGGED VALUES
  basisvar <- do.call("onebasis", modifyList(argvar, list(x=x)))
  
############################################################################
# CREATE THE BASIS FOR THE LAG SPACE
  
  # SET FUN="STRATA" AND DF=1 UNDER SPECIFIC CIRCUMSTANCES
  if(length(arglag)==0L || diff(lag)==0L) 
    arglag <- list(fun="strata", df=1, intercept=TRUE)
  
  # IF NOT SPECIFIED AND AN ARGUMENT, INCLUDE AN INTERCEPT BY DEFAULT
  if((is.null(arglag$fun) || "intercept" %in% names(formals(arglag$fun))) && 
      sum(pmatch(names(arglag),"intercept",nomatch=0))==0)
    arglag$intercept <- TRUE
  # FORCE UNCENTERED TRANSFORMATIONS
  arglag$cen <- NULL
  
  # THE BASIS TRANSFORMATIONS ARE ONLY APPLIED TO THE LAG VECTOR
  # DIMENSIONS ACCOUNTED FOR IN CROSS-BASIS COMPUTATIONS BELOW
  seqlag <- seqlag(lag)
  basislag <- do.call("onebasis", modifyList(arglag, list(x=seqlag))) 
  
############################################################################
# EXTRACT ATTRIBUTES AND NAMES
  
  # NAMES
  #   NB: ORDER CHANGED SINCE VERSION 2.2.4
  cnames <- paste0("v", rep(seq(ncol(basisvar)), each=ncol(basislag)), ".l",
    rep(seq(ncol(basislag)), ncol(basisvar)))
  
  # REDEFINE ARGUMENTS FOR BASES, THEY MIGHT HAVE BEEN CHANGED BY onebasis
  # FIRST VAR
  ind <- match(names(formals(attributes(basisvar)$fun)),
    names(attributes(basisvar)), nomatch=0)
  argvar <- c(attributes(basisvar)["fun"], attributes(basisvar)[ind])
  # THEN LAG
  ind <- match(names(formals(attributes(basislag)$fun)),
    names(attributes(basislag)), nomatch=0)
  arglag <- c(attributes(basislag)["fun"], attributes(basislag)[ind])
  
############################################################################
# CROSSBASIS COMPUTATION
  
  # COMPUTE CROSS-BASIS, CALLING INTERNAL FUNCTION CONSISTENT WITH FORMAT
  cbfun <- ifelse(xncol==1L, "cbts", "cbhist")
  cb <- do.call(cbfun, list(basisvar=basisvar, basislag=basislag, seqlag=seqlag,
    group=group))
  
  # DEFINE AS A MATRIX WITH PROPER DIMENSIONS
  vc <- ncol(basisvar)
  lc <- ncol(basislag)
  dim(cb) <- c(nrow(basisvar)/xncol, vc*lc)
  
############################################################################
# ASSIGN NAMES, ATTRIBUTES, CLASS
  
  # NAMES
  dimnames(cb) <- list(rownames(x), cnames)
  
  # ATTRIBUTES
  attributes(cb) <- c(attributes(cb),
    list(df=c(vc, lc), range=range(x,na.rm=T), lag=lag, argvar=argvar, 
      arglag=arglag))
  if(!is.null(group)) attributes(cb)$group <- length(unique(group))
  
  # CLASS
  class(cb) <- c("crossbasis","matrix")
  
  # RETURN
  return(cb)
}

