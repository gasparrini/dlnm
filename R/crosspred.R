###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#
crosspred <-
function(basis, model=NULL, coef=NULL, vcov=NULL, model.link=NULL, at=NULL,
  from=NULL, to=NULL, by=NULL, lag, bylag=1, cen=NULL, ci.level=0.95,
  cumul=FALSE) {
#
################################################################################
# DETERMINE THE TYPE OF MODEL AND CHECKS
#
  # TYPE OF PREDICTION: CROSSBASIS, ONEBASIS, OR PENALIZED GAM
  type <- if(any(class(basis)%in%"crossbasis")) "cb" else 
    if(any(class(basis)%in%"onebasis")) "one" else "gam"
#
  # CHECKS ON TYPE, AND SET name, basis AND RESET type
  errormes <- "arguments 'basis' and 'model' not consistent. See help(crosspred)"
  if(type=="gam") {
    if(!is.character(basis)||length(basis)>1L) stop(errormes)
    if(is.null(model)||!any(class(model)%in%"gam")) stop(errormes)
    name <- basis
    sterms <- sapply(model$smooth,function(x) x$term[1])
    if(name%in%sterms) basis <- model$smooth[[which(sterms==name)[1]]] else 
      stop(errormes)
    if(length(which(sterms==name))>1)
      warning(paste(name,"included in multiple smoothers, only the first one taken"))
    if(!"cb.smooth"%in%class(basis) && basis$dim>1L)
      stop("predictions not provided for multidimensional smoothers")
  } else name <- deparse(substitute(basis))
#
  #  EXTRACT lag (DEPENDENT ON TYPE)
  lag <- if(missing(lag)) switch(type,
    cb = attr(basis,"lag"),
    one = c(0,0),
    gam = if(is.null(basis$lag)) c(0,0) else basis$lag
  ) else mklag(lag)
  if(type=="cb" && lag!=attr(basis,"lag") && attr(basis,"arglag")$fun=="integer")
    stop("prediction for lag sub-period not allowed for type 'integer'")
#
  # OTHER COHERENCE CHECKS
  if(is.null(model)&&(is.null(coef)||is.null(vcov)))
    stop("At least 'model' or 'coef'-'vcov' must be provided")
  if(!is.numeric(ci.level)||ci.level>=1||ci.level<=0)
    stop("'ci.level' must be numeric and between 0 and 1")
#
  # CUMULATIVE EFFECTS ONLY WITH LAGGED EFFECTS AND lag[1]==0
  if(cumul==TRUE && (diff(lag)==0L || lag[1]!=0L)) {
    cumul <- FALSE
    warning("Cumulative predictions only computed if diff(lag)>0 and lag[1]=0")
  }
#
################################################################################
# SET COEF, VCOV CLASS AND LINK FOR EVERY TYPE OF MODELS
#
  # WRITE CONDITIONS (DEPENDENT ON TYPE AND IF MATRIX/VECTOR)
  cond <- if(type=="gam") with(basis,first.para:last.para) else
    if(ncol(basis)==1L) name else
    if(type=="one") paste(name,"[[:print:]]*b[0-9]{1,2}",sep="") else
      paste(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
#
  # IF MODEL PROVIDED, EXTRACT FROM HERE, OTHERWISE DIRECTLY FROM COEF AND VCOV
  if(!is.null(model)) {
    model.class <- class(model)
    coef <- getcoef(model,model.class)
    ind <- if(type=="gam") cond else grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- getlink(model,model.class)
  } else model.class <- NA
#
  # CHECK
  npar <- if(type=="gam") length(ind) else ncol(basis)
  if(length(coef)!=npar || length(coef)!=dim(vcov)[1] || any(is.na(coef)) ||
      any(is.na(vcov)))
    stop("coef/vcov not consistent with basis matrix. See help(crosspred)")
#
##########################################################################
# AT, PREDVAR, PREDLAG AND CENTERING
#
  # RANGE
  range <- if(type=="gam") range(model$model[[basis$term[1]]]) else
    attr(basis,"range")
#
  # SET at, predvar AND predlag
  at <- mkat(at,from,to,by,range,lag,bylag)
  predvar <- if(is.matrix(at)) rownames(at) else at
  predlag <- seqlag(lag,bylag)
#
  # DEFINE CENTERING VALUE (NULL IF UNCENTERED), AND REMOVE INFO FROM BASIS
  cen <- mkcen(cen,type,basis,range)
  if(type=="one") attributes(basis)$cen <- NULL
  if(type=="cb") attributes(basis)$argvar$cen <- NULL
#
################################################################################
# PREDICTION OF LAG-SPECIFIC EFFECTS
#
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON TYPE)
  Xpred <- mkXpred(type,basis,at,predvar,predlag,cen)
#
  # CREATE LAG-SPECIFIC EFFECTS AND SE
  matfit <- matrix(Xpred%*%coef,length(predvar),length(predlag)) 
  matse <- matrix(sqrt(pmax(0,rowSums((Xpred%*%vcov)*Xpred))),length(predvar),
    length(predlag)) 
#
  # NAMES
  rownames(matfit) <- rownames(matse) <- predvar
  colnames(matfit) <- colnames(matse) <- outer("lag",predlag,paste,sep="")
#
################################################################################
# PREDICTION OF OVERALL+CUMULATIVE EFFECTS
#
  # RE-CREATE LAGGED VALUES (NB: ONLY LAG INTEGERS)
  predlag <- seqlag(lag)
#
  # CREATE THE MATRIX OF TRANSFORMED VARIABLES (DEPENDENT ON TYPE)
  Xpred <- mkXpred(type,basis,at,predvar,predlag,cen)
#
  # CREATE OVERALL AND (OPTIONAL) CUMULATIVE EFFECTS AND SE
  Xpredall <- 0
  if(cumul) {
    cumfit <- cumse <- matrix(0,length(predvar),length(predlag))
  }
  for (i in seq(length(predlag))) {
    ind <- seq(length(predvar))+length(predvar)*(i-1)
    Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    if(cumul) {
      cumfit[, i] <- Xpredall %*% coef
      cumse[, i] <- sqrt(pmax(0,rowSums((Xpredall%*%vcov)*Xpredall)))
    }
  }
  allfit <- as.vector(Xpredall %*% coef)
  allse <- sqrt(pmax(0,rowSums((Xpredall%*%vcov)*Xpredall)))
#
  # NAMES
  names(allfit) <- names(allse) <- predvar
  if(cumul) {
    rownames(cumfit) <- rownames(cumse) <- predvar
    colnames(cumfit) <- colnames(cumse) <- outer("lag",seqlag(lag),paste,sep="")
  }
#
################################################################################
# CREATE THE OBJECT
#
  # INITIAL LIST, THEN ADD COMPONENTS
  list <- list(predvar=predvar)
  if(!is.null(cen)) list$cen <- cen
  list <- c(list,list(lag=lag,bylag=bylag,coefficients=coef,vcov=vcov,
    matfit=matfit,matse=matse,allfit=allfit,allse=allse))
  if(cumul) list <- c(list,list(cumfit=cumfit,cumse=cumse))
#
  # MATRICES AND VECTORS WITH EXPONENTIATED EFFECTS AND CONFIDENCE INTERVALS
  z <- qnorm(1-(1-ci.level)/2)
  if(!is.null(model.link) && model.link %in% c("log","logit")) {
    list$matRRfit <- exp(matfit)
    list$matRRlow <- exp(matfit-z*matse)
    list$matRRhigh <- exp(matfit+z*matse)
    list$allRRfit <- exp(allfit)
    list$allRRlow <- exp(allfit-z*allse)
    names(list$allRRlow) <- names(allfit)
    list$allRRhigh <- exp(allfit+z*allse)
    names(list$allRRhigh) <- names(allfit)
    if(cumul) {
      list$cumRRfit <- exp(cumfit)
      list$cumRRlow <- exp(cumfit-z*cumse)
      list$cumRRhigh <- exp(cumfit+z*cumse)
    }
  } else {
    list$matlow <- matfit-z*matse
    list$mathigh <- matfit+z*matse
    list$alllow <- allfit-z*allse
    names(list$alllow) <- names(allfit)
    list$allhigh <- allfit+z*allse
    names(list$allhigh) <- names(allfit)
    if(cumul) {
      list$cumlow <- cumfit-z*cumse
      list$cumhigh <- cumfit+z*cumse
    }
  }
#
  list$ci.level <- ci.level
  list$model.class <- model.class
  list$model.link <- model.link
#
  class(list) <- "crosspred"
#
  return(list)
}

