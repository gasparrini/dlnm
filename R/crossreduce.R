###
### R routines for the R package dlnm (c)
#
crossreduce <-
function(basis, model=NULL, type="overall", value=NULL, coef=NULL, vcov=NULL,
  model.link=NULL, at=NULL, from=NULL, to=NULL, by=NULL, lag, bylag=1, cen=NULL,
  ci.level=0.95) {
#
################################################################################
# CHECK BASIS AND WRITE CONDITION (REGULAR EXPRESSION) TO EXTRACT COEF-VCOV
#
  if(all(class(basis)!="crossbasis")) {
    stop("the first argument must be an object of class 'crossbasis'")
  }
  name <- deparse(substitute(basis))
  attr <- attributes(basis)
  if(ncol(basis)==1) cond <- name
#
###########################################################################
# COHERENCE CHECKS (SEE CROSSPRED)
#
  if(is.null(model) && (is.null(coef)||is.null(vcov))) {
    stop("At least 'model' or 'coef'-'vcov' must be provided")
  }
  type <- match.arg(type,c("overall","var","lag"))
  if(type!="overall") {
    if(is.null(value)) stop("'value' must be provided for type 'var' or 'lag'")
    else if(!is.numeric(value) || length(value)>1) {
      stop("'value' must be a numeric scalar")
    }
    if(type=="lag" && (any(value<attr$lag[1])||any(value>attr$lag[2]))) {
      stop("'value' of lag-specific effects must be within the lag range")
    }
  } else value <- NULL
#
  #  lag MUST BE A POSITIVE INTEGER VECTOR, BY DEFAULT THAT USED FOR ESTIMATION
  lag <- if(missing(lag)) attr$lag else mklag(lag)
  if(any(lag!=attr$lag) && attr$arglag$fun=="integer")
      stop("prediction for lag sub-period not allowed for type 'integer'")
#
  if(!is.numeric(ci.level) || ci.level>=1 || ci.level<=0) {
    stop("'ci.level' must be numeric and between 0 and 1")
  }
#
###########################################################################
# SET COEF, VCOV CLASS AND LINK FOR EVERY TYPE OF MODELS
#
  # WRITE CONDITIONS (DEPENDENT ON TYPE AND IF MATRIX/VECTOR)
  cond <- if(ncol(basis)==1L) name else 
    paste(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}",sep="")
#
  # IF MODEL PROVIDED, EXTRACT FROM HERE, OTHERWISE DIRECTLY FROM COEF AND VCOV
  if(!is.null(model)) {
    model.class <- class(model)
    coef <- getcoef(model,model.class)
    vcov <- getvcov(model, model.class)
    indcoef <- grep(cond,names(coef))
    indvcov <- grep(cond,rownames(vcov))
    coef <- coef[indcoef]
    vcov <- vcov[indvcov,indvcov,drop=FALSE]
    model.link <- getlink(model,model.class)
  } else model.class <- NA
#
  # CHECK COEF AND VCOV
  npar <- if(type=="gam") length(indcoef) else ncol(basis)
  if(length(coef)!=npar || length(coef)!=dim(vcov)[1] || any(is.na(coef)) ||
      any(is.na(vcov)))
    stop("coef/vcov do not consistent with basis matrix. See help(crossreduce)")
#
##########################################################################
# AT AND CENTERING
#
  # SET at
  if(is.matrix(at)) stop("argument 'at' must be a vector")
  range <- attr$range
  at <- mkat(at,from,to,by,range,lag,bylag)
#
  # DEFINE CENTERING VALUE (NULL IF UNCENTERED), AND REMOVE INFO FROM BASIS
  cen <- mkcen(cen,type="cb",basis,range)
  attributes(basis)$argvar$cen <- attr$argvar$cen <- NULL
#
##########################################################################
# REDUCTION
#
  # CREATE (CENTERED) TRANSFORMATION MATRIX AND BASIS
  # NB: ORDER OF THE TENSOR REVERSED SINCE VERSION 2.2.4
  # NOW ORDER COMPATIBLE WITH crossbasis BUT REVERSE OF GASPARRINI BMCmrm 2013
  if(type=="overall") {
    lagbasis <- do.call("onebasis",c(list(x=seqlag(lag)),attr$arglag))
    M <- diag(ncol(basis)/ncol(lagbasis)) %x%
      (t(rep(1,diff(lag)+1)) %*% lagbasis)  
    newbasis <- do.call("onebasis",c(list(x=at),attr$argvar))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attr$argvar))
      newbasis <- scale(newbasis,center=basiscen,scale=FALSE)
    }
  } else if(type=="lag") {
    lagbasis <- do.call("onebasis",c(list(x=value),attr$arglag))
    M <- diag(ncol(basis)/ncol(lagbasis)) %x% lagbasis
    newbasis <- do.call("onebasis",c(list(x=at),attr$argvar))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attr$argvar))
      newbasis <- scale(newbasis,center=basiscen,scale=FALSE)
    }
  } else if(type=="var") {
    varbasis <- do.call("onebasis",c(list(x=value),attr$argvar))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attr$argvar))
      varbasis <- scale(varbasis,center=basiscen,scale=FALSE)
    }
    M <- varbasis %x% diag(ncol(basis)/ncol(varbasis))
    newbasis <- do.call("onebasis",c(list(x=seqlag(lag,bylag)),attr$arglag))
  }
#
  # NAMES
  dimnames(newbasis) <- list(seq(nrow(newbasis)),paste0("b",seq(ncol(newbasis))))
#
  # CREATE NEW SET OF COEF AND VCOV
  newcoef <- as.vector(M%*%coef)
  names(newcoef) <- colnames(newbasis)
  newvcov <- M%*%vcov%*%t(M)
  dimnames(newvcov) <- list(colnames(newbasis),colnames(newbasis))
#
##########################################################################
# PREDICTION
#
  fit <- as.vector(newbasis%*%newcoef)
  se <- sqrt(pmax(0,rowSums((newbasis%*%newvcov)*newbasis)))
  if(type=="var") {
    names(fit) <- names(se) <- outer("lag",seqlag(lag,bylag),paste,sep="")
  }else names(fit) <- names(se) <- at
#
###########################################################################
# CREATE THE OBJECT
#
  # INITIAL LIST
  list <- list(coefficients=newcoef, vcov=newvcov, basis=newbasis, type=type,
    value=value)
  if(type!="var") list$predvar <- at
  if(!is.null(cen)) list$cen <- cen
  list <- c(list, list(lag=lag, bylag=bylag, fit=fit, se=se))
#
  # VECTORS WITH EXPONENTIATED EFFECTS AND CONFIDENCE INTERVALS
  z <- qnorm(1-(1-ci.level)/2)
  if(model.link %in% c("log","logit")) {
    list$RRfit <- exp(fit)
    list$RRlow <- exp(fit-z*se)
    names(list$RRlow) <- names(fit)
    list$RRhigh <- exp(fit+z*se)
    names(list$RRhigh) <- names(fit)
  } else {
    list$low <- fit-z*se
    names(list$low) <- names(fit)
    list$high <- fit+z*se
    names(list$high) <- names(fit)
  }
#
  list$ci.level <- ci.level
  list$model.class <- model.class
  list$model.link <- model.link
#
  class(list) <- "crossreduce"
#
  return(list)
}
