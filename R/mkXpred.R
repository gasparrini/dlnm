###
### R routines for the R package dlnm (c) Antonio Gasparrini 2016
#
mkXpred <-
function(type, basis, at, predvar, predlag, cen) {
#
################################################################################
# CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON TYPE)
#
  # CREATE VECTORIZED LAGGED VALUES
  varvec <- if(is.matrix(at)) as.numeric(at) else rep(at,length(predlag))
  lagvec <- rep(predlag,each=length(predvar))
#
  if(type=="cb") {
    # IF STANDARD CROSS-BASIS, CREATE MARGINAL BASIS AND CALL TENSOR
    # NB: ORDER OF BASIS MATRICES IN TENSOR CHANGED SINCE VERSION 2.2.4
    # CENTERING APPLIED ONLY MARGINALLY TO VAR DIMENSION
    basisvar <- do.call("onebasis",c(list(x=varvec),attr(basis,"argvar")))
    basislag <- do.call("onebasis",c(list(x=lagvec),attr(basis,"arglag")))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attr(basis,"argvar")))
      basisvar <- scale(basisvar,center=basiscen,scale=FALSE)
    }
    Xpred <- tensor.prod.model.matrix(list(basisvar,basislag))
  } else if(type=="one") {
    # IF ONEBASIS, SIMPLY CALL THE FUNCTION WITH PROPER ARGUMENTS
    ind <- match(c("fun",names(formals(attr(basis,"fun")))),
      names(attributes(basis)),nomatch=0)
    basisvar <- do.call("onebasis",c(list(x=varvec),attributes(basis)[ind]))
    if(!is.null(cen)) {
      basiscen <- do.call("onebasis",c(list(x=cen),attributes(basis)[ind]))
      basisvar <- scale(basisvar,center=basiscen,scale=FALSE)
    }
    Xpred <- basisvar
  } else {
    # FINALLY, IF GAM, CALL PredictMat WITH PROPER DATA
    # CENTERING APPLIED TO THE TENSOR PRODUCT (NOT EFFICIENT BUT EASIER)
    data <- list(varvec,lagvec)
    names(data) <- basis$term
    Xpred <- PredictMat(basis,data,n=length(varvec))
    if(!is.null(cen)) {
      data[[1]] <- rep(cen,length(varvec))
      cbcen <- PredictMat(basis,data,n=length(varvec))
      Xpred <- Xpred-cbcen
    }
  }
#
  return(Xpred)
}

