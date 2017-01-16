###
### R routines for the R package dlnm (c) Antonio Gasparrini and Fabian Scheipl 2016-2017
#
Predict.matrix.cb.smooth <- function(object, data) {
#
################################################################################
#
  # TERMS AND DIMENSIONS
  term <- object$term
  dim <- length(term)
#
  # BUILD MARGINAL BASES
  Xm <- list()
  for (i in seq(dim)) { 
    margin <- object$margin[[i]]
    if(!"onebasis"%in%class(margin)) {
      Xm[[i]] <- if(object$mc[i]) PredictMat(margin,data,n=length(data[[1]])) else 
        Predict.matrix(margin,data)
    } else {
      Xm[[i]] <- do.call("onebasis",c(list(x=data[[term[i]]]),margin))
    }
  }
#
  # NB: NO REPARAMETERIZATION THROUGH XP
  # TENSOR (USING mgcv FUNCTION)
  X <- tensor.prod.model.matrix(Xm)
#
  return(X)
}
