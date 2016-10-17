###
### R routines for the R package dlnm (c) Antonio Gasparrini 2016
#
#' @describeIn internals returns a list of penalty matrices defining
#' additional penalties on the lag structure.
#' @param addSlag matrix or vector (or list of matrices and/or vectors)
#' defining additional penalties on the lag structure.
#' @param d numeric vector of length 2 providing the cross-basis dimensions.
mkaddSlag <- function(addSlag, d) {
  #
  ################################################################################
  #
  # LIST OF PENALTY MATRICES
  Slist <- if(is.list(addSlag)) addSlag else list(addSlag)
  if(!all(sapply(Slist,is.numeric)))
    stop("non-numeric values supplied in 'addSlag'")
  Slist <- lapply(Slist,function(x) if(is.matrix(x)) x else diag(x))
  #
  # CHECKS
  for(i in seq(Slist)) if(any(dim(Slist[[i]])!=d[2])) 
    stop("terms in addSlag with dimensions not consistent with basis for lag")
  #
  # RESCALING AND EXPANSION
  Slist <- lapply(Slist,function(X) {
    X <- X/eigen(X,symmetric=TRUE,only.values=TRUE)$values[1]
    diag(d[1])%x%X
  })
  names(Slist) <- paste0("Slag",seq(Slist)+1)
  #
  return(Slist)
}
