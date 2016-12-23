### R routines for the R package dlnm (c) Antonio Gasparrini 2016
#' @describeIn internals used internally in \code{\link{crosspred}} to
#' define the centering value for computing predictions.
#' @param cen logical or a numeric scalar. It specifies the centering value,
#' then used as a reference for predictions.
#' @param type type of model from which predictions are needed. See \code{\link{crosspred}}.
#' @param basis type of basis object from which predictions are needed. See \code{\link{crosspred}}.
#' @param range range of values used for prediction.
mkcen <- function(cen, type, basis, range) {
  # IF NULL, TRY TO EXTRACT IT FROM BASIS
  if (is.null(cen)) 
    cen <- switch(type, cb = attributes(basis)$argvar$cen, one = attributes(basis)$cen, gam = NULL)
  # DEPENDING ON FUNCTION
  fun <- switch(type, cb = attributes(basis)$argvar$fun, one = attributes(basis)$fun, gam = basis$margin[[1]]$fun)
  if (!is.null(fun) && fun %in% c("thr", "strata", "integer", "lin")) {
    if (is.logical(cen)) 
      cen <- NULL
  } else {
    # IF NULL OR TRUE, MID-RANGE
    if (is.null(cen) || is.logical(cen) && cen) 
      cen <- mean(range)
    # IF FALSE, NULL
    if (is.logical(cen) && !cen) 
      cen <- NULL
  }
  # HOWEVER, IF INTERCEPT IS PRESENT, SET TO NULL
  int <- switch(type, cb = attributes(basis)$argvar$intercept, one = attributes(basis)$intercept, gam = basis$margin[[1]]$intercept)
  if (is.logical(int) && int) 
    cen <- NULL
  # 
  return(cen)
}
