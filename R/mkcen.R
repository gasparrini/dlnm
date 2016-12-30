###
### R routines for the R package dlnm (c) Antonio Gasparrini 2016-2017
#
mkcen <- function(cen, type, basis, range) {
#
################################################################################
#
  # IF NULL, TRY TO EXTRACT IT FROM BASIS
  if (nocen <- is.null(cen))
    cen <- switch(
      type,
      cb = attributes(basis)$argvar$cen,
      one = attributes(basis)$cen,
      gam = NULL
    )
#
  # DEPENDING ON FUNCTION
  fun <- switch(
    type,
    cb = attributes(basis)$argvar$fun,
    one = attributes(basis)$fun,
    gam = basis$margin[[1]]$fun
  )
  # SET CENTERING DEPENDING ON FUNCTION AND cen TYPE
  if (!is.null(fun) && fun %in% c("thr", "strata", "integer", "lin")) {
    if (is.logical(cen)) cen <- NULL
  } else {
    # IF NULL OR TRUE, SET TO (APPROXIMATELY) MID-RANGE AND MESSAGE
    if (is.null(cen) || (is.logical(cen) && cen)) cen <- median(pretty(range))
    # IF FALSE, NULL
    if (is.logical(cen) && !cen) cen <- NULL
  }
#
  # HOWEVER, IF INTERCEPT IS PRESENT, SET TO NULL
  int <- switch(
    type,
    cb = attributes(basis)$argvar$intercept,
    one = attributes(basis)$intercept,
    gam = basis$margin[[1]]$intercept
  )
  if (is.logical(int) && int) cen <- NULL
#
  # MESSAGE
  if(nocen && !is.null(cen))
    message("centering value unspecified. Automatically set to mid-range of predictor")
#
  return(cen)
}
