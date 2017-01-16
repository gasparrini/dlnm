###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2017
#
summary.onebasis <-
function(object, ...) {
#
################################################################################
#
  attr <- attributes(object)
  cat("BASIS FUNCTION\n")
  cat("observations:",nrow(object),"\n")
  cat("range:",attr$range,"\n")
  cat("df:",ncol(object),"\n")
#
  ind <- match(names(formals(attr$fun)),names(attr),nomatch=0)
  args <- c(list(fun=attr$fun),attr[ind])
  for(i in seq(args)) {
    cat(names(args[i]),": ",sep="")
    cat(args[[i]],"\n",sep=" ")
  }
}
