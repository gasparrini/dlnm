###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#
summary.crossreduce <-
function(object, ...) {
#
################################################################################
#
  cat("REDUCED FIT\n")
  cat("type:",object$type,"\n")
  cat("dimension:",ifelse(object$type!="var","predictor","lag"),"\n")
  if(object$type!="overall") cat("value:",ifelse(object$type=="lag",
    paste("lag",object$value),object$value),"\n")
  cat("reduced df:",length(coef(object)),"\n")
#
  cat("\nBASIS:\n")
  attr <- attributes(object$basis)
  ind <- match(names(formals(attr$fun)),names(attr),nomatch=0)
  args <- c(list(fun=attr$fun),attr[ind])
  for(i in seq(args)) {
    cat(names(args[i]),": ",sep="")
    cat(args[[i]],"\n",sep=" ")
  }
#  
  cat("\nPREDICTIONS:\n")
  if(!is.null(object$cen)) cat("centered at:",object$cen,"\n")
  if(object$type!="var") cat("range:",min(object$predvar),"to",
    max(object$predvar),"\n")
  if(object$type=="var") cat("lag period:",object$lag,"\n")
  if(object$type!="var") cat("values:",length(object$predvar),"\n")
  if(object$type=="var") cat("by:",length(object$bylag),"\n")
  cat("exponentiated:",ifelse(!is.null(object$RRfit),"yes","no"),"\n")
  cat("\n")
}
