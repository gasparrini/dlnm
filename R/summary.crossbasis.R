### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2016
#' @export
summary.crossbasis <- function(object, ...) {
  # 
  attr <- attributes(object)
  cat("CROSSBASIS FUNCTIONS\n")
  cat("observations:", nrow(object), "\n")
  if (!is.null(attr$group)) 
    cat("groups:", attr$group, "\n")
  cat("range:", attr$range[1], "to", attr$range[2], "\n")
  cat("lag period:", attr$lag, "\n")
  cat("total df: ", ncol(object), "\n")
  # 
  cat("\nBASIS FOR VAR:\n")
  attr$argvar$cen <- NULL
  for (i in seq(attr$argvar)) {
    cat(names(attr$argvar[i]), ": ", sep = "")
    if (length(attr$argvar[[i]]) < 7) {
      cat(attr$argvar[[i]], "\n", sep = " ")
    } else cat(attr$argvar[[i]][seq(7)], "...", "\n", sep = " ")
  }
  # 
  cat("\nBASIS FOR LAG:\n")
  for (i in seq(attr$arglag)) {
    cat(names(attr$arglag[i]), ": ", sep = "")
    if (length(attr$arglag[[i]]) < 7) {
      cat(attr$arglag[[i]], "\n", sep = " ")
    } else cat(attr$arglag[[i]][seq(7)], "...", "\n", sep = " ")
  }
  cat("\n")
}

