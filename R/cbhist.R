###
### R routines for the R package dlnm (c)
#
cbhist <-
function(basisvar, basislag, seqlag, group) {
  
  # DEFINE DIMENSIONS
  vd <- dim(basisvar) 
  ld <- dim(basislag)

  # ITERATE ALONG VAR BASIS
  cblist <- lapply(seq(vd[2]), function(v) {
    
    # SET AS MATRIX, SET BY ROW
    mat <- matrix(basisvar[,v], vd[1]/ld[1], ld[1], byrow=TRUE)
    
    # ITERATE ACROSS LAG BASIS
    lapply(seq(ld[2]), function(l) {

      # CUMULATION THROUGH MATRIX MULTIPLICATION
      drop(mat %*% (basislag[,l]))
    })
  })
  
  # RETURN UNLISTED VALUES
  return(unlist(cblist))
}