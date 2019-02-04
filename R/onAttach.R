###
### R routines for the R package dlnm (c)
#
.onAttach <- 
function(lib, pkg) {
#
################################################################################
#
  meta <- packageDescription("dlnm")
  attachmsg <- paste("This is dlnm ",meta$Version,
    ". For details: help(dlnm) and vignette('dlnmOverview').",
    #"\n",
    #"Important changes: see file.show(system.file('Changesince220',package='dlnm'))",
    sep="")
  packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
}

