###
### R routines for the R package dlnm (c) Antonio Gasparrini 2012-2017
#
.onAttach <- 
function(lib, pkg) {
#
################################################################################
#
  meta <- packageDescription("dlnm")
  attachmsg <- paste("This is dlnm ",meta$Version,
    ". For details: help(dlnm) and vignette('dlnmOverview').",
    "\n",
    "Important changes: see file.show(system.file('Changesince220',package='dlnm'))",
    sep="")
  packageStartupMessage(attachmsg, domain = NULL, appendLF = TRUE)
}

