.First.lib <- function(lib, pkg) {
  ## Use local=FALSE to allow easy loading of Tcl extensions
  library.dynam("mrb4R", pkg, lib,local=FALSE)
  
  ## dyndoc initialization
  #.dynTmpl<-rbObj(curDyn)$tmpl
  #singleton object
  local({
  dynVar<-list()
  class(dynVar)<-"dynVarSrv"
  },globalenv())
}

#.Last.lib <- function(libpath) {
#  cat("Bye Ruby\n")
#  .C("rb4R_finalize", PACKAGE="rb4R")
#}
