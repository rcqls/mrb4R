.onLoad<- function(lib,pkg) {
  ## Use local=FALSE to allow easy loading of Tcl extensions
  library.dynam("mrb4R", pkg, lib)#,local=FALSE)
  ## IMPORTANT : do not uncomment before for use in ruby directly!!!
  ##.C("rb4R_init", PACKAGE="rb4R")
  ## TODO : in cqls/projet/EBVoronoi/test/test.R -> ERROR si .rbBegin est initialisé!!!
  ##.rbBegin()

  ## dyndoc initialization
  #.dynTmpl<-rbObj(curDyn)$tmpl
  #singleton object
  local({
  dynVar<-list()
  class(dynVar)<-"dynVarSrv"
  dynVarWithArg<-list()
  class(dynVarWithArg)<-"dynVarWithArgSrv"
  ## OBSOLETE SOON
  dynRbVar<-list()
  class(dynRbVar)<-"dynRbVarSrv"
  },globalenv())
}

#.Last.lib <- function(libpath) {
#  cat("Bye Ruby\n")
#  .C("rb4R_finalize", PACKAGE="rb4R")
#}
