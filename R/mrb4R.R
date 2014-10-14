## the main ruby parsing expression !!!
.mrb <- function(...) 
  .External("dotRb", ..., PACKAGE = "mrb4R")

.rbObj<-function(var) {
  obj<-.External("dotRb",var, PACKAGE = "mrb4R")
  attr(obj,"var")<-var
  obj
}

## slight improvement of .mrb!!
.mrbExec <- function(...,debug=FALSE) {
  instr <- paste(...,sep="")
  if(debug) {cat("instr->");print(instr)}
  .mrb(instr)
  if(debug) cat("instr executed\n")
  return(invisible())
}

# .rbGetGV<- function(obj) {
#   .External("mrb4R_get_gv",obj,PACKAGE="mrb4R")
# }

# .rbSetGV<- function(obj,val) {
#   .External("mrb4R_set_gv",obj,val,PACKAGE="mrb4R")
#   return(invisible())
# }

# .rbApply <- function(obj,meth,...) {
#   rbobj <- (.External("mrb4R_apply",obj,meth,...,PACKAGE="mrb4R"))
#   return(rbobj)
# }

# .rbApply2 <- function(obj,meth,...) {
#   rbobj <- (.External("mrb4R_apply",obj,meth,...,PACKAGE="mrb4R"))
#   if(inherits(rbobj,"rbRVector")) {
#     return(.External("mrb4R_as_Rvector",rbobj,PACKAGE="mrb4R"))
#   } else {
#     return(rbobj)
#   }
# }

##Right now, just deal with global variable!!!
rbObj <- function(obj) {
  .rbGetGV(as.character(substitute(obj)))
}

# PB: c'est difficile de faire cela!
#"<-.rbObj"<-function(obj,value) {
#  .rbSetGV(as.character(substitute(obj)),value)
#  robj
#}
# => idée: tous les objets pourraient être regroupés via un objet de classe rbObjs
# rbObjs[tata]<- 


## not a standard method!!!
is.vector.rbObj<- function(obj) .External("mrb4R_is_Rvector",obj,PACKAGE="mrb4R")

print.rbObj <- function(obj,...) {
  .External("mrb4R_inspect",obj,PACKAGE = "mrb4R")
}

## the magic operators!!!
Ops.rbObj <- function (e1, e2) get(.Generic, mode = "function")(as.vector(e1), as.vector(e2))
Math.rbObj <- function(e1,...)  get(.Generic, mode = "function")(as.vector(e1),...)
Summary.rbObj <- function(e1,...) get(.Generic, mode = "function")(as.vector(e1),...)

## rbRVector class
as.vector.rbRVector<- function(obj,...) {
  .External("mrb4R_as_Rvector",obj,PACKAGE = "mrb4R")
}

.ind4rbObj<-function(ind,ind.char) {
  if(!exists(ind.char)) ind<-ind.char
  as.numeric(ind) -> ind2
  if(is.na(ind2)) { 
    if(substr(ind,1,2)=="..") .rb(paste(":",substring(ind,3),sep=""))
    else ind
  }
  else #ruby is 0-indexed
    ind2-1
}

"[.rbObj" <- function(obj,ind) {
  ind<-.ind4rbObj(ind,as.character(substitute(ind)))
  if(.rbApply2(obj,"respond_to?","[]")) {
    .rbApply(obj,"[]",ind)
  } else warning("this ruby object does not respond to []!!")
}

"[<-.rbObj" <- function(obj,ind,value) {
  ind<-.ind4rbObj(ind,as.character(substitute(ind)))
  if(.rbApply2(obj,"respond_to?","[]=")) {
    .rbApply(obj,"[]=",ind,value)
  } else warning("this ruby object does not respond to []=!!")
  return(obj)
}


"[[.rbObj" <- function(obj,ind) {
  ind<-.ind4rbObj(ind,as.character(substitute(ind)))
  if(.rbApply2(obj,"respond_to?","[]")) {
    .rbApply2(obj,"[]",ind)
  } else warning("this ruby object does not respond to []!!")
}

"[[<-.rbObj" <- function(obj,ind,value) {
  ind<-.ind4rbObj(ind,as.character(substitute(ind)))
  if(.rbApply2(obj,"respond_to?","[]=")) {
    .rbApply(obj,"[]=",ind,value)
  } else warning("this ruby object does not respond to []=!!")
  return(obj)
}


as.rbRVector <- function(obj) .External("mrb4R_as_rbRvector",obj,PACKAGE = "mrb4R")


print.rbRVector <- function(obj,...) {
  print( .External("mrb4R_as_Rvector",obj,PACKAGE = "mrb4R"))
}



## this is the wrapper operator replacing the "." extraction operator 
"$.rbObj" <- function(obj,field) {
  field <- as.character(substitute(field))
  if(!.rbApply2(obj,"respond_to?",field)) {
    warning(paste("rbObj have to respond to  the method '",field,"'!!!"))
    return(invisible())
  }
  ## is a variable or a  method?
  if((field2 <- paste("@",field,sep="")) %in% .rbApply2(obj,"instance_variables")) {
    .External("mrb4R_get_iv",obj,field2)
  } else {
    ## this is a method
    ## TODO : maybe something different form variable
    return(function(...) {.rbApply(obj,field,...)})
  }
}

"$<-.rbObj" <- function(obj,field,value) {
  field <- as.character(substitute(field))
  if(!.rbApply2(obj,"respond_to?",field)) {
    warning(paste("rbObj have to respond to  the method '",field,"'!!!"))
  }
  ## is a variable or a  method?
  if((field2 <- paste("@",field,sep="")) %in% .rbApply2(obj,"instance_variables")) {
    .External("mrb4R_set_iv",obj,field2,value)
  } else {
    field2 = paste(field,"=",sep="")
    if(.rbApply2(obj,"respond_to?",field2)) { 
      ## this is a method
      ## TODO : maybe something different form variable
      .rbApply(obj,field2,value)
    } else {
      warning(paste("rbObj respond to  the method '",field,"' which is not appropriate!!!"))
    }
  }
  return(obj)
}




.mrbBegin<-function() {
  .C("mrb4R_init", PACKAGE="mrb4R")
  return(invisible())
}

.mrbEnd<-function() {
  .C("mrb4R_finalize", PACKAGE="mrb4R")
  return(invisible())
}

.mrbRunning <- function() {
  .Call("mrb4R_running", PACKAGE="mrb4R")
}

# .rbRequire<-function(module) {
#  .External("dotRbRequire", module, PACKAGE="mrb4R")
# }


#################### .rb(...)  parsing use !!!
mrbRequire<-function(module) {
 .mrb(paste("require '",module,"'",sep=""))
}

mrbLoad<-function(prog) {
  .mrb(paste("load '",prog,"'",sep=""))
}

##source2R<-function(file) {
##  .rbBegin()
##  rbRequire("R4rb/2R2R")
##  .rb(paste("R4rb::from2RtoR '",file,".2R'",sep=""))
##  source(paste(file,"_2R.R",sep="")) 
##  .rbEnd()
##}


###############################
# class rbObjSrv
#   rbObjSrv() -> dyn
#   dyn[tata]<-"tataA"
#   print(dyn[tata])
#   print(class(dyn[tata]))
#   tutu<-"tata"
#   print(dyn[tutu])
################################

rbObjSrv<-function() {
  obj<-list(ls=list())
  class(obj)<-"rbObjSrv"
  obj
}

"[.rbObjSrv" <- function(obj,key) {
  key<-.ind4rbObj(key,as.character(substitute(key)))
  obj$ls[[key]]
}

"[<-.rbObjSrv" <- function(obj,key,value) {
  key<-.ind4rbObj(key,as.character(substitute(key)))
  if(is.null(obj$ls[[key]])) {
    if(!inherits(value,"rbObj")) value <- as.rbRVector(value)
    obj$ls[[key]]<-value
  }
  obj   
}

# dyndoc

.dynVar<-function(var) {
  .rb(paste("$curDyn.tmpl.vars.extract_raw(%Q!",var,"!)",sep=""))
}

## for the text dyndoc variables
"[.dynVarSrv" <- function(obj,key) {
  key<-.ind4rbObj(key,as.character(substitute(key)))
  as.vector(.dynVar(key)[..val][0])
}

"[<-.dynVarSrv" <- function(obj,key,value) { 
  key<-.ind4rbObj(key,as.character(substitute(key)))
  ##if(is.null(obj$ls[[key]])) {## VERY WEIRD ls is not a field of obj????
    if(!inherits(value,"rbObj")) value <- as.rbRVector(value)
    tmp<-.dynVar(key)
    tmp[..val][0]<-value
  ##}
  obj   
}

## for the rb dyndoc variables (FIRST VERSION)
if(FALSE) {
"[[.dynVarSrv" <- function(obj,key,asVect=TRUE) {
  key<-.ind4rbObj(key,as.character(substitute(key)))
  tmp<-.dynVar(paste(key,"@",sep=""))
  if(asVect) return(as.vector(tmp[..rb])) else return(tmp[..rb])
}

"[[<-.dynVarSrv" <- function(obj,key,asVect,value) { 
  key<-.ind4rbObj(key,as.character(substitute(key)))
  if(is.null(obj$ls[[key]])) {## VERY WEIRD ls is not a field of obj => BUT this answers always NULL!
    if(!inherits(value,"rbObj")) value <- as.rbRVector(value)
    tmp<-.dynVar(paste(key,"@",sep=""))
    tmp[..rb]<-value
  }
  obj   
}
}

# SECOND VERSION
"[[.dynVarSrv" <- function(obj,key,mode="@",asVect=TRUE) { #mode= @ (for rb), & (for jl)
  key<-.ind4rbObj(key,as.character(substitute(key)))
  tmp<-.dynVar(paste(key,mode,sep=""))
  res <- switch(mode,"@"=tmp[..rb] ,"&"=tmp[..jl])
  if(asVect) return(as.vector(res)) else return(res)
}

"[[<-.dynVarSrv" <- function(obj,key,mode="rb",asVect=TRUE,value) { #mode= @ (for rb), & (for jl)
  key<-.ind4rbObj(key,as.character(substitute(key)))
  #if(is.null(obj$ls[[key]])) {## VERY WEIRD ls is not a field of obj => BUT this answers always NULL!
    if(!inherits(value,"rbObj")) value <- as.rbRVector(value)
    tmp<-.dynVar(paste(key,mode,sep=""))
    switch(mode,"@"=tmp[..rb]<-value,"&"=tmp[..jl]<-value)
  #}
  obj   
}

.dynVarWithArg<-function(var,arg="",mode="@") {
  mode <-switch("@"="rb","&"="jl")
#print(paste("$curDyn.tmpl.vars.extract_raw(%Q!",var,"!)[:",mode,"]",arg,sep=""))
  .rb(paste("$curDyn.tmpl.vars.extract_raw(%Q!",var,"!)[:",mode,"]",arg,sep=""))
}

## only for the rb dyndoc variables
"[[.dynVarWithArgSrv" <- function(obj,key,arg="",mode="@") {
  as.vector(.dynVarWithArg(paste(key,mode,sep=""),arg,mode))
}

"[[<-.dynVarWithArgSrv" <- function(obj,key,arg="",value,mode="@") { 
  if(!inherits(value,"rbObj")) value <- as.rbRVector(value)
  tmp<-.dynVarWithArg(paste(key,mode,sep=""),arg,mode)
  tmp<-value
  obj   
}

## Obsolete soon
.dynRbVar<-function(var,arg="") {
#print(paste("$curDyn.tmpl.vars.extract_raw(%Q!",var,"!)[:rb]",arg,sep=""))
  .rb(paste("$curDyn.tmpl.vars.extract_raw(%Q!",var,"!)[:rb]",arg,sep=""))
}

## only for the rb dyndoc variables
"[.dynRbVarSrv" <- function(obj,key,arg="") {
  as.vector(.dynRbVar(key,arg))
}

"[<-.dynRbVarSrv" <- function(obj,key,arg="",value) { 
  if(!inherits(value,"rbObj")) value <- as.rbRVector(value)
  tmp<-.dynRbVar(key,arg)
  tmp<-value
  obj   
}
