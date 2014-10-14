#include <stdio.h>
#include <string.h>

#include "mruby.h"
#include "mruby/compile.h"
#include "mruby/array.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include <Rdefines.h>
#include <R_ext/PrtUtil.h>

// #ifndef Win32
// #include <R_ext/eventloop.h>
// #endif

static mrb_state* mrb=NULL;
static mrbc_context* mrb_cxt=NULL;

//just testing with require at the beginning!!! (since this fails)
/* void mrb4R_initTEST(void)
{
  ruby_init();
  ruby_init_loadpath();
  printf("ruby initialize!!!\n");
  rb_require("cqlsinit");
  rb_require("rbvor/rbvor");
  rb_require("rbvor/graphe");
  rb_eval_string("$vg=CqlsVor::VorGraph.new(100.0,[0.0,0.0])");
  rb_eval_string("$polyloc=CqlsVor::PolyLoc.new($vg)");
  } */

void mrb4R_init(void)
{
  if(mrb == NULL) {
    mrb = mrb_open();
    mrb_cxt = mrbc_context_new(mrb);
    printf("mruby initialize!!!\n");
  }
}

void mrb4R_finalize(void)
{
  if(mrb != NULL) {
    mrbc_context_free(mrb,mrb_cxt);
    mrb_close(mrb);
    printf("mruby finalize!!!\n");
  }
}

SEXP mrb4R_running(void) {
  SEXP ans;

  PROTECT(ans=allocVector(LGLSXP,1));
  LOGICAL(ans)[0]= (mrb == NULL ? 0 : 1);
  UNPROTECT(1);
  return(ans);
}

// //wrapper !!! une classe R permettant de wrapper un objet Ruby !!!
// static SEXP makeRubyObject(mrb_value rbobj)
// {
//     SEXP obj;

//     obj = R_MakeExternalPtr((void *)&rbobj, R_NilValue, R_NilValue);
    
//     return obj;
// }


int rbIsRVector(mrb_value rbobj) {
  mrb_value rbobj2;
  int i,n;
  
  if(!mrb_obj_is_kind_of(mrb,rbobj,mrb->array_class)) {
     if(!(mrb_obj_is_kind_of(mrb,rbobj,mrb->fixnum_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->float_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->string_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->true_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->false_class))) 
       return 0;
     rbobj2=mrb_ary_new_capa(mrb,1);
     mrb_ary_push(mrb,rbobj2,rbobj);
     rbobj=rbobj2;
     //Seems that the 3 previous lines could be replaced by:
     // return 1;
  }
  n=RARRAY_LEN(rbobj);
  for(i=0;i<n;i++) {
    rbobj2=mrb_ary_entry(rbobj,i);
    if(!(mrb_obj_is_kind_of(mrb,rbobj2,mrb->fixnum_class) || mrb_obj_is_kind_of(mrb,rbobj2,mrb->float_class) || mrb_obj_is_kind_of(mrb,rbobj2,mrb->string_class) || mrb_obj_is_kind_of(mrb,rbobj2,mrb->true_class) || mrb_obj_is_kind_of(mrb,rbobj2,mrb->false_class))) {
      return 0;
    }
  }
  return 1;
}


// SEXP newRbObj(mrb_value rbobj) {
//   SEXP ans,class;

//   ans=(SEXP)makeRubyObject(rbobj);
//   if(rbIsRVector(rbobj)) {
//     PROTECT(class=allocVector(STRSXP,2));
//     SET_STRING_ELT(class,0, mkChar("rbRVector"));
//     SET_STRING_ELT(class,1, mkChar("rbObj"));
//   } else {
//     PROTECT(class=allocVector(STRSXP,1));
//     SET_STRING_ELT(class,0, mkChar("rbObj"));
//   }
//   //classgets(ans,class);
//   SET_CLASS(ans,class);
//   UNPROTECT(1);
//   return ans;
// }

SEXP mrbArray2RVector(mrb_value rbobj);

SEXP dotRb(SEXP args)
{
  SEXP ans;
  char *cmd;
  mrb_value val; //non utilisé pour l'instant!!!

  if(!isValidString(CADR(args)))
    error("invalid argument");
    cmd = (char*)CHAR(STRING_ELT(CADR(args), 0));
//printf("instruction à executer %s\n",cmd);
  val=mrb_load_string_cxt(mrb,cmd,mrb_cxt);
  //printf("state\n");
  if(mrb->exc) {
    if (!mrb_undef_p(val)) {
      mrb_print_error(mrb);
    }
    //rb_p(state);
    printf("MRuby error !!!\n");
    printf("in executing : %s\n",cmd);
    return R_NilValue;
  } else {
    //ans=(SEXP)newRbObj(val);
    ans=mrbArray2RVector(val);
    return ans;
  }
}

// SEXP dotRbRequire(SEXP args)
// {
//   //SEXP ans;
//     char *cmd;
//     //mrb_value val; //non utilisé pour l'instant!!!
//     int state=0;

//     if(!isValidString(CADR(args)))
// 	error("invalid argument");
//     cmd = (char*)CHAR(STRING_ELT(CADR(args), 0));
//     Rprintf("instruction à executer %s\n",cmd);
//     rb_protect((mrb_value (*)(mrb_value))(&mrb4R_require),(mrb_value)cmd,&state);
//     if(state) Rprintf("error in dotRbRequire (state=%d)!!!\n",state);
//     //val non converti en RObject pour l'instant!!!
//     return R_NilValue;//mkString("ok"); //retour arbitraire
// }

// SEXP dotRbLoad(SEXP args)
// {
//   //SEXP ans;
//     char *cmd;
//     //mrb_value val; //non utilisé pour l'instant!!!
//     int status;

//     if(!isValidString(CADR(args)))
// 	error("invalid argument");
//     cmd = (char*)CHAR(STRING_ELT(CADR(args), 0));
//     Rprintf("instruction à loader %s\n",cmd);
//     rb_protect ((mrb_value (*) ()) rb_load_file, (mrb_value) cmd, &status);
//    status = ruby_exec();
//    status = ruby_cleanup(status);
//     //val non converti en RObject pour l'instant!!!
//     return R_NilValue;//mkString("ok"); //retour arbitraire
// }




//convert R Vector in  rbObj
mrb_value RVector2mrbArray(SEXP vect)
{
  mrb_value res;
  //char *name;
  int i,n=0;
  //Rcomplex cpl;
  //mrb_value res2; 

  //vect have to be R Vector!!!
  if(!isVector(vect) | isNewList(vect)) return mrb_nil_value(); 
  n=length(vect);
  if(n>1) {
    res = mrb_ary_new_capa(mrb,n);
    switch(TYPEOF(vect)) {
    case REALSXP:
      for(i=0;i<n;i++) {
	      mrb_ary_push(mrb,res,mrb_float_value(mrb,REAL(vect)[i]));
      }
      break;
    case INTSXP:
      for(i=0;i<n;i++) {
	      mrb_ary_push(mrb,res,mrb_fixnum_value(INTEGER(vect)[i]));
      }
      break;
    case LGLSXP:
      for(i=0;i<n;i++) {
        mrb_ary_push(mrb,res,(INTEGER(vect)[i] ? mrb_true_value() : mrb_false_value()));      }
      break;
    case STRSXP:
      for(i=0;i<n;i++) {
        mrb_ary_push(mrb,res,mrb_str_new_cstr(mrb,CHAR(STRING_ELT(vect,i))));
      }
      break;
    // case CPLXSXP:
    //   rb_require("complex");
    //   for(i=0;i<n;i++) {
	   //    cpl=COMPLEX(vect)[i];
	   //    res2 = rb_eval_string("Complex.new(0,0)");
	   //    rb_iv_set(res2,"@real",rb_float_new(cpl.r));
	   //    rb_iv_set(res2,"@image",rb_float_new(cpl.i));
	   //    rb_ary_store(res,i,res2);
    //   }
    //   break;
    }
  } else {
    switch(TYPEOF(vect)) {
    case REALSXP:
      res=mrb_float_value(mrb,REAL(vect)[0]);
      break;
    case INTSXP:
      res=mrb_fixnum_value(INTEGER(vect)[0]);
      break;
    case LGLSXP:
      res=(INTEGER(vect)[0] ? mrb_true_value() : mrb_false_value());
      break;
    case STRSXP:
      res=mrb_str_new_cstr(mrb,CHAR(STRING_ELT(vect,0)));
      break;
    // case CPLXSXP:
    //   rb_require("complex");
    //   cpl=COMPLEX(vect)[0];
    //   res= rb_eval_string("Complex.new(0,0)");
    //   rb_iv_set(res,"@real",rb_float_new(cpl.r));
    //   rb_iv_set(res,"@image",rb_float_new(cpl.i));
    //   break;
    }
  }
  return res;
}

SEXP mrb4R_as_rbRvector(SEXP args)
{
  mrb_value val;
  SEXP ans; 
  val=(mrb_value)RVector2mrbArray(CADR(args));
  ans=mrbArray2RVector(val);
  return(ans);
}



//convert rbObj in RVector (assumed to be possible!!!)
SEXP mrbArray2RVector(mrb_value rbobj)
{
  SEXP ans;
  mrb_value arr,elt,tmp;
  //char *name;
  int n,i;
  
  if(!mrb_obj_is_kind_of(mrb,rbobj,mrb->array_class)) {
    if(!(mrb_obj_is_kind_of(mrb,rbobj,mrb->fixnum_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->float_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->string_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->true_class) || mrb_obj_is_kind_of(mrb,rbobj,mrb->false_class))) 
      return R_NilValue;
    n=1;
    arr = mrb_ary_new_capa(mrb,1);
    mrb_ary_push(mrb,arr,rbobj);
  } else {
    arr=rbobj;
    n=RARRAY_LEN(rbobj);  
  }

  //Rprintf("n=%d\n",n);

  elt=mrb_ary_entry(arr,0);

  if(mrb_type(elt)==MRB_TT_FLOAT) {
    PROTECT(ans=allocVector(REALSXP,n));
    for(i=0;i<n;i++) {
      REAL(ans)[i]=mrb_float(mrb_ary_entry(arr,i));
    }
  } else if(mrb_type(elt)==MRB_TT_FIXNUM) {
    PROTECT(ans=allocVector(INTSXP,n));
    for(i=0;i<n;i++) {
      INTEGER(ans)[i]=mrb_int(mrb,mrb_ary_entry(arr,i));
    }
  } else if(mrb_type(elt)==MRB_TT_TRUE || mrb_type(elt)==MRB_TT_FALSE) {
    PROTECT(ans=allocVector(LGLSXP,n));
    for(i=0;i<n;i++) {
      LOGICAL(ans)[i]=(mrb_type(mrb_ary_entry(arr,i))==MRB_TT_FALSE ? FALSE : TRUE);
    }
  } else if(mrb_type(elt)==MRB_TT_STRING) {
    PROTECT(ans=allocVector(STRSXP,n));
    for(i=0;i<n;i++) {
      tmp=mrb_ary_entry(arr,i);
      SET_STRING_ELT(ans,i,mkChar(mrb_string_value_ptr(mrb,tmp)));
    }
  } else ans=R_NilValue;
  UNPROTECT(1);
  return ans; 
}



SEXP mrb4R_is_Rvector(SEXP args) {
  SEXP obj,ans;
  mrb_value rbobj;
  //int i,n;

  obj=CADR(args);
  PROTECT(ans=allocVector(LGLSXP,1));  
  if (!inherits(obj, "rbObj"))  {
    LOGICAL(ans)[0]=FALSE;
    UNPROTECT(1);
    return ans;
  }
  rbobj=*((mrb_value*) R_ExternalPtrAddr(CADR(obj)));
  
  if(!rbIsRVector(rbobj)) {
    LOGICAL(ans)[0]=FALSE;
    UNPROTECT(1);
    return ans;
  }
  LOGICAL(ans)[0]=TRUE;
  UNPROTECT(1);
  return ans;
}

//TODO : partially correct : since args is the name of object simple or homogeneous Array!!!
SEXP mrb4R_as_Rvector(SEXP args)
{
  SEXP ans;
  mrb_value rbobj;

  if (inherits(CADR(args), "rbObj")) {
    rbobj=*((mrb_value*) R_ExternalPtrAddr(CADR(args)));
    ans=mrbArray2RVector(rbobj);
    return ans; 
  } else return R_NilValue;
}


//MRuby global variable!!!
SEXP mrb4R_get_gv(SEXP args) {
  SEXP ans;
  mrb_value rbobj;
  char *name;
  
  if(!isValidString(CADR(args)))
    error("invalid argument");
  name = (char*)CHAR(STRING_ELT(CADR(args), 0));
  rbobj=mrb_gv_get(mrb,mrb_intern_cstr(mrb,name));
  ans=mrbArray2RVector(rbobj);
  return ans;
}

SEXP mrb4R_set_gv(SEXP args) {
  SEXP vect;
  mrb_value rbval;
  char *name;

  if(!isValidString(CADR(args)))
    error("invalid argument");
  name = (char*)CHAR(STRING_ELT(CADR(args), 0));
  vect=CADDR(args);
  rbval=(mrb_value)RVector2mrbArray(vect);
  mrb_gv_set(mrb,mrb_intern_cstr(mrb,name),rbval);
  return R_NilValue;
}

// static mrb_value rbobj_inspect(mrb_value rbobj) {
//   mrb_value expr=rb_inspect(rbobj);
//   Rprintf("%s\n",StringValuePtr(expr));
//   fflush(stdout);
//   return mrb_nil_value();
// }

//inspect
SEXP mrb4R_inspect(SEXP args) {
  mrb_value rbobj;

  if (inherits(CADR(args), "rbObj")) {
    rbobj=*((mrb_value*) R_ExternalPtrAddr(CADR(args)));
    //sometimes, it bugs !!! very strange!!!
    mrb_inspect(mrb,rbobj);
  }
  return R_NilValue;
  
}


SEXP mrb4R_get_iv(SEXP args) {
  SEXP ans;
  mrb_value  rbobj;
  char *var;

  if(!inherits(CADR(args),"rbObj")) error("invalid first argument");
  rbobj=*((mrb_value*) R_ExternalPtrAddr(CADR(args)));
  if(!isValidString(CADDR(args))) error("invalid second argument");
  var = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  ans=mrbArray2RVector(mrb_iv_get(mrb,rbobj,mrb_intern_cstr(mrb,var)));
  return ans;
}

SEXP mrb4R_set_iv(SEXP args) {
  SEXP vect;
  mrb_value  rbobj,rbval;
  char *var;

  if(!inherits(CADR(args),"rbObj")) error("invalid first argument");
  rbobj=*((mrb_value*) R_ExternalPtrAddr(CADR(args)));
  if(!isValidString(CADDR(args))) error("invalid second argument");
  var = (char*)CHAR(STRING_ELT(CADDR(args), 0));
  vect=CADDDR(args);
  rbval=(mrb_value)RVector2mrbArray(vect);
  mrb_iv_set(mrb,rbobj,mrb_intern_cstr(mrb,var),rbval);
  return R_NilValue;
}


// SEXP mrb4R_apply(SEXP args)
// {
//   SEXP ans;
//   char *meth;
//   mrb_value rbobj,rbargs;
//   int state,i,nargs;
  
//   nargs=length(args)-1;
//   if(nargs<2) error("number of arguments greater than 2!!! ");
//   args=CDR(args);
//   if(!inherits(CAR(args),"rbObj")) error("invalid first argument");
//   rbobj=(mrb_value) R_ExternalPtrAddr(CAR(args));
//   args=CDR(args);
//   if(!isValidString(CAR(args))) error("invalid second argument");
//   meth = (char*)CHAR(STRING_ELT(CAR(args), 0));
//   nargs=nargs-2;
//   rbargs = rb_ary_new2(nargs);
//   if(nargs > 0) {
//     for(i=0;i<nargs;i++) {
//       args=CDR(args);
//       if(inherits(CAR(args),"rbObj")) {
// 	      rb_ary_store(rbargs,i,(mrb_value) R_ExternalPtrAddr(CAR(args)));
//       } else {
// 	      rb_ary_store(rbargs,i,RVector2rbArray(CAR(args)));
//       }
//     } 
//   }
//   ans=(SEXP) newRbObj(rb_apply(rbobj,rb_intern(meth),rbargs));
//   return ans;
  
//   //strmeth=CHAR(STRING_ELT(meth, 0));
//   //printf("%s.%s executed\n",strobj,strmeth);
//   //rb_funcall(rb_gv_get(strobj),rb_intern(strmeth),0); 
//   //val non converti en RObject pour l'instant!!!
//   //return R_NilValue;//mkString("ok"); //retour arbitraire
// }


//UNUSED!!!!
SEXP getListElement(SEXP list, char *str) 
{
  int i;
  SEXP elmt=R_NilValue, names=getAttrib(list,R_NamesSymbol);
  for ( i=0; i< length(list); i++)
    if(strcmp(CHAR(STRING_ELT(names,i)),str)==0) {
      elmt= VECTOR_ELT(list,i);
      break;
    }
  return elmt;
}


#include <R_ext/Rdynload.h>
static const R_CMethodDef cMethods[] = {
  {"mrb4R_init",(DL_FUNC) &mrb4R_init,0},
  {"mrb4R_finalize",(DL_FUNC) &mrb4R_finalize,0},
  {NULL,NULL,0}
};

static const R_ExternalMethodDef externalMethods[] = {
  {"dotRb",(DL_FUNC) &dotRb,-1}, 
  //{"dotRbRequire",(DL_FUNC) &dotRbRequire,-1}, 
  //{"dotRbLoad",(DL_FUNC) &dotRbLoad,-1}, 
  //{"mrb4R_get_gv",(DL_FUNC)&mrb4R_get_gv,-1},  
  //{"mrb4R_set_gv",(DL_FUNC)&mrb4R_set_gv,-1}, 
  //{"mrb4R_get_iv",(DL_FUNC)&mrb4R_get_iv,-1},  
  //{"mrb4R_set_iv",(DL_FUNC)&mrb4R_set_iv,-1},
  {"mrb4R_as_Rvector",(DL_FUNC)&mrb4R_as_Rvector,-1}, 
  {"mrb4R_is_Rvector",(DL_FUNC)&mrb4R_is_Rvector,-1},
  {"mrb4R_as_rbRvector",(DL_FUNC)&mrb4R_as_rbRvector,-1},
  {"mrb4R_inspect",(DL_FUNC)&mrb4R_inspect,-1},  
  //{"mrb4R_apply",(DL_FUNC)&mrb4R_apply,-1},
  {NULL,NULL,0}
};

static const R_CallMethodDef callMethods[] = {
  {"mrb4R_running",(DL_FUNC) &mrb4R_running,0},
  {NULL,NULL,0}
};

void R_init_rb4R(DllInfo *info) {
  R_registerRoutines(info,cMethods,callMethods,NULL,externalMethods);
}

