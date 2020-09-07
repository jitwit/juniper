#include <emacs-module.h>
int plugin_is_GPL_compatible;

#include <assert.h>
#include <stddef.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <dlfcn.h>

#include "juniper-module.h"

#define R return

typedef V* (*JIT)(); // j init type
typedef V* (*JDT)(); // j do   type
typedef V* (*JFT)(); // j free type
typedef emacs_value EV;
typedef emacs_env EE;
typedef struct emacs_runtime ERT;

static V *jh; // dll handle
static JDT jdo;
static JFT jfree;
static JIT jinit;

// need to get a hold of the j engine and execute the sentence
static EV jedo (EE *e,ptrdiff_t n, EV *args, V *ptr) {
  J j = e->get_user_ptr(e,args[0]);
}

static EV jeini (EE *e,ptrdiff_t n, EV *args, V *ptr) {
  R e->make_user_ptr(e,(V*)jfree,jinit());
}

// dll functions:
// JInit, JDo, JSMX, JFree, etc.
int emacs_module_init (ERT *rt) {
  EE *e = rt->get_environment(rt);
  
  jh = dlopen(LIBJ,RTLD_LAZY);
  jinit = (JIT)dlsym(jh,"JInit");
  jdo=((JDT)dlsym(jh,"JDo"));
  jfree=((JFT)dlsym(jh,"JFree"));

  EV provide = e->intern(e, "provide");
  EV fset = e->intern(e,"fset");
  EV set = e->intern(e,"set");
  EV args[2];

  args[0]=e->intern(e,"j-engine");
  args[1]=e->make_function(e,0,0,jeini,"Create a J Engine",NULL);
  e->funcall(e,fset,2,args);

  args[0] = e->intern(e, "juniper-module");
  e->funcall(e,provide,1,args);
  R 0;
}

// some resources in addition to emacs docs:
//   https://phst.eu/emacs-modules
//   https://nullprogram.com/blog/2017/02/14/
//   http://diobla.info/blog-archive/modules-tut.html

// some examples:
//   https://github.com/janestreet/ecaml
//   https://github.com/syohex/emacs-lua
//   https://github.com/syohex/emacs-perl
