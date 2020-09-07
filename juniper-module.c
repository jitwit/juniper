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

#define R return
#define LIBJ "libj.so"

typedef void V;
typedef intmax_t I;
typedef char C;
typedef V* J;

typedef V* (*JIT)(); // j init type
typedef int (*JDT)(J,C*); // j do   type
typedef V* (*JFT)(J); // j free type
typedef V* (*JST)(); // j set callbacks type
typedef emacs_value EV;
typedef emacs_env EE;
typedef struct emacs_runtime ERT;

static V *jh; // dll handle
static JDT jdo;
static JFT jfree;
static JIT jinit;
static JST jsxm;

// from perl module
static C* estring(EE *e, EV s, ptrdiff_t *sz) {
  *sz = 0; e->copy_string_contents(e, s, NULL, sz);
  C *es = malloc(*sz); if (es == NULL) { *sz = 0; R NULL; }
  e->copy_string_contents(e, s, es, sz); R es;
}

static EV jedo (EE *e,ptrdiff_t n, EV *args, V *ptr) {
  J j = e->get_user_ptr(e,args[0]);
  ptrdiff_t size;
  C *s = estring(e,args[1],&size);
  if (e->non_local_exit_check (e)) { free(s); R NULL; }
  int r = jdo(j,s); free(s); R e->make_integer(e,r);
}

static EV jeini (EE *e,ptrdiff_t n, EV *args, V *ptr) {
  R e->make_user_ptr(e,(V*)jfree,jinit());
}

// need callbacks/initialization

int emacs_module_init (ERT *rt) {
  EE *e      = rt->get_environment(rt);
  
  jh         = dlopen(LIBJ,RTLD_LAZY);
  jinit      = (JIT)dlsym(jh,"JInit");
  jdo        = (JDT)dlsym(jh,"JDo");
  jfree      = (JFT)dlsym(jh,"JFree");

  EV provide = e->intern(e, "provide");
  EV fset    = e->intern(e,"fset");
  EV set     = e->intern(e,"set");
  EV args[2];

  args[0]    = e->intern(e,"j-engine");
  args[1]    = e->make_function(e,0,0,jeini,"Create a J Engine",NULL);
  e->funcall(e,fset,2,args);

  args[0]    = e->intern(e,"j-do");
  args[1]    = e->make_function(e,2,2,jedo,"Execute a J Sentence",NULL);
  e->funcall(e,fset,2,args);

  args[0]    = e->intern(e, "juniper-module");
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
