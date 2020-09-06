#include <emacs-module.h>
int plugin_is_GPL_compatible;

#include <assert.h>
#include <stddef.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "juniper-module.h"

typedef V* (*JIT)(); // j init type
typedef V* (*JDT)(); // j do   type
typedef V* (*JFT)(); // j free type
typedef emacs_value EV;
typedef emacs_env EE;
typedef struct emacs_runtime ERT;

static JDT jdo;
static JFT jfree;
static V* jh;

// need to get a hold of the j engine and execute the sentence
static EV jedo (EE *env,ptrdiff_t n, EV *args, V *ptr) {

}

// dll functions:
// JInit, JDo, JSMX, JFree, etc.
int emacs_module_init (ERT *rt) {
  EE *e = rt->get_environment(rt);
  jh = dlopen(LIBJ,RTLD_LAZY);
  J j=((JIT)dlsym(jh,"JInit"))();
  jdo=((JDT)dlsym(jh,"JDo"));
  jfree=((JFT)dlsym(jh,"JFree"));

  e->make_user_ptr(e,(V*)jfree,j);

  EV provide = e->intern(e, "provide");
  EV jm = e->intern(e, "juniper-module");
  e->funcall(e, provide, 1, &jm);
  return 0;
}

// some resources in addition to emacs docs:
//   https://phst.eu/emacs-modules
//   https://nullprogram.com/blog/2017/02/14/
//   http://diobla.info/blog-archive/modules-tut.html

// some examples:
//   https://github.com/janestreet/ecaml
//   https://github.com/syohex/emacs-lua
//   https://github.com/syohex/emacs-perl
