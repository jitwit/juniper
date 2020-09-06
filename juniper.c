#include <emacs-module.h>
int plugin_is_GPL_compatible;

#include <assert.h>
#include <stddef.h>
#include <limits.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

#include "juniper.h"

V* jdllproc=0;V* jdlljt=0;V* hjdll=0;

// keep J engine in void *PTR?
// void *get_user_ptr (emacs_env *ENV, emacs_value val)
// void set_user_ptr (emacs_env *ENV, emacs_value VALUE, void *PTR)
// emacs_value make_user_ptr (emacs_env *ENV, emacs_finalizer FIN, void *PTR)
// or
// global_ref
// emacs_value make_global_ref (emacs_env *ENV, emacs_value VALUE)
// void free_global_ref (emacs_env *ENV, emacs_value GLOBAL_VALUE)

// need to have the following ingredients:
// pointer to J engine
// callbacks to give J engine
// conform to dynamic module stuff, starting with init function

// maybe useful functions?
// creates symbol, useful with funcall to invoke lisp-side emacs stuff 
// emacs_value intern (emacs_env *ENV, const char *name)
// call given func, NULL is ok for 0 arity
// emacs_value funcall (emacs_env *ENV, emacs_value FUNC, ptrdiff_t NARGS, emacs_value *ARGS)

// also probably useful:
// bool should_quit (emacs_env *ENV)

// enum emacs_funcall_exit non_local_exit_check (emacs_env *ENV)
//   emacs_funcall_exit_return
//   emacs_funcall_exit_signal
//   emacs_funcall_exit_throw


// part of the reason for this is that the jconsole sucks as a
// subprocess, & the j-mode sucks as a mode. Let's cut the middle man
// jconsole and use the J engine directly.

// borrow from ecaml
static emacs_env *active_env = NULL;
static V freeJ (V *w) {
  jefree(w);
}
// static emacs_value
// FJInit(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
// {
// 	lua_State *ls = luaL_newstate();
// 	luaL_openlibs(ls);
// 	return env->make_user_ptr(env, lua_free, ls);
// }



// some resources in addition to emacs docs:
//   https://phst.eu/emacs-modules
//   https://nullprogram.com/blog/2017/02/14/
//   http://diobla.info/blog-archive/modules-tut.html
int emacs_module_init (struct emacs_runtime *ert) {
  emacs_env *env = ert->get_environment(ert);
  assert(active_env == NULL);
  active_env = env;
  emacs_value Qargv = env->intern(env, "argv");
  emacs_value list = env->funcall(env, env->intern(env, "symbol-value"), 1, &Qargv);
  emacs_value vect = env->funcall(env, env->intern(env, "vconcat"), 1, &list);  
  return 0;
}
