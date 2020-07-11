;; ensure libj shared object is loaded
(library (juniper)
  (export libj          ; parameter to find j shared object 
	  j-binpath     ; parameter to tell J what to treat as its binary name
	  profile.ijs   ; parameter to load given profile.ijs file at startup

	  JInit         ; initialize a J engine
	  JFree         ; free a J engine
	  JSMX          ; give J engine callbacks for i/o
	  JDo           ; j-do
	  JGetLocale    ; returns current locale
	  JGetR         ; return captured state as string
	  JGetM         ; fill argument pointers with data referenced by given variable
	  JSetM         ; set variable from data inside given pointers
	  JOutput       ; given ports for out/log/err, return foreign-callable procedure
	  JInput        ; given port for in, return foreign-callable procedure

	  j-do          ; give sentence to J to run
	  j-eval        ; run J sentence and grab result as scheme 
	  j-e/p         ; j-do, printing output
	  j-script      ; read file containing a j script
	  start-j       ; j-start start a j engine
	  exit-j        ; close a j engine
	  dump-log-j    ; dump j engines ports
	  j-get         ; get value of variable in j engine, in j-value record (for now)
	  j-types       ; vector enumerating J types

	  j-value?      ; scheme record to hold data read from within J engine
	  j-value-type
	  j-value-rank
	  j-value-shape
	  j-value-data

	  j?            ; j engine record type
	  j-engine      ; the j engine
	  j-in          ; the ports...
	  j-out
	  j-log
	  j-err)
  (import (chezscheme))

  ;; params for finding and configuring J. The most interesting to
  ;; modify is probably profile.ijs
  (define j-install-path
    (make-parameter "/gnu/store/csr51al3xgpv5dsp2czg0gyj36f7kwhh-j-902"))
  (define libj
    (make-parameter
     (string-append (j-install-path) "/bin/libj.so")))
  (define j-binpath
    (make-parameter
     (string-append (j-install-path) "/bin/jconsole")))
  (define profile.ijs
    (make-parameter
     (string-append (j-install-path) "/bin/profile.ijs")))

  ;; load the goods
  (include "code/misc.scm")
  (include "code/libj-ffi.scm")
  (include "code/j-front-end.scm")

  )
