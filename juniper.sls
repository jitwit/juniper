(load-shared-object "/gnu/store/csr51al3xgpv5dsp2czg0gyj36f7kwhh-j-902/bin/libj.so")

(library (juniper)
  (export libj          ; parameter to find j shared object 
	  j-binpath     ; parameter to tell J what to treat as its binary name
	  profile.ijs   ; parameter to load given profile.ijs file at startup
		        
	  j-e/p         ; eval/print j sentence in given j engine
	  j-script      ; read file containing a j script
	  j-start       ; j-start start a j engine
	  j-exit        ; close a j engine
	  j-dump-log    ; dump j engines ports
	  j-get         ; get value of variable in j engine
	  JGetM	        
	  JGetA	        
	  j-get         ; use JGetM to get j value of j variable in j engine

	  j-value?
	  j-value-type
	  j-value-rank
	  j-value-shape
	  j-value-data
	  j-types

	  j-engine      ; j engine and its ports
	  j-in
	  j-out
	  j-log
	  j-err)
  (import (chezscheme))

  ;; params for finding and confiuring J.
  (define libj
    (make-parameter "/gnu/store/csr51al3xgpv5dsp2czg0gyj36f7kwhh-j-902/bin/libj.so"))
  (define j-binpath
    (make-parameter "/gnu/store/csr51al3xgpv5dsp2czg0gyj36f7kwhh-j-902/bin/jconsole"))
  (define profile.ijs
    (make-parameter "/gnu/store/csr51al3xgpv5dsp2czg0gyj36f7kwhh-j-902/bin/profile.ijs"))

  ;; load the goods
  (include "code/misc.scm")
  (include "code/libj-ffi.scm")
  (include "code/j-front-end.scm")

  )
