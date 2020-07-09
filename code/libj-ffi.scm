(define-ftype J void*)
(define-ftype I long-long)
(define-ftype C char)

;; CDPROC int _stdcall JGetM(J jt, C* name, I* jtype, I* jrank, I* jshape, I* jdata);
;; typedef A     (_stdcall *JgaType)       (J jt, I t, I n, I r, I*s);
(define-ftype A
  (struct
   (k I)
   (flag I)
   (m I)
   (t I)
   (c I)
   (n I)
   (r I)
   (s (array 1 I))))

;; typedef struct AREP_RECORD {
;;   I n,t,c,r,s[1];
;; }* AREP;
(define-ftype AREP
  (struct
   (n I)
   (t I)
   (c I)
   (r I)
   (s (array 1 I))))

;;;; jlib.h
(define JInit ; CDPROC J _stdcall JInit();
  (foreign-procedure "JInit" () J))
(define JFree ; int JFree(J jt)
  (foreign-procedure "JFree" (J) int))
(define JSMX ; void _stdcall JSMX(J jt, void* out, void* wd, void* in, void* poll, I opts)
  (foreign-procedure "JSMX" (J void* void* void* void* I) void))
(define JDo ; I jdo(J jt, C* lp)
  (foreign-procedure "JDo" (J string) int))
(define JGetLocale ; CDPROC C* _stdcall JGetLocale(J jt);
  (foreign-procedure "JGetLocale" (J) string))
(define JGetR
  (foreign-procedure "JGetR" (J) string))
;; CDPROC A _stdcall JGetA(J jt,I n,C* name);         /* get 3!:1 from name */
;; NB. seems to want n = 1
(define JGetA
  (foreign-procedure "JGetA" (J I string) (* A)))
(define JGetM
  (foreign-procedure "JGetM" (J string (* I) (* I) (* I) (* I)) I))

;; pointers refer to places within J memory
;; CDPROC int _stdcall JGetM(J jt, C* name, I* jtype, I* jrank, I* jshape, I* jdata);
(define j-types
  '#(boolean string integer float))
(define-record-type j-value
  (fields type rank shape data))
(define (->I)
  (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
(define (j-get j variable)
  (define jt (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (define jr (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (define js (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (define jd (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (assert (zero? (JGetM (j-engine j) variable jt jr js jd)))
  (let* ((t (vector-ref j-types (fx1- (fxlength (ftype-ref I () jt)))))
	 (r (ftype-ref I () jr))
	 (s (make-vector r))
	 (js@ (make-ftype-pointer I (ftype-ref I () js)))
	 (jd@ (make-ftype-pointer I (ftype-ref I () jd))))
    (do ((i 0 (fx1+ i)))
	((= i r))
      (vector-set! s i (ftype-ref I () js@ i)))
    (let* ((n (apply * (vector->list s)))
	   (d (make-vector n)))
      (do ((i 0 (fx1+ i)))
	  ((= i n))
	(vector-set! d i (ftype-ref I () jd@ i)))
      (foreign-free (ftype-pointer-address jt))
      (foreign-free (ftype-pointer-address jr))
      (foreign-free (ftype-pointer-address js))
      (foreign-free (ftype-pointer-address jd))
      (make-j-value t r s d))))

;;;; J call backs
;; void _stdcall Joutput(J jt, int type, C* s);
;; int _stdcall Jwd(J jt, int x, A parg, A* pres);
;; C* _stdcall Jinput(J jt, C*);

;; // output type
(define MTYOFM   1) ; /* formatted result array output */
(define MTYOER   2) ; /* error output */
(define MTYOLOG  3) ; /* output log */
(define MTYOSYS  4) ; /* system assertion failure */
(define MTYOEXIT 5) ; /* exit */
(define MTYOFILE 6) ; /* output 1!:2[2 */

(define (JOutput out log err)
  (let ((f (foreign-callable
	    __collect_safe
	    (lambda (jt type s)
	      (case type
		((1) (display s out) (flush-output-port out))
		((2) (display s err) (flush-output-port err))
		((3) (display s log) (flush-output-port log))
		((4) (display s err) (flush-output-port err))
		((5) (display "closing JE" log) (newline log) (JFree jt))
		((6) (display s log) (flush-output-port log))
		(else
		 (display (string-append "unknown type"
					 " "
					 (number->string type)
					 " "
					 s)
			  log)
		 (flush-output-port log))))
	    (void* int string)
	    void)))
    (lock-object f)
    (foreign-callable-entry-point f)))

;; C* _stdcall Jinput(J jt,C* prompt)
(define (JInput in)
  (let ((f (foreign-callable
	    (lambda (jt prompt)
	      (get-line in))
	    (void* string)
	    string)))
    (lock-object f)
    (foreign-callable-entry-point f)))
