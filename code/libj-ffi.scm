(define-ftype J void*)
(define-ftype I long-long)
(define-ftype C char)
(define-ftype B unsigned-8)
(define-ftype D double)
(define-ftype Z (struct (re D) (im D)))

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
(define JGetR (foreign-procedure "JGetR" (J) string))
;; give J some points and it will tell them where to find the variable
;; stored in variable name string.
(define JGetM
  (foreign-procedure "JGetM" (J string (* I) (* I) (* I) (* I)) I))
;; reverse of JGetM, tell J where to find J data and it'll set the
;; variable to it
(define JSetM
  (foreign-procedure "JGetM" (J string (* I) (* I) (* I) (* I)) I))

(define j-types
  '#(boolean
     literal
     integer
     float
     complex
     boxed
     extended
     rational
     bit-boolean
     sparse-boolean
     sparse-literal
     sparse-integer
     sparse-floating
     sparse-complex
     sparse-boxed
     symbol
     unicode-2
     unicode-4
     extended-floating
     extended-complex
     ;; etc.?
     ))

(define-record-type j-value
  (fields type rank shape data))
(define (->I)
  (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))

(define (decode-integral-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    (vector-set! V i (ftype-ref I () ->j i))))
(define (decode-boolean-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    (vector-set! V i (ftype-ref B () ->j i))))
(define (decode-string-bytes ->j n)
  (define S (make-string n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) S)
    (string-set! S i (ftype-ref C () ->j i))))
(define (decode-floating-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    (vector-set! V i (ftype-ref D () ->j i))))
(define (decode-complex-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    (vector-set! V i (make-rectangular (ftype-ref Z (re) ->j i)
				       (ftype-ref Z (im) ->j i)))))

;; i2j =: (-2+IF64) & ic NB. (ic =: 3!:4)
;; szi is 8 here (64bit)
;; jgetext=: 3 : 0
;; len=. i2j memr y,(7*SZI), SZI
;; 10000 #. x: |. i2j memr y,(8*SZI),SZI*len
;; so need 7 words to represent it?
(define (decode-extended-j-integer ->j n off)
  (let ((len (ftype-ref I () (make-ftype-pointer I (ftype-ref I () ->j off)) 7))
	(addr (ftype-ref I () ->j off)))
    (let lp ((k (fx1- len)) (x 0))
      (if (fx< k 0)
	  x
	  (lp (fx1- k)
	      (+ (ftype-ref I () (make-ftype-pointer I (fx+ addr (fxsll (fx+ 8 k) 3))))
		 (* 10000 x)))))))

(define (decode-extended-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    (vector-set! V i (decode-extended-j-integer ->j n i))))

(define (decode-rational-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    (vector-set! V i (/ (decode-extended-j-integer ->j n (fxsll i 1))
			(decode-extended-j-integer ->j n (fx1+ (fxsll i 1)))))))

(define (decode-extended-floating-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    ;; typedef struct {I e,p;X x;} DX;
    ;; e is exp, p prec, x mantissa. X=A ie decodable bytes
    ;; prec of -1 => infinite
    ;; prec of -2 => _
    ;; prec of -3 => __
    (vector-set! V i 'todo)))

;; jgetobj=: 3 : 0
;; 'p j j t c l r'=. i2j memr y,0, 7*SZI
;; (t,r,y+7*SZI) jfix y+p
(define (decode-boxed-bytes ->j n)
  (define V (make-vector n))
  (do ((i 0 (fx1+ i)))
      ((fx= i n) V)
    (let* ((type (vector-ref
		  j-types
		  (fx1-
		   (fxlength
		    (ftype-ref I () (make-ftype-pointer I (ftype-ref I () ->j i))
			       3)))))
	   (p (ftype-ref I () (make-ftype-pointer I (ftype-ref I () ->j i)) 0))
	   (shape (decode-integral-bytes
		   (make-ftype-pointer I (fx+ 56 (ftype-ref I () ->j i)))
		   (fxsrl (fx- p 56) 3)))
	   (bytes (fx+ p (ftype-ref I () ->j i))))
      (vector-set! V i
		   (make-j-value type
				 (vector-length shape)
				 shape
				 (decode-bytes type shape bytes))))))

(define (decode-bytes type shape addr)
  (define n (apply fx* (vector->list shape)))
  (case type
    ((integer)  (decode-integral-bytes (make-ftype-pointer I addr) n))
    ((literal)  (decode-string-bytes   (make-ftype-pointer C addr) n))
    ((boolean)  (decode-boolean-bytes  (make-ftype-pointer B addr) n))
    ((float)    (decode-floating-bytes (make-ftype-pointer D addr) n))
    ((complex)  (decode-complex-bytes  (make-ftype-pointer Z addr) n))
    ((rational) (decode-rational-bytes (make-ftype-pointer I addr) n))
    ((extended) (decode-extended-bytes (make-ftype-pointer I addr) n))
    ((boxed)    (decode-boxed-bytes    (make-ftype-pointer I addr) n))
    ;; boxed will be recursive decoding of j values
    (else 'todo)))

(define (j-get j variable)
  (define jt (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (define jr (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (define js (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (define jd (make-ftype-pointer I (foreign-alloc (ftype-sizeof I))))
  (assert (zero? (JGetM (j-engine j) variable jt jr js jd)))
  (let* ((t (vector-ref j-types (fx1- (fxlength (ftype-ref I () jt)))))
	 (r (ftype-ref I () jr))
	 (s (decode-integral-bytes (make-ftype-pointer I (ftype-ref I () js)) r))
	 (d (decode-bytes t s (ftype-ref I () jd))))
    (foreign-free (ftype-pointer-address jt))
    (foreign-free (ftype-pointer-address jr))
    (foreign-free (ftype-pointer-address js))
    (foreign-free (ftype-pointer-address jd))
    (make-j-value t r s d)))

;; // output type
(define MTYOFM   1) ; /* formatted result array output */
(define MTYOER   2) ; /* error output */
(define MTYOLOG  3) ; /* output log */
(define MTYOSYS  4) ; /* system assertion failure */
(define MTYOEXIT 5) ; /* exit */
(define MTYOFILE 6) ; /* output 1!:2[2 */

;;;; Call Backs so J can write stuff and get stuff 
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

(define (JInput in)
  (let ((f (foreign-callable
	    (lambda (jt prompt)
	      (get-line in))
	    (void* string)
	    string)))
    (lock-object f)
    (foreign-callable-entry-point f)))
