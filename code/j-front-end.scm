(define-record-type j
  (fields engine in out log err)
  (protocol
   (lambda (new)
     (lambda (in-port out-port log-port error-port)
       (define j (JInit))
       (define i in-port)
       (define o out-port)
       (define l log-port)
       (define e error-port)
       (JSMX j (JOutput o l e) 0 (JInput i) 0 8)
       (let ((engine (new j i o l e)))
	 (lock-object engine) ; necessary?
	 engine)))))

(define (make-jengine-simple)
  (make-j (current-input-port) ; in
	  (open-output-string) ; out
	  (open-output-string) ; log
	  (open-output-string) ; error
	  ))

(define (j-do j sentence)
  (JDo (j-engine j) sentence))

(define (j-get-output j)
  (display (get-output-string (j-out j))))

(define (j-e/p j sentence)
  (j-do j sentence)
  (j-get-output j))

(define (j-r j)
  (get-line (j-in j)))

(define (j-repl j prompt)
  (let loop ()
    (display prompt)
    (let ((command (j-r j)))
      (cond
       ((eof-object? command) (exit-j j))
       (else (j-e/p j command) (loop))))))

(define (initialize-j J)
  (j-e/p J (string-append "BINPATH_z_ =: '" (j-binpath) "'"))
  (j-e/p J (string-append "ARGV_z_ =: <'" "juniper" "'"))
  (j-e/p J (string-append "0!:0 < '" (profile.ijs) "'")))

(define (start-j)
  (let ((J (make-jengine-simple)))
    (initialize-j J)
    J))

(define (j-script script.ijs)
  (assert (file-exists? script.ijs))
  (let ((J (start-j)))
    (j-do J (string-append "0!:0 <'" script.ijs "'"))
    (exit-j J (current-output-port))))

(define (exit-j j . port)
  (when (pair? port)
    (dump-log-j j (car port))
    (display "---- closing J ----" (car port))
    (newline (car port)))
  (j-do j "2!:55''")
  (close-port (j-in  j))
  (close-port (j-out j))
  (close-port (j-log j))
  (close-port (j-err j))
  (unlock-object j))

(define (dump-log-j j . where)
  (let ((where (if (pair? where) (car where) (current-output-port))))
    (display "------- log -------" where) (newline where)
    (display (get-output-string (j-log j)) where)
    (display "------ error ------" where) (newline where)
    (display (get-output-string (j-err j)) where)))

(define j-eval
  (case-lambda
    ((sentence J)
     (let ((var (fresh-j-var)))
       (if (j-do J (string-append var " =: " sentence))
	   (j->scheme (j-get J var))
	   (begin
	     (dump-log-j J)
	     #f))))
    ((sentence)
     (let* ((J (start-j)) (result (j-eval sentence J)))
       (exit-j J)
       result))))
