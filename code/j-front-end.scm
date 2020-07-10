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
       ((eof-object? command) (j-exit j))
       (else (j-e/p j command) (loop))))))

(define (j-initialize j)
  (j-e/p j (string-append "BINPATH_z_ =: '" (j-binpath) "'"))
  (j-e/p j (string-append "ARGV_z_ =: <'" "juniper" "'"))
  (j-e/p j (string-append "0!:0 < '" (profile.ijs) "'")))

(define (j-start)
  (define je (make-jengine-simple))
  (j-initialize je)
  je)

(define (j-script script.ijs)
  (assert (file-exists? script.ijs))
  (let ((je (j-start)))
    (j-do je (string-append "0!:0 <'" script.ijs "'"))
    (j-exit je (current-output-port))))

(define (j-exit j . port)
  (when (pair? port)
    (j-dump-log j (car port))
    (display "---- closing J ----" (car port))
    (newline (car port)))
  (j-do j "2!:55''")
  (close-port (j-in  j))
  (close-port (j-out j))
  (close-port (j-log j))
  (close-port (j-err j))
  (unlock-object j))

(define (j-dump-log j . where)
  (let ((where (if (pair? where) (car where) (current-output-port))))
    (display "------- log -------" where) (newline where)
    (display (get-output-string (j-log j)) where)
    (display "------ error ------" where) (newline where)
    (display (get-output-string (j-err j)) where)))

(define j-eval
  (case-lambda
    ((J sentence)
     (let ((var (fresh-j-var)))
       (if (j-do J (string-append var " =: " sentence))
	   (j->scheme (j-get J var))
	   (begin
	     (j-dump-log J)
	     #f))))
    ((sentence)
     (let* ((J (j-start)) (result (j-eval J sentence)))
       (j-exit J)
       result))))
