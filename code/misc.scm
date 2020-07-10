;; random convenience functions whose convenience is questionable
(define (display-ln s)
  (display s) (newline))

(define (read-lines script)
  (with-input-from-file script
    (lambda ()
      (let ((in (current-input-port)))
	(let loop ((line (get-line in)) (lines '()))
	  (if (eof-object? line)
	      (reverse lines)
	      (loop (get-line in) (cons line lines))))))))

(define (read-lines->string script)
  (with-output-to-string
    (lambda ()
      (with-input-from-file script
	(lambda ()
	  (let ((in (current-input-port)))
	    (let loop ((l (get-line in)))
	      (unless (eof-object? l)
		(display-ln l)
		(loop (get-line in))))))))))

(define (fresh-j-var)
  (let ((var (gensym->unique-string (gensym))))
    (do ((i (fx1- (string-length var)) (fx1- i)))
	((char=? #\- (string-ref var i))
	 (string-set! var i #\_)
	 var)))
  )
