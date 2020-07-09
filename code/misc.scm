;; random convenience functions whose convenience is questionable
(define (display-ln s)
  (display s) (newline))

(define (read-lines script)
  (with-output-to-string
    (lambda ()
      (with-input-from-file script
	(lambda ()
	  (let ((in (current-input-port)))
	    (let loop ((l (get-line in)))
	      (unless (eof-object? l)
		(display-ln l)
		(loop (get-line in))))))))))

