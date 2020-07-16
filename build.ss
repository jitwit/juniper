(define args (command-line))
(unless (< 1 (length args))
  (error 'juniper-build-script "please indicate where to find libj.so in first argument"
	 args))
(let ((libj.so (cadr args)))
  (load-shared-object libj.so)
  (compile-library "juniper.sls"))
