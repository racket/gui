(thread
 (let ([print-convert
	(parameterize ([current-namespace (make-namespace)])
	  (require-library "pconvert.ss")
	  (global-defined-value 'print-convert))])
   (lambda ()
     (let*-values ([(in out) (tcp-connect "localhost" (load-relative "receive-sexps-port.ss"))]
		   [(continue) (make-semaphore 0)])
       (let loop ()
	 (let ([sexp (read in)])
	   (if (eof-object? sexp)
	       (begin
		 (close-input-port in)
		 (close-output-port out)
		 (exit))
	       (begin
		 (write
		  (with-handlers ([(lambda (x) #t)
				   (lambda (exn)
				     (list 'error (if (exn? exn)
						      (exn-message exn)
						      (format "~s" exn))))])
		    (list 'normal (print-convert (eval sexp))))
		  out)
		 (loop)))))))))
