(let* ([errs null]
       [sema (make-semaphore 1)]
       [protect
	(lambda (f)
	  (semaphore-wait sema)
	  (begin0 (f)
		  (semaphore-post sema)))])
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
		    (let ([these-errs (protect (lambda () (begin0 errs (set! errs null))))])
		      (if (null? these-errs)
			  (with-handlers ([(lambda (x) #t)
					   (lambda (exn)
					     (list 'error (if (exn? exn)
							      (exn-message exn)
							      (format "~s" exn))))])
			    (list 'normal (print-convert (eval sexp))))
			  (list 'error
				(apply string-append
				       (map (lambda (x)
					      (string-append
					       (if (exn? x) (exn-message x) (format "~s" x))
					       (string #\newline)))
					    these-errs)))))
		    
		    out)
		   (loop)))))))))

  (let ([od (event-dispatch-handler)]
	[port (current-output-port)])
    (event-dispatch-handler
     (lambda (evt)
       (parameterize ([current-exception-handler
		       (let ([oe (current-exception-handler)])
			 (lambda (exn)
			   (protect
			    (lambda ()
			      (set! errs (cons exn errs))))
			   (oe exn)))])
	 (od evt))))))
