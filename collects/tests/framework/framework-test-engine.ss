(module framework-test-engine mzscheme
  (require (lib "pconvert.ss")
	   (lib "mred.ss" "mred"))

  (define errs null)
  (define sema (make-semaphore 1))
  (define (protect f)
    (semaphore-wait sema)
    (begin0 (f)
	    (semaphore-post sema)))

#|
  (define (exception->string x)
    (if (exn? x)
	(if (defined? 'print-error-trace)
	    (let ([p (open-output-string)])
	      (print-error-trace p x)
	      (string-append (exn-message x) (string #\newline) (get-output-string p)))
	    (exn-message x))
	(format "~s" x)))
|#

  (define (exception->string x)
    (if (exn? x)
	(exn-message x)
	(format "~s" x)))

  (thread
   (lambda ()
     (let*-values ([(in out) (tcp-connect "localhost"
					  (load
					   (build-path
					    (collection-path "tests" "framework")
					    "receive-sexps-port.ss")))]
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
					 (lambda (x) (list 'error (exception->string x)))])
			  (list 'normal (print-convert (eval sexp))))
			(list 'error
			      (apply string-append
				     (map (lambda (x) (string-append (exception->string x) (string #\newline)))
					  these-errs)))))
		  out)
		 (loop))))))))
  
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
