(printf "creating thread~n")
(thread
 (letrec ([restart
	   (lambda ()
	     (printf "initializing loop~n")
	     (let*-values ([(listener) (tcp-listen (require-library "receive-sexps-port.ss" "tests" "framework"))]
			   [(in out) (tcp-accept listener)]
			   [(continue) (make-semaphore 0)]
			   [(error) #f]
			   [(answer) (void)])
	       (let loop ()
		 (let ([sexp (read in)])
		   (if (eof-object? sexp)
		       (begin
			 (close-input-port in)
			 (close-output-port out)
			 (tcp-close listener)
			 (restart))
		       (begin
			 (queue-callback (lambda ()
					   (set! error #f)
					   (with-handlers ([(lambda (x) #t)
							    (lambda (exn)
							      (set! error exn))])
					     (set! answer (eval sexp)))
					   (semaphore-post continue)))
			 (semaphore-wait continue)
			 (write
			  (if error
			      (list 'error (exn-message error))
			      (list 'normal answer))
			  out)
			 (loop)))))))])
   restart))
