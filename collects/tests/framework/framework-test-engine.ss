(printf "mred:creating thread~n")
(thread
 (letrec ([restart
	   (lambda ()
	     (printf "mred:initializing loop~n")
	     (let*-values ([(in out) (tcp-connect "localhost" (load-relative "receive-sexps-port.ss"))]
			   [(continue) (make-semaphore 0)]
			   [(error) #f]
			   [(answer) (void)])
	       (printf "mred:made connection~n")
	       (let loop ()
		 (let ([sexp (read in)])
		   (if (eof-object? sexp)
		       (begin
			 (close-input-port in)
			 (close-output-port out)
			 (exit))
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
