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
		 (printf "waiting for sexp~n")
		 (let ([sexp (read in)])
		   (printf "read sexp: ~s~n" sexp)
		   (if (eof-object? sexp)
		       (begin
			 (printf "restarting loop~n")
			 (close-input-port in)
			 (close-output-port out)
			 (restart))
		       (begin
			 (printf "enqueing callback~n")
			 (queue-callback (lambda ()
					   (printf "callback~n")
					   (set! error #f)
					   (with-handlers ([(lambda (x) #t)
							    (lambda (exn)
							      (printf "abnormal termination: ~a~n" (exn-message exn))
							      (set! error exn))])
					     (set! answer (eval sexp))
					     (printf "normal termination~n"))
					   (printf "posting to continue~n")
					   (semaphore-post continue)))
			 (printf "enqueued; waiting~n")
			 (semaphore-wait continue)
			 (printf "passed continue~n")
			 (write
			  (if error
			      (list 'error (exn-message error))
			      (list 'normal answer))
			  out)
			 (printf "looping~n")
			 (loop)))))))])
   restart))
