(require-library "launcher.ss" "launcher")
(require-library "function.ss")
(require-library "macro.ss")

;; old, hopefully unnecessary
'(case (system-type)
   [(macos) 

    (when running?
      (let ([tmp-file (build-path (find-system-path 'temp-dir)
				  "frameworkempty.ss")])
	(call-with-output-file tmp-file
	  (lambda (port)
	    (newline port))
	  'truncate)
	(send-event "MrEd" "aevt" "quit")
	(let loop ()
	  (sleep 1)
	  (with-handlers ([(lambda (x) #t) void])
	    (printf "looping~n")
	    (send-event "MrEd" "aevt" "odoc" (vector 'file tmp-file))
	    (loop)))))
    (printf "macos: mred no longer running~n")])

(unless (file-exists? "receive-sexps-port.ss")
  (call-with-output-file "receive-sexps-port.ss"
    (lambda (port)
      (write 6012 port))))

(define-values (shutdown-listener shutdown-mred send-sexp-to-mred)
  (let ([listener
	 (let loop ()
	   (let ([port (load-relative "receive-sexps-port.ss")])
	     (with-handlers ([(lambda (x) #t)
			      (lambda (x)
				(let ([next (+ port 1)])
				  (call-with-output-file "receive-sexps-port.ss"
				    (lambda (p)
				      (write next p))
				    'truncate)
				  (printf "tcp-listen failed for port ~a, attempting ~a~n"
					  port next)
				  (loop)))])
	       (tcp-listen port))))]
	[in-port #f]
	[out-port #f])
    (let ([restart-mred
	   (lambda ()
	     (shutdown-mred)
	     (let-values ([(base _1 _2) (split-path program)])
	       ((case (system-type)
		  [(macos) system*]
		  [else (lambda (x) (begin (process* x) (void)))])
		(mred-program-launcher-path "Framework Test Engine")))
	     (let-values ([(in out) (tcp-accept listener)])
	       (set! in-port in)
	       (set! out-port out))
	     (send-sexp-to-mred '(require-library "framework.ss" "framework")))])
      (values
       (lambda ()
	 (shutdown-mred)
	 (tcp-close listener))
       (lambda ()
	 (when (and in-port
		    out-port)
	   (close-output-port out-port)
	   (close-input-port in-port)
	   (set! in-port #f)
	   (set! in-port #f)))
       (lambda (sexp)
	 (unless (and in-port out-port)
	   (restart-mred))
	 (write sexp out-port)
	 (newline out-port)
	 (let ([answer
		(with-handlers ([(lambda (x) #t)
				 (lambda (x) (list 'cant-read
						   (string-append
						    (exn-message x)
						    "; rest of string: "
						    (apply
						     string
						     (let loop ()
						       (if (char-ready? in-port)
							   (cons (read-char in-port)
								 (loop))
							   null))))))])
		  (read in-port))])
	   (unless (and (list? answer)
			(= 2 (length answer)))
	     (error 'framework-test-suite "unpected result from mred: ~s~n" answer))
	   (case (car answer)
	     [(error) (error 'mred (second answer))]
	     [(cant-read) (error 'mred/cant-parse (second answer))]
	     [(normal) (second answer)])))))))

(define section-jump void)

(define test
  (opt-lambda (test-name failed? test [jump 'section])
    (let ([failed
	   (with-handlers ([(lambda (x) #t)
			    (lambda (x)
			      (if (exn? x)
				  (exn-message x)
				  x))])
	     (failed? (eval (send-sexp-to-mred test))))])
      (when failed
	(printf "FAILED ~a: ~a~n" test-name failed)
	(case jump
	  [(section) (section-jump)]
	  [(continue) (void)]
	  [else (jump)])))))

(let ([all-files (map symbol->string (load-relative "README"))])
  (for-each (lambda (x)
	      (when (member x all-files)
		(let ([oh (error-escape-handler)])
		  (let/ec k
		    (error-escape-handler (lambda ()
					    (error-escape-handler oh)
					    (k (void))))
		    (set! section-jump k)
		    (printf "beginning ~a test suite~n" x)
		    (load-relative x)
		    (error-escape-handler oh)))))
	    (if (equal? (vector) argv)
		all-files
		(vector->list argv))))

(shutdown-listener)
