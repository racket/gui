(require-library "launcher.ss" "launcher")
(require-library "function.ss")
(require-library "macro.ss")
(require-library "pretty.ss")

(unless (file-exists? (build-path (current-load-relative-directory) "receive-sexps-port.ss"))
  (call-with-output-file (build-path (current-load-relative-directory) "receive-sexps-port.ss")
    (lambda (port)
      (write 6012 port))))

(define-struct eof-result ())

(define-values (shutdown-listener shutdown-mred mred-running? send-sexp-to-mred)
  (let ([listener
	 (let loop ()
	   (let ([port (load-relative "receive-sexps-port.ss")])
	     (with-handlers ([(lambda (x) #t)
			      (lambda (x)
				(let ([next (+ port 1)])
				  (call-with-output-file (build-path (current-load-relative-directory)
								     "receive-sexps-port.ss")
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
       (lambda ()
	 (if (char-ready? in-port)
	     (not (eof-object? (peek-char in-port)))
	     #t))
       (lambda (sexp)
	 (unless (and in-port
		      out-port
		      (or (not (char-ready? in-port))
			  (not (eof-object? (peek-char in-port)))))
	   (restart-mred))
	 (printf "send-sexp-to-mred.sending:~n")
	 (pretty-print sexp)
	 (newline)
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
	   (printf "send-sexp-to-mred.received result~n")
	   (unless (or (eof-object? answer)
		       (and (list? answer)
			    (= 2 (length answer))))
	     (error 'send-sexp-to-mred "unpected result from mred: ~s~n" answer))
	   (if (eof-object? answer)
	       (raise (make-eof-result))
	       (case (car answer)
		 [(error)
		  (error 'send-sexp-to-mred (format "mred raised \"~a\" with input: ~s" (second answer) sexp))]
		 [(cant-read) (error 'mred/cant-parse (second answer))]
		 [(normal) (second answer)]))))))))

(define section-jump void)

(define test
  (opt-lambda (test-name failed? test [jump 'section])
    (let ([failed
	   (with-handlers ([(lambda (x) #t)
			    (lambda (x)
			      (if (exn? x)
				  (exn-message x)
				  x))])
	     (failed?
	      (if (procedure? test)
		  (test)
		  (eval (send-sexp-to-mred test)))))])
      (when failed
	(printf "FAILED ~a: ~a~n" test-name failed)
	(case jump
	  [(section) (section-jump)]
	  [(continue) (void)]
	  [else (jump)])))))

(define preferences-file (build-path (find-system-path 'pref-dir)
				     (case (system-type)
				       [(macos) "MrEd Preferences"]
				       [(windows) "mred.pre"]
				       [(unix) ".mred.prefs"])))
(define old-preferences-file (let-values ([(base name _2) (split-path preferences-file)])
			       (build-path base (string-append name ".save"))))
					 

(when (file-exists? preferences-file)
  (printf "saving preferences file ~s to ~s~n" preferences-file old-preferences-file)
  (when (file-exists? old-preferences-file)
    (error 'framework-test "backup preferences file exists, aborting"))
  (printf "saved preferences file~n")
  (copy-file preferences-file old-preferences-file))

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
(printf "restoring preferences file ~s to ~s~n" old-preferences-file preferences-file)
(when (file-exists? preferences-file)
  (unless (file-exists? old-preferences-file)
    (error 'framework-test "lost preferences file backup!"))
  (delete-file preferences-file)
  (copy-file old-preferences-file preferences-file)
  (delete-file old-preferences-file))
(printf "restored preferences file~n")

(shutdown-listener)
