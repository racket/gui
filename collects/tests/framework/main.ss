(require-library "launchers.ss" "launcher")
(require-library "cores.ss")
(require-library "cmdlines.ss")
(require-library "macro.ss")

(unless (file-exists? (build-path (current-load-relative-directory) "receive-sexps-port.ss"))
  (call-with-output-file (build-path (current-load-relative-directory) "receive-sexps-port.ss")
    (lambda (port)
      (write 6012 port))))

(define-signature TestSuite^
  ((struct eof-result ())
   load-framework-automatically
   shutdown-listener shutdown-mred mred-running? send-sexp-to-mred
   test
   wait-for-frame
   wait-for))

(define-signature internal-TestSuite^
  ((open TestSuite^)
   test-name
   failed-tests))

(define-signature Engine^
  (only-these-tests
   section-name
   section-jump))

(define TestSuite
  (unit/sig internal-TestSuite^
    (import (program)
	    Engine^
	    launcher-maker^
	    mzlib:pretty-print^
	    mzlib:function^)

    (define test-name "<<setup>>")
    (define failed-tests null)

    (define-struct eof-result ())

    (define load-framework-automatically? #t)

    (define listener
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
			       (printf "  tcp-listen failed for port ~a, attempting ~a~n"
				       port next)
			       (loop)))])
	    (tcp-listen port)))))
    (define in-port #f)
    (define out-port #f)

    (define restart-mred
      (lambda ()
	(shutdown-mred)
	(let-values ([(base _1 _2) (split-path program)])
	  ((case (system-type)
	     [(macos) system*]
	     [else (lambda (x) (thread (lambda () (system* x))))])
	   (mred-program-launcher-path "Framework Test Engine")))
	(let-values ([(in out) (tcp-accept listener)])
	  (set! in-port in)
	  (set! out-port out))
	(when load-framework-automatically?
	  (send-sexp-to-mred
	   '(let ([s (make-semaphore 0)])
	      (queue-callback (lambda ()
				(require-library "framework.ss" "framework")
				(test:run-interval 11)
				(semaphore-post s)))
	      (semaphore-wait s))))))
    (define load-framework-automatically
      (case-lambda
       [(new-load-framework-automatically?)
	(unless (eq? (not (not new-load-framework-automatically?))
		     load-framework-automatically?)
	  (set! load-framework-automatically? (not (not new-load-framework-automatically?)))
	  (shutdown-mred))]
       [() load-framework-automatically?]))

    (define shutdown-listener
      (lambda ()
	(shutdown-mred)
	(tcp-close listener)))

    (define shutdown-mred
      (lambda ()
	(when (and in-port
		   out-port)
	  (close-output-port out-port)
	  (close-input-port in-port)
	  (set! in-port #f)
	  (set! in-port #f))))

    (define mred-running?
      (lambda ()
	(if (char-ready? in-port)
	    (not (eof-object? (peek-char in-port)))
	    #t)))

    (define send-sexp-to-mred
      (lambda (sexp)
	(unless (and in-port
		     out-port
		     (or (not (char-ready? in-port))
			 (not (eof-object? (peek-char in-port)))))
	  (restart-mred))
	(printf "  ~a // ~a: sending to mred:~n" section-name test-name)
	(parameterize ([pretty-print-print-line
			(let ([prompt "  "]
			      [old-liner (pretty-print-print-line)])
			  (lambda (ln port ol cols)
			    (let ([ov (old-liner ln port ol cols)])
			      (if ln 
				  (begin (display prompt port)
					 (+ (string-length prompt) ov))
				  ov))))])
	  (pretty-print sexp))
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
	  (unless (or (eof-object? answer)
		      (and (list? answer)
			   (= 2 (length answer))))
	    (error 'send-sexp-to-mred "unpected result from mred: ~s~n" answer))
	  (if (eof-object? answer)
	      (raise (make-eof-result))
	      (case (car answer)
		[(error)
		 (error 'send-sexp-to-mred (format "mred raised \"~a\"" (second answer)))]
		[(cant-read) (error 'mred/cant-parse (second answer))]
		[(normal) (eval (second answer))])))))


    (define test
      (case-lambda
       [(in-test-name passed? sexp/proc) (test in-test-name passed? sexp/proc 'section)]
       [(in-test-name passed? sexp/proc jump)
	(fluid-let ([test-name in-test-name])
	  (when (or (not only-these-tests)
		    (memq test-name only-these-tests))
	    (let ([failed
		   (with-handlers ([(lambda (x) #t)
				    (lambda (x)
				      (if (exn? x)
					  (exn-message x)
					  x))])
		     (let ([result
			    (if (procedure? sexp/proc)
				(sexp/proc)
				(begin0 (send-sexp-to-mred sexp/proc)
					(send-sexp-to-mred ''check-for-errors)))])

		       (not (passed? result))))])
	      (when failed
		(printf "FAILED ~a: ~a~n" failed test-name)
		(set! failed-tests (cons (cons section-name test-name) failed-tests))
		(case jump
		  [(section) (section-jump)]
		  [(continue) (void)]
		  [else (jump)])))))]))

    (define (wait-for sexp)
      (let ([timeout 10]
	    [pause-time 1/2])
	(send-sexp-to-mred
	 `(let loop ([n ,(/ timeout pause-time)])
	    (if (zero? n)
		(error 'wait-for
		       ,(format "after ~a seconds, ~s didn't come true" timeout sexp))
		(unless ,sexp
		  (sleep ,pause-time)
		  (loop (- n 1))))))))

    (define (wait-for-frame name)
      (wait-for `(let ([win (get-top-level-focus-window)])
		   (and win (string=? (send win get-label) ,name)))))))

(define Engine
  (unit/sig Engine^
    (import (argv)
	    internal-TestSuite^
	    mzlib:command-line^
	    mzlib:function^
	    mzlib:file^
	    mzlib:string^
	    mzlib:pretty-print^)

    (define section-jump void)
    (define section-name "<<setup>>")
    (define only-these-tests #f)

    (define preferences-file (build-path (find-system-path 'pref-dir)
					 (case (system-type)
					   [(macos) "MrEd Preferences"]
					   [(windows) "mred.pre"]
					   [(unix) ".mred.prefs"])))
    (define old-preferences-file (let-values ([(base name _2) (split-path preferences-file)])
				   (build-path base (string-append name ".save"))))
    

    (with-handlers ([(lambda (x) #f)
		     (lambda (x) (display (exn-message x)) (newline))])
      (let ([all-files (map symbol->string (load-relative "README"))]
	    [files-to-process null]
	    [command-line-flags
	     `((multi
		[("-o" "--only")
		 ,(lambda (flag _only-these-tests)
		    (set! only-these-tests (cons (string->symbol _only-these-tests)
						 (or only-these-tests null))))
		 ("Only run test named <test-name>" "test-name")]))])
	(parse-command-line "framework-test" argv command-line-flags
			    (lambda (collected . files)
			      (set! files-to-process (if (null? files) all-files files)))
			    `("Names of the tests; defaults to all tests"))

	(when (file-exists? preferences-file)
	  (printf "  saving preferences file ~s to ~s~n" preferences-file old-preferences-file)
	  (when (file-exists? old-preferences-file)
	    (error 'framework-test "backup preferences file exists, aborting"))
	  (printf "  saved preferences file~n")
	  (copy-file preferences-file old-preferences-file))
    
	(for-each (lambda (x)
		    (when (member x all-files)
		      (let/ec k
			(fluid-let ([section-name x]
				    [section-jump k])
			  (with-handlers ([(lambda (x) #t)
					   (lambda (exn)
					     (printf "~a~n" (if (exn? exn) (exn-message exn) exn)))])
			    (printf "beginning ~a test suite~n" x)
			    (invoke-unit/sig
			     (eval
			      `(unit/sig ()
				 (import TestSuite^
					 mzlib:function^
					 mzlib:file^
					 mzlib:string^
					 mzlib:pretty-print^)
				 (include ,x)))
			     TestSuite^
			     mzlib:function^
			     mzlib:file^
			     mzlib:string^
			     mzlib:pretty-print^)
			    (printf "PASSED ~a test suite~n" x))))))
		  files-to-process)))

    (printf "  restoring preferences file ~s to ~s~n" old-preferences-file preferences-file)
    (when (file-exists? preferences-file)
      (unless (file-exists? old-preferences-file)
	(error 'framework-test "lost preferences file backup!"))
      (delete-file preferences-file)
      (copy-file old-preferences-file preferences-file)
      (delete-file old-preferences-file))
    (printf "  restored preferences file~n")

    (shutdown-listener)

    (unless (null? failed-tests)
      (printf "FAILED tests:~n")
      (for-each (lambda (failed-test)
		  (printf "  ~a // ~a~n" (car failed-test) (cdr failed-test)))
		failed-tests))))

(invoke-unit/sig
 (compound-unit/sig
   (import (P : (program))
	   (A : (argv)))
   (link
    [L : launcher-maker^ ((require-library "launcherr.ss" "launcher"))]
    [C : mzlib:core^ ((require-library "corer.ss"))]
    [M : mzlib:command-line^ ((require-library "cmdliner.ss"))]
    [T : internal-TestSuite^ (TestSuite P E L (C pretty-print) (C function))]
    [E : Engine^ (Engine A T M (C function) (C file) (C string) (C pretty-print))])
   (export))
 (program)
 (argv))
