(module main mzscheme 
  (require (lib "launcher.ss" "launcher")
	   (lib "cmdline.ss")
	   (lib "unitsig.ss")
	   "test-suite-utils.ss"
	   (lib "guis.ss" "tests" "utils"))

  (provide
   only-these-tests
   section-name
   section-jump)

  (define initial-port 6012)

  (define section-jump void)
  (define section-name "<<setup>>")
  (define only-these-tests #f)

  (unless (file-exists? (build-path (current-load-relative-directory) "receive-sexps-port.ss"))
    (call-with-output-file (build-path (current-load-relative-directory) "receive-sexps-port.ss")
      (lambda (port)
	(write initial-port port))))

  (define preferences-file (build-path (find-system-path 'pref-dir)
				       (case (system-type)
					 [(macos) "MrEd Preferences"]
					 [(windows) "mred.pre"]
					 [(unix) ".mred.prefs"])))
  (define old-preferences-file (let-values ([(base name _2) (split-path preferences-file)])
				 (build-path base (string-append name ".save"))))
  

  (with-handlers ([(lambda (x) #f)
		   (lambda (x) (display (exn-message x)) (newline))])
    (let* ([all-files (map symbol->string (load-relative "README"))]
	   [all? #f]
	   [files-to-process null]
	   [command-line-flags
	    `((once-each
	       [("-a" "--all")
		,(lambda (flag)
		   (set! all? #t))
		("Run all of the tests")])
	      (multi
	       [("-o" "--only")
		,(lambda (flag _only-these-tests)
		   (set! only-these-tests (cons (string->symbol _only-these-tests)
						(or only-these-tests null))))
		("Only run test named <test-name>" "test-name")]))])
      
      (let* ([saved-command-line-file (build-path (collection-path "tests" "framework") "saved-command-line.ss")]
	     [parsed-argv (if (equal? argv (vector))
			      (if (file-exists? saved-command-line-file)
				  (begin
				    (let ([result (call-with-input-file saved-command-line-file read)])
				      (printf "reusing command-line arguments: ~s~n" result)
				      result))
				  (vector))
			      argv)])
	(parse-command-line "framework-test" parsed-argv command-line-flags
			    (lambda (collected . files)
			      (set! files-to-process (if (or all? (null? files)) all-files files)))
			    `("Names of the tests; defaults to all tests"))
	(call-with-output-file saved-command-line-file
	  (lambda (port)
	    (write parsed-argv port))
	  'truncate))

      
      (when (file-exists? preferences-file)
	(printf "  saving preferences file ~s to ~s~n" preferences-file old-preferences-file)
	(if (file-exists? old-preferences-file)
	    (printf "  backup preferences file exists, using that one~n")
	    (begin (copy-file preferences-file old-preferences-file)
		   (printf "  saved preferences file~n"))))
      
      (for-each (lambda (x)
		  (when (member x all-files)
		    (shutdown-mred)
		    (let/ec k
		      (fluid-let ([section-name x]
				  [section-jump k])
			(with-handlers ([(lambda (x) #t)
					 (lambda (exn)
					   (printf "~a~n" (if (exn? exn) (exn-message exn) exn)))])
			  (printf "beginning ~a test suite~n" x)

			  (eval `(require ,x))
			  
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
	      failed-tests)))