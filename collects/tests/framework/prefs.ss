(local [(define pref-file (build-path (find-system-path 'pref-dir) ".mred.prefs"))
	(define old-prefs (if (file-exists? pref-file)
			      (call-with-input-file pref-file read)
			      null))
	(define (check-eq? m s) (lambda (t) (if (eq? s t) #f m)))
	(define pref-sym 'framework:test-suite)]

  (call-with-output-file pref-file
    (lambda (port) (write (filter (lambda (x) (not (eq? (car x) pref-sym)))
				  old-prefs)
			  port))
    'truncate)
  (shutdown-mred)
  (test
   'preference-unbound
   (check-eq? "couldn't remove preference binding" 'passed)
   `(with-handlers ([exn:unknown-preference?
		     (lambda (x)
		       'passed)])
      (preferences:get ',pref-sym)))
  (test 'preference-set-default/get
	(check-eq? "set-default followed by get didn't work" 'passed)
	`(begin (preferences:set-default ',pref-sym 'passed symbol?)
		(preferences:get ',pref-sym)))
  (test 'preference-set/get
	(check-eq? "set followed by get didn't work" 'new-pref)
	`(begin (preferences:set ',pref-sym 'new-pref)
		(preferences:get ',pref-sym)))
  (send-sexp-to-mred '(exit:exit))
  (shutdown-mred)
  (test 'preference-get-after-restart
	(check-eq? "get after restart didn't work" 'new-pref)
	`(preferences:get ',pref-sym)))
