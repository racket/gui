(define mred:preferences@
  (unit/s mred:preferences^
    (import [mred:debug mred:debug^]
	    [mred:exn mred:exn^])
    
    (define preferences-filename
      (case wx:platform
	[(unix) (build-path (expand-path "~") ".mred.prefs")]
	[(macintosh) "Mred Preferences"]
	[else "mred.pre"])) ;; windows 

    (define preferences (make-hash-table))

    (define get-preference 
      (lambda (p) 
	(hash-table-get preferences p
			(lambda () 
			  (raise (mred:exn:make-exn:unknown-preference
				  (format "unknown preference: ~a" p)))))))

    (define set-preference
      (lambda (p value) (hash-table-put! preferences p value)))

    (define defaults '((highlight-parens #t)))

    (define restore-defaults
      (lambda ()
	(for-each (lambda (x) (apply set-preference x)) 
		  defaults)))

    (define save-user-preferences (lambda () (mred:debug:printf "saving prefs~n")))
    (define read-user-preferences (lambda () (mred:debug:printf "reading prefs~n")))
    
    (restore-defaults)
    (read-user-preferences)))