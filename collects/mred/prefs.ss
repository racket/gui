;; need a preference for pconvert

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
				  (format "unknown preference: ~a" p)
				  ((debug-info-handler))))))))

    (define set-preference
      (lambda (p value) (hash-table-put! preferences p value)))

    (define defaults '((highlight-parens #t)
		       (autosaving-on? #t)
		       (autosave-delay 60)
		       (autoload-paths ("/usr/local/lib/plt/mred/autoload/"))))

    (define restore-defaults
      (lambda ()
	(for-each (lambda (x) (apply set-preference x)) 
		  defaults)))

    (define save-user-preferences 
      (lambda () 
	(call-with-output-file preferences-filename
	  (lambda (p) (write (hash-table-map preferences list) p))
	  'replace)))

    (define read-user-preferences 
      (lambda ()
	(when (file-exists? preferences-filename)
	  (for-each (lambda (x) (apply set-preference x))
		    (call-with-input-file preferences-filename read)))))

    (define preferences-dialog
      (lambda () 
	(restore-defaults)
	(save-user-preferences)
	(wx:message-box "Saved default preferences.")))
    
    (restore-defaults)
    (read-user-preferences)))