;; need a preference for pconvert

;; timing problem with prefernces and marshalling and unmarshalling
;; possibly stage it. So, first read in the marshalled preference,
;; then when I am asked for the preference for the first time, check
;; to see if it needs to be unmarshalled.

(define mred:preferences@
  (unit/sig mred:preferences^
    (import [mred:debug : mred:debug^]
	    [mred:exn : mred:exn^]
	    [mzlib:function : mzlib:function^])
    
    (mred:debug:printf 'invoke "mred:preferences@")
    
    (define preferences-filename
      (case wx:platform
	[(unix) (build-path (expand-path "~") ".mred.prefs")]
	[(macintosh) "Mred Preferences"]
	[else "mred.pre"])) ;; windows 
    
    (define preferences (make-hash-table))
    (define marshall-unmarshall (make-hash-table))
    
    (define-struct un/marshall (marshall unmarshall))
    
    (define-struct marshalled (data))
    
    (define get-preference-box
      (lambda (p) 
	(let ([ans (hash-table-get preferences p
				   (lambda () 
				     (raise (mred:exn:make-exn:unknown-preference
					     (format "unknown preference: ~a" p)
					     ((debug-info-handler))))))])
	  (if (marshalled? ans)
	      (let* ([marshalled (marshalled-data ans)]
		     [unmarshalled 
		      ((un/marshall-unmarshall
			(hash-table-get marshall-unmarshall p
					(lambda () mzlib:function:identity)))
		       marshalled)]
		     [boxed (box unmarshalled)])
		(hash-table-put! preferences p boxed)
		boxed)
	      ans))))
    
    (define get-preference (mzlib:function:compose unbox get-preference-box))
    
    (define set-preference
      (lambda (p value)
	(let/ec k
	  (set-box! (hash-table-get preferences p 
				    (lambda () 
				      (let ([box (box value)])
					(hash-table-put! preferences p box)
					(k box))))
		    value))))
    
    (define set-preference-default
      (lambda (p value)
	(hash-table-get preferences p 
			(lambda () 
			  (hash-table-put! preferences p (box value))))
	(set! defaults (cons (list p value) defaults))))
    
    (define set-preference-un/marshall
      (lambda (p marshall unmarshall)
	(hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))))
    
    (define defaults '((highlight-parens #t)
		       (autosaving-on? #t)
		       (autosave-delay 60)
		       (autoload-paths ("/usr/local/lib/plt/mred/autoload/"))))
    
    (define restore-defaults
      (lambda ()
	(for-each (lambda (x) (apply set-preference x)) 
		  defaults)))
    
    
    (define save-user-preferences 
      (let ([marshall-pref
	     (lambda (p boxed-value)
	       (let* ([value (unbox boxed-value)]
		      [marshalled
		       (let/ec k
			 ((un/marshall-marshall
			   (hash-table-get marshall-unmarshall p
					   (lambda ()
					     (k value))))
			  value))])
		 (list p marshalled)))])
	(lambda () 
	  (call-with-output-file preferences-filename
	    (lambda (p)
	      (write (hash-table-map preferences marshall-pref) p)
	      'replace)))))
    
    (define read-user-preferences 
      (let ([unmarshall-pref
	     (lambda (input)
	       (let ([p (mzlib:function:first input)]
		     [marshalled (mzlib:function:second input)])
		 (let/ec k
		   (let* ([not-in-table
			   (lambda ()
			     (k (hash-table-put! preferences p (make-marshalled marshalled))))]
			  [ht-pref (hash-table-get preferences p not-in-table)]
			  [unmarshall (hash-table-get marshall-unmarshall p (lambda () mzlib:function:identity))])
		     (if (box? ht-pref)
			 (set-box! ht-pref (unmarshall marshalled))
			 (set-marshalled-data! marshalled))))))])
	(lambda ()
	  (when (file-exists? preferences-filename)
	    (let ([input (call-with-input-file preferences-filename read)])
	      (when (list? input)
		(for-each unmarshall-pref input)))))))
    
    (define preferences-dialog
      (lambda ()
	(save-user-preferences)
	(wx:message-box "Saved preferences.")))
    
    (restore-defaults)
    (read-user-preferences)))