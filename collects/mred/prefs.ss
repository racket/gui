;; need a preference for pconvert

(define mred:preferences@
  (unit/sig mred:preferences^
    (import [mred:debug : mred:debug^]
	    [mred:exn : mred:exn^]
	    [mred : mred:container^] ;; warning -- to use the mred:panel macros, 
	                             ;; need to have mred:container be prefixed with "mred"
	    [mred:edit : mred:edit^]
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
		      (let/ec k
			((un/marshall-unmarshall
			  (hash-table-get marshall-unmarshall p
					  (lambda () (k marshalled))))
			 marshalled))]
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
    
    (define defaults null)
    
    (define restore-defaults
      (lambda ()
	(for-each (lambda (x) (apply set-preference x)) 
		  defaults)))
    
    (define save-user-preferences 
      (let ([marshall-pref
	     (lambda (p boxed-value)
	       (if (marshalled? boxed-value)
		   (list p (marshalled-data boxed-value))
		   (let* ([value (unbox boxed-value)]
			  [marshalled
			   (let/ec k
			     ((un/marshall-marshall
			       (hash-table-get marshall-unmarshall p
					       (lambda ()
						 (k value))))
			      value))])
		     (list p marshalled))))])
	(lambda () 
	  (mred:debug:printf 'startup "saving user preferences")
	  (call-with-output-file preferences-filename
	    (lambda (p)
	      (write (hash-table-map preferences marshall-pref) p))
	      'replace)	  
	  (mred:debug:printf 'startup "saved user preferences"))))
    
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
			 (set-marshalled-data! ht-pref marshalled))))))])
	(lambda ()
	  (mred:debug:printf 'startup "reading user preferences")
	  (when (file-exists? preferences-filename)
	    (let ([input (call-with-input-file preferences-filename read)])
	      (when (list? input)
		(for-each unmarshall-pref input))))	  
	  (mred:debug:printf 'startup "read user preferences"))))
    
    (define-struct ppanel (title container))
    (define ppanels 
      (list 
       (make-ppanel "General"
		    (lambda (parent)
		      (let ([autosave-buffer (make-object mred:edit:edit%)])
			(mred:vertical-panel parent #t #t
			  (horizontal-panel #t #f
					    (check-box (lambda (_ command) 
							 (set-preference 'mred:highlight-parens (send command checked?)))
						       "Highlight Parens?")
					    (panel #t #t))
			  (horizontal-panel #t #f
					    (check-box (lambda (_ command) 
							 (set-preference 'mred:autosaving-on? (send command checked?)))
						       "Autosave?")
					    (panel #t #t))
			  (horizontal-panel #t #f
					    (message "Autosave timeout")
					    (let ([media (media-canvas)])
					      (send media set-media autosave-buffer)
					      media))))))
       (make-ppanel "Goodbye"
		    (lambda (parent)
		      (let* ([other-panel (make-object mred:vertical-panel% parent)]
			     [msg3 (make-object mred:message% other-panel "Goodbye")]
			     [msg4 (make-object mred:message% other-panel "For Now")])
			(send other-panel change-children (lambda (l) (list msg3 msg4)))
			other-panel)))))

    (define make-run-once
      (lambda ()
	(let ([semaphore (make-semaphore 1)])
	  (lambda (t)
	    (dynamic-wind (lambda () (semaphore-wait semaphore))
			  t
			  (lambda () (semaphore-post semaphore)))))))

    (define preferences-dialog #f)

    (define add-preference-panel
      (lambda (title container)
	(run-once
	 (lambda ()
	   (set! ppanels (cons (make-ppanel title container) ppanels))
	   (when preferences-dialog
	     (send preferences-dialog added-pane))))))

    (define make-preferences-dialog
      (lambda ()
	(save-user-preferences)
	(letrec* ([frame (make-object (class-asi mred:frame% (public [added-pane refresh-menu]))
				      '() "Preferences")]
		  [panel (make-object mred:vertical-panel% frame)]
		  [top-panel (make-object mred:horizontal-panel% panel)]
		  [single-panel (make-object mred:single-panel% panel)]
		  [panels (map (lambda (p) ((ppanel-container p) single-panel)) ppanels)]
		  [bottom-panel (make-object mred:horizontal-panel% panel)]
		  [popup-callback
		   (lambda (choice command-event)
		     (send single-panel active-child (list-ref panels (send command-event get-command-int))))]
		  [popup-menu (make-object mred:choice% top-panel popup-callback
					   "Category" -1 -1 -1 -1
					   (map ppanel-title ppanels))]
		  [refresh-menu
		   (lambda ()
		     (send single-panel change-children 
			   (lambda (l) 
			     (set! panels (map (lambda (p) ((ppanel-container p) single-panel)) ppanels))
			     panels))
		     (send popup-menu clear)
		     (send popup-menu clear)
		     (for-each (lambda (p) (send popup-menu append (ppanel-title p))) ppanels))]
		  [ok-callback (lambda args 
				 (save-user-preferences)
				 (hide-preferences-dialog))]
		  [ok-button (make-object mred:button% bottom-panel ok-callback "OK")]
		  [cancel-callback (lambda args
				     (read-user-preferences)
				     (hide-preferences-dialog))]
		  [cancel-button (make-object mred:button% bottom-panel cancel-callback "Cancel")])
	  (send single-panel change-children (lambda (l) panels))
	  (send top-panel change-children
		(lambda (l) 
		  (list (make-object mred:panel% top-panel)
			popup-menu
			(make-object mred:panel% top-panel))))
	  (send bottom-panel change-children
		(lambda (l)
		  (cons (make-object mred:panel% bottom-panel) l)))
	  (send popup-menu stretchable-in-x? #f)
	  (send bottom-panel stretchable-in-y? #f)
	  (send top-panel stretchable-in-y? #f)
	  (send single-panel change-children (lambda (l) panels))
	  (send popup-menu set-selection 0)
	  (send single-panel active-child (car panels))
	  (send frame show #t))))

    (define run-once (make-run-once))

    (define hide-preferences-dialog
      (lambda ()
	(run-once
	 (lambda ()
	   (when preferences-dialog
	     (send preferences-dialog show #f)
	     (set! preferences-dialog #f))))))

    (define show-preferences-dialog
      (lambda ()
	(wx:bell)
	'(run-once (lambda () 
		    (if preferences-dialog
			(send preferences-dialog show #t)
			(set! preferences-dialog (make-preferences-dialog)))))))

    (read-user-preferences)))