;; need a preference for pconvert

(define mred:preferences@
  (unit/sig mred:preferences^
    (import [mred:debug : mred:debug^]
	    [mred:exn : mred:exn^]
	    [mred : mred:container^] ;; warning -- to use the mred:panel macros, 
	                             ;; need to have mred:container be prefixed with "mred"
	    [mred:exit : mred:exit^]
	    [mred:gui-utils : mred:gui-utils^]
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
	  (cond
	    [(marshalled? ans)
	     (let* ([marshalled (marshalled-data ans)]
		    [unmarshalled
		     (let/ec k
		       ((un/marshall-unmarshall
			 (hash-table-get marshall-unmarshall p
					 (lambda () (k marshalled))))
			marshalled))]
		    [boxed (box unmarshalled)])
	       (hash-table-put! preferences p boxed)
	       boxed)]
	    [(box? ans) ans]
	    [else (error 'prefs.ss "robby error.1: ~a" ans)]))))
    
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
	     (lambda (p ht-value)
	       (cond
		 [(marshalled? ht-value) (list p (marshalled-data ht-value))]
		 [(box? ht-value)
		  (let* ([value (unbox ht-value)]
			 [marshalled
			  (let/ec k
			    ((un/marshall-marshall
			      (hash-table-get marshall-unmarshall p
					      (lambda ()
						(k value))))
			     value))])
		    (list p marshalled))]
		 [else (error 'prefs.ss "robby error.2: ~a" ht-value)]))])
	(lambda () 
	  (mred:debug:printf 'startup "saving user preferences")
	  (call-with-output-file preferences-filename
	    (lambda (p)
	      (write (hash-table-map preferences marshall-pref) p))
	      'replace)	  
	  (mred:debug:printf 'startup "saved user preferences"))))
    
    (mred:exit:insert-exit-callback save-user-preferences)

    (define read-user-preferences 
      (let ([parse-pref
	     (lambda (p marshalled)
	       (let/ec k
		 (let* ([ht-pref (hash-table-get preferences p (lambda () 'not-in-table))]
			[unmarshall-struct (hash-table-get marshall-unmarshall p (lambda () #f))])
		   (cond
		     [(box? ht-pref)
		      (if unmarshall-struct
			  (set-box! ht-pref ((un/marshall-unmarshall unmarshall-struct) marshalled))
			  (set-box! ht-pref marshalled))]
		     [(marshalled? ht-pref) (set-marshalled-data! ht-pref marshalled)]
		     [(eq? 'not-in-table ht-pref)
		      (if unmarshall-struct
			  (hash-table-put! preferences p (box ((un/marshall-unmarshall unmarshall-struct) marshalled)))
			  (hash-table-put! preferences p (make-marshalled marshalled)))]
		     [else (error 'prefs.ss "robby error.3: ~a" ht-pref)]))))])
	(lambda ()
	  (mred:debug:printf 'startup "reading user preferences")
	  (when (file-exists? preferences-filename)
	    (let ([input (call-with-input-file preferences-filename read)])
	      (when (list? input)
		(for-each (lambda (x) (apply parse-pref x)) input))))
	  (mred:debug:printf 'startup "read user preferences"))))
    
    (define-struct ppanel (title container))
    
    (define ppanels 
      (list 
       (make-ppanel "General"
		    (lambda (parent)
		      (mred:vertical-panel parent #t #t
			(list (horizontal-panel #t #f
						(list (let ([c (check-box (lambda (_ command) 
									    (set-preference 'mred:highlight-parens (send command checked?)))
									  "Highlight Parens?")])
							(send c set-value (get-preference 'mred:highlight-parens))
							c)
						      (panel #t #t)))
			      (horizontal-panel 
			       #t #f
			       (list (let ([c (check-box (lambda (_ command) 
							   (set-preference 'mred:autosaving-on? (send command checked?)))
							 "Autosave?")])
				       (send c set-value (get-preference 'mred:autosaving-on?))
				       c)
				     (panel #t #t)))
			      (horizontal-panel 
			       #t #f
			       (list (let ([c (check-box (lambda (_ command) 
							   (set-preference 'mred:delete-forward? (not (send command checked?))))
							 "Remap delete to backspace?")])
				       (send c set-value (not (get-preference 'mred:delete-forward?)))
				       c)
				     (panel #t #t)))))))))

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
	   (set! ppanels (append ppanels (list (make-ppanel title container))))
	   (when preferences-dialog
	     (send preferences-dialog added-pane))))))

    (define make-preferences-dialog
      (lambda ()
	(save-user-preferences)
	(letrec* ([frame (make-object (class-asi mred:frame% (public [added-pane refresh-menu]))
				      '() "Preferences")]
		  [panel (make-object mred:vertical-panel% frame)]
		  [top-panel (make-object mred:horizontal-panel% panel)]
		  [single-panel (make-object mred:single-panel% panel -1 -1 -1 -1 wx:const-border)]
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
	  (send frame show #t)
	  frame)))

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
	(mred:gui-utils:show-busy-cursor
	 (lambda ()
	   (run-once
	    (lambda () 
	      (if preferences-dialog
		  (send preferences-dialog show #t)
		  (set! preferences-dialog (make-preferences-dialog)))))))))

    (read-user-preferences)))