;; dynamic adding of panels! (it's halfway in there now)
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
	[(macintosh) "MrEd Preferences"]
	[else "mred.pre"])) ;; windows 
    
    (define preferences (make-hash-table))
    (define marshall-unmarshall (make-hash-table))
    
    (define-struct un/marshall (marshall unmarshall))
    
    (define-struct marshalled (data))
    (define-struct pref (value callbacks))

    (define unmarshall
      (lambda (p marshalled)
	(let/ec k
	  (let* ([data (marshalled-data marshalled)]
		 [unmarshall-fn (un/marshall-unmarshall (hash-table-get marshall-unmarshall
						        p
							(lambda () (k data))))])
	    (unmarshall-fn data)))))

    (define add-preference-callback
      (lambda (p callback)
	(let ([ans (hash-table-get preferences p (lambda () #f))])
	  (cond
	    [(marshalled? ans) (let* ([value (unmarshall p ans)]
				      [pref (make-pref value (list callback))])
				 (hash-table-put! preferences p pref)
				 (callback p value)
				 (lambda ()
				   (set-pref-callbacks! pref (mzlib:function:remove callback (pref-callbacks pref) eq?))))]
	    [(pref? ans)
	     (set-pref-callbacks! ans (cons callback (pref-callbacks ans)))
	     (lambda ()
	       (set-pref-callbacks! ans (mzlib:function:remove callback (pref-callbacks ans) eq?)))]
	    [(not ans) (raise (mred:exn:make-exn:unknown-preference
			       (format "adding callback to unknown preference: ~a" p)
			       ((debug-info-handler))))]
	    [else (error 'prefs.ss "robby error.4: ~a ~a" p ans)]))))

    (define get-preference
      (lambda (p) 
	(let ([ans (hash-table-get preferences p
				   (lambda () 
				     (raise (mred:exn:make-exn:unknown-preference
					     (format "attempted to get unknown preference: ~a" p)
					     ((debug-info-handler))))))])
	  (cond
	    [(marshalled? ans)
	     (let* ([unmarshalled (unmarshall p ans)]
		    [pref (make-pref unmarshalled null)])
	       (hash-table-put! preferences p pref)
	       (mred:debug:printf 'prefs "get-preference.1 returning ~a as ~a"
				  p unmarshalled)
	       unmarshalled)]
	    [(pref? ans)
	     (let ([ans (pref-value ans)])
	       (mred:debug:printf 'prefs "get-preference.2 returning ~a as ~a"
				  p ans)
	       ans)]
	    [else (error 'prefs.ss "robby error.1: ~a" ans)]))))
    
    (define set-preference
      (lambda (p value)
	(let ([pref (hash-table-get preferences p (lambda () #f))])
	    (cond 
	      [(pref? pref)
	       (set-pref-value! pref value)
	       (for-each (lambda (x) (x p value)) (pref-callbacks pref))]
	      [(or (marshalled? pref) 
		   (not pref))
	       (hash-table-put! preferences p (make-pref value null))]
	      [else
	       (error 'prefs.ss "robby error.0: ~a" pref)]))))

    (define set-preference-default
      (lambda (p value)
	(hash-table-get preferences p 
			(lambda () 
			  (hash-table-put! preferences p (make-pref value null))))
	(set! defaults (cons (list p value) defaults))))
    
    ;; this is here becuase exit has to come before
    ;; prefs.ss in the loading order.
    (set-preference-default 'mred:verify-exit #t)

    (define set-preference-un/marshall
      (lambda (p marshall unmarshall)
	(hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))))
    
    (define defaults null)
    
    (define restore-defaults
      (lambda ()
	(for-each (lambda (x) (apply set-preference x)) defaults)))
    
    (define save-user-preferences 
      (let ([marshall-pref
	     (lambda (p ht-value)
	       (cond
		 [(marshalled? ht-value) (list p (marshalled-data ht-value))]
		 [(pref? ht-value)
		  (let* ([value (pref-value ht-value)]
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
	  (mred:debug:printf 'prefs "saving user preferences")
	  (call-with-output-file preferences-filename
	    (lambda (p)
	      (write (hash-table-map preferences marshall-pref) p))
	      'replace)	  
	  (mred:debug:printf 'prefs "saved user preferences"))))
    
    (mred:exit:insert-exit-callback save-user-preferences)

    (define read-user-preferences 
      (let ([parse-pref
	     (lambda (p marshalled)
	       (let/ec k
		 (let* ([ht-pref (hash-table-get preferences p (lambda () #f))]
			[unmarshall-struct (hash-table-get marshall-unmarshall p (lambda () #f))])
		   (cond
		     [(and (pref? ht-pref) unmarshall-struct)
		      (set-preference p ((un/marshall-unmarshall unmarshall-struct) marshalled))]

		     ;; in this case, assume that no marshalling/unmarshalling 
		     ;; is going to take place with the pref, since an unmarshalled 
		     ;; pref was already there.
		     [(pref? ht-pref)
		      (set-preference p marshalled)]

		     [(marshalled? ht-pref) (set-marshalled-data! ht-pref marshalled)]
		     [(and (not ht-pref) unmarshall-struct)
		      (set-preference p ((un/marshall-unmarshall unmarshall-struct) marshalled))]
		     [(not ht-pref)
		      (hash-table-put! preferences p (make-marshalled marshalled))]
		     [else (error 'prefs.ss "robby error.3: ~a" ht-pref)]))))])
	(lambda ()
	  (mred:debug:printf 'prefs "reading user preferences")
	  (when (file-exists? preferences-filename)
	    (let ([input (call-with-input-file preferences-filename read)])
	      (when (list? input)
		(for-each (lambda (x) (apply parse-pref x)) input))))
	  (mred:debug:printf 'prefs "read user preferences"))))
    
    (define-struct ppanel (title container))
    
    (define ppanels 
      (list 
       (make-ppanel
	"General"
	(lambda (parent)
	  (let* ([main (make-object mred:vertical-panel% parent)]
		 [make-check
		  (lambda (callback title initial-value)
		    (let*  ([h (make-object mred:horizontal-panel% main)]
			    [c (make-object mred:check-box% h callback title)]
			    [p (make-object mred:horizontal-panel% h)])
		      (send* h (spacing 1) (border 1))
		      (send* p (spacing 1) (border 1))
		      (send c set-value initial-value)))])
	    (send main spacing 1)
	    (make-check (lambda (_ command) 
			  (set-preference 'mred:highlight-parens (send command checked?)))
			"Highlight between matching parens?" (get-preference 'mred:highlight-parens))
	    (make-check (lambda (_ command) 
			  (set-preference 'mred:autosaving-on? (send command checked?)))
			"Auto-save files?" (get-preference 'mred:autosaving-on?))
	    (make-check (lambda (_ command) 
			  (set-preference 'mred:delete-forward? (not (send command checked?))))
			"Map delete to backspace?" (not (get-preference 'mred:delete-forward?)))
	    (make-check (lambda (_ command) 
			  (set-preference 'mred:file-dialogs (if (send command checked?)
								 'std
								 'common)))
			"Use platform-specific file dialogs?" (eq? (get-preference 'mred:file-dialogs) 'std))
	    ;; sleep is not effecient, so we wait for the next release to turn this on.
	    '(make-check (lambda (_ command) 
			  (set-preference 'mred:status-line (send command checked?)))
			"Show clock?" (get-preference 'mred:status-line))
	    (make-check (lambda (_ command) 
			  (set-preference 'mred:verify-exit (send command checked?)))
			"Verify exit?" (get-preference 'mred:verify-exit))
	    main)))))

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
	   (let ([new-ppanel (make-ppanel title container)])
	     (set! ppanels (append ppanels (list new-ppanel)))
	     (when preferences-dialog
	       (send preferences-dialog added-pane new-ppanel)))))))

    (define make-preferences-dialog
      (lambda ()
	(letrec* ([frame (make-object (class-asi mred:frame%
						 (public [added-pane
							  (lambda (ppanel)
							    (refresh-menu ppanel))]))
				      '() "Preferences")]
		  [panel (make-object mred:vertical-panel% frame)]
		  [top-panel (make-object mred:horizontal-panel% panel)]
		  [single-panel (make-object mred:single-panel% panel -1 -1 -1 -1 wx:const-border)]
		  [panels (map (lambda (p) ((ppanel-container p) single-panel)) ppanels)]
		  [bottom-panel (make-object mred:horizontal-panel% panel)]
		  [popup-callback
		   (lambda (choice command-event)
		     (send single-panel active-child (list-ref panels (send command-event get-command-int))))]
		  [make-popup-menu 
		   (lambda ()
		     (let ([menu (make-object mred:choice% top-panel popup-callback
					      "Category" -1 -1 -1 -1
					      (map ppanel-title ppanels))])
		       (send menu stretchable-in-x #f)
		       menu))]
		  [top-left (make-object mred:vertical-panel% top-panel)]
		  [popup-menu (make-popup-menu)]
		  [top-right (make-object mred:vertical-panel% top-panel)]
		  [refresh-menu
		   (lambda (ppanel)
		     (let ([new-panel ((ppanel-container ppanel) single-panel)])
		       (set! panels (append panels (list new-panel)))
		       (let ([new-popup (make-popup-menu)])
			 (send new-popup set-selection (send popup-menu get-selection))
			 (send top-panel change-children
			       (lambda (l) (list top-left new-popup top-right))))))]
		  [ok-callback (lambda args
				 (save-user-preferences)
				 (hide-preferences-dialog))]
		  [_1 (make-object mred:panel% bottom-panel)]
		  [ok-button (make-object mred:button% bottom-panel ok-callback "OK")]
		  [cancel-callback (lambda args
				     (read-user-preferences)
				     (hide-preferences-dialog))]
		  [cancel-button (make-object mred:button% bottom-panel cancel-callback "Cancel")])
	  (send ok-button user-min-width (send cancel-button get-width))
	  (send bottom-panel stretchable-in-y #f)
	  (send top-panel stretchable-in-y #f)
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
	     (send preferences-dialog show #f))))))


    (define show-preferences-dialog
      (lambda ()
	(mred:gui-utils:show-busy-cursor
	 (lambda ()
	   (run-once
	    (lambda () 
	      (save-user-preferences)
	      (if preferences-dialog
		  (send preferences-dialog show #t)
		  (set! preferences-dialog (make-preferences-dialog)))))))))

    (read-user-preferences)))
