;; need a preference for pconvert

  (unit/sig mred:preferences^
    (import mred:wx^
	    [mred:constants : mred:constants^]
	    [mred:exn : mred:exn^]
	    [mred : mred:container^] ;; warning -- to use the mred:panel macros, 
	                             ;; need to have mred:container be prefixed with "mred"
	    [mred:exit : mred:exit^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:edit : mred:edit^]
	    [mzlib:pretty-print : mzlib:pretty-print^]
	    [mzlib:function : mzlib:function^])
    
    (mred:debug:printf 'invoke "mred:preferences@")
    
    (define preferences-filename (wx:find-path 'pref-file))
    
    (define preferences (make-hash-table))
    (define marshall-unmarshall (make-hash-table))
    (define callbacks (make-hash-table))    
    (define defaults (make-hash-table))

    (define-struct un/marshall (marshall unmarshall))
    (define-struct marshalled (data))
    (define-struct pref (value))
    (define-struct default (value checker))

    (define guard
      (lambda (when p value thunk)
	(let ([h
                (lambda (x)
                  (let ([msg
                          (format "exception raised ~a for ~a with ~a: ~a~n"
                            when p value
                            (exn-message x))])
                    (raise (mred:exn:make-exn:during-preferences
                             msg
                             ((debug-info-handler))))))])

          (with-handlers ([(lambda (x) #t) h])
             (thunk)))))

    (define unmarshall
      (lambda (p marshalled)
	(let/ec k
	  (let* ([data (marshalled-data marshalled)]
		 [unmarshall-fn (un/marshall-unmarshall (hash-table-get marshall-unmarshall
						        p
							(lambda () (k data))))])
	    (guard "unmarshalling" p marshalled
		   (lambda () (unmarshall-fn data)))))))

    (define get-callbacks
      (lambda (p)
	(hash-table-get callbacks
			p
			(lambda () null))))

    (define add-preference-callback
      (lambda (p callback)
	(hash-table-put! callbacks p (append (get-callbacks p) (list callback)))
	(lambda ()
	  (hash-table-put!
	   callbacks p
	   (mzlib:function:remove callback
				  (get-callbacks p)
				  eq?)))))

    (define check-callbacks
      (lambda (p value)
	(andmap (lambda (x)
		  (guard "calling callback" p value
			 (lambda () (x p value))))
		(get-callbacks p))))

    (define get-preference
      (lambda (p) 
	(let ([ans (hash-table-get preferences p
				   (lambda () 
				     (raise (mred:exn:make-exn:unknown-preference
					     (format "attempted to get unknown preference: ~a" p)
					     ((debug-info-handler))))))])
	  (cond
	    [(marshalled? ans)
	     (let* ([default-s
		      (hash-table-get
		       defaults p
		       (lambda ()
			 (error 'get-preference
				"no default pref for: ~a~n"
				p)))]
		    [default (default-value default-s)]
		    [checker (default-checker default-s)]
		    [unmarshalled (let ([unmarsh (unmarshall p ans)])
				    (if (checker unmarsh)
					unmarsh
					(begin
					  (printf "WARNING: ~s rejecting invalid pref ~s in favor of ~s~n"
						  p unmarsh default)
					  default)))]
		    [_ (mred:debug:printf 'prefs "get-preference checking callbacks: ~a to ~a"
					  p unmarshalled)]
		    [pref (if (check-callbacks p unmarshalled)
			      unmarshalled
			      default)])
	       (hash-table-put! preferences p (make-pref pref))
	       (mred:debug:printf 'prefs "get-preference.1 returning ~a as ~a"
				  p pref)
	       pref)]
	    [(pref? ans)
	     (let ([ans (pref-value ans)])
	       (mred:debug:printf 'prefs "get-preference.2 returning ~a as ~a"
				  p ans)
	       ans)]
	    [else (error 'prefs.ss "robby error.1: ~a" ans)]))))
    
    (define set-preference
      (lambda (p value)
	(let* ([pref (hash-table-get preferences p (lambda () #f))])
	    (cond 
	      [(pref? pref)
	       (mred:debug:printf 'prefs "set-preference.1 checking callbacks: ~a to ~a" p value)
	       (when (check-callbacks p value)
		 (mred:debug:printf 'prefs "set-preference.1 setting ~a to ~a"
				    p value)
		 (set-pref-value! pref value))]
	      [(or (marshalled? pref) 
		   (not pref))
	       (mred:debug:printf 'prefs "set-preference.2 checking callbacks: ~a to ~a" p value)
	       (when (check-callbacks p value)
		 (mred:debug:printf 'prefs "set-preference.2 setting ~a to ~a"
				    p value)
		 (hash-table-put! preferences p (make-pref value)))]
	      [else
	       (error 'prefs.ss "robby error.0: ~a" pref)]))))

    (define set-preference-un/marshall
      (lambda (p marshall unmarshall)
	(hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))))
    
    (define restore-defaults
      (lambda ()
	(mred:debug:printf 'prefs "setting prefs to default values")
	(hash-table-for-each
	 defaults
	 (lambda (p v) (set-preference p v)))
	(mred:debug:printf 'prefs "finished setting prefs to default values")))
    
    (define set-preference-default
      (lambda (p value checker)
	(mred:debug:printf 'prefs "setting default value for ~a to ~a" p value)
	(hash-table-get preferences p 
			(lambda () 
			  (hash-table-put! preferences p (make-pref value))))
	(hash-table-put! defaults p (make-default value checker))))
    
    ;; this is here becuase exit has to come before
    ;; prefs.ss in the loading order.
    (set-preference-default 'mred:verify-exit #t
			    (lambda (x)
			      (or (not x)
				  (eq? x #t))))

    (define save-user-preferences 
      (let ([marshall-pref
	     (lambda (p ht-value)
	       (cond
		 [(marshalled? ht-value) (list p (marshalled-data ht-value))]
		 [(pref? ht-value)
		  (let* ([value (pref-value ht-value)]
			 [marshalled
			  (let/ec k
			    (guard "marshalling" p value
				   (lambda ()
				     ((un/marshall-marshall
				       (hash-table-get marshall-unmarshall p
						       (lambda ()
							 (k value))))
				      value))))])
		    (list p marshalled))]
		 [else (error 'prefs.ss "robby error.2: ~a" ht-value)]))])
	(lambda () 
	  (mred:debug:printf 'prefs "saving user preferences")
	  (with-handlers ([(lambda (x) #t)
			   (lambda (exn)
			     (mred:gui-utils:message-box
			      (format "Error saving preferences~n~a"
				      (exn-message exn))
			      "Error saving preferences"))])
	    (call-with-output-file preferences-filename
	      (lambda (p)
		(mzlib:pretty-print:pretty-print
		 (hash-table-map preferences marshall-pref) p))
	      'truncate 'text))	  
	  (mred:debug:printf 'prefs "saved user preferences"))))
    
    (mred:exit:insert-exit-callback 
     (lambda ()
       (with-handlers ([(lambda (x) #t)
			(lambda (exn)
			  (mred:gui-utils:message-box
			   (format "Error saving preferences: ~a"
				   (exn-message exn))
			   "Saving Prefs"))])
	 (save-user-preferences))))

    (define read-user-preferences 
      (let ([parse-pref
	     (lambda (p marshalled)
	       (let/ec k
		 (let* ([ht-pref (hash-table-get preferences p (lambda () #f))]
			[unmarshall-struct (hash-table-get marshall-unmarshall p (lambda () #f))])
		   (mred:debug:printf 'prefs "read-user-preferences; p: ~a ht-pref: ~a; marshalled: ~a"
				      p ht-pref marshalled)
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
	  (let/ec k
	    (when (file-exists? preferences-filename)
	      (let ([err
		     (lambda (input)
		       (wx:message-box (format "found bad pref: ~n~a" input)
				       "Preferences"))])
		(let loop ([input (with-handlers
				      ([(lambda (exn) #t)
					(lambda (exn)
					  (wx:message-box
					   (format "Error reading preferences~n~a"
						   (exn-message exn))
					   "Error reading preferences")
					  (k #f))])
				    (call-with-input-file preferences-filename
				      read
				      'text))])
		  (cond
		   [(pair? input)
		    (let/ec k
		      (let ([first (car input)])
			(when (pair? first)
			  (let ([arg1 (car first)]
				[t1 (cdr first)])
			    (when (pair? t1)
			      (let ([arg2 (car t1)]
				    [t2 (cdr t1)])
				(when (null? t2)
				  (parse-pref arg1 arg2)
				  (k #t)))))))
		      (err input))
		    (loop (cdr input))]
		   [(null? input) (void)]
		   [else (err input)])))))
	  (mred:debug:printf 'prefs "read user preferences"))))
    
    (define-struct ppanel (title container panel))
    
    (define ppanels 
      (list 
       (make-ppanel
	"General"
	(lambda (parent)
	  (let* ([main (make-object mred:vertical-panel% parent)]
		 [make-check
		  (lambda (pref title bool->pref pref->bool)
		    (let*  ([callback
			     (lambda (_ command)
			       (set-preference pref (bool->pref (send command checked?))))]
			    [pref-value (get-preference pref)]
			    [initial-value (pref->bool pref-value)]
			    [h (make-object mred:horizontal-panel% main)]
			    [c (make-object mred:check-box% h callback title)]
			    [p (make-object mred:horizontal-panel% h)])
		      (send c set-value initial-value)
		      (add-preference-callback pref
					       (lambda (p v)
						 (send c set-value (pref->bool v))))))]
		 [id (lambda (x) x)])
	    (make-check 'mred:highlight-parens "Highlight between matching parens?" id id)
	    (make-check 'mred:fixup-parens "Correct parens?" id id)
	    (make-check 'mred:paren-match "Flash paren match?" id id)
	    (make-check 'mred:autosaving-on? "Auto-save files?" id id)
	    (make-check 'mred:delete-forward? "Map delete to backspace?" not not)
	    (make-check 'mred:file-dialogs "Use platform-specific file dialogs?"
			(lambda (x) (if x 'std 'common))
			(lambda (x) (eq? x 'std)))

	    (make-check 'mred:verify-exit "Verify exit?" id id)
	    (make-check 'mred:verify-change-format "Ask before changing save format?" id id)
	    (make-check 'mred:auto-set-wrap? "Wordwrap editor buffers?" id id)

	    (make-check 'mred:show-status-line "Show status-line?" id id)
	    (make-check 'mred:line-offsets "Count line and column numbers from one?" id id)
	    (make-check 'mred:menu-bindings "Enable keybindings in menus?" id id)
	    main))
	#f)
       (make-ppanel
	"Default Fonts"
	(lambda (parent)
	  (let* ([main (make-object mred:vertical-panel% parent)]
		 [families (list  "Default" "Roman" "Decorative"
				  "Modern" "Swiss" "Script")]
		 [font-size-entry "defaultFontSize"]
		 [default-string "Default Value"]
		 [fonts (cons default-string (wx:get-font-list))]
		 [file  (wx:find-path 'setup-file)]
		 [section "mred"]
		 [build-entry (lambda (x) (string-append "Screen" x "__"))]
		 [make-family-panel
		  (lambda (name)
		    (let* ([horiz (make-object mred:horizontal-panel% main
					       -1 -1 -1 -1 wx:const-border)]
			   [label (make-object mred:message% horiz name)]
			   [space (make-object mred:horizontal-panel% horiz)]
			   [_ (make-object mred:message% horiz 
					   (let ([b (box "")])
					     (if (and (wx:get-resource 
						       section 
						       (build-entry name)
						       b)
						      (not (string=? (unbox b) 
								     "")))
						 (unbox b)
						 default-string)))]
			   [button 
			    (make-object 
			     mred:button% horiz 
			     (lambda (button evt)
			       (let ([new-value
				      (mred:gui-utils:get-single-choice
				       "message"
				       "caption"
				       fonts)])
				 (unless (null? new-value)
				   (wx:write-resource 
				    section
				    (build-entry name)
				    (if (string=? default-string new-value)
					""
					new-value)
				    file)
				   (send horiz change-children
					 (lambda (l)
					   (list label space 
						 (make-object
						  mred:message%
						  horiz
						  new-value)
						 button))))))
			     "Change")])
		      (void)))])
	    (for-each make-family-panel families)
	    (let ([size-panel (make-object mred:horizontal-panel% main -1 -1 -1 -1 wx:const-border)]
		  [default-font-size
		   (case wx:platform
		     [(unix) 12]
		     [(windows) 10]
		     [(macintosh) 9])])
	      '(make-object mred:message% size-panel "Size")
	      '(make-object mred:horizontal-panel% size-panel)
	      (make-object mred:slider% size-panel 
			   (lambda (slider evt)
			     (wx:write-resource 
			      section
			      font-size-entry
			      (send slider get-value)
			      file))
			   "Size"
			   (let ([b (box 0)])
			     (if (wx:get-resource section 
						  font-size-entry
						  b)
				 (unbox b)
				 default-font-size))
			   1 127 50))
	    main))
	#f)))

    (define make-run-once
      (lambda ()
	(let ([semaphore (make-semaphore 1)])
	  (lambda (t)
	    (dynamic-wind (lambda () (semaphore-wait semaphore))
			  t
			  (lambda () (semaphore-post semaphore)))))))

    (define run-once (make-run-once))

    (define preferences-dialog #f)

    (define add-preference-panel
      (lambda (title container)
	(run-once
	 (lambda ()
	   (let ([new-ppanel (make-ppanel title container #f)])
	     (set! ppanels 
		   (let loop ([ppanels ppanels])
		     (cond
		      [(null? ppanels) (list new-ppanel)]
		      [(string=? (ppanel-title (car ppanels))
				 title)
		       (loop (cdr ppanels))]
		      [else (cons (car ppanels)
				  (loop (cdr ppanels)))])))
	     (when preferences-dialog
	       (send preferences-dialog added-pane)))))))

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
		  (set! preferences-dialog
			(let ([cursor-off (mred:gui-utils:delay-action
					   2 wx:begin-busy-cursor
					   wx:end-busy-cursor)])
			  (begin0 (make-preferences-dialog)
				  (cursor-off)))))))))))

    (define make-preferences-dialog
      (lambda ()
	(letrec* ([frame 
                    (make-object (class-asi mred:frame%
                                   (public [added-pane (lambda () 
							 (ensure-constructed)
							 (refresh-menu)
							 (send popup-menu set-selection (sub1 (length ppanels)))
							 (send single-panel active-child 
							       (ppanel-panel (car (list-tail ppanels (sub1 (length ppanels)))))))]))
                      '() "Preferences")]
		  [panel (make-object mred:vertical-panel% frame)]
		  [top-panel (make-object mred:horizontal-panel% panel)]
		  [single-panel (make-object mred:single-panel% panel -1 -1 -1 -1 wx:const-border)]
		  [bottom-panel (make-object mred:horizontal-panel% panel)]
		  [popup-callback
		   (lambda (choice command-event)
		     (send single-panel active-child
			   (ppanel-panel (list-ref ppanels (send command-event get-command-int)))))]
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
		  [ensure-constructed
		   (lambda ()
		     (for-each (lambda (ppanel)
				 (unless (ppanel-panel ppanel)
				   (let ([panel ((ppanel-container ppanel) single-panel)])
				     (unless (is-a? panel mred:panel%)
				       (error 'preferences-dialog
					      "expected the preference panel to be a mred:panel%. Got ~a instead~n"
					      panel))
				     (set-ppanel-panel! ppanel panel))))
			       ppanels)
		     (send single-panel change-children (lambda (l) (map ppanel-panel ppanels)))
		     (send single-panel active-child (ppanel-panel (car ppanels))))]
		  [refresh-menu
		   (lambda ()
		     (let ([new-popup (make-popup-menu)])
		       (send new-popup set-selection (send popup-menu get-selection))
		       (set! popup-menu new-popup)
		       (send top-panel change-children
			     (lambda (l) (list top-left new-popup top-right)))))]
		  [ok-callback (lambda args
				 (save-user-preferences)
				 (hide-preferences-dialog))]
		  [_1 (make-object mred:panel% bottom-panel)]
		  [ok-button (make-object mred:button% bottom-panel ok-callback "OK")]
		  [cancel-callback (lambda args
				     (hide-preferences-dialog)
				     (read-user-preferences))]
		  [cancel-button (make-object mred:button% bottom-panel cancel-callback "Cancel")])
	  (send ok-button user-min-width (send cancel-button get-width))
	  (send bottom-panel stretchable-in-y #f)
	  (send top-panel stretchable-in-y #f)
	  (ensure-constructed)
	  (send popup-menu set-selection 0)
	  (send frame show #t)
	  frame)))

    (read-user-preferences))
