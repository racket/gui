(unit/sig framework:preferences^
  (import mred-interfaces^
	  [exn : framework:exn^]
	  [exit : framework:exit^]
	  [panel : framework:panel^]
	  [mzlib:pretty-print : mzlib:pretty-print^]
	  [mzlib:function : mzlib:function^])

  (rename [-read read])
  
  (define preferences-filename (build-path (find-system-path 'pref-dir)
					   (case (system-type)
					     [(macos) "MrEd Preferences"]
					     [(windows) "mred.pre"]
					     [else ".mred.prefs"])))
  
  (define preferences (make-hash-table))
  (define marshall-unmarshall (make-hash-table))
  (define callbacks (make-hash-table))    
  (define defaults (make-hash-table))
  
  (define-struct un/marshall (marshall unmarshall))
  (define-struct marshalled (data))
  (define-struct pref (value))
  (define-struct default (value checker))
  
  (define guard
    (lambda (when p value thunk failure)
      (let ([h
	     (lambda (x)
	       (let ([msg
		      (format "exception raised ~a for ~a with ~a: ~a~n"
			      when p value
			      (exn-message x))])
		 (failure x)))])
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
		 (lambda () (unmarshall-fn data))
		 (lambda (exn)
		   (hash-table-get
		    defaults
		    p
		    (lambda ()
		      (message-box
		       "No Default"
		       (format
			"no default for ~a"
			p))
		      (raise exn)))))))))
  
  (define get-callbacks
    (lambda (p)
      (hash-table-get callbacks
		      p
		      (lambda () null))))
  
  (define add-callback
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
		       (lambda () (x p value))
		       raise))
	      (get-callbacks p))))
  
  (define get
    (lambda (p) 
      (let ([ans (hash-table-get preferences p
				 (lambda () 
				   (raise (exn:make-unknown-preference
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
					(printf "WARNING: ~s rejecting invalid pref ~s in favor of ~s (pred: ~s)~n"
						p unmarsh default checker)
					default)))]
		  [pref (if (check-callbacks p unmarshalled)
			    unmarshalled
			    default)])
	     (hash-table-put! preferences p (make-pref pref))
	     pref)]
	  [(pref? ans) (pref-value ans)]
	  [else (error 'prefs.ss "robby error.1: ~a" ans)]))))
  
  (define set
    (lambda (p value)
      (let* ([pref (hash-table-get preferences p (lambda () #f))])
	(cond 
	  [(pref? pref)
	   (when (check-callbacks p value)
	     (set-pref-value! pref value))]
	  [(or (marshalled? pref) 
	       (not pref))
	   (when (check-callbacks p value)
	     (hash-table-put! preferences p (make-pref value)))]
	  [else
	   (error 'prefs.ss "robby error.0: ~a" pref)]))))
  
  (define set-un/marshall
    (lambda (p marshall unmarshall)
      (hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))))
  
  (define restore-defaults
    (lambda ()
      (hash-table-for-each
       defaults
       (lambda (p v) (set p v)))))
  
  (define set-default
    (lambda (p value checker)
      (let ([t (checker value)])
	(unless t
	  (error 'set-default "~s: checker (~s) returns ~s for ~s, expected #t~n" p checker t value)))
      (hash-table-get preferences p 
		      (lambda () 
			(hash-table-put! preferences p (make-pref value))))
      (hash-table-put! defaults p (make-default value checker))))
  
  (define save
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
				    value))
				 raise))])
		  (list p marshalled))]
	       [else (error 'prefs.ss "robby error.2: ~a" ht-value)]))])
      (lambda () 
	(with-handlers ([(lambda (x) #t)
			 (lambda (exn)
			   (message-box
			    (format "Error saving preferences~n~a"
				    (exn-message exn))
			    "Error saving preferences"))])
	  (call-with-output-file preferences-filename
	    (lambda (p)
	      (mzlib:pretty-print:pretty-print
	       (hash-table-map preferences marshall-pref) p))
	    'truncate 'text)))))
  
  (define -read
    (let ([parse-pref
	   (lambda (p marshalled)
	     (let/ec k
	       (let* ([ht-pref (hash-table-get preferences p (lambda () #f))]
		      [unmarshall-struct (hash-table-get marshall-unmarshall p (lambda () #f))])
		 (cond
		   [(and (pref? ht-pref) unmarshall-struct)
		    (set p ((un/marshall-unmarshall unmarshall-struct) marshalled))]
		   
		   ;; in this case, assume that no marshalling/unmarshalling 
		   ;; is going to take place with the pref, since an unmarshalled 
		   ;; pref was already there.
		   [(pref? ht-pref)
		    (set p marshalled)]
		   
		   [(marshalled? ht-pref) (set-marshalled-data! ht-pref marshalled)]
		   [(and (not ht-pref) unmarshall-struct)
		    (set p ((un/marshall-unmarshall unmarshall-struct) marshalled))]
		   [(not ht-pref)
		    (hash-table-put! preferences p (make-marshalled marshalled))]
		   [else (error 'prefs.ss "robby error.3: ~a" ht-pref)]))))])
      (lambda ()
	(let/ec k
	  (when (file-exists? preferences-filename)
	    (let ([err
		   (lambda (input msg)
		     (message-box "Preferences"
				  (let* ([max-len 150]
					 [s1 (format "~s" input)]
					 [ell "..."]
					 [s2 (if (<= (string-length s1) max-len)
						 s1
						 (string-append (substring s1 0 (- max-len
										   (string-length ell)))
								ell))])
				    (format "found bad pref: ~a~n~a" msg s2))))])
	      (let loop ([input (with-handlers
				    ([(lambda (exn) #t)
				      (lambda (exn)
					(message-box
					 "Error reading preferences"
					 (format "Error reading preferences~n~a"
						 (exn-message exn)))
					(k #f))])
				  (call-with-input-file preferences-filename
				    read
				    'text))])
		(cond
		  [(pair? input)
		   (let ([err-msg
			  (let/ec k
			    (let ([first (car input)])
			      (unless (pair? first)
				(k "expected pair of pair"))
			      (let ([arg1 (car first)]
				    [t1 (cdr first)])
				(unless (pair? t1)
				  (k "expected pair of two pairs"))
				(let ([arg2 (car t1)]
				      [t2 (cdr t1)])
				  (unless (null? t2)
				    (k "expected null after two pairs"))
				  (parse-pref arg1 arg2)
				  (k #f)))))])
		     (when err-msg
		       (err input err-msg)))
		   (loop (cdr input))]
		  [(null? input) (void)]
		  [else (err input "expected a pair")]))))))))
  
  (define-struct ppanel (title container panel))

  (define font-families-name/const
    (list (list "Default" 'default)
	  (list "Decorative" 'decorative)
	  (list "Roman" 'roman)
	  (list "Decorative" 'script)
	  (list "Swiss" 'swiss)
	  (list "Modern" 'modern)))

  (define font-families (map car font-families-name/const))
  
  (define font-size-entry "defaultFontSize")
  (define font-default-string "Default Value")
  (define font-default-size 12)
  (define font-section "mred")
  (define build-font-entry (lambda (x) (string-append "Screen" x "__")))
  (define font-file (find-graphical-system-path 'setup-file))
  (define (build-font-preference-symbol family)
    (string->symbol (string-append "framework:" family)))
  
  (let ([set-default
	 (lambda (build-font-entry default pred)
	   (lambda (family)
	     (let ([name (build-font-preference-symbol family)]
		   [font-entry (build-font-entry family)])
	       (set-default name
			    default
			    (cond
			      [(string? default) string?]
			      [(number? default) number?]
			      [else (error 'internal-error.set-default "unrecognized default: ~a~n" default)]))
	       (add-callback 
		name 
		(lambda (p new-value)
		  '(write-resource 
		   font-section
		   font-entry
		   (if (and (string? new-value)
			    (string=? font-default-string new-value))
		       ""
		       new-value)
		   font-file))))))])
    (for-each (set-default build-font-entry font-default-string string?)
	      font-families)
    ((set-default (lambda (x) x)
		  font-default-size
		  number?)
     font-size-entry))
  
  (define (later-on)
    (local [(define sema (make-semaphore 1))
	    (define running #f)
	    (define (start-one thunk)
	      (local [(define (do-one)
			(thunk)
			(semaphore-wait sema)
			(set! running #f)
			(semaphore-post sema))]
		(semaphore-wait sema)
		(when running
		  (kill-thread running))
		(set! running (thread do-one))
		(semaphore-post sema)))]
      start-one))

  (define ppanels 
    (list 
     (make-ppanel
      "General"
      (lambda (parent)
	(let* ([main (make-object vertical-panel% parent)]
	       [make-check
		(lambda (pref title bool->pref pref->bool)
		  (let*  ([callback
			   (lambda (check-box _)
			     (set pref (bool->pref (send check-box get-value))))]
			  [pref-value (get pref)]
			  [initial-value (pref->bool pref-value)]
			  [c (make-object check-box% title main callback)])
		    (send c set-value initial-value)
		    (add-callback pref
				  (lambda (p v)
				    (send c set-value (pref->bool v))))))]
	       [id (lambda (x) x)])
	  (send main set-alignment 'left 'center)
	  (make-check 'framework:highlight-parens "Highlight between matching parens" id id)
	  (make-check 'framework:fixup-parens "Correct parens" id id)
	  (make-check 'framework:paren-match "Flash paren match" id id)
	  (make-check 'framework:autosaving-on? "Auto-save files" id id)
	  (make-check 'framework:delete-forward? "Map delete to backspace" not not)
	  (make-check 'framework:file-dialogs "Use platform-specific file dialogs"
		      (lambda (x) (if x 'std 'common))
		      (lambda (x) (eq? x 'std)))
	  
	  (make-check 'framework:verify-exit "Verify exit" id id)
	  (make-check 'framework:verify-change-format "Ask before changing save format" id id)
	  (make-check 'framework:auto-set-wrap? "Wordwrap editor buffers" id id)
	  
	  (make-check 'framework:show-status-line "Show status-line" id id)
	  (make-check 'framework:line-offsets "Count line and column numbers from one" id id)
	  (make-check 'framework:menu-bindings "Enable keybindings in menus" id id)
	  (unless (eq? (system-type) 'unix) 
	    (make-check 'framework:print-output-mode "Automatically print to postscript file"
			(lambda (b) 
			  (if b 'postscript 'standard))
			(lambda (n) (eq? 'postscript n))))

	  
	  (make-check 'framework:display-line-numbers "Display line numbers in buffer; not character offsets" id id)
	  (when (eq? (system-type) 'windows)
	    (make-check 'framework:windows-mdi "Use MDI Windows" id id))

	  main))
      #f)
     (make-ppanel
      "Default Fonts"
      (lambda (parent)
	(letrec ([font-size-pref-sym (build-font-preference-symbol font-size-entry)]
		 [ex-string "The quick brown fox jumped over the lazy dogs."]
		 [main (make-object vertical-panel% parent)]
		 [fonts (cons font-default-string (get-face-list))]
		 [make-family-panel
		  (lambda (name)
		    (let* ([pref-sym (build-font-preference-symbol name)]
			   [family-const-pair (assoc name font-families-name/const)]
			   
			   [edit (make-object text%)]
			   [_ (send edit insert ex-string)]
			   [set-edit-font
			    (lambda (size)
			      (let ([delta (make-object style-delta% 'change-size size)]
				    [face (get pref-sym)])
				(if (and (string=? face font-default-string)
					 family-const-pair)
				    (send delta set-family (cadr family-const-pair))
				    (send delta set-delta-face (get pref-sym)))
				
				(send edit change-style delta 0 (send edit last-position))))]
			   
			   [horiz (make-object horizontal-panel% main '(border))]
			   [label (make-object message% name horiz)]
			   
			   [message (make-object message%
				      (let ([b (box "")])
					(if (and (get-resource 
						  font-section 
						  (build-font-entry name)
						  b)
						 (not (string=? (unbox b) 
								"")))
					    (unbox b)
					    font-default-string)) 
				      horiz)]
			   [button 
			    (make-object button%
			      "Change" 
			      horiz
			      (lambda (button evt)
				(let ([new-value
				       (get-choices-from-user
					"Fonts"
					(format "Please choose a new ~a font"
						name)
					fonts)])
				  (when new-value
				    (set pref-sym new-value) 
				    (set-edit-font (get font-size-pref-sym))))))]
			   [canvas (make-object editor-canvas% horiz
						edit
						(list 'hide-hscroll
						      'hide-vscroll))])
		      (set-edit-font (get font-size-pref-sym))
		      (add-callback
		       pref-sym
		       (lambda (p new-value)
			 (send horiz change-children
			       (lambda (l)
				 (let ([new-message (make-object message%
						      new-value
						      horiz)])
				   (set! message new-message)
				   (update-message-sizes font-message-get-widths 
							 font-message-user-min-sizes)
				   (list label 
					 new-message
					 button
					 canvas))))))
		      (vector set-edit-font
			      (lambda () (send message get-width))
			      (lambda (width) (send message min-width width))
			      (lambda () (send label get-width))
			      (lambda (width) (send label min-width width)))))]
		 [set-edit-fonts/messages (map make-family-panel font-families)]
		 [collect (lambda (n) (map (lambda (x) (vector-ref x n)) set-edit-fonts/messages))]
		 [set-edit-fonts (collect 0)]
		 [font-message-get-widths (collect 1)]
		 [font-message-user-min-sizes (collect 2)]
		 [category-message-get-widths (collect 3)]
		 [category-message-user-min-sizes (collect 4)]
		 [update-message-sizes
		  (lambda (gets sets)
		    (let ([width (mzlib:function:foldl (lambda (x l) (max l (x))) 0 gets)])
		      (for-each (lambda (set) (set width)) sets)))]
		 [size-panel (make-object horizontal-panel% main '(border))]
		 [size-slider
		  (make-object slider%
		    "Size"
		    1 127
		    size-panel
		    (lambda (slider evt)
		      (set font-size-pref-sym (send slider get-value)))
		    (let ([b (box 0)])
		      (if (get-resource font-section 
					font-size-entry
					b)
			  (unbox b)
			  font-default-size)))]
		 [guard-change-font (later-on)])
	  (update-message-sizes font-message-get-widths font-message-user-min-sizes)
	  (update-message-sizes category-message-get-widths category-message-user-min-sizes)
	  (add-callback
	   font-size-pref-sym
	   (lambda (p value)
	     (guard-change-font
	      (lambda ()
		(map (lambda (f) (f value)) set-edit-fonts)))
	     (unless (= value (send size-slider get-value))
	       (send size-slider set-value value))
	     #t))
	  (make-object message% "Restart to see font changes" main)
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
  
  (define add-panel
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
  
  (define hide-dialog
    (lambda ()
      (run-once
       (lambda ()
	 (when preferences-dialog
	   (send preferences-dialog show #f))))))
  
  (define show-dialog
    (lambda ()
      (run-once
       (lambda () 
	 (save)
	 (if preferences-dialog
	     (send preferences-dialog show #t)
	     (set! preferences-dialog
		   (make-preferences-dialog)))))))
  
  (define make-preferences-dialog
    (lambda ()
      (letrec ([frame 
		(make-object (class-asi frame%
			       (public [added-pane (lambda () 
						     (ensure-constructed)
						     (refresh-menu)
						     (send popup-menu set-selection (sub1 (length ppanels)))
						     (send single-panel active-child 
							   (ppanel-panel (car (list-tail ppanels (sub1 (length ppanels)))))))]))
		  "Preferences")]
	       [panel (make-object vertical-panel% frame)]
	       [popup-callback
		(lambda (choice command-event)
		  (send single-panel active-child
			(ppanel-panel (list-ref ppanels (send choice get-selection)))))]
	       [make-popup-menu 
		(lambda ()
		  (let ([menu (make-object choice% "Category"
					   (map ppanel-title ppanels)
					   panel popup-callback)])
		    (send menu stretchable-width #f)
		    menu))]
	       [popup-menu (make-popup-menu)]
	       [single-panel (make-object panel:single%
			       panel '(border))]
	       [bottom-panel (make-object horizontal-panel% panel)]
	       [ensure-constructed
		(lambda ()
		  (for-each (lambda (ppanel)
			      (unless (ppanel-panel ppanel)
				(let ([panel ((ppanel-container ppanel) single-panel)])
				  (unless (and (object? panel)
					       (implementation? (object-class panel) area-container<%>))
				    (error 'preferences-dialog
					   "expected the result of the function passed to preferences:add-panel to implement the area-container% interface. Got ~a~n"
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
		    (send panel change-children
			  (lambda (l) (list new-popup
					    single-panel
					    bottom-panel)))))]
	       [ok-callback (lambda args
			      (save)
			      (hide-dialog))]
	       [ok-button (make-object button% "OK" bottom-panel ok-callback)]
	       [cancel-callback (lambda args
				  (hide-dialog)
				  (-read))]
	       [cancel-button (make-object button% "Cancel" bottom-panel cancel-callback)])
	(send ok-button min-width (send cancel-button get-width))
	(send* bottom-panel
	  (stretchable-height #f)
	  (set-alignment 'right 'center))
	(ensure-constructed)
	(send popup-menu set-selection 0)
	(send frame show #t)
	frame))))
