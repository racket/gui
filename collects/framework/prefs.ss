(unit/sig framework:preferences^
  (import mred^
	  [prefs-file : framework:prefs-file^]
	  [exn : framework:exn^]
	  [exit : framework:exit^]
	  [panel : framework:panel^]
	  [mzlib:pretty-print : mzlib:pretty-print^]
	  [mzlib:function : mzlib:function^])

  (rename [-read read])
  
  (define default-preferences-filename (build-path (collection-path "defaults") "prefs.ss"))
  
  ;; preferences : sym -o> (union marshalled pref)
  (define preferences (make-hash-table))

  ;; marshall-unmarshall : sym -o> un/marshall
  (define marshall-unmarshall (make-hash-table))

  ;; callbacks : sym -o> (listof (sym TST -> boolean))
  (define callbacks (make-hash-table))

  ;; saved-defaults : sym -o> (union marshalled pref)
  (define saved-defaults (make-hash-table))

  ;; defaults : sym -o> default
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
  
  (define (unmarshall p marshalled)
    (let/ec k
      (let* ([data (marshalled-data marshalled)]
	     [unmarshall-fn (un/marshall-unmarshall
			     (hash-table-get marshall-unmarshall
					     p
					     (lambda () (k data))))])
	(guard "unmarshalling" p marshalled
	       (lambda () (unmarshall-fn data))
	       (lambda (exn)
		 (begin0
		  (hash-table-get
		   defaults
		   p
		   (lambda ()
		     (raise exn)))
		  (message-box (format "Error unmarshalling ~a preference" p)
			       (if (exn? exn)
				   (exn-message exn)
				   (format "~s" exn)))))))))
  
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
	 callbacks
	 p
	 (let loop ([callbacks (get-callbacks p)])
	   (cond
	    [(null? callbacks) null]
	    [else (if (eq? (car callbacks) callback)
		      (loop (cdr callbacks))
		      (cons (car callbacks) (loop (cdr callbacks))))]))))))
  
  (define check-callbacks
    (lambda (p value)
      (andmap (lambda (x)
		(guard "calling callback" p value
		       (lambda () (x p value))
		       raise))
	      (get-callbacks p))))
  
  (define (get p) 
    (let ([ans (hash-table-get preferences p
			       (lambda () 
				 (raise (exn:make-unknown-preference
					 (format "attempted to get unknown preference: ~a" p)
					 (current-continuation-marks)))))])
      (cond
       [(marshalled? ans)
	(let* ([default-s
		 (hash-table-get
		  defaults p
		  (lambda ()
		    (error 'preferences:get
			   "no default pref for: ~a~n"
			   p)))]
	       [default (default-value default-s)]
	       [checker (default-checker default-s)]
	       [unmarshalled (let ([unmarsh (unmarshall p ans)])
			       (if (checker unmarsh)
				   unmarsh
				   default))]
	       [pref (if (check-callbacks p unmarshalled)
			 unmarshalled
			 default)])
	  (hash-table-put! preferences p (make-pref pref))
	  pref)]
       [(pref? ans)
	(pref-value ans)]
       [else (error 'prefs.ss "robby error.1: ~a" ans)])))
  
  (define (set p value)
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
	(error 'prefs.ss "robby error.0: ~a" pref)])))
  
  (define set-un/marshall
    (lambda (p marshall unmarshall)
      (when (hash-table-get defaults p (lambda () #f))
	(error 'set-un/marshall "must call set-default for ~s before calling set-un/marshall for ~s"
	       p p))
      (hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))))
  
  (define restore-defaults
    (lambda ()
      (hash-table-for-each
       defaults
       (lambda (p v) (set p v)))))
  
  ;; set-default : (sym TST (TST -> boolean) -> void
  (define (set-default p in-default-value checker)
    (let* ([default-value
	     (let/ec k
	       (let ([saved-default
		      (hash-table-get saved-defaults p (lambda ()
							 (k in-default-value)))])
		 (cond
		  [(marshalled? saved-default)
		   (let* ([unmarsh (unmarshall p saved-default)]
			  [unmarshalled 
			   (if (checker unmarsh)
			       unmarsh
			       (begin
				 '(printf
				  "WARNING: rejected saved default ~s for ~s; using ~s instead"
				  unmarsh p in-default-value)
				 in-default-value))]
			  [pref (if (check-callbacks p unmarshalled)
				    unmarshalled
				    in-default-value)])
		     (hash-table-put! saved-defaults p (make-pref pref))
		     pref)]
		  [(pref? saved-default)
		   (pref-value saved-default)])))]
	   [default-okay? (checker default-value)])
      (unless default-okay?
	(error 'set-default "~s: checker (~s) returns ~s for ~s, expected #t~n"
	       p checker default-okay? default-value))
      (hash-table-get preferences p 
		      (lambda ()
			(hash-table-put! preferences p (make-pref default-value))))
      (hash-table-put! defaults p (make-default default-value checker))))
  
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
			    "Error saving preferences"
			    (exn-message exn)))])
	  (call-with-output-file (prefs-file:get-preferences-filename)
	    (lambda (p)
	      (mzlib:pretty-print:pretty-print
	       (hash-table-map preferences marshall-pref) p))
	    'truncate 'text)))))

  (define (for-each-pref-in-file parse-pref preferences-filename)
    (let/ec k
      (let ([err
	     (lambda (input msg)
	       (message-box "Preferences"
			    (let* ([max-len 150]
				   [s1 (format "~s" input)]
				   [ell "..."]
				   [s2 (if (<= (string-length s1) max-len)
					   s1
					   (string-append
					    (substring s1 0 (- max-len
							       (string-length ell)))
					    ell))])
			      (format "found bad pref in ~a: ~a~n~a"
				      preferences-filename msg s2))))])
	(let ([input (with-handlers
			 ([(lambda (exn) #t)
			   (lambda (exn)
			     (message-box
			      "Error reading preferences"
			      (format "Error reading preferences~n~a"
				      (exn-message exn)))
			     (k #f))])
		       (call-with-input-file (prefs-file:get-preferences-filename)
			 read
			 'text))])
	  (if (eof-object? input)
	      (void)
	      (let loop ([input input])
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
		 [else (err input "expected a pair")])))))))
  
  ;; read-from-file-to-ht : string hash-table -> void
  (define (read-from-file-to-ht filename ht)
    (let* ([parse-pref
	    (lambda (p marshalled)
	      (let* ([ht-pref (hash-table-get ht p (lambda () #f))]
		     [unmarshall-struct (hash-table-get marshall-unmarshall p (lambda () #f))])
		(cond
		 [unmarshall-struct
		  (set p ((un/marshall-unmarshall unmarshall-struct) marshalled))]
		 
		 ;; in this case, assume that no marshalling/unmarshalling 
		 ;; is going to take place with the pref, since an unmarshalled 
		 ;; pref was already there.
		 [(pref? ht-pref)
		  (set p marshalled)]
		 
		 [(marshalled? ht-pref) 
		  (set-marshalled-data! ht-pref marshalled)]
		 [(and (not ht-pref) unmarshall-struct)
		  (set p ((un/marshall-unmarshall unmarshall-struct) marshalled))]
		 [(not ht-pref)
		  (hash-table-put! ht p (make-marshalled marshalled))]
		 [else (error 'prefs.ss "robby error.3: ~a" ht-pref)])))])
      (when (file-exists? filename)
	(for-each-pref-in-file parse-pref filename))))

  ;; read : -> void
  (define (-read)
    (read-from-file-to-ht (prefs-file:get-preferences-filename) preferences))


  ;; read in the saved defaults. These should override the
  ;; values used with set-default.
  (read-from-file-to-ht default-preferences-filename saved-defaults)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                 ;;;
  ;;;               preferences dialog                ;;;
  ;;;                                                 ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  (define-struct ppanel (title container panel))
  
  (define ppanels null)

  (define (add-general-panel)
    (add-panel
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

	 ;; not exposed to the user anymore. Only left in for automated testing.
	 ;(make-check 'framework:file-dialogs "Use platform-specific file dialogs"
	 ;(lambda (x) (if x 'std 'common))
	 ;(lambda (x) (eq? x 'std)))
	 
	 (make-check 'framework:verify-exit "Verify exit" id id)
	 (make-check 'framework:verify-change-format "Ask before changing save format" id id)
	 (make-check 'framework:auto-set-wrap? "Wrap words in editor buffers" id id)
	 
	 (make-check 'framework:show-status-line "Show status-line" id id)
	 (make-check 'framework:line-offsets "Count line and column numbers from one" id id)
	 (make-check 'framework:display-line-numbers "Display line numbers in buffer; not character offsets" id id)
	 (make-check 'framework:menu-bindings "Enable keybindings in menus" id id)
	 (unless (eq? (system-type) 'unix) 
	   (make-check 'framework:print-output-mode "Automatically print to postscript file"
		       (lambda (b) 
			 (if b 'postscript 'standard))
		       (lambda (n) (eq? 'postscript n))))

	 
	 '(when (eq? (system-type) 'windows)
	    (make-check 'framework:windows-mdi "Use MDI Windows" id id))
	 (make-check 'framework:search-using-dialog?
		     "Use separate dialog for searching"
		     id id)

	 main)))
    (set! add-general-panel void))

  (define (add-font-panel)
    (let* ([font-families-name/const
	    (list (list "Default" 'default)
		  (list "Decorative" 'decorative)
		  (list "Modern" 'modern)
		  (list "Roman" 'roman)
                  (list "Script" 'script)
		  (list "Swiss" 'swiss))]
	   
	   [font-families (map car font-families-name/const)]
	   
	   [font-size-entry "defaultFontSize"]
	   [font-default-string "Default Value"]
	   [font-default-size (case (system-type)
				[(windows) 10]
				[else 12])]
	   [font-section "mred"]
	   [build-font-entry (lambda (x) (string-append "Screen" x "__"))]
	   [font-file (find-graphical-system-path 'setup-file)]
	   [build-font-preference-symbol
	    (lambda (family)
	      (string->symbol (string-append "framework:" family)))]
	   
	   [set-default
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
		     (write-resource 
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
       font-size-entry)
      (add-panel
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
				     (set pref-sym (list-ref fonts (car new-value))) 
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
		       (send canvas set-line-count 1)
		       (vector set-edit-font
			       (lambda () (send message get-width))
			       (lambda (width) (send message min-width width))
			       (lambda () (send label get-width))
			       (lambda (width) (send label min-width width)))))]
		  [set-edit-fonts/messages (map make-family-panel font-families)]
		  [collect (lambda (n) (map (lambda (x) (vector-ref x n))
					    set-edit-fonts/messages))]
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
		  [initial-font-size
		   (let ([b (box 0)])
		     (if (get-resource font-section 
				       font-size-entry
				       b)
			 (unbox b)
			 font-default-size))]
		  [size-slider
		   (make-object slider%
		     "Size"
		     1 127
		     size-panel
		     (lambda (slider evt)
		       (set font-size-pref-sym (send slider get-value)))
		     initial-font-size)])
	   (update-message-sizes font-message-get-widths font-message-user-min-sizes)
	   (update-message-sizes category-message-get-widths category-message-user-min-sizes)
	   (add-callback
	    font-size-pref-sym
	    (lambda (p value)
	      (for-each (lambda (f) (f value)) set-edit-fonts)
	      (unless (= value (send size-slider get-value))
		(send size-slider set-value value))
	      #t))
	   (for-each (lambda (f) (f initial-font-size)) set-edit-fonts)
	   (make-object message% "Restart to see font changes" main)
	   main))))
    (set! add-font-panel void))
  
  (define preferences-dialog #f)
  
  (define add-panel
    (lambda (title container)
      (set! ppanels
	    (append ppanels (list (make-ppanel title container #f))))
      (when preferences-dialog
	(send preferences-dialog added-pane))))
  
  (define hide-dialog
    (lambda ()
      (when preferences-dialog
	(send preferences-dialog show #f))))
  
  (define show-dialog
    (lambda ()
      (save)
      (if preferences-dialog
	  (send preferences-dialog show #t)
	  (set! preferences-dialog
		(make-preferences-dialog)))))
  
  (define make-preferences-dialog
    (lambda ()
      (letrec ([frame 
		(make-object (class-asi frame%
			       (public
				 [added-pane
				  (lambda () 
				    (ensure-constructed)
				    (refresh-menu)
				    (unless (null? ppanels)
				      (send popup-menu set-selection (sub1 (length ppanels)))
				      (send single-panel active-child 
					    (ppanel-panel
					     (car
					      (list-tail ppanels
							 (sub1 (length ppanels))))))))]))
		  "Preferences")]
	       [panel (make-object vertical-panel% frame)]
	       [popup-callback
		(lambda (choice command-event)
		  (unless (null? ppanels)
		    (send single-panel active-child
			  (ppanel-panel (list-ref ppanels (send choice get-selection))))))]
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
					       (is-a? panel area-container<%>))
				    (error 'preferences-dialog
					   "expected the result of the function passed to preferences:add-panel to implement the area-container% interface. Got ~a~n"
					   panel))
				  (set-ppanel-panel! ppanel panel))))
			    ppanels)
		  (send single-panel change-children (lambda (l) (map ppanel-panel ppanels)))
		  (unless (null? ppanels)
		    (send single-panel active-child (ppanel-panel (car ppanels)))))]
	       [refresh-menu
		(lambda ()
		  (let ([new-popup (make-popup-menu)]
			[old-selection (send popup-menu get-selection)])
		    (when old-selection
		      (send new-popup set-selection old-selection))
		    (set! popup-menu new-popup)
		    (send panel change-children
			  (lambda (l) (list new-popup
					    single-panel
					    bottom-panel)))))]
	       [ok-callback (lambda args
			      (save)
			      (hide-dialog))]
	       [ok-button (make-object button% "OK" bottom-panel ok-callback '(border))]
	       [cancel-callback (lambda args
				  (hide-dialog)
				  (-read))]
	       [cancel-button (make-object button% "Cancel" bottom-panel cancel-callback)]
	       [grow-box-space (make-object grow-box-spacer-pane% bottom-panel)])
	(send ok-button min-width (send cancel-button get-width))
	(send* bottom-panel
	  (stretchable-height #f)
	  (set-alignment 'right 'center))
	(ensure-constructed)
	(unless (null? ppanels)
	  (send popup-menu set-selection 0))
	(send popup-menu focus)
	(send frame show #t)
	frame))))
