
(module preferences mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "file.ss")
	   (lib "class100.ss")
	   "sig.ss"
           "../gui-utils.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "pretty.ss")
	   (lib "list.ss"))
  
  (provide preferences@)
  (define preferences@
    (unit/sig framework:preferences^
      (import mred^
              [exn : framework:exn^]
              [exit : framework:exit^]
              [panel : framework:panel^])
      
      (rename [-read read])
      
      (define main-preferences-symbol 'plt:framework-prefs)
      
      ;; preferences : sym -o> (union marshalled pref)
      (define preferences (make-hash-table))
      
      ;; marshall-unmarshall : sym -o> un/marshall
      (define marshall-unmarshall (make-hash-table))
      
      ;; callbacks : sym -o> (listof (sym TST -> boolean))
      (define callbacks (make-hash-table))
      
      ;; defaults : sym -o> default
      (define defaults (make-hash-table))
      
      (define-struct un/marshall (marshall unmarshall))
      (define-struct marshalled (data))
      (define-struct pref (value))
      (define-struct default (value checker))
      
      (define guard
        (lambda (when p value thunk failure)
          (with-handlers ([not-break-exn? failure])
            (thunk))))
      
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
                       (message-box (format (string-constant error-unmarshalling) p)
                                    (if (exn? exn)
                                        (exn-message exn)
                                        (format "~s" exn)))))))))
      
      ;; get-callbacks : sym -> (listof (-> void))
      (define (get-callbacks p)
        (hash-table-get callbacks
                        p
                        (lambda () null)))
      
      ;; pref-callback : (make-pref-callback (sym tst -> void))
      ;; this is used as a wrapped to hack around the problem
      ;; that different procedures might be eq?.
      (define-struct pref-callback (cb))

      ;; add-callback : sym (-> void) -> void
      (define (add-callback p callback)
        (let ([new-cb (make-pref-callback callback)])
          (hash-table-put! callbacks p 
                           (append 
                            (hash-table-get callbacks p (lambda () null))
                            (list new-cb)))
          (lambda ()
            (hash-table-put!
             callbacks
             p
             (let loop ([callbacks (hash-table-get callbacks p (lambda () null))])
               (cond
                 [(null? callbacks) null]
                 [else 
                  (let ([callback (car callbacks)])
                    (cond
                      [(eq? callback new-cb)
                       (loop (cdr callbacks))]
                      [else
                       (cons (car callbacks) (loop (cdr callbacks)))]))]))))))
      
      (define (check-callbacks p value)
	(for-each (lambda (x)
		    (guard "calling callback" p value
			   (lambda () ((pref-callback-cb x) p value))
			   raise))
                  (get-callbacks p)))
      
      (define (get p) 
        (let ([ans (hash-table-get preferences p
                                   (lambda () 
                                     (raise (exn:make-unknown-preference
                                             (format "preferences:get: attempted to get unknown preference: ~e" p)
                                             (current-continuation-marks)))))])
          (cond
            [(marshalled? ans)
             (let* ([default-s
                     (hash-table-get
                      defaults p
                      (lambda ()
                        (raise (exn:make-unknown-preference
                                (format "preferences:get: no default pref for: ~e" p)
                                (current-continuation-marks)))))]
                    [default (default-value default-s)]
                    [checker (default-checker default-s)]
                    [unmarshalled (let ([unmarsh (unmarshall p ans)])
                                    (if (checker unmarsh)
                                        unmarsh
                                        default))]
                    [pref (begin (check-callbacks p unmarshalled)
				 unmarshalled)])
               (hash-table-put! preferences p (make-pref pref))
               pref)]
            [(pref? ans)
             (pref-value ans)]
            [else (error 'prefs.ss "robby error.1: ~a" ans)])))
      
      (define (default-set? p)
        (let/ec k
          (hash-table-get defaults p (lambda () (k #f)))
          #t))

      (define (set p value)
        (let* ([pref (hash-table-get preferences p (lambda () #f))])
          (unless (default-set? p)
            (error 'preferences:set "tried to set a preference but no default set for ~e, with ~e"
                   p value))
          (cond
            [(pref? pref)
             (check-callbacks p value)
	     (set-pref-value! pref value)]
            [(or (marshalled? pref) 
                 (not pref))
             (check-callbacks p value)
	     (hash-table-put! preferences p (make-pref value))]
            [else
             (error 'prefs.ss "robby error.0: ~a" pref)])))
      
      (define set-un/marshall
        (lambda (p marshall unmarshall)
          (unless (default-set? p)
            (error 'set-un/marshall "must call set-default for ~s before calling set-un/marshall for ~s"
                   p p))
          (hash-table-put! marshall-unmarshall p (make-un/marshall marshall unmarshall))))
      
      (define restore-defaults
        (lambda ()
          (hash-table-for-each
           defaults
           (lambda (p v) (set p v)))))
      
      ;; set-default : (sym TST (TST -> boolean) -> void
      (define (set-default p default-value checker)
        (let ([default-okay? (checker default-value)])
          (unless default-okay?
            (error 'set-default "~s: checker (~s) returns ~s for ~s, expected #t~n"
                   p checker default-okay? default-value))
          (hash-table-get preferences p 
                          (lambda ()
                            (hash-table-put! preferences p (make-pref default-value))))
          (hash-table-put! defaults p (make-default default-value checker))))
      
      (define (save)
        (with-handlers ([(lambda (x) #t)
                         (lambda (exn)
                           (message-box
                            (string-constant preferences)
                            (format (string-constant error-saving-preferences)
                                    (exn-message exn)))
                           #f)])
          (let ([syms (list main-preferences-symbol)]
                [vals (list (hash-table-map preferences marshall-pref))]
                [res #t])
            (put-preferences
             syms vals
             (lambda (filename)
               (let* ([d (make-object dialog% (string-constant preferences))]
                      [m (make-object message% (string-constant waiting-for-pref-lock) d)])
                 (thread
                  (lambda ()
                    (sleep 2)
                    (send d show #f)))
                 (send d show #t)
                 (put-preferences 
                  syms vals
                  (lambda (filename)
                    (set! res #f)
                    (message-box
                     (string-constant preferences)
                     (format (string-constant pref-lock-not-gone) filename)))))))
            res)))
      
      (define (marshall-pref p ht-value)
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
          [else (error 'prefs.ss "robby error.2: ~a" ht-value)]))
      
      (define (read-err input msg)
        (message-box 
         (string-constant preferences)
         (let* ([max-len 150]
                [s1 (format "~s" input)]
                [ell "..."]
                [s2 (if (<= (string-length s1) max-len)
                        s1
                        (string-append
                         (substring s1 0 (- max-len
                                            (string-length ell)))
                         ell))])
           (string-append
            (string-constant error-reading-preferences)
            "\n"
            msg
            s2))))

      (define (for-each-pref-in-file parse-pref preferences-filename)
        (let/ec k
          (let ([input (with-handlers
                           ([not-break-exn?
                             (lambda (exn)
                               (message-box
                                (string-constant error-reading-preferences)
                                (string-append
                                 (string-constant error-reading-preferences)
                                 (format "\n~a" (exn-message exn))))
                               (k #f))])
                         (call-with-input-file preferences-filename read 'text))])
            (if (eof-object? input)
                (void)
                (for-each-pref-in-sexp input parse-pref)))))
      
      ;; for-each-pref-in-sexp : sexp (symbol TST -> void) -> void
      (define (for-each-pref-in-sexp input parse-pref)
        (let/ec k
          (let loop ([input input])
            (when (pair? input)
              (let ([pre-pref (car input)])
                (if (and (pair? pre-pref)
                         (pair? (cdr pre-pref))
                         (null? (cddr pre-pref)))
                    (parse-pref (car pre-pref) (cadr pre-pref))
                    (begin (read-err input (string-constant expected-list-of-length2))
                           (k #f))))
              (loop (cdr input))))))
      
      ;; add-raw-pref-to-ht : hash-table symbol marshalled-preference -> void
      (define (add-raw-pref-to-ht ht p marshalled)
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
            [else (error 'prefs.ss "robby error.3: ~a" ht-pref)])))

      ;; read : -> void
      (define (-read)
        (let/ec k
          (let ([sexp (get-preference main-preferences-symbol (lambda () (k #f)))])
            (install-stashed-preferences sexp))))
      
      ;; install-stashed-preferences : sexp -> void
      ;; ensure that `prefs' is actuall a well-formed preferences
      ;; table and installs them as the current preferences.
      (define (install-stashed-preferences prefs)
        (for-each-pref-in-sexp 
         prefs
         (lambda (p marshalled)
           (add-raw-pref-to-ht preferences p marshalled))))
 
                                          
    ;;    ;           ;;;                 
     ;                  ;                 
     ;                  ;                 
  ;;;;  ;;;    ;;;;     ;     ;;;    ;;; ;
 ;   ;    ;        ;    ;    ;   ;  ;   ; 
 ;   ;    ;     ;;;;    ;    ;   ;  ;   ; 
 ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
 ;   ;    ;    ;   ;    ;    ;   ;  ;   ; 
  ;;; ; ;;;;;   ;;; ; ;;;;;;  ;;;    ;;;; 
                                        ; 
                                        ; 
                                     ;;;  
      
      
      (define-struct ppanel (title container panel))
      
      (define ppanels null)
      
      (define (add-to-scheme-checkbox-panel f)
        (set! scheme-panel-procs 
              (let ([old scheme-panel-procs])
                (lambda (parent) (old parent) (f parent)))))
      
      (define (add-to-editor-checkbox-panel f)
        (set! editor-panel-procs 
              (let ([old editor-panel-procs])
                (lambda (parent) (old parent) (f parent)))))
      
      (define (add-to-warnings-checkbox-panel f)
        (set! warnings-panel-procs 
              (let ([old warnings-panel-procs])
                (lambda (parent) (old parent) (f parent)))))

      (define scheme-panel-procs void)
      (define editor-panel-procs void)
      (define warnings-panel-procs void)
      
      (define (add-checkbox-panel label proc)
        (add-panel
         label
         (lambda (parent)
           (let* ([main (make-object vertical-panel% parent)])
             (send main set-alignment 'left 'center)
             (proc main)
             main))))
      
      ;; make-check : panel symbol string (boolean -> any) (any -> boolean)
      ;; adds a check box preference to `main'.
      (define (make-check main pref title bool->pref pref->bool)
        (let* ([callback
                (lambda (check-box _)
                  (set pref (bool->pref (send check-box get-value))))]
               [pref-value (get pref)]
               [initial-value (pref->bool pref-value)]
               [c (make-object check-box% title main callback)])
          (send c set-value initial-value)
          (add-callback pref
                        (lambda (p v)
                          (send c set-value (pref->bool v))))))

      (define (make-recent-items-slider parent)
        (let ([slider (instantiate slider% ()
                        (parent parent)
                        (label (string-constant number-of-open-recent-items))
                        (min-value 1)
                        (max-value 100)
                        (init-value (get 'framework:recent-max-count))
                        (callback (lambda (slider y)
                                    (set 'framework:recent-max-count
                                         (send slider get-value)))))])
          (add-callback
           'framework:recent-max-count
           (lambda (p v)
             (send slider set-value v)))))
      
      (define (add-scheme-checkbox-panel)
        (letrec ([add-scheme-checkbox-panel
                  (lambda ()
                    (set! add-scheme-checkbox-panel void)
                    (add-checkbox-panel
                     (string-constant scheme-prefs-panel-label)
                     (lambda (scheme-panel)
                       (make-check scheme-panel
                                   'framework:highlight-parens
                                   (string-constant highlight-parens)
                                   values values)
                       (make-check scheme-panel
                                   'framework:fixup-parens
                                   (string-constant fixup-parens)
                                   values values)
                       (make-check scheme-panel
                                   'framework:paren-match
                                   (string-constant flash-paren-match)
                                   values values)
                       (scheme-panel-procs scheme-panel))))])
          (add-scheme-checkbox-panel)))
      
      (define (add-editor-checkbox-panel)
        (letrec ([add-editor-checkbox-panel
                  (lambda ()
                    (set! add-editor-checkbox-panel void)
                    (add-checkbox-panel 
                     (string-constant editor-prefs-panel-label)
                     (lambda (editor-panel)
                       (make-recent-items-slider editor-panel)
                       (make-check editor-panel
                                   'framework:autosaving-on? 
                                   (string-constant auto-save-files)
                                   values values)
                       (make-check editor-panel  'framework:backup-files? (string-constant backup-files) values values)
                       (make-check editor-panel  'framework:delete-forward? (string-constant map-delete-to-backspace)
                                   not not)
                       (make-check editor-panel 'framework:show-status-line (string-constant show-status-line) values values)
                       (make-check editor-panel 'framework:col-offsets (string-constant count-columns-from-one) values values)
                       (make-check editor-panel 
                                   'framework:display-line-numbers
                                   (string-constant display-line-numbers)
                                   values values)
                       
                       (make-check editor-panel 
                                   'framework:auto-set-wrap?
                                   (string-constant wrap-words-in-editor-buffers)
                                   values values)
                       (make-check editor-panel 
                                   'framework:search-using-dialog?
                                   (string-constant separate-dialog-for-searching)
                                   values values)
                       (make-check editor-panel 
                                   'framework:open-here?
                                   (string-constant reuse-existing-frames)
                                   values values)
                       (make-check editor-panel 
                                   'framework:menu-bindings
                                   (string-constant enable-keybindings-in-menus)
                                   values values)
                       (unless (eq? (system-type) 'unix) 
                         (make-check editor-panel 
                                     'framework:print-output-mode 
                                     (string-constant automatically-to-ps)
                                     (lambda (b) 
                                       (if b 'postscript 'standard))
                                     (lambda (n) (eq? 'postscript n))))
                       (editor-panel-procs editor-panel))))])
          (add-editor-checkbox-panel)))

      (define (add-warnings-checkbox-panel)
        (letrec ([add-warnings-checkbox-panel
                  (lambda ()
                    (set! add-warnings-checkbox-panel void)
                    (add-checkbox-panel 
                     (string-constant warnings-prefs-panel-label)
                     (lambda (warnings-panel)
                       (make-check warnings-panel 
                                   'framework:verify-change-format 
                                   (string-constant ask-before-changing-format)
                                   values values)
                       (make-check warnings-panel 
                                   'framework:verify-exit
                                   (string-constant verify-exit)
                                   values values)
                       
                       (warnings-panel-procs warnings-panel))))])
          (add-warnings-checkbox-panel)))
                  
      (define (local-add-font-panel)
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
                                    [(macosx) 13]
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
           (string-constant default-fonts)
           (lambda (parent)
             (letrec ([font-size-pref-sym (build-font-preference-symbol font-size-entry)]
                      [ex-string (string-constant font-example-string)]
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
                                   (string-constant change-font-button-label)
                                   horiz
                                   (lambda (button evt)
                                     (let ([new-value
                                            (get-choices-from-user
                                             (string-constant fonts)
                                             (format (string-constant choose-a-new-font)
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
                         (let ([width (foldl (lambda (x l) (max l (x))) 0 gets)])
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
                         (string-constant font-size-slider-label)
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
               (make-object message% (string-constant restart-to-see-font-changes) main)
               main))))
        (set! local-add-font-panel void))
      
      (define (add-font-panel) (local-add-font-panel))
      
      (define preferences-dialog #f)
      
      (define add-panel
        (lambda (title container)
	  (unless (and (string? title)
		       (procedure? container)
		       (procedure-arity-includes? container 1))
	    (error 'preferences:add-panel
		   "expected a string and a function that can accept one argument, got ~e and ~e"
		   title container))
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
          (letrec ([stashed-prefs (get-preference main-preferences-symbol (lambda () null))]
                   [frame 
                    (make-object (class100 frame% args
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
                                                             (sub1 (length ppanels))))))))])
                                   (sequence
                                     (apply super-init args)))
                      (string-constant preferences))]
                   [panel (make-object vertical-panel% frame)]
                   [popup-callback
                    (lambda (choices-canvas selection)
                      (unless (null? ppanels)
                        (send single-panel active-child
                              (ppanel-panel (list-ref ppanels selection)))))]
                   [make-popup-menu 
                    (lambda ()
                      (let ([menu (instantiate gui-utils:choices-canvas% ()
                                    (choices (map ppanel-title ppanels))
				    (parent panel)
				    (callback popup-callback))])
                        (send menu stretchable-width #f)
                        menu))]
                   [popup-menu (make-popup-menu)]
                   [single-panel (make-object panel:single% panel)]
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
                   [cancel-callback (lambda (_1 _2)
                                      (hide-dialog)
                                      (install-stashed-preferences stashed-prefs))])
            (gui-utils:ok/cancel-buttons
             bottom-panel
             ok-callback
             cancel-callback)
            (make-object grow-box-spacer-pane% bottom-panel)
            (send* bottom-panel
              (stretchable-height #f)
              (set-alignment 'right 'center))
            (ensure-constructed)
            (unless (null? ppanels)
              (send popup-menu set-selection 0))
            (send popup-menu focus)
            (send frame show #t)
            frame))))))
