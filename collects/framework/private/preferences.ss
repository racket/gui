
(module preferences mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
           (lib "file.ss")
	   (lib "etc.ss")
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
              [panel : framework:panel^]
              [frame : framework:frame^])
      
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
      
      ;; changed : hash-table[symbol -o> true]
      ;; the mapped symbols are the ones that have changed
      ;; but not yet written out to disk.
      (define changed (make-hash-table))
      
      ;; no-more-defaults? : boolean
      ;; when #t, no more default prefs may be set.
      (define no-more-defaults? #f)
      
      (define-struct un/marshall (marshall unmarshall))
      (define-struct marshalled (data))
      (define-struct pref (value))
      (define-struct default (value checker))

      ;; reset-changed : -> void
      ;; resets the changed table to indicate no changes have occurred
      (define (reset-changed) 
        (set! changed (make-hash-table)))
      
      ;; add-changed-pref : symbol -> void
      ;; marks the pref p as changed
      (define (add-changed-pref p)
        (hash-table-put! changed p #t))
      
      ;; periodically checks to see if changes need to be written out.
      (define (start-writing-timer)
        (set! no-more-defaults? #t)
	;; don't actually start the timer anymore.
	;; turns out that preferences are set far too often
	;; and I need a better test for when things should be written out.
        '(new timer%
             [notify-callback (lambda () (maybe-flush-changes))]
             [interval (* 10 1000)])
        (void))
      
      (define last-time-read #f)
      (define (maybe-flush-changes)
        
        ;; writing out changes
        (let ([changed-syms (hash-table-map changed (lambda (k v) k))])
          (unless (null? changed-syms)
            (let/ec k
              (let ([sexp (get-disk-prefs (lambda () (k #f)))])
                (install-stashed-preferences sexp changed-syms)
                (raw-save #t)
                (reset-changed)))))
        
        ;; reading in changes
        (let* ([filename (find-system-path 'pref-file)])
	  (when (file-exists? filename)
	    (let ([mod (file-or-directory-modify-seconds filename)])
	      (when (or (not last-time-read)
			(last-time-read . < . mod))
		(let* ([failed? #f]
		       [new-stuff (get-preference main-preferences-symbol (lambda () (set! failed? #t)))])
		  (unless failed?
		    (set! last-time-read mod)
		    (install-stashed-preferences new-stuff '())
		    (reset-changed))))))))

      (define guard
        (lambda (when p value thunk)
          (with-handlers ([not-break-exn? 
                           (lambda (exn)
                             (error "excetion raised ~s, pref ~s val ~s, msg: ~a"
                                    when
                                    p
                                    value
                                    (if (exn? exn)
                                        (exn-message exn)
                                        (format "~s" exn))))])
            (thunk))))
      
      (define (unmarshall p marshalled)
        (let/ec k
          (let* ([data (marshalled-data marshalled)]
                 [unmarshall-fn (un/marshall-unmarshall
                                 (hash-table-get marshall-unmarshall
                                                 p
                                                 (lambda () (k data))))])
            (with-handlers ([not-break-exn?
                             (lambda (exn)
                               (begin0
                                 (hash-table-get defaults p (lambda () (raise exn)))
                                 (message-box (format (string-constant error-unmarshalling) p)
                                              (if (exn? exn)
                                                  (format "~a" (exn-message exn))
                                                  (format "~s" exn)))))])
              (unmarshall-fn data)))))
      
      ;; pref-callback : (make-pref-callback (union (weak-box (sym tst -> void)) (sym tst -> void)))
      ;; this is used as a wrapped to hack around the problem
      ;; that different procedures might be eq?.
      (define-struct pref-callback (cb))

      ;; add-callback : sym (-> void) -> void
      (define add-callback 
        (opt-lambda (p callback [weak? #f])
          (let ([new-cb (make-pref-callback (if weak?
                                                (make-weak-box callback)
                                                callback))])
            (hash-table-put! callbacks
                             p 
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
                         (cons (car callbacks) (loop (cdr callbacks)))]))])))))))
      
      ;; check-callbacks : sym val -> void
      (define (check-callbacks p value)
        (let ([new-callbacks
               (let loop ([callbacks (hash-table-get callbacks p (lambda () null))])
                 (cond
                   [(null? callbacks) null]
                   [else 
                    (let* ([callback (car callbacks)]
                           [cb (pref-callback-cb callback)])
                      (cond
                        [(weak-box? cb)
                         (let ([v (weak-box-value cb)])
                           (if v
                               (begin 
                                 (guard "calling callback" p value
                                        (lambda () (v p value)))
                                 (cons callback (loop (cdr callbacks))))
                               (loop (cdr callbacks))))]
                        [else
                         (guard "calling callback" p value
                                (lambda () (cb p value)))
                         (cons callback (loop (cdr callbacks)))]))]))])
          (if (null? new-callbacks)
              (hash-table-remove! callbacks p)
              (hash-table-put! callbacks p new-callbacks))))
      
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
      
      ;; set : symbol any -> void
      ;; updates the preference
      (define (set p value)
        (add-changed-pref p)
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
        (when no-more-defaults?
          (error 'set-default "tried to register the pref ~e too late; preferences:start-writing-timer has already been called" p))
        (let ([default-okay? (checker default-value)])
          (unless default-okay?
            (error 'set-default "~s: checker (~s) returns ~s for ~s, expected #t~n"
                   p checker default-okay? default-value))
          (hash-table-get preferences p 
                          (lambda ()
                            (hash-table-put! preferences p (make-pref default-value))))
          (hash-table-put! defaults p (make-default default-value checker))))
      
      (define (save) (raw-save #f))
      
      ;; raw-save : boolean -> boolean
      ;; input determines if there is a dialog box showing the errors (and other msgs)
      ;; and result indicates if there was an error
      (define (raw-save silent?)
        (with-handlers ([not-break-exn?
                         (lambda (exn)
                           (unless silent?
                             (message-box
                              (string-constant preferences)
                              (format (string-constant error-saving-preferences)
                                      (exn-message exn))))
                           #f)])
          (let ([syms (list main-preferences-symbol)]
                [vals (list (hash-table-map preferences marshall-pref))]
                [res #t])
            (put-preferences
             syms vals
             (lambda (filename)
               (unless silent?
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
                       (format (string-constant pref-lock-not-gone) filename))))))))
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
                               value))))])
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
      (define (-read) (get-disk-prefs/install void))
      
      ;; get-disk-prefs/install : (-> A) -> (union A sexp)
      (define (get-disk-prefs/install fail)
        (let/ec k
          (let ([sexp (get-disk-prefs (lambda () (k (fail))))])
            (install-stashed-preferences sexp '())
            (reset-changed)
            sexp)))

      ;; get-disk-prefs : (-> A) -> (union A sexp)
      ;; effect: updates the flag for the modified seconds
      ;; (note: if this is not followed by actually installing 
      ;;        the preferences, things break)
      (define (get-disk-prefs fail)
        (let/ec k
          (let* ([filename (find-system-path 'pref-file)]
		 [mod (and (file-exists? filename) (file-or-directory-modify-seconds filename))]
		 [sexp (get-preference main-preferences-symbol (lambda () (k (fail))))])
            (set! last-time-read mod)
            sexp)))
      
      ;; install-stashed-preferences : sexp (listof symbol) -> void
      ;; ensure that `prefs' is actuall a well-formed preferences
      ;; table and installs them as the current preferences.
      (define (install-stashed-preferences prefs skip)
        (for-each-pref-in-sexp
         prefs
         (lambda (p marshalled)
           (unless (memq p skip)
             (add-raw-pref-to-ht preferences p marshalled)))))
 
                                          
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
      
      
      ;; ppanel-tree = 
      ;;  (union (make-ppanel-leaf string (union #f panel) (panel -> panel))
      ;;         (make-ppanel-interior string (union #f panel) (listof panel-tree)))
      (define-struct ppanel (name panel))
      (define-struct (ppanel-leaf ppanel) (maker))
      (define-struct (ppanel-interior ppanel) (children))
      
      ;; ppanels : (listof ppanel-tree)
      (define ppanels null)
      
      (define preferences-dialog #f)
      
      (define (add-panel title make-panel)
        (when preferences-dialog
          (error 'add-panel "preferences dialog already open, cannot add new panels"))
        (let ([titles (if (string? title)
                          (list title)
                          title)])
          (add-to-existing-children
           titles 
           make-panel
           (lambda (new-subtree) (set! ppanels (cons new-subtree ppanels))))))
      
      ;; add-to-existing-children : (listof string) (panel -> panel) (ppanel -> void)
      ;; adds the child specified by the path in-titles to the tree.
      (define (add-to-existing-children in-titles make-panel banger)
        (let loop ([children ppanels]
                   [title (car in-titles)]
                   [titles (cdr in-titles)]
                   [banger banger])
          (cond
            [(null? children)
             (banger (build-new-subtree (cons title titles) make-panel))]
            [else
             (let ([child (car children)])
               (if (string=? (ppanel-name child) title)
                   (cond
                     [(null? titles) 
                      (error 'add-child "child already exists with this path: ~e" in-titles)]
                     [(ppanel-leaf? child)
                      (error 'add-child "new child's path conflicts with existing path: ~e" in-titles)]
                     [else
                      (loop
                       (ppanel-interior-children child)
                       (car titles)
                       (cdr titles)
                       (lambda (x)
                         (set-ppanel-interior-children! 
                          (cons
                           x
                           (ppanel-interior-children child)))))])
                   (loop 
                    (cdr children)
                    title
                    titles
                    (lambda (x)
                      (set-cdr! children
                                (cons x (cdr children)))))))])))
      
      ;; build-new-subtree : (cons string (listof string)) (panel -> panel) -> ppanel
      (define (build-new-subtree titles make-panel)
        (let loop ([title (car titles)]
                   [titles (cdr titles)])
          (cond
            [(null? titles) (make-ppanel-leaf title #f make-panel)]
            [else
             (make-ppanel-interior 
              title
              #f
              (list (loop (car titles) (cdr titles))))])))
           
      
      (define (hide-dialog)
	(when preferences-dialog
	  (send preferences-dialog show #f)))
      
      (define (show-dialog)
	(save)
	(if preferences-dialog
	    (send preferences-dialog show #t)
	    (set! preferences-dialog
		  (make-preferences-dialog))))

      (define (add-can-close-dialog-callback cb)
	(set! can-close-dialog-callbacks
	      (cons cb can-close-dialog-callbacks)))

      (define (add-on-close-dialog-callback cb)
	(set! on-close-dialog-callbacks
	      (cons cb on-close-dialog-callbacks)))

      (define on-close-dialog-callbacks null)

      (define can-close-dialog-callbacks null)
      
      (define (make-preferences-dialog)
        (letrec ([stashed-prefs (get-disk-prefs/install (lambda () null))]
		 [frame-stashed-prefs%
		  (class frame:basic%
		    (rename [super-show show])
                    (define/override (show on?)
                      (when on?
                        (set! stashed-prefs (get-disk-prefs/install (lambda () null))))
		      (super-show on?))
		    (super-instantiate ()))]
                 [frame 
                  (make-object frame-stashed-prefs%
                    (string-constant preferences))]
                 [build-ppanel-tree
                  (lambda (ppanel tab-panel single-panel)
                    (send tab-panel append (ppanel-name ppanel))
                    (cond
                      [(ppanel-leaf? ppanel) 
                       ((ppanel-leaf-maker ppanel) single-panel)]
                      [(ppanel-interior? ppanel)
                       (let-values ([(tab-panel single-panel) (make-tab/single-panel single-panel #t)])
                         (for-each
                          (lambda (ppanel) (build-ppanel-tree ppanel tab-panel single-panel))
                          (ppanel-interior-children ppanel)))]))]
                 [make-tab/single-panel 
                  (lambda (parent inset?)
                    (letrec ([spacer (and inset?
                                          (instantiate vertical-panel% ()
                                            (parent parent)
                                            (border 10)))]
                             [tab-panel (instantiate tab-panel% ()
                                          (choices null)
                                          (parent (if inset? spacer parent))
                                          (callback (lambda (_1 _2) 
                                                      (tab-panel-callback
                                                       single-panel
                                                       tab-panel))))]
                             [single-panel (instantiate panel:single% ()
                                             (parent tab-panel))])
                      (values tab-panel single-panel)))]
                 [tab-panel-callback
                  (lambda (single-panel tab-panel)
                    (send single-panel active-child
                          (list-ref (send single-panel get-children)
                                    (send tab-panel get-selection))))]
                 [panel (make-object vertical-panel% (send frame get-area-container))]
                 [_ (let-values ([(tab-panel single-panel) (make-tab/single-panel panel #f)])
                      (for-each
                       (lambda (ppanel)
                         (build-ppanel-tree ppanel tab-panel single-panel))
                       ppanels)
                      (let ([single-panel-children (send single-panel get-children)])
                        (unless (null? single-panel-children)
                          (send single-panel active-child (car single-panel-children))
                          (send tab-panel set-selection 0)))
                      (send tab-panel focus))]
                 [bottom-panel (make-object horizontal-panel% panel)]
                 [ok-callback (lambda args
                                (when (andmap (lambda (f) (f))
                                              can-close-dialog-callbacks)
                                  (for-each
                                   (lambda (f) (f))
                                   on-close-dialog-callbacks)
                                  (save)
                                  (hide-dialog)))]
                 [cancel-callback (lambda (_1 _2)
                                    (hide-dialog)
                                    (install-stashed-preferences stashed-prefs '()))])
          (gui-utils:ok/cancel-buttons
           bottom-panel
           ok-callback
           cancel-callback)
          (make-object grow-box-spacer-pane% bottom-panel)
          (send* bottom-panel
            (stretchable-height #f)
            (set-alignment 'right 'center))
          (send frame show #t)
          frame))
      
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
                     (list 
                      (string-constant editor-prefs-panel-label) 
                      (string-constant scheme-prefs-panel-label))
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
                       (scheme-panel-procs scheme-panel)
                       (make-highlight-color-choice scheme-panel))))])
          (add-scheme-checkbox-panel)))
      
      (define (make-highlight-color-choice panel)
        (let* ([hp (instantiate horizontal-panel% ()
                     (parent panel)
                     (stretchable-height #f))]
               [msg (make-object message% (string-constant paren-match-color) hp)]
               [scheme-higlight-canvas (make-object scheme-highlight-canvas% hp)]
               [button (make-object button% 
                         (string-constant choose-color) 
                         hp
                         (lambda (x y) (change-highlight-color panel)))])
          (void)))
      
      (define scheme-highlight-canvas%
        (class canvas%
          (inherit get-client-size get-dc)
          (define/override (on-paint)
            (do-draw (get 'framework:paren-match-color)))
          (define/public (do-draw color)
            (let ([dc (get-dc)])
              (send dc set-pen (send the-pen-list find-or-create-pen
                                     color
                                     1
                                     'solid))
              (send dc set-brush (send the-brush-list find-or-create-brush
                                       color 
                                       'solid))
              (let-values ([(w h) (get-client-size)])
                (send dc draw-rectangle 0 0 w h))))
          (super-instantiate ())
          (inherit stretchable-width min-width)
          (add-callback
           'framework:paren-match-color
           (lambda (p v)
             (do-draw v)))))
      
      (define (change-highlight-color parent)
        (let ([new-color
               (get-color-from-user (string-constant choose-paren-highlight-color)
                                    (send parent get-top-level-window)
                                    (get 'framework:paren-match-color))])
          (when new-color
            (set 'framework:paren-match-color new-color))))
      
      (define (add-editor-checkbox-panel)
        (letrec ([add-editor-checkbox-panel
                  (lambda ()
                    (set! add-editor-checkbox-panel void)
                    (add-checkbox-panel 
                     (list (string-constant editor-prefs-panel-label) 
                           (string-constant general-prefs-panel-label))
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
                       (make-check editor-panel 
                                   'framework:coloring-active
                                   (string-constant online-coloring-active)
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
      
      (define (add-font-panel) (local-add-font-panel)))))
