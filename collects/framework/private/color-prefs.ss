(module color-prefs mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           "sig.ss")
           
  
  (provide color-prefs@)
  
  (define color-prefs@
    (unit/sig framework:color-prefs^
      (import [preferences : framework:preferences^]
              [editor : framework:editor^])
      
      (define standard-style-list-text% (editor:standard-style-list-mixin text%))

      (define color-selection%
        (class horizontal-panel%
          (init symbol prefix)
          (super-instantiate () (style '(border)))
          
          (define sym (string->symbol (format "~a:~a" prefix symbol)))
          
          (define delta (preferences:get sym))
          (define style-name (symbol->string sym))
          (define c (make-object editor-canvas% this
                      #f
                      (list 'hide-hscroll
                            'hide-vscroll)))
          (send c set-line-count 1)
          (send c allow-tab-exit #t)
          (define e (new (class standard-style-list-text%
                           (inherit change-style get-style-list)
                           (rename [super-after-insert after-insert])
                           (override after-insert)
                           (define (after-insert pos offset)
                             (super-after-insert pos offset)
                             (let ([style (send (get-style-list)
                                                find-named-style
                                                style-name)])
                               (change-style style pos (+ pos offset) #f)))
                           (super-instantiate ()))))
          (preferences:add-callback sym
                                    (lambda (sym v)
                                      (set-slatex-style sym v)
                                      #t))
          (set-slatex-style sym delta)
          (define (make-check name on off)
            (let* ([c (lambda (check command)
                        (if (send check get-value)
                            (on)
                            (off))
                        (preferences:set sym delta))]
                   [check (make-object check-box% name this c)])
              check))
          (send c set-editor e)
          (send* e
            (insert (symbol->string symbol))
            (set-position 0))
          (define slant-check
            (make-check (string-constant cs-italic)
                        (lambda ()
                          (send delta set-style-on 'slant)
                          (send delta set-style-off 'base))
                        (lambda ()
                          (send delta set-style-on 'base)
                          (send delta set-style-off 'slant))))
          (define bold-check
            (make-check (string-constant cs-bold)
                        (lambda ()
                          (send delta set-weight-on 'bold)
                          (send delta set-weight-off 'base))
                        (lambda ()
                          (send delta set-weight-on 'base)
                          (send delta set-weight-off 'bold))))
          (define underline-check
            (make-check (string-constant cs-underline)
                        (lambda ()
                          (send delta set-underlined-on #t)
                          (send delta set-underlined-off #f))
                        (lambda ()
                          (send delta set-underlined-off #t)
                          (send delta set-underlined-on #f))))
          (define color-button
            (and (>= (get-display-depth) 8)
                 (make-object button%
                   (string-constant cs-change-color)
                   this
                   (lambda (color-button evt)
                     (let* ([add (send delta get-foreground-add)]
                            [color (make-object color%
                                     (send add get-r)
                                     (send add get-g)
                                     (send add get-b))]
                            [users-choice
                             (get-color-from-user
                              (format "Choose a color for ~a"
                                      (symbol->string symbol))
                              (send color-button get-top-level-window)
                              color)])
                       (when users-choice
                         (send delta set-delta-foreground users-choice)
                         (preferences:set sym delta)))))))
          (define style (send (send e get-style-list) find-named-style style-name))
          (send slant-check set-value (eq? (send style get-style) 'slant))
          (send bold-check set-value (eq? (send style get-weight) 'bold))
          (send underline-check set-value (send style get-underlined))))
      
      (define (add/mult-set m v)
        (send m set (car v) (cadr v) (caddr v)))
      
      (define (add/mult-get m)
        (let ([b1 (box 0)]
              [b2 (box 0)]
              [b3 (box 0)])
          (send m get b1 b2 b3)
          (map unbox (list b1 b2 b3))))
      
      (define style-delta-get/set
        (list (cons (lambda (x) (send x get-alignment-off))
                    (lambda (x v) (send x set-alignment-off v)))
              (cons (lambda (x) (send x get-alignment-on))
                    (lambda (x v) (send x set-alignment-on v)))
              (cons (lambda (x) (add/mult-get (send x get-background-add)))
                    (lambda (x v) (add/mult-set (send x get-background-add) v)))
              (cons (lambda (x) (add/mult-get (send x get-background-mult)))
                    (lambda (x v) (add/mult-set (send x get-background-mult) v)))
              (cons (lambda (x) (send x get-face))
                    (lambda (x v) (send x set-face v)))
              (cons (lambda (x) (send x get-family))
                    (lambda (x v) (send x set-family v)))
              (cons (lambda (x) (add/mult-get (send x get-foreground-add)))
                    (lambda (x v) (add/mult-set (send x get-foreground-add) v)))
              (cons (lambda (x) (add/mult-get (send x get-foreground-mult)))
                    (lambda (x v) (add/mult-set (send x get-foreground-mult) v)))
              (cons (lambda (x) (send x get-size-add))
                    (lambda (x v) (send x set-size-add v)))
              (cons (lambda (x) (send x get-size-mult))
                    (lambda (x v) (send x set-size-mult v)))
              (cons (lambda (x) (send x get-style-off))
                    (lambda (x v) (send x set-style-off v)))
              (cons (lambda (x) (send x get-style-on))
                    (lambda (x v) (send x set-style-on v)))
              (cons (lambda (x) (send x get-underlined-off))
                    (lambda (x v) (send x set-underlined-off v)))
              (cons (lambda (x) (send x get-underlined-on))
                    (lambda (x v) (send x set-underlined-on v)))
              (cons (lambda (x) (send x get-weight-off))
                    (lambda (x v) (send x set-weight-off v)))
              (cons (lambda (x) (send x get-weight-on))
                    (lambda (x v) (send x set-weight-on v)))))
      
      (define (marshall-style style)
        (map (lambda (fs) ((car fs) style)) style-delta-get/set))
      
      (define (unmarshall-style info)
        (let ([style (make-object style-delta%)])
          (for-each (lambda (fs v) ((cdr fs) style v)) style-delta-get/set info)
          style))
      
      (define (set-default sym code-style)
        (preferences:set-default
         sym
         code-style
         (lambda (x)
           (is-a? x style-delta%))))
      
      ; a symbol naming the style  and a delta to set it to
      (define (set-slatex-style sym delta)
        (let* ([style-list (editor:get-standard-style-list)]
               [name (symbol->string sym)]
               [style (send style-list find-named-style name)])
          (if style
              (send style set-delta delta)
              (send style-list new-named-style name
                    (send style-list find-or-create-style
                          (send style-list find-named-style "Standard")
                          delta)))))
      
      
      (define (make-style-delta color bold? underline? italic?)
        (let ((sd (make-object style-delta%)))
          (send sd set-delta-foreground color)
          (cond
            (bold?
             (send sd set-weight-on 'bold)
             (send sd set-weight-off 'base))
            (else
             (send sd set-weight-on 'base)
             (send sd set-weight-off 'bold)))
          (send sd set-underlined-on underline?)
          (send sd set-underlined-off (not underline?))
          (cond
            (italic?
             (send sd set-style-on 'italic)
             (send sd set-style-off 'base))
            (else
             (send sd set-style-on 'base)
             (send sd set-style-off 'italic)))
          sd))
      
      
      (define color-selection-panel%
        (class vertical-panel%
          (init symbols prefix)
          
          (super-instantiate ())
          
          (for-each
           (lambda (s)
             (new color-selection% (prefix prefix) (symbol s) (parent this)))
           symbols)
          ))
      
      (define (add-staged tab-name symbols/defaults)
        (let* ((prefix (string->symbol (format "syntax-coloring:~a" tab-name)))
               (active-pref (string->symbol (format "~a:active" prefix)))
               (syms (map (lambda (s/d) (string->symbol (format "~a:~a" prefix (car s/d))))
                          symbols/defaults)))
          (for-each set-default syms (map cadr symbols/defaults))
          (for-each (lambda (s)
                      (preferences:set-un/marshall s marshall-style unmarshall-style))
                    syms)
          (for-each set-slatex-style syms (map preferences:get syms))
          (preferences:set-default active-pref #t (lambda (x) #t))
          (lambda ()
            (preferences:add-panel `("Syntax Coloring" ,tab-name)
                                   (lambda (p)
                                     (let ((vp (new vertical-panel% (parent p))))
                                       (new color-selection-panel%
                                            (parent vp)
                                            (prefix prefix)
                                            (symbols (map car symbols/defaults)))
                                       (let ((cb (new check-box%
                                                      (parent vp)
                                                      (label "Color syntax interactively")
                                                      (callback (lambda (checkbox y)
                                                                  (preferences:set 
                                                                   active-pref
                                                                   (send checkbox get-value)))))))
                                         (send cb set-value (preferences:get active-pref)))
                                       vp)))
            (preferences:add-callback active-pref
                                      (lambda (_ on?)
                                        (do-active-pref-callbacks active-pref on?))))))
      
      (define (add tab-name symbols/defaults)
        ((add-staged tab-name symbols/defaults)))
       
      
      
      ;; The following 4 defines are a mini-prefs system that uses a weak hash table
      ;; so the preferences won't hold on to a text when it should otherwise be GCed.
      (define active-pref-callback-table (make-hash-table))
            
      (define (do-active-pref-callbacks pref-sym on?)
        (hash-table-for-each (hash-table-get active-pref-callback-table pref-sym (lambda () (make-hash-table)))
                             (lambda (k v)
                               (v k on?))))
      
      (define (remove-active-pref-callback pref-sym k)
        (let ((ht (hash-table-get active-pref-callback-table pref-sym (lambda () #f))))
          (when ht
            (hash-table-remove! ht k))))
      
      (define (register-active-pref-callback pref-sym k v)
        (hash-table-put! (hash-table-get active-pref-callback-table pref-sym
                                         (lambda ()
                                           (let ((ht (make-hash-table 'weak)))
                                             (hash-table-put! active-pref-callback-table pref-sym ht)
                                             ht)))
                         k v)))))

