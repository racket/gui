(module color-prefs mzscheme
  (require (lib "class.ss")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           "sig.ss")
 
  (provide color-prefs@)
  
  (define sc-color-syntax-interactively "Color syntax interactively")
  (define sc-syntax-coloring "Syntax Coloring")
  (define sc-choose-color "Choose a color for ~a")
  
  (define color-prefs@
    (unit/sig framework:color-prefs^
      (import [preferences : framework:preferences^]
              [editor : framework:editor^]
              [panel : framework:panel^])
      
      (define standard-style-list-text% (editor:standard-style-list-mixin text%))
      
      ;; build-color-selection-panel : (is-a?/c area-container<%>) symbol string string -> void
      ;; constructs a panel containg controls to configure the preferences panel.
      ;; BUG: style changes don't update the check boxes.
      (define (build-color-selection-panel parent pref-sym style-name example-text)
        (define hp (new horizontal-panel% 
                        (parent parent)
                        (style '(border))
                        (stretchable-height #f)))
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
        (define c (new editor-canvas%
                       (parent hp)
                       (editor e)
                       (style '(hide-hscroll
                                hide-vscroll))))
        
        (define delta (preferences:get pref-sym))
        (define (make-check name on off)
          (let* ([c (lambda (check command)
                      (if (send check get-value)
                          (on)
                          (off))
                      (preferences:set pref-sym delta))]
                 [check (make-object check-box% name hp c)])
            check))
        
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
                 hp
                 (lambda (color-button evt)
                   (let* ([add (send delta get-foreground-add)]
                          [color (make-object color%
                                   (send add get-r)
                                   (send add get-g)
                                   (send add get-b))]
                          [users-choice
                           (get-color-from-user
                            (format sc-choose-color example-text)
                            (send color-button get-top-level-window)
                            color)])
                     (when users-choice
                       (send delta set-delta-foreground users-choice)
                       (preferences:set pref-sym delta)))))))
        (define style (send (send e get-style-list) find-named-style style-name))
        
        (send c set-line-count 1)
        (send c allow-tab-exit #t)
        
        (send e insert example-text)
        (send e set-position 0)
        
        (send slant-check set-value (eq? (send style get-style) 'slant))
        (send bold-check set-value (eq? (send style get-weight) 'bold))
        (send underline-check set-value (send style get-underlined)))
      
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
      
      
      ;; prefs-panel-mapping : (union #f 
      ;;                              hash-table[symbol -o> (is-a?/c vertical-panel)])
      ;; #f => prefs panel not yet opened
      ;; hash-table => prefs panel opened
      ;;   the table maps from the name of the color preference to
      ;;   the corresponding subpanel
      (define prefs-panel-mapping #f)
      
      ;; prefs-panel-todo : (union #f (listof (cons string (vertical-panel -> void))))
      ;; list => prefs panel not opened yet
      ;; #f => prefs panel already opened.
      (define prefs-panel-todo '())
      
      ;; prefs-panel-tab-panel : (union #f tab-panel)
      (define prefs-panel-tab-panel #f)
      ;; prefs-panel-single : (union #f single-panel%))
      (define prefs-panel-single #f)
      
      (define prefs-panel-children '())
      
      ;; update-panel-single : -> void
      ;; callback for the prefs-panel-tab-panel
      (define (update-panel-single)
        (let ([sel (send prefs-panel-tab-panel get-selection)])
          (when sel
            (let* ([label (list-ref prefs-panel-children sel)]
                   [panel (hash-table-get prefs-panel-mapping (string->symbol label))])
              (send prefs-panel-single active-child panel)))))
      
      ;; add-preferences-panel : -> void
      ;; calls preferences:add-panel to add the coloring configuration panels
      (define (add-preferences-panel)
        (preferences:add-panel
         (list sc-syntax-coloring)
         (lambda (parent)
           (set! prefs-panel-tab-panel (new tab-panel% 
                                            (parent parent)
                                            (choices (map car prefs-panel-todo))
                                            (callback (lambda (x y) (update-panel-single)))))
           (set! prefs-panel-single (new panel:single% (parent prefs-panel-tab-panel)))
           (set! prefs-panel-mapping (make-hash-table))
           (for-each
            (lambda (pr)
              (let* ([name (car pr)]
                     [proc (cdr pr)]
                     [panel (build-new-prefs-panel name)])
                (proc panel)))
            prefs-panel-todo)
           (set! prefs-panel-todo #f)
           prefs-panel-tab-panel)))
      
      ;; add-to-preferences-panel : string (vertical-panel -> void) -> void
      (define (add-to-preferences-panel panel-name func)
        (let ([key (string->symbol panel-name)])
          (cond
            [prefs-panel-todo
             (let ([prev-pr (assq key prefs-panel-todo)])
               (if prev-pr
                   (let ([proc (cdr prev-pr)])
                     (set-cdr! prev-pr 
                               (lambda (parent)
                                 (proc parent)
                                 (func parent))))
                   (set! prefs-panel-todo
                         (append prefs-panel-todo (list (cons panel-name func))))))]
            [else
             (let ([prev-panel (hash-table-get prefs-panel-mapping key (lambda () #f))])
               (cond
                 [prev-panel (func prev-panel)]
                 [else
                  (send prefs-panel-tab-panel append panel-name)
                  (let ([new-panel (build-new-prefs-panel panel-name)])
                    (func new-panel))]))])))
      
      ;; build-new-prefs-panel : string -> horizontal-panel
      (define (build-new-prefs-panel name)
        (let ([panel (new vertical-panel% (parent prefs-panel-single))])
          (set! prefs-panel-children (append prefs-panel-children (list name)))
          (hash-table-put! prefs-panel-mapping (string->symbol name) panel)
          panel))
      
      ;; see docs
      (define (register-color-pref pref-name style-name color)
        (let ([sd (new style-delta%)])
          (send sd set-delta-foreground color)
          (preferences:set-default pref-name sd (lambda (x) (is-a? x style-delta%))))
        (preferences:set-un/marshall pref-name marshall-style unmarshall-style)
        (preferences:add-callback pref-name
                                  (lambda (sym v)
                                    (editor:set-standard-style-list-delta style-name v)))
        (editor:set-standard-style-list-delta style-name (preferences:get pref-name))))))

