#|

need to put all state into snipclass saveable form. yuck.

|#

(module syntax-browser mzscheme
  (require (lib "pretty.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide render-syntax/snip render-syntax/window)
  
  (define (render-syntax/window syntax)
    (let ([es (render-syntax/snip syntax)])
      (define f (make-object frame% "frame" #f 400 400))
      (define t (make-object text%))
      (define ec (make-object editor-canvas% f t))
      (send t insert es)
      (send f show #t)))

  (define (render-syntax/snip main-stx)
    (define-values (datum stx-ht) (syntax-object->datum/ht main-stx))
    
    (define output-text (make-object text%))
    (define output-port (make-text-port output-text))
    (define info-text (make-object text%))
    (define info-port (make-text-port info-text))
    
    ;; assume that there aren't any eq? sub structures, only eq? flat stuff (symbols, etc)
    ;; this is guaranteed by syntax-object->datum/ht
    (define range-start-ht (make-hash-table))
    (define range-ht (make-hash-table))
    (define original-output-port (current-output-port))
    (define (range-pretty-print-pre-hook x v)
      (hash-table-put! range-start-ht x (send output-text last-position)))
    (define (range-pretty-print-post-hook x v)
      (hash-table-put! range-ht x 
                       (cons
                        (cons
                         (hash-table-get range-start-ht x)
                         (send output-text last-position))
                        (hash-table-get range-ht x (lambda () null)))))
    
    (define (make-modern text)
      (send text change-style
            (make-object style-delta% 'change-family 'modern)
            0
            (send text last-position)))
    
    (define dummy
      (begin (parameterize ([current-output-port output-port]
                            [pretty-print-pre-print-hook range-pretty-print-pre-hook]
                            [pretty-print-post-print-hook range-pretty-print-post-hook]
                            [pretty-print-columns 30])
               (pretty-print datum))
             (make-modern output-text)))
    
    (define ranges 
      (quicksort 
       (apply append (hash-table-map range-ht (lambda (k vs) (map (lambda (v) (cons k v)) vs))))
       (lambda (x y)
         (<= (- (car (cdr x)) (cdr (cdr x)))
             (- (car (cdr y)) (cdr (cdr y)))))))
    
    (define (show-info stx)
      (insert/big "General Info\n")
      (piece-of-info "Source" (syntax-source stx))
      (piece-of-info "Source Module" (syntax-source-module stx))
      (piece-of-info "Position" (syntax-position stx))
      (piece-of-info "Line" (syntax-line stx))
      (piece-of-info "Column" (syntax-column stx))
      (piece-of-info "Span" (syntax-span stx))
      (piece-of-info "Original?" (syntax-original? stx))
      (insert/big "Properties\n")
      (show-property stx 'bound-in-source)
      (show-property stx 'origin))
    
    (define (show-property stx prop)
      (piece-of-info (format "'~a" prop) (syntax-property stx prop)))
    
    (define (piece-of-info label info)
      (insert/bold label)
      (newline info-port)
      (print info info-port)
      (optional-newline)
      (newline info-port))
    
    (define (insert/bold str)
      (let ([pos (send info-text last-position)])
        (send info-text insert str 
              (send info-text last-position)
              (send info-text last-position))
        (send info-text change-style
              (make-object style-delta% 'change-bold)
               pos 
               (send info-text last-position))))
    
    (define (insert/big str)
      (let ([sd (make-object style-delta% 'change-bold)])
        (send sd set-delta-foreground "Navy")
        (let ([pos (send info-text last-position)])
          (send info-text insert str 
                (send info-text last-position)
                (send info-text last-position))
          (send info-text change-style
                sd
                pos 
                (send info-text last-position)))))
    
    (define (optional-newline)
      (unless (equal?
               (send info-text get-character (- (send info-text last-position) 1))
               #\newline)
        (send info-text insert "\n" (send info-text last-position))))
    
    (define (show-range stx start end)
      (send output-text begin-edit-sequence)
      (send output-text lock #f)
      (send output-text change-style black-style-delta 0 (send output-text last-position))
      (send output-text change-style green-style-delta start end)
      (send output-text lock #t)
      (send output-text end-edit-sequence)
      
      (send info-text begin-edit-sequence)
      (send info-text lock #f)
      (send info-text erase)
      (show-info stx)
      (make-modern info-text)
      (send info-text lock #t)
      (send info-text end-edit-sequence))
    
    (define outer-t (make-object text%))
    (define inner-t (make-object text%))
    (define outer-es (instantiate editor-snip% ()
                       (editor outer-t)
                       (with-border? #f)
                       (left-margin 3)
                       (top-margin 0)
                       (right-margin 0)
                       (bottom-margin 0)
                       (left-inset 1)
                       (top-inset 0)
                       (right-inset 0)
                       (bottom-inset 0)))
    (define inner-es (instantiate editor-snip% ()
                       (editor inner-t)
                       (with-border? #f)
                       (left-margin 0)
                       (top-margin 0)
                       (right-margin 0)
                       (bottom-margin 0)
                       (left-inset 0)
                       (top-inset 0)
                       (right-inset 0)
                       (bottom-inset 0)))
    
    (define details-shown? #t)
    
    (define (hide-details)
      (when details-shown?
        (send outer-t lock #f)
        (send outer-es show-border #f)
        (send outer-t release-snip inner-es)
        (send outer-t delete (send outer-t last-position))
        (send outer-t lock #t)
        (set! details-shown? #f)))
    
    (define (show-details)
      (unless details-shown?
        (send outer-t lock #f)
        (send outer-es show-border #t)
        (send outer-t insert #\newline
              (send outer-t last-position)
              (send outer-t last-position))
        (send outer-t insert inner-es
              (send outer-t last-position)
              (send outer-t last-position))
        (send outer-t lock #t)
        (set! details-shown? #t)))
    
    (for-each
     (lambda (range)
       (let* ([obj (car range)]
              [stx (hash-table-get stx-ht obj)]
              [start (cadr range)]
              [end (cddr range)])
         (when (syntax? stx)
           (send output-text set-clickback start end 
                 (lambda (_1 _2 _3)
                   (show-range stx start end))))))
     ranges)

    (send outer-t insert (make-object turn-snip% hide-details show-details))
    (send outer-t insert (format "~s\n" main-stx))
    (send outer-t insert inner-es)
    (make-modern outer-t)
    
    (send inner-t insert (make-object editor-snip% output-text))
    (send inner-t insert (make-object editor-snip% info-text))
    (send inner-t change-style (make-object style-delta% 'change-alignment 'top) 0 2)
    
    (send info-text auto-wrap #t)
    (send info-text set-styles-sticky #f)
    (let ([rng (car ranges)])
      (show-range (hash-table-get stx-ht (car rng))
                  (cadr rng)
                  (cddr rng)))
    
    (send output-text hide-caret #t)
    (send info-text hide-caret #t)
    (send inner-t hide-caret #t)
    (send outer-t hide-caret #t)
    (send output-text lock #t)
    (send info-text lock #t)
    (send inner-t lock #t)
    (send outer-t lock #t)
    
    (hide-details)
    
    outer-es)
  
  (define black-style-delta (make-object style-delta% 'change-normal-color))
  (define green-style-delta (make-object style-delta%))
  (send green-style-delta set-delta-foreground "forest green")

  (define turn-snip%
    (class snip%
      
      (init-field on-up on-down)
      
      ;; state : (union 'up 'down 'up-click 'down-click))
      (init-field [state 'up])
      
      (define/override (copy)
        (instantiate turn-snip% ()
          (on-up on-up)
          (on-down on-down)
          (state state)))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (let ([bitmap (case state
                        [(up) up-bitmap]
                        [(down) down-bitmap]
                        [(up-click) up-click-bitmap]
                        [(down-click) down-click-bitmap])])
          (cond
            [(send bitmap ok?)
             (send dc draw-bitmap bitmap x y)]
            [(send dc draw-rectangle x y 10 10)
             (send dc drawline x y 10 10)])))
             

      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f! descent 0)
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0)
        (set-box/f! w arrow-snip-width)
        (set-box/f! h arrow-snip-height))
      
      (rename [super-on-event on-event])
      (define/override (on-event dc x y editorx editory evt)
        (let ([snip-evt-x (- (send evt get-x) x)]
              [snip-evt-y (- (send evt get-y) y)])
          (cond
            [(send evt button-down? 'left)
             (set-state (case state
                          [(up) 'up-click]
                          [(down) 'down-click]
                          [else 'down-click]))]
            [(and (send evt button-up? 'left)
                  (<= 0 snip-evt-x arrow-snip-width)
                  (<= 0 snip-evt-y arrow-snip-height))
             (set-state (case state
                          [(up up-click) 
                           (on-down)
                           'down]
                          [(down down-click)
                           (on-up)
                           'up]
                          [else 'down]))]
            [(send evt button-up? 'left)
             (set-state (case state
                          [(up up-click) 'up]
                          [(down down-click) 'down]
                          [else 'up]))]
            [(and (send evt get-left-down)
                  (send evt dragging?)
                  (<= 0 snip-evt-x arrow-snip-width)
                  (<= 0 snip-evt-y arrow-snip-height))
             (set-state (case state
                          [(up up-click) 'up-click]
                          [(down down-click) 'down-click]
                          [else 'up-click]))]
            [(and (send evt get-left-down)
                  (send evt dragging?))
             (set-state (case state
                          [(up up-click) 'up]
                          [(down down-click) 'down]
                          [else 'up-click]))]
            [else
             (super-on-event dc x y editorx editory evt)])))

      (inherit get-admin)
      (define/private (set-state new-state)
        (unless (eq? state new-state)
          (set! state new-state)
          (let ([admin (get-admin)])
            (when admin
              (send admin needs-update this 0 0 arrow-snip-width arrow-snip-height)))))
      
      (define/override (adjust-cursor dc x y editorx editory event) arrow-snip-cursor)
      
      (super-instantiate ())
      
      (inherit get-flags set-flags)
      (set-flags (cons 'handles-events (get-flags)))))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  (define down-bitmap (make-object bitmap% (build-path (collection-path "icons") "turn-down.gif")))
  (define up-bitmap (make-object bitmap% (build-path (collection-path "icons") "turn-up.gif")))
  (define down-click-bitmap (make-object bitmap% (build-path (collection-path "icons") "turn-down-click.gif")))
  (define up-click-bitmap (make-object bitmap% (build-path (collection-path "icons") "turn-up-click.gif")))
  (define arrow-snip-height
    (max 10
         (send up-bitmap get-height)
         (send down-bitmap get-height)
         (send up-click-bitmap get-height)
         (send down-click-bitmap get-height)))
  (define arrow-snip-width
    (max 10
         (send up-bitmap get-width)
         (send down-bitmap get-width)
         (send up-click-bitmap get-width)
         (send down-click-bitmap get-width)))
  (define arrow-snip-cursor (make-object cursor% 'arrow))
  
  ;; build-ht : stx -> hash-table
  ;; the resulting hash-table maps from the each sub-object's to it's syntax.
  (define (syntax-object->datum/ht stx)
    (let ([ht (make-hash-table)])
      (values (let loop ([stx stx])
                (let ([obj (syntax-e stx)])
                  (cond
                    [(list? obj) 
                     (let ([res (map loop obj)])
                       (hash-table-put! ht res stx)
                       res)]
                    [(pair? obj) 
                     (let ([res (cons (loop (car obj))
                                      (loop (cdr obj)))])
                       (hash-table-put! ht res stx)
                       res)]
                    [(vector? obj) 
                     (let ([res (list->vector (map loop (vector->list obj)))])
                       (hash-table-put! ht res stx)
                       res)]
                    [else 
                     (let ([res (syntax-object->datum stx)])
                       (hash-table-put! ht res stx)
                       res)])))
              ht)))
  
  ;; make-text-port : text -> port
  ;; builds a port from a text object.  
  (define (make-text-port text)
    (make-custom-output-port #f
			     (lambda (s start end flush?) 
			       (send text insert (substring s start end)
				     (send text last-position)
				     (send text last-position))
			       (- end start))
			     void
			     void)))
