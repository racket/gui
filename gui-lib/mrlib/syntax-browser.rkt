#lang racket/base
(module+ test (require rackunit))

#|

needed to really make this work:

  - marshallable syntax objects (compile and write out the compiled form)

|#

  (require racket/pretty
           racket/class
           racket/gui/base
           racket/match
           racket/contract
           (only-in racket/base [read :read])
           "expandable-snip.rkt")

  (define orig-output-port (current-output-port))
  (define (oprintf . args) (apply fprintf orig-output-port args))
  
  (provide
   (contract-out
    [render-syntax/snip
     (->* (syntax?) (#:summary-width (or/c 0 (integer-in 3 #f) +inf.0 #f)) (is-a?/c snip%))]
    [render-syntax/window
     (->* (syntax?) (#:summary-width (or/c 0 (integer-in 3 #f) +inf.0 #f)) void?)])
   render-syntax-subtitle-color-style-name
   render-syntax-focused-syntax-color-style-name
   snip-class)
  
  ;; this is doing the same thing as the class in
  ;; the framework by the same name, but we don't
  ;; use the framework here because it would 
  ;; introduce a cyclic dependency
  (define text:hide-caret/selection% 
    (class text%
      (inherit get-start-position get-end-position hide-caret)
      (define/augment (after-set-position)
        (hide-caret (= (get-start-position) (get-end-position))))
      (super-new)))

(define default-width 32)

  (define (render-syntax/window syntax #:summary-width [summary-width default-width])
    (define es (render-syntax/snip syntax summary-width))
    (define f (new frame% [label "frame"] [width 850] [height 500]))
    (define mb (new menu-bar% [parent f]))
    (define edit-menu (new menu% [label "Edit"] [parent mb]))
    (define t (new text%))
    (define ec (new editor-canvas% [parent f] [editor t]))
    (append-editor-operation-menu-items edit-menu)
    (send t insert es)
    (send f show #t))

  (define (render-syntax/snip stx #:summary-width [summary-width default-width])
    (new syntax-snip%
         [main-stx stx]
         [summary-width summary-width]))
  
  (define syntax-snipclass%
    (class snip-class%
      (define/override (read stream)
        (new syntax-snip%
             [main-stx (unmarshall-syntax (:read (open-input-bytes (send stream get-bytes))))]
             ;; we don't save this in the stream to avoid having a new version of the snip data
             [summary-width default-width]))
      (super-new)))
  
  (define snip-class (new syntax-snipclass%))
  (send snip-class set-version 1)
  (send snip-class set-classname (format "~s" '(lib "syntax-browser.ss" "mrlib")))
  (send (get-the-snip-class-list) add snip-class)
  
  (define-struct range (stx start end))

  (define Syntax\ Info "Syntax Info")

  (define syntax-snip%
    (class expandable-snip%
      (init-field main-stx)
      (init summary-width)
      (define _summary-width summary-width)
      
      (unless (syntax? main-stx)
        (error 'syntax-snip% "got non-syntax object"))
      
      (define/public (get-syntax) main-stx)
      
      (define/override (copy) (new syntax-snip% [main-stx main-stx] [summary-width _summary-width]))
      (define/override (write stream)
        (send stream put (string->bytes/utf-8 (format "~s" (marshall-syntax main-stx)))))
      
      (define output-text (new text:hide-caret/selection%))
      (define output-text-filled-in? #f)
      (define info-text (new text:hide-caret/selection%))
      (define info-port (make-text-port info-text))
      
      (define/private (show-info stx)
        (insert-subtitle (string-append Syntax\ Info "\n"))
        (piece-of-info "Source" (syntax-source stx))
        (piece-of-info "Source module" (syntax-source-module stx))
        (piece-of-info "Position" (syntax-position stx))
        (piece-of-info "Line" (syntax-line stx))
        (piece-of-info "Column" (syntax-column stx))
        (piece-of-info "Span" (syntax-span stx))
        (piece-of-info "Original?" (syntax-original? stx))
        (when (identifier? stx)
          (piece-of-info "Identifier-binding" (identifier-binding stx))
          (piece-of-info "Identifier-transformer-binding" (identifier-transformer-binding stx))
          (piece-of-info "Identifier-template-binding" (identifier-template-binding stx)))
        
        (let ([properties (syntax-property-symbol-keys stx)])
          (unless (null? properties)
            (insert-subtitle "Known properties\n")
            (for-each
             (λ (prop) (show-property stx prop))
             properties))))
      
      (define/private (render-mpi mpi)
        (string-append
         "#<module-path-index "
         (let loop ([mpi mpi])
           (cond
             [(module-path-index? mpi)
              (let-values ([(x y) (module-path-index-split mpi)])
                (string-append
                 "("
                 (format "~s" x)
                 " . "
                 (loop y)
                 ")"))]
             [else (format "~s" mpi)]))
         ">"))
      
      (define/private (show-property stx prop)
        (piece-of-info (format "'~a" prop) (syntax-property stx prop)))
      
      (define/private (piece-of-info label info)
        (let ([small-newline 
               (λ (port text)
                 (let ([before-newline (send text last-position)])
                   (newline port)
                   (send info-text change-style
                         (make-object style-delta% 'change-size 4)
                         before-newline
                         (+ before-newline 1))))])
          
          (insert/bold label)
          (newline info-port)
          
          ;; should just be using generic `print'
          ;; but won't work without built-in support for
          ;; editors as output ports
          (parameterize ([pretty-print-size-hook
                          (λ (val d/p port)
                            (if (is-a? val syntax-snip%)
                                (+ (string-length (format "~a" (send val get-syntax))) 2)
                                #f))]
                         [pretty-print-print-hook
                          (λ (val d/p port)
                            (send info-text insert (send val copy) 
                                  (send info-text last-position)
                                  (send info-text last-position)))])
            (define start-position (send info-text last-position))
            (pretty-print (replace-syntaxes info) info-port)
            (change-the-style info-text
                              plain-color-style-name
                              start-position
                              (send info-text last-position)))
          
          (optional-newline)
          (small-newline info-port info-text)))
      
      (define/private (replace-syntaxes obj)
        (let loop ([obj obj])
          (cond
            [(pair? obj) (cons (loop (car obj)) (loop (cdr obj)))]
            [(syntax? obj) (new syntax-snip% [main-stx obj] [summary-width _summary-width])]
            [(hash? obj)
             (for/hash ([(k v) (in-hash obj)])
               (values (loop k) (loop v)))]
            [(vector? obj)
             (for/vector ([v (in-vector obj)])
               (loop v))]
            [else obj])))
      
      (define/private (insert/bold str)
        (let ([pos (send info-text last-position)])
          (send info-text insert str 
                (send info-text last-position)
                (send info-text last-position))
          (change-the-style info-text
                            plain-color-style-name
                            pos
                            (send info-text last-position))
          (send info-text change-style
                (make-object style-delta% 'change-bold)
                pos 
                (send info-text last-position))))
      
      (define/private (insert-subtitle str)
        (define pos (send info-text last-position))
        (send info-text insert str pos pos)
        (change-the-style info-text
                          subtitle-style-name
                          pos
                          (send info-text last-position)))
      
      (define/private (optional-newline)
        (unless (equal?
                 (send info-text get-character (- (send info-text last-position) 1))
                 #\newline)
          (send info-text insert "\n" (send info-text last-position))))
      
      (define/private (show-range stx start end)
        (send output-text begin-edit-sequence)
        (send output-text lock #f)
        (change-the-style output-text plain-color-style-name 0 (send output-text last-position))
        (change-the-style output-text render-syntax-focused-syntax-style-name start end)
        (send output-text lock #t)
        (send output-text end-edit-sequence)
        
        (send info-text begin-edit-sequence)
        (send info-text lock #f)
        (send info-text erase)
        (show-info stx)
        (send info-text lock #t)
        (send info-text end-edit-sequence))

      ;; ----

      (inherit show-border set-tight-text-fit)

      (define/override (update-style-list sl)
        (super update-style-list sl)
        (send summary-t set-style-list sl)
        (send inner-t set-style-list sl)
        (send info-text set-style-list sl)
        (send info-header-t set-style-list sl)
        (send info-snip update-style-list sl)
        (send output-text set-style-list sl))

      (define summary-t (new text:hide-caret/selection%))
      (define inner-t (new text:hide-caret/selection%))
      (define info-header-t (new text:hide-caret/selection%))

      (super-new
        (with-border? #f)
        (closed-editor summary-t)
        (open-editor inner-t)
        (left-margin 3)
        (top-margin 0)
        (right-margin 0)
        (bottom-margin 0)
        (left-inset 1)
        (top-inset 0)
        (right-inset 0)
        (bottom-inset 0)
        (callback
         (lambda (details-shown?)
           (fill-in-output-text)
           (show-border details-shown?)
           (set-tight-text-fit (not details-shown?)))))

      (send summary-t insert
            (parameterize ([print-syntax-width (or summary-width (print-syntax-width))])
              (format "~s" main-stx)))
      (change-the-style summary-t plain-color-style-name
                        0 (send summary-t last-position))

      (send info-header-t insert Syntax\ Info)
      (change-the-style info-header-t subtitle-style-name
                        0 (send info-header-t last-position))
      (send info-header-t lock #t)
      (define info-snip (new expandable-snip%
                             (closed-editor info-header-t)
                             (open-editor info-text)
                             (layout 'replace)
                             (with-border? #t)))

      (let ([es (new editor-snip%
                     (editor output-text)
                     (with-border? #f)
                     (left-margin 0)
                     (top-margin 0)
                     (right-margin 0)
                     (bottom-margin 0)
                     (left-inset 0)
                     (top-inset 0)
                     (right-inset 0)
                     (bottom-inset 0))])
        (send es use-style-background #t)
        (send inner-t insert es))
      (send inner-t insert " ")
      (send inner-t insert info-snip)
      (change-the-style inner-t plain-color-style-name
                        (- (send inner-t last-position) 1)
                        (send inner-t last-position))
      (send inner-t change-style (make-object style-delta% 'change-alignment 'top)
            0 (send inner-t last-position))

      (send output-text lock #t)
      (send info-text lock #t)
      (send inner-t lock #t)
      (send summary-t lock #t)

      (define/private (fill-in-output-text)
        (unless output-text-filled-in?
          (set! output-text-filled-in? #t)
          (send output-text begin-edit-sequence)
          (send output-text lock #f)
          (define-values (range-start-ht range-ht)
            (populate-range-ht main-stx output-text))
          (define ranges
            (sort
             (apply append
                    (hash-map
                     range-ht
                     (λ (k vs)
                       (map (λ (v) (make-range k (car v) (cdr v)))
                            vs))))
             (λ (x y)
               (>= (- (range-end x) (range-start x))
                   (- (range-end y) (range-start y))))))
          (for ([range (in-list ranges)])
            (define stx (range-stx range))
            (define start (range-start range))
            (define end (range-end range))
            (when (syntax? stx)
              (send output-text set-clickback start end 
                    (λ (_1 _2 _3)
                      (show-range stx start end)))))
          (send info-text auto-wrap #t)
          (send info-text set-styles-sticky #f)
          (unless (null? ranges)
            (let ([rng (car ranges)])
              (show-range (range-stx rng) (range-start rng) (range-end rng))))
          (send output-text end-edit-sequence)
          (send output-text lock #t)))

      (inherit set-snipclass use-style-background)
      (set-snipclass snip-class)
      (use-style-background #t)))

;; ------------------------------------------------------------

;; record-paths : val -> hash-table[path -o> syntax-object]
(define (syntax-object->datum/record-paths val)
  (define path '())
  (define next-push 0)
  (define (push!)
    (set! path (cons next-push path))
    (set! next-push 0))
  (define (pop!)
    (set! next-push (+ (car path) 1))
    (set! path (cdr path)))
  (let* ([ht (make-hash)]
         [record
          (λ (val enclosing-stx)
            (hash-set! ht path enclosing-stx))])
    (values
     (let loop ([val val]
                [enclosing-stx #f])
       (cond
         [(syntax? val)
          (loop (syntax-e val)
                val)]
         [(pair? val)
          (push!)
          (record val enclosing-stx)
          (begin0
            (let lst-loop ([val val])
              (cond
                [(pair? val)
                 (cons (loop (car val) #f)
                       (lst-loop (cdr val)))]
                [(null? val) '()]
                [(and (syntax? val) (pair? (syntax-e val)))
                 (define pr (syntax-e val))
                 (lst-loop pr)]
                [else
                 (loop val enclosing-stx)]))
            (pop!))]
         [(vector? val)
          (push!)
          (record val enclosing-stx)
          (begin0
            (apply
             vector
             (let lst-loop ([val (vector->list val)])
               (cond
                 [(pair? val)
                  (cons (loop (car val) #f)
                        (lst-loop (cdr val)))]
                 [(null? val) '()])))
            (pop!))]
         [(hash? val)
          (push!)
          (record val enclosing-stx)
          (begin0
            (for/hash ([(k v) (in-hash val)])
              (values (loop k #f)
                      (loop v #f)))
            (pop!))]
         [else
          (push!)
          (record val enclosing-stx)
          (pop!)
          val]))
     ht)))

;; populate-range-ht : Datum text%
;;                  -> (values Hash[Datum -> Nat] Hash[Datum -> (listof (cons Nat Nat))])
(define (populate-range-ht main-stx output-text)
  (define-values (datum paths-ht) (syntax-object->datum/record-paths main-stx))

  ;; range-start-ht : hash-table[obj -o> number]
  (define range-start-ht (make-hasheq))

  ;; range-ht : hash-table[obj -o> (listof (cons number number))]
  (define range-ht (make-hasheq))

  (define path '())
  (define next-push 0)
  (define (push!)
    (set! path (cons next-push path))
    (set! next-push 0))
  (define (pop!)
    (set! next-push (+ (car path) 1))
    (set! path (cdr path)))

  (let* ([range-pretty-print-pre-hook
          (λ (x port)
            (push!)
            (let ([stx-object (hash-ref paths-ht path (λ () #f))])
              (hash-set! range-start-ht stx-object (send output-text last-position))))]
         [range-pretty-print-post-hook
          (λ (x port)
            (let ([stx-object (hash-ref paths-ht path (λ () #f))])
              (when stx-object
                (let ([range-start (hash-ref range-start-ht stx-object (λ () #f))])
                  (when range-start
                    (hash-set! range-ht
                               stx-object
                               (cons
                                (cons
                                 range-start
                                 (send output-text last-position))
                                (hash-ref range-ht stx-object (λ () null))))))))
            (pop!))])

    ;; reset `path' and `next-push' for use in pp hooks.
    (set! path '())
    (set! next-push 0)
    (parameterize ([current-output-port (make-text-port output-text)]
                   [pretty-print-pre-print-hook range-pretty-print-pre-hook]
                   [pretty-print-post-print-hook range-pretty-print-post-hook]
                   [pretty-print-columns 30])
      (pretty-write datum)))

  (values range-start-ht range-ht))

(module+ test
  (let ([x (datum->syntax #f 'x #f #f)]
        [y (datum->syntax #f 'y #f #f)])
    (check-equal? (call-with-values
                   (λ ()
                     (syntax-object->datum/record-paths (list x y)))
                   list)
                  (list '(x y)
                        (make-hash `(((0) . #f) ((1 0) . ,y) ((0 0) . ,x))))))

  (let* ([x (datum->syntax #f 'x #f #f)]
         [y (datum->syntax #f 'y #f #f)]
         [ly (datum->syntax #f (list y) #f #f)])
    (check-equal? (call-with-values
                   (λ ()
                     (syntax-object->datum/record-paths (cons x ly)))
                   list)
                  (list '(x y)
                        (make-hash `(((0) . #f) ((1 0) . ,y) ((0 0) . ,x)))))))

  ;; make-text-port : text -> port
  ;; builds a port from a text object.  
  (define (make-text-port text)
    (make-output-port #f
                      always-evt
                      (λ (s start end flush? breaks?) 
                        (send text insert (bytes->string/utf-8 (subbytes s start end))
                              (send text last-position)
                              (send text last-position))
                        (- end start))
                      void))
  
  ;; marshall-syntax : syntax -> printable
  (define (marshall-syntax stx)
    (unless (syntax? stx)
      (error 'marshall-syntax "not syntax: ~s\n" stx))
    `(syntax
      (source ,(marshall-object (syntax-source stx)))
      (source-module ,(marshall-object (syntax-source-module stx)))
      (position ,(syntax-position stx))
      (line ,(syntax-line stx))
      (column ,(syntax-column stx))
      (span ,(syntax-span stx))
      (original? ,(syntax-original? stx))
      (properties 
       ,@(map (λ (x) `(,x ,(marshall-object (syntax-property stx x))))
              (syntax-property-symbol-keys stx)))
      (contents
       ,(marshall-object (syntax-e stx)))))

  ;; marshall-object : any -> printable
  ;; really only intended for use with marshall-syntax
  (define (marshall-object obj)
    (cond
      [(syntax? obj) (marshall-syntax obj)]
      [(pair? obj) 
       `(pair ,(cons (marshall-object (car obj))
                     (marshall-object (cdr obj))))]
      [(or (symbol? obj)
           (char? obj)
           (number? obj)
           (string? obj)
           (boolean? obj)
           (null? obj))
       `(other ,obj)]
      [else (string->symbol (format "unknown-object: ~s" obj))]))
  
  (define (unmarshall-syntax stx)
    (match stx
      [`(syntax
         (source ,src)
         (source-module ,source-module) ;; marshalling
         (position ,pos)
         (line ,line)
         (column ,col)
         (span ,span)
         (original? ,original?)
         (properties ,properties ...)
         (contents ,contents))
       (foldl
        add-properties
        (datum->syntax
         #'here ;; ack
         (unmarshall-object contents)
         (list (unmarshall-object src)
               line
               col
               pos
               span))
        properties)]
      [else #'unknown-syntax-object]))

  ;; add-properties : syntax any -> syntax
  (define (add-properties prop-spec stx)
    (match prop-spec
      [`(,(and sym (? symbol?))
         ,prop)
       (syntax-property stx sym (unmarshall-object prop))]
      [else stx]))
  
  (define (unmarshall-object obj)
    (let ([unknown (λ () (string->symbol (format "unknown: ~s" obj)))])
      (if (and (pair? obj)
               (symbol? (car obj)))
          (case (car obj)
            [(pair) 
             (if (pair? (cdr obj))
                 (let ([raw-obj (cadr obj)])
                   (if (pair? raw-obj)
                       (cons (unmarshall-object (car raw-obj))
                             (unmarshall-object (cdr raw-obj)))
                       (unknown)))
                 (unknown))]
            [(other) 
             (if (pair? (cdr obj))
                 (cadr obj)
                 (unknown))]
            [(syntax) (unmarshall-syntax obj)]
            [else (unknown)])
          (unknown))))

(define (change-the-style text style-name start end)
  (define st (find/create-style style-name (send text get-style-list)))
  (send text change-style st start end))

(define plain-color-style-name "framework:default-color")
(define render-syntax-subtitle-color-style-name "mrlib/syntax-browser:subtitle-color")
(define render-syntax-focused-syntax-color-style-name "mrlib/syntax-browser:focused-syntax-color")

;; these names are not actually added to the style list
(define render-syntax-focused-syntax-style-name "mrlib/syntax-browser:focused-syntax")
(define subtitle-style-name "mrlib/syntax-browser:subtitle")

(define (find/create-style style-name sl)
  (cond
    [(equal? style-name subtitle-style-name)
     (create-combined-style sl
                            "Standard"
                            render-syntax-subtitle-color-style-name
                            (make-object style-delta% 'change-bold))]
    [(equal? style-name render-syntax-focused-syntax-style-name)
     (create-combined-style sl
                            "Standard"
                            render-syntax-focused-syntax-color-style-name)]

    [(send sl find-named-style style-name) => values]

    [(equal? style-name plain-color-style-name)
     (create-colored-named-style sl style-name "black")]
    [(equal? style-name render-syntax-subtitle-color-style-name)
     (create-colored-named-style sl style-name "navy")]
    [(equal? style-name render-syntax-focused-syntax-color-style-name)
     (create-colored-named-style sl style-name "forestgreen")]

    [else (error 'syntax-browser.rkt:find/create-style "unknown style name ~s" style-name)]))

(define (create-colored-named-style sl style-name color)
  (define st (send sl new-named-style style-name (send sl basic-style)))
  (define sd (send (make-object style-delta%) set-delta-foreground color))
  (send st set-delta sd)
  st)

(define (create-combined-style sl style-name1 style-name2 [extra-sd #f])
  (define st1 (find/create-style style-name1 sl))
  (define st2 (find/create-style style-name2 sl))
  (define join-st (send sl find-or-create-join-style st1 st2))
  (cond
   [extra-sd
    (send sl find-or-create-style join-st extra-sd)]
   [else join-st]))
