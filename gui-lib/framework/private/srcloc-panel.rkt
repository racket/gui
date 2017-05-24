#lang racket/base
(require racket/class
         racket/gui/base
         (for-syntax racket/base))

(define-local-member-name
  get-srcloc-of-constructor
  set-srcloc-of-constructor)

(define located-panel<%>
  (interface ()
    get-srcloc-of-constructor
    set-srcloc-of-constructor))

(define-syntax (define/provide-located-panel stx)
  (syntax-case stx ()
    [(_ new-constructor orig-class)
     #'(begin
         (define subclass
           (let ([orig-class
                  (class* orig-class (located-panel<%>)
                    (define srcloc-of-constructor #f)
                    (define/public (set-srcloc-of-constructor srcloc)
                      (set! srcloc-of-constructor srcloc))
                    (define/public (get-srcloc-of-constructor)
                      srcloc-of-constructor)
                    (super-new))])
             orig-class))
         (provide new-constructor)
         (define-syntax (new-constructor stx)
           (syntax-case stx ()
             [(_ . args)
              (with-syntax ([srcloc (vector (syntax-source stx)
                                            (syntax-line stx)
                                            (syntax-column stx)
                                            (syntax-position stx)
                                            (syntax-span stx))])
                #'(let ([w (new subclass . args)])
                    (send w set-srcloc-of-constructor 'srcloc)
                    w))])))]))

(define/provide-located-panel
  new-horizontal-panel%
  horizontal-panel%)
(define/provide-located-panel
  new-vertical-panel%
  vertical-panel%)

(provide show-srclocs)
(define (show-srclocs p)
  (define max-panel-depth 0)
  (define leaves (make-hasheq))
  (define worst-path #f)

  (let loop ([parent #f]
             [p p]
             [depth 0]
             [panel-depth 0]
             [path '()])
    (when (< max-panel-depth panel-depth)
      (set! max-panel-depth panel-depth)
      (set! worst-path path))
    (cond
      [(is-a? p area-container<%>)
       (for ([child (in-list (send p get-children))])
         (loop p
               child
               (+ depth 1)
               (+ panel-depth (if (is-a? p pane%) 0 1))
               (cons p path)))]
      [else
       (hash-set! leaves parent panel-depth)]))

  (printf "max panel depth ~a\n" max-panel-depth)
  (printf "worst path:\n")
  (for ([p (in-list worst-path)])
    (define info
      (cond
        [(is-a? p horizontal-pane%)
         "pane"]
        [(is-a? p vertical-pane%)
         "pane"]
        [(is-a? p located-panel<%>)
         (send p get-srcloc-of-constructor)]
        [(is-a? p (dynamic-require 'framework 'panel:dragable<%>))
         "dragable"]
        [else "unknown"]))
    (printf "~a~a: ~a\n"
            (if (equal? info "unknown") "*" " ")
            p
            info))
  (printf "panel depths at last area-containers: ~s\n"
          (sort (hash-map leaves (λ (k v) v)) <)))
