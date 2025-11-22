#lang racket/base

(require racket/class
         "sig.rkt"
         mred/mred-sig
         racket/unit
         "text-sig.rkt")

(provide text-max-width-paragraph@)

(define-unit text-max-width-paragraph@
  (import mred^
          text-basic^
          [prefix editor: framework:editor^]
          [prefix frame: framework:frame^])
  (export text-max-width-paragraph^)

  (define max-width-paragraph<%>
    (interface ()
      get-max-width-paragraph
      after-max-width-paragraph-change))

  (define max-width-paragraph-mixin
    (mixin ((class->interface text%)) (max-width-paragraph<%>)
      (inherit last-paragraph
               paragraph-end-position
               paragraph-start-position
               position-paragraph)

      (define max-width-paragraph #f)
      (define max-width #f)
      (define mpw-changed-in-on-delete? #f)

      (define/public-final (get-max-width-paragraph)
        (unless max-width-paragraph
          (calc-max-width-paragraph))
        max-width-paragraph)

      (define/private (set-max-widths _max-width-paragraph _max-width)
        (set! max-width-paragraph _max-width-paragraph)
        (set! max-width _max-width))
      
      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (define insert-start-para (position-paragraph start))
        (define insert-end-para (position-paragraph (+ start len)))
        (cond
          [max-width-paragraph
           (cond
             [(< insert-start-para insert-end-para)
              ;; multi-paragraph insertion, just give up on the cache
              (set-max-widths #f #f)
              (after-max-width-paragraph-change)]
             [(= insert-start-para max-width-paragraph)
              ;; we made the max paragraph wider
              (set-max-widths insert-start-para (get-paragraph-width max-width-paragraph))
              (after-max-width-paragraph-change)]
             [else
              ;; made some other paragraph wider
              (define paragraph-new-width (get-paragraph-width insert-start-para))
              ;; if it got wider than the previous max one or the same but earlier
              ;; in the file, it is the new widest
              (when (or (and (= max-width paragraph-new-width)
                             (< insert-start-para max-width-paragraph))
                        (< max-width paragraph-new-width))
                (set-max-widths insert-start-para
                                paragraph-new-width)
                (after-max-width-paragraph-change))])]
          [else
           (after-max-width-paragraph-change)]))

      (define/augment (on-delete start len)
        (inner (void) on-delete start len)
        (define delete-start-para (position-paragraph start))
        (define delete-end-para (position-paragraph (+ start len)))
        (cond
          [max-width-paragraph
           (cond
             [(< delete-start-para delete-end-para)
              ;; multi-paragraph deletion, just give up on the cache
              (set-max-widths #f #f)
              (set! mpw-changed-in-on-delete? #t)]
             [(= delete-start-para max-width-paragraph)
              ;; we made the max paragraph narrower
              (set-max-widths #f #f)
              (set! mpw-changed-in-on-delete? #t)]
             [else
              ;; made some other paragraph narrower
              (void)])]
          [else
           (set! mpw-changed-in-on-delete? #t)]))

      (define/augment (after-delete start len)
        (inner (void) after-delete start len)
        (when mpw-changed-in-on-delete?
          (set! mpw-changed-in-on-delete? #f)
          (after-max-width-paragraph-change)))

      (define/private (get-paragraph-width para)
        (- (paragraph-end-position para)
           (paragraph-start-position para)))
        
      (define/private (calc-max-width-paragraph)
        (set!-values
         (max-width max-width-paragraph)
         (for/fold ([width 0]
                    [para-with-max-width 0])
                   ([this-para (in-inclusive-range 0 (last-paragraph))])
           (define this-width (get-paragraph-width this-para))
           (cond
             [(<= this-width width)
              (values width para-with-max-width)]
             [else
              (values this-width this-para)]))))

      (define/pubment (after-max-width-paragraph-change)
        (inner (void) after-max-width-paragraph-change))
      (super-new))))
