#lang racket/base
(require racket/class
         racket/format
         wxme
         pict)

(provide reader)

(define circle-reader%
  (class* object% (snip-reader<%>)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream)
      (define size (send stream read-inexact "circle-snip"))
      (cond
        [text-only?
         (string->bytes/utf-8 (~s `(circle ,size)))]
        [else
         (new circle-readable [size size])]))
    (super-new)))

(define circle-readable
  (class* object% (readable<%>)
    (init-field size)
    (define/public (read-special source line column position)
      ;; construct a syntax object holding a 3d value that
      ;; is a circle from the pict library with an appropriate
      ;; source location
      (datum->syntax #f
                     (circle size)
                     (vector source line column position 1)
                     #f))
    (super-new)))

(define reader (new circle-reader%))
