#lang racket/base
(require "../preferences.rkt"
         simple-tree-text-markup/text)

(preferences:set-default 'framework:fraction-snip-style 
                         'mixed (λ (x) (memq x '(mixed improper decimal))))

(provide make-pretty-print-size)
(define (make-pretty-print-size #:exact-prefix [exact-prefix 'never] #:inexact-prefix [inexact-prefix 'never]
                                #:fraction-view [fraction-view #f])
  (lambda (number display? port)
    (let ([fraction-view (or fraction-view (preferences:get 'framework:fraction-snip-style))])
      (cond
        [(or (inexact? number)
             (integer? number)
             (not (real? number)))
         (string-length (number-markup->string number
                                               #:exact-prefix exact-prefix #:inexact-prefix inexact-prefix
                                               #:fraction-view fraction-view))]
        [else 1]))))
