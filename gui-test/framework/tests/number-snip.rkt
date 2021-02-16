#lang racket/base
(require "test-suite-utils.rkt"
         racket/contract
         racket/class
         framework
         file/convertible
         rackunit
         (only-in framework/private/interfaces get-fully-computed-finite-decimal-string))

(check-true
 (let ()
   (define x
     (convert
      (number-snip:make-fraction-snip 1/2 #f)
      'text
      #f))
   (or (equal? "1/2" x) (equal? "0.5" x))))


(check-true
 (bytes?
  (convert
   (number-snip:make-fraction-snip 1/2 #f)
   'png-bytes
   #f)))

(define (make-fraction-snip n b)
  (define s (number-snip:make-fraction-snip n b))
  (send s set-fraction-view 'decimal)
  s)

(check-true (number-snip:is-number-snip? (number-snip:make-fraction-snip 3/2 #t)))
(check-false (number-snip:is-number-snip? 3/2))
(check-equal? 3/2 (number-snip:get-number (number-snip:make-fraction-snip 3/2 #t)))

(check-equal? (send (make-fraction-snip 3/2 #t) get-fully-computed-finite-decimal-string)
              "#e1.5")
(check-equal? (send (make-fraction-snip 3/2 #f) get-fully-computed-finite-decimal-string)
              "1.5")
(check-equal? (send (number-snip:make-repeating-decimal-snip 1/3 #f) get-fully-computed-finite-decimal-string)
              #f)

