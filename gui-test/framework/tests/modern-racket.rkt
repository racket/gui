#lang racket
(require framework rackunit)

#|

These are tests that could be in racket.rkt, but
it uses an old test framework so they are here,
using a modern test framework

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing if the parens are highlighted properly
;;

(define (get-paren-highlight txt)
  (define t
    (new (class racket:text%
           (define/override (has-focus?) #t)
           (super-new))))
  (send t insert txt)
  (send t set-position 0)
  (send t freeze-colorer)
  (send t thaw-colorer)
  (for/set ([r (in-list (send t get-highlighted-ranges))])
    (list (text:range-start r)
          (text:range-end r))))

(let ([pref-ht (make-hash)])
  (parameterize ([preferences:low-level-put-preferences
                  (λ (syms vals)
                    (for ([sym (in-list syms)]
                          [val (in-list vals)])
                      (hash-set! pref-ht sym val)))]
                 [preferences:low-level-get-preference
                  (λ (val [fail (λ () (error 'ack::modern-racket.rkt))])
                    (hash-ref pref-ht val fail))])
      
    (preferences:set 'framework:paren-color-scheme 'basic-grey)
    (check-equal? (get-paren-highlight "()") (set '(0 2)))
    (check-equal? (get-paren-highlight "(())") (set '(0 4)))
      
    (preferences:set 'framework:paren-color-scheme 'shades-of-gray)
    (check-equal? (get-paren-highlight "()") (set '(0 2)))
    (check-equal? (get-paren-highlight "(())") (set '(0 4) '(1 3)))
    (check-equal? (get-paren-highlight "([])") (set '(0 4) '(1 3)))
    (check-equal? (get-paren-highlight "([] ())") (set '(0 7) '(1 3) '(4 6)))

    (check-equal?

     (let ()
       (define t
         (new (class racket:text%
                (define/override (has-focus?) #t)
                (super-new))))
       (send t insert "a\n\n")
       (send t freeze-colorer)
       (send t thaw-colorer)
       (send t reset-regions (list (list 0 1) (list 2 'end)))
       (send t set-position 3)
       (send t insert "(() ())" 3 3)
       (send t freeze-colorer)
       (send t thaw-colorer)
       (for/set ([r (in-list (send t get-highlighted-ranges))])
         (list (text:range-start r)
               (text:range-end r))))
     
     (set '(3 10) '(4 6) '(7 9)))))
