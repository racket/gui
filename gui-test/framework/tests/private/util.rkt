#lang racket/base
(require framework/private/focus-table
         framework/preferences
         racket/gui/base
         racket/class
         (for-syntax racket/base))

(provide wait-for-frame wait-for/here
         with-private-prefs)

(define (wait-for/here test)
  (define timeout 10)
  (define pause-time 1/2)
  (let loop ([n (ceiling (/ timeout pause-time))])
    (if (zero? n)
        (error 'wait-for "after ~a seconds, ~s didn't come true" timeout test)
        (unless (test)
          (sleep pause-time)
          (loop (- n 1))))))

(define (wait-for-frame name [eventspace (current-eventspace)])
  (define (check-for-frame)
    (for/or ([frame (in-list (frame:lookup-focus-table eventspace))])
      (and (equal? name (send frame get-label))
           frame)))
  (wait-for/here
   (procedure-rename check-for-frame
                     (string->symbol (format "check-for-frame-named-\"~a\"" name)))))

(define-syntax (with-private-prefs stx)
  (syntax-case stx ()
    [(_ e1 e2 ...)
     #'(with-private-prefs/proc (λ () e1 e2 ...))]))

(define (with-private-prefs/proc t)
  (define pref-ht (make-hash))
  (parameterize ([preferences:low-level-get-preference
                  (λ (sym [fail (λ () #f)])
                    (hash-ref pref-ht sym fail))]
                 [preferences:low-level-put-preferences
                  (λ (syms vals)
                    (for ([sym (in-list syms)]
                          [val (in-list vals)])
                      (hash-set! pref-ht sym val)))])
    
    ;; make sure we're back to a clean preferences state
    ;; and the parameterize above ensure that we won't
    ;; look at the disk so together this should mean
    ;; no interference between different concurrent tests
    (preferences:restore-defaults)
    (t)))
