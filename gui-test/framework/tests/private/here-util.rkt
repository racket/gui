#lang racket/base
(require framework/private/focus-table
         racket/gui/base
         racket/class)

(provide wait-for-frame)

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
