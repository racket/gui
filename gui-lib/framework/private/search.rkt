#lang racket/base
(require racket/contract/base
         racket/class
         racket/gui/base)

(provide
 (contract-out
  [find-string-embedded
   (->* ((is-a?/c text%)
         string?)
        ((or/c 'forward 'backward)
         (or/c 'start number?)
         (or/c 'eof number?)
         boolean?
         boolean?
         boolean?)
        (values (is-a?/c editor<%>)
                (or/c #f number?)))]))

(define (find-string-embedded a-text
                              str
                              [direction 'forward]
                              [start 'start]
                              [end 'eof]
                              [get-start #t]
                              [case-sensitive? #t]
                              [pop-out? #f])
  (let/ec k
    (let loop ([a-text a-text]
               [start start]
               [end end])
      (define found (send a-text find-string-embedded str direction start end get-start case-sensitive?))
      (define (done)
        (cond
          [(not found)
           (k a-text found)]
          [else
           (let loop ([a-text a-text]
                      [found found])
             (cond
               [(number? found)
                (k a-text found)]
               [else (loop (car found) (cdr found))]))]))
      (when found (done))
      (unless pop-out? (done))
      (define a-text-admin (send a-text get-admin))
      (unless (is-a? a-text-admin editor-snip-editor-admin<%>) (done))
      (define editor-snip (send a-text-admin get-snip))
      (define editor-snip-admin (send editor-snip get-admin))
      (unless editor-snip-admin (done))
      (define enclosing-text (send editor-snip-admin get-editor))
      (unless (is-a? enclosing-text text%) (done))
      (loop enclosing-text
            (+ (send enclosing-text get-snip-position editor-snip)
               (send editor-snip get-count))
            'eof))))

(module+ test
  (require rackunit)

  (define abcX (new text%))
  (send abcX insert "abcX")

  (define abc/abcX/abcQ (new text%))
  (send abc/abcX/abcQ insert "abc")
  (send abc/abcX/abcQ insert (new editor-snip% [editor abcX]))
  (send abc/abcX/abcQ insert "abcQ")

  (define abc//abc/abcX/abcQ//abcZ (new text%))
  (send abc//abc/abcX/abcQ//abcZ insert "abc")
  (send abc//abc/abcX/abcQ//abcZ insert (new editor-snip% [editor abc/abcX/abcQ]))
  (send abc//abc/abcX/abcQ//abcZ insert "abcZ")
  
  (let ()
    (define-values (ta pos) (find-string-embedded abcX "b" 'forward 0))
    (check-equal? ta abcX)
    (check-equal? pos 1))

  (let ()
    (define-values (ta pos) (find-string-embedded abcX "c" 'forward 0))
    (check-equal? ta abcX)
    (check-equal? pos 2))

  (let ()
    (define-values (ta pos) (find-string-embedded abcX "d" 'forward 2))
    (check-equal? pos #f))

  (let ()
    (define-values (ta pos) (find-string-embedded abc/abcX/abcQ "b" 'forward 0))
    (check-equal? ta ta)
    (check-equal? pos 1))

  (let ()
    (define-values (ta pos) (find-string-embedded abc/abcX/abcQ "b" 'forward 2))
    (check-equal? ta abcX)
    (check-equal? pos 1))

  (let ()
    (define-values (ta pos) (find-string-embedded abc//abc/abcX/abcQ//abcZ "X" 'forward 0))
    (check-equal? ta abcX)
    (check-equal? pos 3))

  (let ()
    (define-values (ta pos) (find-string-embedded abcX "Q" 'forward 0 'eof #t #t #t))
    (check-equal? ta abc/abcX/abcQ)
    (check-equal? pos 7))

  (let ()
    (define-values (ta pos) (find-string-embedded abcX "Z" 'forward 0 'eof #t #t #t))
    (check-equal? ta abc//abc/abcX/abcQ//abcZ)
    (check-equal? pos 7))

  (let ()
    (define-values (ta pos) (find-string-embedded abcX "c" 'forward 4 'eof #t #t #t))
    (check-equal? ta abc/abcX/abcQ)
    (check-equal? pos 6)))
