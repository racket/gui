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
                              [_start 'start]
                              [_end 'eof]
                              [get-start #t]
                              [case-sensitive? #t]
                              [pop-out? #f])
  (define start
    (if (eq? _start 'start)
        (send a-text get-start-position)
        _start))
  (define end
    (if (eq? 'eof _end)
        (if (eq? direction 'forward)
            (send a-text last-position)
            0)
        end))
  (define flat (send a-text find-string str direction
                     start end get-start
                     case-sensitive?))
  (define (pop-out)
    (define admin (send a-text get-admin))
    (cond
      [(is-a? admin editor-snip-editor-admin<%>)
       (define snip (send admin get-snip))
       (define edit-above (send (send snip get-admin) get-editor))
       (define pos (send edit-above get-snip-position snip))
       (define pop-out-pos (if (eq? direction 'forward) (add1 pos) pos))
       (find-string-embedded
        edit-above
        str
        direction 
        pop-out-pos
        (if (eq? direction 'forward) 'eof 0)
        get-start
        case-sensitive?
        pop-out?)]
      [else (values a-text #f)]))
  (let loop ([current-snip (send a-text find-snip start
                                 (if (eq? direction 'forward)
                                     'after-or-none
                                     'before-or-none))])
    (define (next-loop)
      (if (eq? direction 'forward)
          (loop (send current-snip next))
          (loop (send current-snip previous))))
    (cond
      [(or (not current-snip)
           (and flat
                (let* ([start (send a-text get-snip-position current-snip)]
                       [end (+ start (send current-snip get-count))])
                  (if (equal? direction 'forward)
                      (and (<= start flat)
                           (< flat end))
                      (and (< start flat)
                           (<= flat end))))))
       (if (and (not flat) pop-out?)
           (pop-out)
           (values a-text flat))]
      [(is-a? current-snip editor-snip%)
       (define media (send current-snip get-editor))
       (define-values (embedded embedded-pos)
         (cond
           [(and media (is-a? media text%))
            (find-string-embedded 
             media 
             str
             direction
             (if (eq? 'forward direction)
                 0
                 (send media last-position))
             'eof
             get-start case-sensitive?)]
           [else
            (values #f #f)]))
       (if (not embedded-pos)
           (next-loop)
           (values embedded embedded-pos))]
      [else (next-loop)])))

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
