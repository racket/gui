#lang racket

;; Check the fix for https://github.com/racket/gui/issues/119

(require racket/gui pict pict/snip rackunit)
(define s1 (make-object image-snip% (pict->bitmap (text "hello"))))
(define s2 (make-object pict-snip% (text "hello")))
(define s3 (make-object image-snip% (pict->bitmap (text "hello"))))

(check-true (equal? s1 s1))
(check-false (equal? s1 s2))
(check-false (equal? s2 s1)) ; this case used to report a contract violation
(check-true (equal? s1 s3))
(check-true (equal? s3 s1))
