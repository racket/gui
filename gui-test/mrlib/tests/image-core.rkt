#lang racket/base

(require rackunit 
         mrlib/image-core
         racket/class
         (only-in racket/gui/base make-bitmap))

;; just check there is no error
(check-equal? (begin (un/cache-image (make-bitmap 1 1) #t)
                     (void))
              (void))

(check-equal?
 (let ([b (make-bitmap 1 1)])
   (definitely-same-image? b b))
 #t)

(check-equal?
 (let* ([b (make-bitmap 1 1)])
   (define s1 (make-object image-snip% b))
   (define s2 (make-object image-snip% b))
   (definitely-same-image? s1 s2))
 #t)
