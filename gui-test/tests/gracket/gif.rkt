#lang racket/base
(require file/gif
         rackunit)

(define g (gif-start (open-output-bytes) 10 10 0 #f))
(check-equal? #t (gif-stream? g))
(check-equal? #t (image-ready-gif-stream? g))
(check-equal? #t (image-or-control-ready-gif-stream? g))
(check-equal? #t (empty-gif-stream? g))

