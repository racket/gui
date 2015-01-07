#lang racket/base
(require ffi/unsafe)

(provide (protect-out keep-forever))

(define forever (box null))

;; Keeps as long as the place runs, at least:
(void (malloc-immobile-cell forever))

(define (keep-forever v)
  (set-box! forever (cons v (unbox forever))))
