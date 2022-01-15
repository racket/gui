#lang racket/base
(require racket/class
         racket/draw/private/color)
(provide special-control-key
         special-option-key
         any-control+alt-is-altgr
         file-creator-and-type
         get-panel-background
         fill-private-color
         luminance)

(define special-control-key? #f)
(define special-control-key
  (case-lambda
   [() special-control-key?]
   [(on?) (set! special-control-key? (and on? #t))]))

(define special-option-key? #f)
(define special-option-key
  (case-lambda
   [() special-option-key?]
   [(on?) (set! special-option-key? (and on? #t))]))

(define any-control+alt-is-altgr? #f)
(define any-control+alt-is-altgr
  (case-lambda
   [() any-control+alt-is-altgr?]
   [(on?) (set! any-control+alt-is-altgr? (and on? #t))]))

(define file-creator-and-type
  (case-lambda
   [(path cr ty) (void)]
   [(path) (values #"????" #"????")]))

(define (get-panel-background)
  (make-object color% "gray"))

(define (fill-private-color dc col)
  (send dc set-background col)
  (send dc clear))

(define (luminance c)
  ;; from https://en.wikipedia.org/wiki/Relative_luminance
  (define r (/ (send c red) 255))
  (define g (/ (send c green) 255))
  (define b (/ (send c blue) 255))
  (+ (* .2126 r)
     (* .7152 g)
     (* .0722 b)))
