#lang racket

(require framework rackunit)

(define (rgb->hsl/l r g b) (call-with-values (λ () (color-model:rgb->hsl r g b)) list))
(define (hsl->rgb/l r g b) (call-with-values (λ () (color-model:hsl->rgb r g b)) list))

(check-within (rgb->hsl/l 108 186 112) (list 123.07 .36 .58) 0.01)
(check-within (rgb->hsl/l 50 100 150) (list 210 .50 .39) 0.01)
(check-within (rgb->hsl/l 150 100 50) (list 30 .50 .39) 0.01)
(check-within (rgb->hsl/l 0 0 0) (list 0 0 0) 0.01)
(check-within (rgb->hsl/l 100 100 100) (list 0 0 .4) 0.01)
(check-within (rgb->hsl/l 255 255 255) (list 0 0 1) 0.01)

(check-within (hsl->rgb/l 123.07 .36 .58) (list 108 186 112) 1)
(check-within (hsl->rgb/l 210 .50 .39) (list 50 100 150) 1)
(check-within (hsl->rgb/l 30 .50 .39) (list 150 100 50) 1)
(check-within (hsl->rgb/l 0 0 0) (list 0 0 0) 1)
(check-within (hsl->rgb/l 0 0 .4) (list 102 102 102) 1)
(check-within (hsl->rgb/l 0 0 1) (list 255 255 255) 1)

(for ([_ (in-range 100)])
  (define-values (r g b) (values (random 256) (random 256) (random 256)))
  (define-values (h s l) (color-model:rgb->hsl r g b))
  (check-within (hsl->rgb/l h s l) (list r g b) 2))
