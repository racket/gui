#lang racket
(require racket/class "wx/platform.rkt")
(provide white-on-black-panel-scheme?)

(define (luminance c)
  ;; from https://en.wikipedia.org/wiki/Relative_luminance
  (define r (/ (send c red) 255))
  (define g (/ (send c green) 255))
  (define b (/ (send c blue) 255))
  (+ (* .2126 r)
     (* .7152 g)
     (* .0722 b)))

(define (white-on-black-panel-scheme?)
  ;; if the background and foreground are the same
  ;; color, probably something has gone wrong;
  ;; in that case we want to return #f.
  (< (luminance (get-label-background-color))
     (luminance (get-label-foreground-color))))