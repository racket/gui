#lang racket/gui

;; Test case for https://github.com/racket/gui/pull/195, note that with the
;; bug present, this code should hang and not terminate.

(define frame
  (new frame% [label "my frame"] [width 400] [height 300]))

(define canvas (new editor-canvas% [parent frame]))
(define text (new text%))

(send canvas set-editor text) ;; won't hang if this is called below

(send text auto-wrap #t)
(send text set-padding 10 10 10 10) ;; hangs here

;; (send canvas set-editor text) ;; won't hang if this is called here

(send frame show #t)
(sleep/yield 0.5)
(send frame show #f)
