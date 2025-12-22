#lang racket/base
(require racket/match
         "../preferences.rkt"
         mrlib/panel-wob)

(provide white-on-black-color-scheme?)
(define (white-on-black-color-scheme?)
  (match (preferences:get 'framework:white-on-black-mode?)
    ['platform (white-on-black-panel-scheme?)]
    [#t #t]
    [#f #f]))

