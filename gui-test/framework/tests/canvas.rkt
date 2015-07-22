#lang racket/base
(require "private/here-util.rkt"
         framework
         racket/class
         racket/gui/base
         rackunit)

(define (test-creation cls name)
  (check-true
   (parameterize ([current-eventspace (make-eventspace)])
     (define f #f)
     (queue-callback
      (λ ()
        (set! f (make-object frame:basic% "test canvas" #f 300 300))
        (define c (make-object cls (send f get-area-container)))
        (send c set-editor (make-object text:wide-snip%))
        (send f show #t)))
     (wait-for-frame "test canvas" (current-eventspace))
     (queue-callback (λ () (send f show #f)))
     #t)))

(test-creation (canvas:basic-mixin editor-canvas%)
               'canvas:basic-mixin-creation)
(test-creation canvas:basic%
               'canvas:basic%-creation)

(test-creation (canvas:wide-snip-mixin canvas:basic%)
               'canvas:wide-snip-mixin-creation)
(test-creation canvas:wide-snip%
               'canvas:wide-snip%-creation)
