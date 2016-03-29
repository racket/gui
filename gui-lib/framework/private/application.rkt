#lang typed/racket

(require ;"sig.rkt"
         typed/racket/unit)

(require/typed "sig.rkt"
               [#:signature framework:application^
                ([current-app-name : (Parameterof String)])])

(define-unit framework:application@
  (import)
  (export framework:application^)


  (: current-app-name (Parameterof String))
  (define current-app-name ((inst make-parameter String String)
                          "GRacket"
                          (λ ([x : Any])
                            (unless (string? x)
                              (error 'current-app-name
                                     "the app name must be a string")) 
                            x))))
