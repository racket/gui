#lang typed/racket

(require "sig.rkt"
           ;mzlib/string
           mzlib/list
           typed/racket/unit)

(require/typed mzlib/string
               [expr->string (-> Any String)])

(define-signature framework:version^
  ([version : (-> Any)]
   [add-spec : (-> Any Number Void)]))

(define-unit framework:version@
  (import)
  (export (rename framework:version^
                  [-version version]))

  (: specs (Listof (List String String)))
  (define specs null)

  (: -version (-> Any))
  (define (-version)
    (foldr (lambda ([entry : (List String String)] [sofar : String])
             (let ([sep : String (car entry)]
                   [num : String (cadr entry)])
               (string-append sofar sep num)))
           (version)
           specs))

  (: add-spec (-> Any Number Void))
  (define (add-spec sep num)
    (set! specs (cons (list (expr->string sep) (format "~a" num)) 
                      specs))))

;(define-values/invoke-unit (import) (export framework:version^))
