#lang typed/racket/base
;; owner: ryanc
(require typed/racket/class)
(require (for-syntax racket/base syntax/parse racket/syntax racket/class))

(provide define-notify
         notify-box%
         notify-box/pref
         Notify-Box%)

;; Non-gui parts of notify-boxes
;; Worth splitting into two libraries?
;; Probably not, very few non-gui uses of classes.

(define-for-syntax (mk-init name)
  (format-id name "init-~a" (syntax-e name)))

(define-for-syntax (mk-get name)
  (format-id name "get-~a" (syntax-e name)))

(define-for-syntax (mk-set name)
  (format-id name "set-~a" (syntax-e name)))

(define-for-syntax (mk-listen name)
  (format-id name "listen-~a" (syntax-e name)))

(define-syntax (define-notify stx)
  (syntax-parse stx
    [(define-notify name:id
       (~optional value:expr
                  #:defaults ([value #'(new notify-box% (value #f))]))
       (~optional (~and #:init-method init-method)))
     (with-syntax ([init-name (mk-init #'name)]
                   [get-name (mk-get #'name)]
                   [set-name (mk-set #'name)]
                   [listen-name (mk-listen #'name)])
       (with-syntax ([(init-expr init-method-decl)
                      (if (attribute init-method)
                          (list #'(init-name)
                                #'(define/public (init-name) value))
                          (list #'value
                                #'(begin)))])
         (quasisyntax/loc stx
           (begin (field [name init-expr])
                  init-method-decl
                  (define/public-final (get-name)
                    (send name get))
                  (define/public-final (set-name new-value)
                    (send name set new-value))
                  (define/public-final (listen-name listener)
                    (send name listen listener))))))]))

(define-type Notify-Box% (All (T) (Class (init [value  T])
                                [get (-> T)]
                                [set (-> T Void)]
                                [listen (-> (-> T Void) Void)]
                                [remove-listener (-> (-> T Void) Void)]
                                [remove-all-listeners (-> Void)])))

;(: notify-box% Notify-Box%)
(define notify-box%
  (class object%
    #:forall (T)
    (init [value : T])
    (: v : T)
    (define v value)
    (: listeners (Listof (-> T Void)))
    (define listeners null)
    
    ;; Fetch current value
    (: get (-> T))
    (define/public (get)
      v)

    ;; Update value and notify listeners
    (: set (-> T Void))
    (define/public (set nv)
      (set! v nv)
      (for-each (lambda ([p : (-> T Void)]) (p nv)) listeners))

    ;; Add a listener
    (: listen (-> (-> T Void) Void))
    (define/public (listen p)
      (set! listeners (cons p listeners)))

    ;; remove-listener : (T -> void) -> void
    (: remove-listener (-> (-> T Void) Void))
    (define/public (remove-listener p)
      (set! listeners (remq p listeners)))

    ;; remove-all-listeners : -> void
    (: remove-all-listeners (-> Void))
    (define/public (remove-all-listeners)
      (set! listeners null))

    (super-new)))

(: notify-box/pref (All (T) (->* [(case-> (-> T) (-> T Void))] [#:readonly? Boolean] (Instance (Notify-Box% T)))))
(define (notify-box/pref pref #:readonly? [readonly? #f])
  (define nb (new (inst notify-box% T) (value (pref))))
  (send nb listen pref)
   nb)
