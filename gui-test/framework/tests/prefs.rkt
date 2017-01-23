#lang racket/base
(require framework/preferences
         racket/format
         rackunit
         racket/contract)

;(define ((check-equal? x) y) (equal? x y))
(define pref-sym 'plt:not-a-real-preference)
(define marshalling-pref-sym 'plt:not-a-real-preference-marshalling)
(define default-test-sym 'plt:not-a-real-preference-default-test)

(define the-prefs-table (make-hash))
(parameterize ([preferences:low-level-put-preferences
                (λ (syms vals)
                    (for ([sym (in-list syms)]
                          [val (in-list vals)])
                    (hash-set! the-prefs-table sym val)))]
               [preferences:low-level-get-preference
                (λ (sym [fail void])
                  (hash-ref the-prefs-table sym fail))])

  (check-exn
   exn:unknown-preference?
   (λ ()
     (preferences:get pref-sym)))

  (check-equal?
   (begin
     (preferences:set-default pref-sym 'passed symbol?)
     (preferences:get pref-sym))
   'passed)

  (check-equal?
   (begin (preferences:set pref-sym 'new-pref)
          (preferences:get pref-sym))
   'new-pref)

  (check-true (preferences:default-set? pref-sym))
  (check-false (preferences:default-set? 'unknown-preference))
  (check-false (begin
                 (preferences:add-callback 'pref-with-only-callback-set void)
                 (preferences:default-set? 'pref-with-only-callback-set)))

  (check-equal?
   (begin (preferences:set-default marshalling-pref-sym (lambda () 'the-answer) procedure?)
          (preferences:set-un/marshall marshalling-pref-sym
                                       (lambda (f) (f))
                                       (lambda (v) (lambda () v)))
          (begin0 ((preferences:get marshalling-pref-sym))
                  (preferences:set marshalling-pref-sym (lambda () 2))))
   'the-answer)

  (check-equal? ((preferences:get marshalling-pref-sym)) 2)

  ;; make sure the preference actually got "written out"
  (check-equal? (hash-ref the-prefs-table
                          (string->symbol (~a "plt:framework-pref:" pref-sym)))
                'new-pref)

  (let ()
    (preferences:set-default 'unmarshalling-enumerate-test '() (listof exact-nonnegative-integer?))
    (preferences:set-un/marshall 'unmarshalling-enumerate-test
                                 (λ (lon) (~s lon))
                                 (λ (s) (read (open-input-string s))))

    ;; simulate a value having been saved from some prior run of the preferences library
    (hash-set! the-prefs-table 'plt:framework-pref:unmarshalling-enumerate-test
               (~s '(1 2 3 4 5)))

    (check-equal? (preferences:get 'unmarshalling-enumerate-test) '(1 2 3 4 5)))

  (check-equal?
   (let ([x 1])
     (preferences:set-default default-test-sym 'default symbol?)
     (define remove-it (preferences:add-callback default-test-sym (λ (a b) (set! x (+ x 1)))))
     (preferences:set default-test-sym 'xyz)
     (remove-it)
     (preferences:set default-test-sym 'pdq)
     x)
   2)

  (check-equal?
   (let ([x 1])
     (define remove-it (preferences:add-callback 'callback-before-delete (λ (a b) (set! x (+ x 1)))))
     (preferences:set-default 'callback-before-delete 'default symbol?)
     (preferences:set 'callback-before-delete 'xyz)
     (remove-it)
     (preferences:set 'callback-before-delete 'pdq)
     x)
   2)

  (check-equal?
   (let ([x 1])
     (define f (λ (a b) (set! x (+ x 1))))
     (define remove-it (preferences:add-callback default-test-sym f #t))
     (preferences:set default-test-sym 'xyz)
     (remove-it)
     (preferences:set default-test-sym 'pdq)
     x)
   2)

  (check-equal?
   (let ([x 1])
     (define f (λ (a b) (set! x (+ x 1))))
     (unless (zero? (random 1)) (set! f 'not-a-proc))
     (define remove-it (preferences:add-callback default-test-sym f #t))
     (collect-garbage) (collect-garbage) (collect-garbage)
     (preferences:set default-test-sym 'xyz)
     (remove-it)
     (preferences:set default-test-sym 'pdq)
     (f 'a 'b)  ;; make sure safe-for-space doesn't free 'f' earlier
     x)
   3)

  (check-equal?
   (let ([x 1])
     (define f (λ (a b) (set! x (+ x 1))))
     (define wb (make-weak-box f))
     (define remove-it (preferences:add-callback default-test-sym f #t))
     (set! f #f)
     (let loop ([n 10])
       (cond
         [(not (weak-box-value wb)) #t]
         [(zero? n) 'f-still-alive]
         [else
          (collect-garbage)
          (loop (- n 1))]))
     (preferences:set default-test-sym 'xyz)
     (remove-it)
     (preferences:set default-test-sym 'pdq)
     x)
   1)

  (let ()
    (hash-set! the-prefs-table
               'plt:framework-pref:preferences-aliases-test:1
               "1")
    (preferences:set-default 'preferences-aliases-test
                             0
                             exact-nonnegative-integer?
                             #:aliases '(preferences-aliases-test:1)
                             #:rewrite-aliases (list (λ (v) (read (open-input-string v)))))
    (check-equal? (preferences:get 'preferences-aliases-test) 1)))

