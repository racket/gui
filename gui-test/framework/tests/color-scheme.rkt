#lang racket
(require framework rackunit racket/draw)

;; just makes sure the contract is okay.
(check-true
 (symbol? (color-prefs:get-current-color-scheme-name)))


(define-values (color-names style-names)
  (color-prefs:get-color-scheme-names))

(check-true
 (for/and ([color-name (in-set color-names)])
   (color-prefs:color-scheme-color-name? color-name)))

(check-true
 (for/and ([color-name (in-set color-names)])
   (color-prefs:known-color-scheme-name? color-name)))

(check-true
 (for/and ([style-name (in-set style-names)])
   (color-prefs:color-scheme-style-name? style-name)))

(check-true
 (for/and ([style-name (in-set style-names)])
   (color-prefs:known-color-scheme-name? style-name)))

(define a-known-color-name 'framework:paren-match-color)

(let ()
  (define called 0)
  (color-prefs:register-color-scheme-entry-change-callback
   a-known-color-name
   (λ (x) (set! called (+ called 1))))
  (check-equal? called 0)
  (define old-color (color-prefs:lookup-in-color-scheme a-known-color-name))
  (define new-color (make-object color%
                      (modulo (+ (send old-color red) 1) 255)
                      (send old-color green)
                      (send old-color blue)))
  (color-prefs:set-in-color-scheme a-known-color-name new-color)
  (check-equal? called 1)
  (color-prefs:set-in-color-scheme a-known-color-name old-color)
  (check-equal? called 2))

(let ()
  (define called 0)
  (define proc (λ (x) (set! called (+ called 1))))
  (color-prefs:register-color-scheme-entry-change-callback a-known-color-name
                                                           proc
                                                           #t)
  (check-equal? called 0)

   ;; try to clear out the callback, it should be retained because of `proc`
  (for ([x (in-range 10)]) (collect-garbage))

  (define old-color (color-prefs:lookup-in-color-scheme a-known-color-name))
  (define new-color (make-object color%
                      (modulo (+ (send old-color red) 1) 255)
                      (send old-color green)
                      (send old-color blue)))

  (color-prefs:set-in-color-scheme a-known-color-name new-color)
  (check-equal? called 1)
  (color-prefs:set-in-color-scheme a-known-color-name old-color)
  (check-equal? called 2)

  ;; make a call here to `proc` so that sfs doesn't clear it
  (proc 'ignored)
  (check-equal? called 3)

  ;; clear it and now gc should remove the callback
  (set! proc void)
  (for ([x (in-range 10)]) (collect-garbage))

  ;; these two calls shouldn't change the counter
  (color-prefs:set-in-color-scheme a-known-color-name new-color)
  (check-equal? called 3)
  (color-prefs:set-in-color-scheme a-known-color-name old-color)
  (check-equal? called 3))
