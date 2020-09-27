#lang racket
(require framework rackunit)

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

