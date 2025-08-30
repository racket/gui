#lang racket/base

(require "private/util.rkt"
         rackunit
         racket/class
         framework
         framework/private/color-local-member-name
         racket/gui/base)

(module+ test
  (with-private-prefs
    (test-commenting)
    (test-get-matching-paren-string)
    (open-paren-typing)
    (test-forward-match)
    (test-text-balanced)
    (test-highlighting)
    (indentation-tests)
    (magic-square-bracket-tests)
    (insert-return-tests)
    (test-message-send)
    (ensure-new-racket-mode-parameter-preserves-alt-as-meta-keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing comment-out-selection and related methods
;;

(define (test-commenting)

  (let ()
    (define t (new racket:text%))
    (send t comment-out-selection)
    (check-equal? (send t get-text) ";"))

  (let ()
    (define t (new racket:text%))
    (send t insert "ab\ncd")
    (send t set-position 0 (send t last-position))
    (send t comment-out-selection)
    (check-equal? (send t get-text) ";ab\n;cd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "ab\ncd")
    (send t set-position 1 (- (send t last-position) 1))
    (send t comment-out-selection)
    (check-equal? (send t get-text) ";ab\n;cd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "ab\ncd")
    (send t set-position 1 (- (send t last-position) 1))
    (send t comment-out-selection #:start "#")
    (check-equal? (send t get-text) "#ab\n#cd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "ab\ncd")
    (send t set-position 1 (- (send t last-position) 1))
    (send t comment-out-selection #:start "#" #:padding " ")
    (check-equal? (send t get-text) "# ab\n# cd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "ab\ncd")
    (send t set-position 1 2)
    (send t region-comment-out-selection)
    (check-equal? (send t get-text)
                  "a#|  b  |#\ncd"))
  (let ()
    (define t (new racket:text%))
    (send t insert "ab\ncd")
    (send t set-position 1 4)
    (send t region-comment-out-selection)
    (check-equal? (send t get-text)
                  "a#|  b\nc  |#d"))
  (let ()
    (define t (new racket:text%))
    (send t insert "ab\ncd\nef")
    (send t set-position 1 7)
    (send t region-comment-out-selection)
    (check-equal? (send t get-text)
                  "a#|  b\n  cd\ne  |#f"))

  (let ()
    (define t (new racket:text%))
    (send t insert "  # ab\n  ;cd")
    (send t set-position 0 (send t last-position))
    (check-equal? (send t commented-out/line? #:start "#") #t)
    (check-equal? (send t commented-out/line? #:start ";") #t)
    (check-equal? (send t commented-out/line? #:start ";" 0 0) #f)
    (check-equal? (send t commented-out/line? #:start ";"
                        (send t last-position) (send t last-position))
                  #t)
    (check-equal? (send t commented-out/line? #:start "#" 0 0) #t)
    (check-equal? (send t commented-out/line? #:start "#"
                        (send t last-position) (send t last-position))
                  #f))

  (let ()
    (define t (new racket:text%))
    (send t insert "  #| ab\n c |# d")
    (send t set-position 0 (send t last-position))
    (check-equal? (send t commented-out/region?) #t))

  (let ()
    (define t (new racket:text%))
    (send t insert "  #| ab\n qq\n c |# d")
    (send t set-position 0 (send t last-position))
    (check-equal? (send t commented-out/region?) #t))

  (let ()
    (define t (new racket:text%))
    (send t insert "a  #| |# z")
    (send t set-position 0 (send t last-position))
    (check-equal? (send t commented-out/region?) #t))

  (let ()
    (define t (new racket:text%))
    (send t insert ";ab\n;cd")
    (send t set-position 0 (send t last-position))
    (send t uncomment-selection)
    (check-equal? (send t get-text) "ab\ncd"))

  (let ()
    (define t (new racket:text%))
    (send t insert ";ab\n;cd")
    (send t set-position 1 (- (send t last-position) 1))
    (send t uncomment-selection)
    (check-equal? (send t get-text) "ab\ncd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "  ; ab\n  ;cd")
    (send t set-position 1 (- (send t last-position) 1))
    (send t uncomment-selection)
    (check-equal? (send t get-text) "   ab\n  cd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "#ab\n#cd")
    (send t set-position 0 (send t last-position))
    (send t uncomment-selection #:start "#")
    (check-equal? (send t get-text) "ab\ncd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "##ab\n##cd")
    (send t set-position 0 (send t last-position))
    (send t uncomment-selection #:start "##")
    (check-equal? (send t get-text) "ab\ncd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "  # ab\n  #cd")
    (send t set-position 1 (- (send t last-position) 1))
    (send t uncomment-selection #:start "#")
    (check-equal? (send t get-text) "   ab\n  cd"))

  (let ()
    (define t (new racket:text%))
    (send t insert "  # ab\n  # cd\n# ef\n#g")
    (send t set-position 1 (- (send t last-position) 1))
    (send t uncomment-selection #:start "#" #:padding " ")
    (check-equal? (send t get-text) "  ab\n  cd\nef\ng"))

  (let ()
    (define t (new racket:text%))
    (send t insert "  #| ab\n c |# d")
    (send t set-position 0 (send t last-position))
    (send t uncomment-selection/region)
    (check-equal? (send t get-text) "   ab\n c  d"))

  (let ()
    (define t (new racket:text%))
    (send t insert "  #| ab\n qq\n c |# d")
    (send t set-position 0 (send t last-position))
    (send t uncomment-selection/region)
    (check-equal? (send t get-text) "   ab\n qq\n c  d"))

  (let ()
    (define t (new racket:text%))
    (send t insert "  #| ab\n   qq\n c |# d")
    (send t set-position 0 (send t last-position))
    (send t uncomment-selection/region)
    (check-equal? (send t get-text) "   ab\n qq\n c  d"))

  (let ()
    (define t (new racket:text%))
    (send t insert "a  #| |# z")
    (send t set-position 0 (send t last-position))
    (send t uncomment-selection/region)
    (check-equal? (send t get-text) "a    z")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing get-matching-paren-string method
;;

(define (test-get-matching-paren-string)
  (define t (new racket:text%))

  (check-equal? (send t get-matching-paren-string "(") ")")
  (check-equal? (send t get-matching-paren-string "(" 'close) ")")
  (check-equal? (send t get-matching-paren-string "(" 'open) #f)
  (check-equal? (send t get-matching-paren-string "]") "[")
  (check-equal? (send t get-matching-paren-string "]" 'open) "[")
  (check-equal? (send t get-matching-paren-string "{" 'either) "}")
  (check-equal? (send t get-matching-paren-string "}" 'close) #f)
  (check-exn exn:fail? (λ () (send t get-matching-paren-string "(" #f)))
  (check-exn exn:fail? (λ () (send t get-matching-paren-string "(" 'forward)))
  (check-equal? (send t get-matching-paren-string "[]") #f)
  (check-equal? (send t get-matching-paren-string "} ") #f)
  (check-equal? (send t get-matching-paren-string "") #f)
  (check-equal? (send t get-matching-paren-string "abc") #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing inserting parens and the automatic-parens prefs
;;

(define (type-something to-type
                        #:control-down [control-down #f]
                        #:stop-colorer? [stop-colorer? #f])
  (define f (new frame:basic% [label ""]))
  (define t (new racket:text%))
  (define ec (new canvas:basic%
                  [parent (send f get-area-container)]
                  [editor t]))
  (when stop-colorer? (send t stop-colorer))
  (send t on-char (new key-event% [key-code to-type] [control-down control-down]))
  (send t get-text))

(define (open-paren-typing)
  (preferences:set 'framework:fixup-parens #f)
  (preferences:set 'framework:fixup-open-parens #f)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "[")
  (check-equal? (type-something #\") "\"")
  (check-equal? (type-something #\( #:stop-colorer? #t) "(")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[")
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"")

  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\() "()")
  (check-equal? (type-something #\[) "[]")
  (check-equal? (type-something #\") "\"\"")
  (check-equal? (type-something #\( #:stop-colorer? #t) "()")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[]")
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"\"")

  (preferences:set 'framework:fixup-parens #f)
  (preferences:set 'framework:fixup-open-parens #t)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "(")
  (check-equal? (type-something #\[ #:control-down #t) "[")
  (check-equal? (type-something #\") "\"")
  (check-equal? (type-something #\( #:stop-colorer? #t) "(")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[") ;; if the colorer is off, no auto parens
  (check-equal? (type-something #\[ #:stop-colorer? #t #:control-down #t) "[")
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"")
  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\() "()")
  (check-equal? (type-something #\[) "()")
  (check-equal? (type-something #\[ #:control-down #t) "[]")
  (check-equal? (type-something #\") "\"\"")
  (check-equal? (type-something #\( #:stop-colorer? #t) "()")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[]") ;; if the colorer is off, no auto parens
  (check-equal? (type-something #\[ #:stop-colorer? #t #:control-down #t) "[]")
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"\"")

  (preferences:set 'framework:fixup-parens #t)
  (preferences:set 'framework:fixup-open-parens #f)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "[")
  (check-equal? (type-something #\") "\"")
  (check-equal? (type-something #\( #:stop-colorer? #t) "(")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[")
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"")
  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\( #:stop-colorer? #t) "()")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[]")
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"\"")

  (preferences:set 'framework:fixup-parens #t)
  (preferences:set 'framework:fixup-open-parens #t)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "(")
  (check-equal? (type-something #\") "\"")
  (check-equal? (type-something #\( #:stop-colorer? #t) "(")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[")  ;; if the colorer is off, no auto parens
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"")
  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\() "()")
  (check-equal? (type-something #\[) "()")
  (check-equal? (type-something #\") "\"\"")
  (check-equal? (type-something #\( #:stop-colorer? #t) "()")
  (check-equal? (type-something #\[ #:stop-colorer? #t) "[]") ;; if the colorer is off, no auto parens
  (check-equal? (type-something #\" #:stop-colorer? #t) "\"\""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing highlight-range method
;;
;

(define (text-balanced? str start end)
  (define t (new racket:text%))
  (send t insert str)
  (racket:text-balanced? t start end))

(define (test-text-balanced)
  (check-equal? (text-balanced? "" 0 #f) #f)
  (check-equal? (text-balanced? "  \n " 0 #f) #f)
  (check-equal? (text-balanced? "foo)" 0 #f) #t)
  (check-equal? (text-balanced? "(foo" 0 #f) #f)
  (check-equal? (text-balanced? "(foo)" 0 #f) #t)
  (check-equal? (text-balanced? "(🏴‍☠️)" 0 6) #t)
  (check-equal? (text-balanced? "(🏴‍☠️)" 0 #f) #t)
  (check-equal? (text-balanced? "(foo 'bar))" 0 #f) #t)
  (check-equal? (text-balanced? "(foo) bar ([buz])" 0 #f) #t)
  (check-equal? (text-balanced? "(foo]" 0 #f) #t)
  (check-equal? (text-balanced? "{foo} ((bar) [5.9])" 0 #f) #t)
  (check-equal? (text-balanced? "#(1 2 . 3)" 0 #f) #t))

(define (test-forward-match)
  (define t (new racket:text%))
  (send t insert " (\n")
  (send t reset-regions '((3 3) (5 end)))
  (send t insert "> (f 01234 56 789 2 3\n")
  (send t freeze-colorer)
  (send t thaw-colorer)
  (check-equal? (send t forward-match 8 (send t last-position)) 13)
  (check-equal? (send t forward-match 13 (send t last-position)) 16))

(define shrubbery-available?
  (with-handlers ([exn:fail? (lambda (x) #f)])
    (collection-path "shrubbery")
    #t))
(unless shrubbery-available?
  (printf "racket.rkt: skipping tests that require shrubbery\n"))

(define (test-highlighting)
  (preferences:set 'framework:paren-color-scheme 'shades-of-gray)
  (define t (new racket:text%))
  (define f (new frame% [label ""] [width 600] [height 600]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send f reflow-container)
  (send t freeze-colorer)

  (define (check-parens str pos)
    (send t thaw-colorer)
    (send t erase)
    (send t insert str)
    (send t set-position pos)
    (send t freeze-colorer)
    (send t match-parens)
    (sort
     (for/list ([r (in-list (send t get-highlighted-ranges))])
       (list (text:range-start r)
             (text:range-end r)))
     range<?))

  ;; useful for testing invisible parens
  (define (check-shrub-parens str pos)
    (define prefix "#lang shrubbery\n")
    (define pl (string-length prefix))
    (define res
      (check-parens (string-append prefix str)
                    (+ pos pl)))
    (for/list ([item (in-list res)])
      (list (- (list-ref item 0) pl)
            (- (list-ref item 1) pl))))

  (define (range<? r1 r2)
    (cond
      [(= (list-ref r1 0) (list-ref r2 0))
       (< (list-ref r1 1) (list-ref r2 1))]
      [else
       (< (list-ref r1 0) (list-ref r2 0))]))

  (check-equal? (check-parens "x" 0) '())
  (check-equal? (check-parens "()" 0) '((0 2)))
  (check-equal? (check-parens "(())" 0) '((0 4) (1 3)))
  (check-equal? (check-parens "( () () )" 0) '((0 9) (2 4) (5 7)))
  (when shrubbery-available?
    (check-equal? (check-shrub-parens "1+2" 0) '((0 3)))
    (check-equal? (check-shrub-parens " 1+2\n 3+4" 1) '((1 4)))
    (check-equal? (check-shrub-parens " 1+2\n 3+4" 4) '((1 4)))
    (check-equal? (check-shrub-parens "block:\n 1+2\n 3+4" 0) '((0 16) (8 11) (8 16) (13 16)))
    (check-equal? (check-shrub-parens "1+2\n\n3+4" 5) '((5 8)))))

(define (test-indentation before)
  (define t (new racket:text%))
  (define f (new frame% [label ""] [width 600] [height 600]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send f reflow-container)
  (send t insert before)
  (send t tabify-all)
  (send t get-text))

(define (indentation-tests)
  (check-equal? (test-indentation "a") "a")
  (check-equal? (test-indentation "(a\n b)") "(a\n b)")
  (check-equal? (test-indentation "(🏴‍☠️\n b)") "(🏴‍☠️\n b)")
  (check-equal? (test-indentation "(a\nb)") "(a\n b)")
  (check-equal? (test-indentation "(a b\nc)") "(a b\n   c)")
  (check-equal? (test-indentation "(a ...\nb)") "(a ...\n b)")
  (check-equal? (test-indentation "(lambda (x)\nb)") "(lambda (x)\n  b)")
  (check-equal? (test-indentation "(lambdaa (x)\nb)") "(lambdaa (x)\n         b)")
  (check-equal?
   (test-indentation "(define x\n  (let/ec return\n    (when 1\n      (when 2\n\t\t      3))\n    2))")
   "(define x\n  (let/ec return\n    (when 1\n      (when 2\n        3))\n    2))")
  (check-equal? (test-indentation "(for ([x 1])\nx)")
                "(for ([x 1])\n  x)")
  (check-equal? (test-indentation "(for/list ([x 1])\nx)")
                "(for/list ([x 1])\n  x)")
  (check-equal? (test-indentation "(for/anything ([x 1])\nx)")
                "(for/anything ([x 1])\n  x)")
  (check-equal? (test-indentation "(for*/anything ([x 1])\nx)")
                "(for*/anything ([x 1])\n  x)")
  (check-equal? (test-indentation "(for-anything ([x 1])\nx)")
                "(for-anything ([x 1])\n              x)")
  (check-equal? (test-indentation "(for/fold ([x 1])\n([y 2])\n3\n4)")
                "(for/fold ([x 1])\n          ([y 2])\n  3\n  4)")
  (check-equal? (test-indentation "a\na\na\n") "a\na\na\n")
  (check-equal? (test-indentation "(begin\n1)") "(begin\n  1)")
  (check-equal? (test-indentation "(lambda\n(x) x)") "(lambda\n    (x) x)")
  (check-equal? (test-indentation "(some-random-function x\nx)")
                "(some-random-function x\n                      x)")
  (check-equal? (test-indentation "(some-random-function x y\nx)")
                "(some-random-function x y\n                      x)")
  (check-equal? (test-indentation "(x ...\nx)")
                "(x ...\n x)")
  (check-equal? (test-indentation "(x\n;;; hello\ny)")
                "(x\n ;;; hello\n y)")
  (check-equal? (test-indentation "(require racket/contract\nracket/unit\nracket/class")
                "(require racket/contract\n         racket/unit\n         racket/class")
  (check-equal? (test-indentation "(r a\nb\nc\nd\ne\nf\ng")
                "(r a\n   b\n   c\n   d\n   e\n   f\n   g")
  (check-equal? (test-indentation "(r a b\nc d\ne f\ng h")
                "(r a b\n   c d\n   e f\n   g h")
  (check-equal? (test-indentation "(#:x\n1)")
                "(#:x\n 1)")
  (check-equal? (test-indentation "(#:x 0\n1)")
                "(#:x 0\n 1)")
  (check-equal? (test-indentation "(a b c d\n---)")
                "(a b c d\n ---)")
  (check-equal? (test-indentation "[---- \"β\"\na")
                "[---- \"β\"\n a")

  (check-equal? (test-indentation "\"\n            a\"")
                "\"\n            a\"")
  (check-equal? (test-indentation "\"\n a\"")
                "\"\n a\"")
  (check-equal? (test-indentation "\"\na\"")
                "\"\na\""))


(define (run-magic-square-bracket before)
  (define t (new racket:text%))
  (define f (new frame% [label ""] [width 600] [height 600]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send f reflow-container)
  (send t insert before)
  (send (racket:get-paren-keymap) call-function "maybe-insert-[]-pair-maybe-fixup-[]" t (new event%))
  (send t get-text))

(define (magic-square-bracket-tests)
  (preferences:set 'framework:automatic-parens #f)
  (preferences:set 'framework:fixup-open-parens #t)
  (check-equal? (run-magic-square-bracket "") "(")
  (check-equal? (run-magic-square-bracket "(() ") "(() (")
  (check-equal? (run-magic-square-bracket "([] ") "([] [")
  (check-equal? (run-magic-square-bracket "(\"") "(\"[")
  (check-equal? (run-magic-square-bracket "(#\\") "(#\\[")
  (check-equal? (run-magic-square-bracket "(let ") "(let (")
  (check-equal? (run-magic-square-bracket "(let (") "(let ([")
  (check-equal? (run-magic-square-bracket "(let loop ") "(let loop (")
  (check-equal? (run-magic-square-bracket "(let loop (") "(let loop ([")
  (check-equal? (run-magic-square-bracket "(let rec (") "(let rec ([")
  (check-equal? (run-magic-square-bracket "(cond ") "(cond [")
  (check-equal? (run-magic-square-bracket "(cond [") "(cond [(")
  (check-equal? (run-magic-square-bracket "(syntax-case x ") "(syntax-case x (")
  (check-equal? (run-magic-square-bracket "(syntax-case x () ") "(syntax-case x () [")
  (check-equal? (run-magic-square-bracket "(syntax-case 'x ") "(syntax-case 'x (")
  (check-equal? (run-magic-square-bracket "(syntax-case 'x () ") "(syntax-case 'x () [")
  (check-equal? (run-magic-square-bracket "(syntax-case #'x ") "(syntax-case #'x (")
  (check-equal? (run-magic-square-bracket "(syntax-case #'x () ") "(syntax-case #'x () [")
  (check-equal? (run-magic-square-bracket "(local ") "(local [")
  (check-equal? (run-magic-square-bracket "(local [") "(local [(")
  (check-equal? (run-magic-square-bracket "(local [(define x 1)] ") "(local [(define x 1)] (")
  (check-equal? (run-magic-square-bracket "(for/fold (") "(for/fold ([")
  (check-equal? (run-magic-square-bracket "(for/fold ([x 1]) (") "(for/fold ([x 1]) (["))



(define (do-insert-return before-txt before-pos #:tabify? [tabify? #t])
  (define t (new (class racket:text%
                   (define/override (tabify-on-return?) tabify?)
                   (super-new))))
  (send t insert before-txt)
  (send t set-position before-pos)
  (send t insert-return)
  (vector (send t get-text)
          (send t get-start-position)))

(define (insert-return-tests)
  (check-equal? (do-insert-return "" 0) (vector "\n" 1))
  (check-equal? (do-insert-return "" 0 #:tabify? #f) (vector "\n" 1))
  (check-equal? (do-insert-return " " 1) (vector "\n" 1))
  (check-equal? (do-insert-return " " 1 #:tabify? #f) (vector "\n" 1))
  (check-equal? (do-insert-return "( " 2) (vector "(\n " 3))
  (check-equal? (do-insert-return "( " 2  #:tabify? #f) (vector "(\n" 2))
  (check-equal? (do-insert-return "hellothere" 5) (vector "hello\nthere" 6))
  (check-equal? (do-insert-return "hellothere" 5 #:tabify? #f) (vector "hello\nthere" 6))
  (check-equal? (do-insert-return "#lang racket\n(+ 123 456)\n 4"      20)
                (vector"#lang racket\n(+ 123\n   456)\n 4" 23))
  (check-equal? (do-insert-return "#lang racket\n(+ 123 456)\n 4" 20 #:tabify? #f)
                (vector "#lang racket\n(+ 123\n456)\n 4" 20))
  (check-equal? (do-insert-return "#lang racket\n(+ 123      456)\n 4" 22)
                (vector "#lang racket\n(+ 123\n   456)\n 4" 23))
  (check-equal? (do-insert-return "#lang racket\n(+ 123      456)\n 4" 22 #:tabify? #f)
                (vector "#lang racket\n(+ 123\n   456)\n 4" 20))
  (check-equal? (do-insert-return "#lang racket\n(+ 123 \n   456)\n 4" 22)
                (vector "#lang racket\n(+ 123 \n\n   456)\n 4" 25))
  (check-equal? (do-insert-return "#lang racket\n(+ 123 \n   456)\n 4" 22 #:tabify? #f)
                (vector "#lang racket\n(+ 123 \n\n  456)\n 4" 22)))

(define (do-message-send before pos send-msg
                           #:check-result? [check-result? #f])
  (define f (new frame% [label ""]))
  (define t (new racket:text%))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send t insert before)
  (send t set-position pos)
  (define ans (send-msg t))
  (if check-result?
      ans
      (send t get-text)))

(define (test-message-send)
  (check-equal? (do-message-send "" 0 (λ (t) (send t insert-close-paren 0 #\] #t #t 'adjacent)))
                 "]")
  (check-equal? (do-message-send "" 0 (λ (t) (send t insert-close-paren 0 #\] #t #t #f)))
                "]")
  (check-equal? (do-message-send "(" 1 (λ (t) (send t insert-close-paren 1 #\] #t #t #f)))
                "()")
  (check-equal? (do-message-send "(🏴‍☠️" 6 (λ (t) (send t insert-close-paren (send t grapheme-position 2) #\] #t #t #f)))
                "(🏴‍☠️)")
  (check-equal? (do-message-send "(" 1 (λ (t) (send t insert-close-paren 1 #\] #f #f #f)))
                "(]")
  (check-equal? (do-message-send "" 0 (λ (t) (send t insert-close-paren 0 #\] #t #t 'forward)))
                "]")

  (check-equal? (do-message-send "(1)" 1 (λ (t) (send t kill-enclosing-parens 1)))
                "1")
  (check-equal? (do-message-send "(1 2 3)" 3 (λ (t) (send t kill-enclosing-parens 3)))
                "1 2 3")
  (check-equal? (do-message-send "()" 1 (λ (t) (send t kill-enclosing-parens 1)))
                "")
  ;; test tabify call
  (check-equal? (do-message-send "(1\n 2\n 3)" 1 (λ (t) (send t kill-enclosing-parens 1)))
                "1\n2\n3")
  (check-equal? (do-message-send "(1\n 🏴‍☠️\n 3)" 1 (λ (t) (send t kill-enclosing-parens 1)))
                "1\n🏴‍☠️\n3")

  (check-equal? (do-message-send "abc" 1 (λ (t) (send t backward-containing-sexp 1 3))
                                 #:check-result? #t)
                #f))

;; tests what happens when a given key/s is/are typed in an editor with initial
;;       text and cursor position, under different settings of the auto-parentheses and
;;       smart-skip-parentheses preferences   .nah.

;; test-auto-parens-behavior
;;    : any string 
;;      [or num (list num num)] [or char symbol 1string (list char) (list key-event%)]
;;      [or num (list num num)] string
#;(define (test-auto-parens-behavior which initial-text initial-pos keys final-text final-pos
                                   [auto-parens? #f])
  (test
   (string->symbol (format "racket:test-auto-parens-behavior ~a" which))
   (λ (x) (if (list? final-pos)
              (equal? x (list (car final-pos) (cadr final-pos) final-text))
              (equal? x (list final-pos final-pos final-text))))))


(define (ensure-new-racket-mode-parameter-preserves-alt-as-meta-keys)
  (define alt-as-meta-before (preferences:get 'framework:alt-as-meta))
  (define mode (new racket:text-mode% [include-paren-keymap? #t]))
  (preferences:set 'framework:alt-as-meta #t)
  (define t (new racket:text%))
  (define keys-with-default-mode (send (send t get-keymap) get-map-function-table))
  (send t set-surrogate mode)
  (define keys-without-paren (send (send t get-keymap) get-map-function-table))
  (check-equal? keys-with-default-mode keys-without-paren)
  (preferences:set 'framework:alt-as-meta alt-as-meta-before))
