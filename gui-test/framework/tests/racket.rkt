#lang racket/base

(require "private/util.rkt"
         rackunit
         racket/class
         framework
         racket/gui/base)

(module+ test
  (with-private-prefs
    (test-get-matching-paren-string)
    (open-paren-typing)
    (test-text-balanced)
    (indentation-tests)
    (magic-square-bracket-tests)
    (insert-return-tests)
    (test-message-send)))

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

(define (type-something to-type [control-down #f])
  (define f (new frame:basic% [label ""]))
  (define t (new racket:text%))
  (define ec (new canvas:basic%
                  [parent (send f get-area-container)]
                  [editor t]))
  (send t on-char (new key-event% [key-code to-type] [control-down control-down]))
  (send t get-text))

(define (open-paren-typing)
  (preferences:set 'framework:fixup-parens #f)
  (preferences:set 'framework:fixup-open-parens #f)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "[")
  (check-equal? (type-something #\") "\"")

  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\() "()")
  (check-equal? (type-something #\[) "[]")
  (check-equal? (type-something #\") "\"\"")

  (preferences:set 'framework:fixup-parens #f)
  (preferences:set 'framework:fixup-open-parens #t)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "(")
  (check-equal? (type-something #\[ #t) "[")
  (check-equal? (type-something #\") "\"")
  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\() "()")
  (check-equal? (type-something #\[) "()")
  (check-equal? (type-something #\[ #t) "[]")
  (check-equal? (type-something #\") "\"\"")

  (preferences:set 'framework:fixup-parens #t)
  (preferences:set 'framework:fixup-open-parens #f)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "[")
  (check-equal? (type-something #\") "\"")
  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\() "()")
  (check-equal? (type-something #\[) "[]")
  (check-equal? (type-something #\") "\"\"")

  (preferences:set 'framework:fixup-parens #t)
  (preferences:set 'framework:fixup-open-parens #t)
  
  (preferences:set 'framework:automatic-parens #f)
  (check-equal? (type-something #\() "(")
  (check-equal? (type-something #\[) "(")
  (check-equal? (type-something #\") "\"")
  (preferences:set 'framework:automatic-parens #t)
  (check-equal? (type-something #\() "()")
  (check-equal? (type-something #\[) "()")
  (check-equal? (type-something #\") "\"\""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; testing highlight-range method
;;


(define (text-balanced? number str start end)
  (define t (new racket:text%))
  (send t insert str)
  (racket:text-balanced? t start end))

(define (test-text-balanced)
  (check-equal? (text-balanced? 0 "" 0 #f) #f)
  (check-equal? (text-balanced? 1 "  \n " 0 #f) #f)
  (check-equal? (text-balanced? 2 "foo)" 0 #f) #t)
  (check-equal? (text-balanced? 3 "(foo" 0 #f) #f)
  (check-equal? (text-balanced? 4 "(foo)" 0 #f) #t)
  (check-equal? (text-balanced? 5 "(foo 'bar))" 0 #f) #t)
  (check-equal? (text-balanced? 6 "(foo) bar ([buz])" 0 #f) #t)
  (check-equal? (text-balanced? 7 "(foo]" 0 #f) #t)
  (check-equal? (text-balanced? 8 "{foo} ((bar) [5.9])" 0 #f) #t)
  (check-equal? (text-balanced? 9 "#(1 2 . 3)" 0 #f) #t))

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
                "[---- \"β\"\n a"))


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

(define (run-auto-parens initial-text initial-pos keys [auto-parens? #f])
  (define t (new racket:text%))
  (define f (new frame% [label ""] [width 600] [height 600]))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (preferences:set 'framework:fixup-parens #t)
  (preferences:set 'framework:automatic-parens auto-parens?)
  (send f reflow-container)
  (send t insert initial-text)
  (if (number? initial-pos)
      (send t set-position initial-pos)
      (send t set-position (car initial-pos) (cadr initial-pos)))
  (for ([k (in-list (if (list? keys) keys (list keys)))])
    (cond
      [(char? k)
       (send (racket:get-keymap) handle-key-event t (new key-event% [key-code k]))]
      [(string? k)
       (send (racket:get-keymap) handle-key-event t
             (new key-event% [key-code (car (string->list k))]))]
      [(symbol? k)
       (send (racket:get-keymap)
             handle-key-event t (new key-event% [key-code k]))]
      [else (send (racket:get-keymap) handle-key-event t k)]))
  (list (send t get-start-position)
        (send t get-end-position)
        (send t get-text)))


;; this takes an initial editor state (specified by the text before the cursor,
;;   some selected text (may be blank string), and text after the cursor), and
;;   a key(s), and runs tests to check what happens when that key(s) is/are
;;   typed - in both possible settings of the 'automatic-parens preference
;;
;; final-states is a list of 2-pairs of strings. each pair is the final text before
;;   and after the cursor, for auto-parens disabled and enabled respectively
;;  (NB. final-states could also contain 3-pairs of strings, the middle portion 
;;       representing text that is selected after the insertion)
(define (test-parens-behavior/full which
                                   init-text-before init-text-selected init-text-after
                                   keys
                                   final-states)
  (define initial-text (string-append init-text-before init-text-selected init-text-after))
  (define initial-start-pos (string-length init-text-before))
  (define initial-end-pos (+ initial-start-pos (string-length init-text-selected)))
  (for ([auto? (in-list '(#f #t))]
        [final-pair (in-list final-states)])
    (cond
      [(= 3 (length final-pair))
       (check-equal?
        (run-auto-parens initial-text
                         (list initial-start-pos initial-end-pos)
                         keys
                         auto?)
        (list (string-length (car final-pair))
              (string-length (string-append (car final-pair)
                                            (cadr final-pair)))
              (apply string-append final-pair)))]
      [else
       (define final-pos (string-length (car final-pair)))
       (check-equal?
        (run-auto-parens initial-text
                         (list initial-start-pos initial-end-pos)
                         keys
                         auto?)
        (list final-pos
              final-pos
              (apply string-append final-pair)))])))
   

(define SPECIAL-CHARS '(#\( #\) #\[ #\] #\" #\| #\{ #\}))

(for ([k SPECIAL-CHARS])
  ;; test that character literals never result in a pair of characters typed...
  (test-parens-behavior/full (format "literal-~a" k)
                             "(list 1 #\\" "" ")"
                             k
                             `([,(string-append "(list 1 #\\" (string k)) ")"]
                               [,(string-append "(list 1 #\\" (string k)) ")"]))
  ;; test that auto-delete doesn't delete closing paren in the above cases, even for literal-(
  (test-parens-behavior/full (format "backspace-after-literal-~a" k)
                             (string-append "(list 1 #\\" (string k)) "" ")"
                             #\backspace
                             '(["(list 1 #\\" ")"] ["(list 1 #\\" ")"]))
  ;; test of basic cases for auto-parens followed by auto-delete
  (test-parens-behavior/full (format "backspace-after-~a" k)
                             "" "" ""
                             (list k #\backspace)
                             '([""] [""]))
  ;; test that escaped characters in a string never result in a pair of characters typed...
  ;; except for | which is a hard case to detect, because the tokenizer ends up
  ;; in an error state
  (unless (or (eq? #\| k))
    (test-parens-behavior/full (format "literal-~a-in-string" k)
                               "\"abc \\" "" "def\""
                               k
                               `([,(string-append "\"abc \\" (string k)) "def\""]
                                 [,(string-append "\"abc \\" (string k)) "def\""])))
  ;; test that auto-parens has no effect in strings, *except for double quotes*
  (unless (eq? #\" k)
    (test-parens-behavior/full (format "~a-in-string" k)
                               "\" abc def " "" " \""
                               k
                               `([,(string-append "\" abc def " (string k)) " \""]
                                 [,(string-append "\" abc def " (string k)) " \""])))

  ;; test that auto-parens has no effect in various comment situations
  (define scenarios
    ;    description     before-cursor    after-cursor
    '(("in-line-comment"  ";; abc def "    " ghi ")
      ("end-of-line-comment" ";; abc def " "")
      ("end-of-line-comment-with-newline" ";; abc def " "\n")
      ("end-of-line-comment-with-close-paren" ";; abc def " "   ) \n )")
      ("in-block-comment" "#| abc def "  " ghi |#")
      ))
  (for ([s scenarios])
    (let* ([before (cadr s)]
           [after (caddr s)]
           [before-final (string-append before (string k))]
           [result (list before-final after)])
      (test-parens-behavior/full (format "~a-~a" k (car s))
                                 before "" after k `(,result ,result)))))

;;; assorted other scenarios...
(test-parens-behavior/full 'open-parens
                           "abcd" "" "efg"  ; editor state: before, selected, after
                           #\(              ; key(s) pressed
                           '(["abcd(" "efg"]  ; result state sep by cursor, no auto-parens
                             ["abcd(" ")efg"])) ; result state with auto-parens

(test-parens-behavior/full 'open-parens-before-string
                           "abcd" "" "\"efg\""
                           #\(
                           '(["abcd(" "\"efg\""]  ["abcd(" ")\"efg\""]))
(test-parens-behavior/full 'open-parens-before-comment
                           "abcd" "" "; efg"
                           #\(
                           '(["abcd(" "; efg"]  ["abcd(" "); efg"]))

(test-parens-behavior/full 'close-1
                           "abcd" "" "efg"
                           #\)
                           '(["abcd)" "efg"]  ["abcd)" "efg"]))
(test-parens-behavior/full 'close-2
                           "(abcd" "" "efg"
                           #\)
                           '(["(abcd)" "efg"]  ["(abcd)" "efg"]))
(test-parens-behavior/full 'close-3
                           "(abcd" "" ")efg"
                           #\)
                           '(["(abcd)" ")efg"]  ["(abcd)" "efg"]))
(test-parens-behavior/full 'close-4
                           "(abcd efg " "" "   ) efg"
                           #\)
                           '(["(abcd efg )" "   ) efg"]
                             ["(abcd efg    )" " efg"]))
(test-parens-behavior/full 'close-5
                           "(define before+afters `([\"\" abc \"efg\" 12345 xyz]) [84])"
                           ""
                           ""
                           #\)
                           '(["(define before+afters `([\"\" abc \"efg\" 12345 xyz]) [84]))" ""]
                             ["(define before+afters `([\"\" abc \"efg\" 12345 xyz]) [84]))" ""]))
(test-parens-behavior/full 'close-6
                           "(define before+afters `([\"\" abc \"efg\""
                           ""
                           " 12345 xyz]) [84])"
                           #\)
                           '(["(define before+afters `([\"\" abc \"efg\"]" " 12345 xyz]) [84])"]
                             ["(define before+afters `([\"\" abc \"efg\"]" " 12345 xyz]) [84])"]))


(test-parens-behavior/full 'close-skip-1
                           "(define before+afters `([\"\" abc \"efg\" 12345 xyz]"
                           ""
                           "  ) [84])"
                           #\)
                           '(["(define before+afters `([\"\" abc \"efg\" 12345 xyz])" "  ) [84])"]
                             ["(define before+afters `([\"\" abc \"efg\" 12345 xyz]  )" " [84])"]))
(test-parens-behavior/full 'close-skip-fixup-1
                           "(define before+afters `{[abc 123]"
                           ""
                           "  ) [84])"
                           #\)   ; here the next close after ) doesn't match the {, so no skip happens
                           '(["(define before+afters `{[abc 123]}" "  ) [84])"]
                             ["(define before+afters `{[abc 123]}" "  ) [84])"]))
(test-parens-behavior/full 'close-skip-fixup-2
                           "(define before+afters `{[abc 123]"
                           ""
                           "  } [84])"
                           #\)   ; here the next close does match the {, so  skip
                           '(["(define before+afters `{[abc 123]}" "  } [84])"]
                             ["(define before+afters `{[abc 123]  }" " [84])"]))

(test-parens-behavior/full 'surround-open-1
                           "abcd" "ef" "g"
                           #\(
                           '(["abcd(" "g"]  ["abcd(" "ef)g"]))

(test-parens-behavior/full 'double-quote-1
                           "" "" ""
                           #\"
                           '(["\"" ""] ["\"" "\""]))
(test-parens-behavior/full 'double-quote-2
                           "abc " "" ""
                           #\"
                           '(["abc \"" ""] ["abc \"" "\""]))
(test-parens-behavior/full 'double-quote-selection-1
                           "(abc " "def 123" " xyz]"
                           #\"
                           '(["(abc \"" " xyz]"] ["(abc \"" "def 123\" xyz]"]))
(test-parens-behavior/full 'double-quote-skip-1
                           "\"abc def " "" "\" 123"
                           #\"
                           '(["\"abc def \"" "\" 123"] ["\"abc def \"" " 123"]))
(test-parens-behavior/full 'double-quote-escaped-1
                           "\"abcd \\" "" ""
                           #\"
                           '(["\"abcd \\\"" ""]
                             ["\"abcd \\\"" "\""])) ; this inserts double since string was not closed
(test-parens-behavior/full 'double-quote-escaped-2
                           "\"abcd \\" "" "\""
                           #\"
                           '(["\"abcd \\\"" "\""]
                             ["\"abcd \\\"" "\""]))
(test-parens-behavior/full 'double-quote-before-comment
                           "" "" "; 123"
                           #\"
                           '(["\"" "; 123"] ["\"" "\"; 123"]))
(test-parens-behavior/full 'double-quote-before-later-string
                           "" "" " \"\" "
                           #\"
                           '(["\"" " \"\" "] ["\"" "\" \"\" "]))

(test-parens-behavior/full 'bar
                           "abc " "" "123"
                           #\|
                           '(["abc |" "123"] ["abc |" "|123"]))
(test-parens-behavior/full 'bar-literal
                           "(list 1 #\\" "" ")"
                           #\|
                           '(["(list 1 #\\|" ")"] ["(list 1 #\\|" ")"]))
(test-parens-behavior/full 'bar-skip
                           "abc |def" "" "|123"
                           #\|
                           '(["abc |def|" "|123"] ["abc |def|" "123"]))
(test-parens-behavior/full 'bar-selection
                           "abc |def " "hij" "|123"
                           #\|
                           '(["abc |def |" "|123"] ["abc |def |" "hij||123"]))
(test-parens-behavior/full 'bar-before-string
                           "abc " "" "\"123\""
                           #\|
                           '(["abc |" "\"123\""] ["abc |" "|\"123\""]))
(test-parens-behavior/full 'bar-before-comment
                           "abc " "" "; 123"
                           #\|
                           '(["abc |" "; 123"] ["abc |" "|; 123"]))


(test-parens-behavior/full 'block-comment-1
                           " #" "" ""
                           #\|
                           '([" #|" ""]
                             [" #|" "|#"]))
(test-parens-behavior/full 'block-comment-2
                           "(123 abc#" "" " def 456)"
                           #\|
                           '(["(123 abc#|" " def 456)"]
                             ["(123 abc#|" "|# def 456)"]))
(test-parens-behavior/full 'block-comment-skip-1
                           "#| (123 abc" "" "|# def 456)"
                           #\|
                           '(["#| (123 abc|" "|# def 456)"]
                             ["#| (123 abc|#" " def 456)"]))

(test-parens-behavior/full 'close-adjusts-properly-when-space-follows-paren
                           "( x" "" ""
                           #\]
                           '(["( x)" "" ""]
                             ["( x)" "" ""]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-a-comment
                           "[();" "" ""
                           #\)
                           '(["[();)" "" ""]
                             ["[();)" "" ""]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-a-comment.2
                           "[;" "" "\n"
                           #\)
                           '(["[;)" "" "\n"]
                             ["[;)" "" "\n"]))
(test-parens-behavior/full 'close-adjusts-properly-at-eol-of-line-comment
                           "(;" "" "\n)"
                           #\)
                           '(["(;)" "" "\n)"]
                             ["(;)" "" "\n)"]))
(test-parens-behavior/full 'close-adjusts-properly-after-a-block-comment
                           "(#||#" "" "\n)"
                           #\)
                           '(["(#||#)" "" "\n)"]
                             ["(#||#\n)" "" ""]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-an-unclosed-string
                           "[()\"" "" ""
                           #\)
                           '(["[()\")" "" ""]
                             ["[()\")" "" ""]))
(test-parens-behavior/full 'close-adjusts-properly-when-inside-a-string
                           "[()\"" "" "\""
                           #\)
                           '(["[()\")" "" "\""]
                             ["[()\")" "" "\""]))

(test-parens-behavior/full 'close-adjusts-properly-when-no-containing-sexp
                           ")" "" ""
                           #\]
                           '([")]" "" ""]
                             [")]" "" ""]))

(test-parens-behavior/full '|"-splits-string|
                           " \"abcd" "" "efg\" "
                           #\"
                           '([" \"abcd\""     "efg\" "]
                             [" \"abcd\" "    "\"efg\" "]))
(test-parens-behavior/full '|"-splits-string-at-beginning|
                           " \"" "" "abcdefg\" "
                           #\"
                           '([" \"\""     "abcdefg\" "]
                             [" \"\" "    "\"abcdefg\" "]))
(test-parens-behavior/full '|"-splits-out-selected-string|
                           " \"abc" "def" "ghi\" "
                           #\"
                           '([" \"abc\"" "" "ghi\" "]
                             ; test that "def" remains selected afterwards...
                             [" \"abc\" "   "\"def\""    " \"ghi\" "]))

(test-parens-behavior/full 'delete-empty-block-comment
                           " #|" "" "|#"
                           #\backspace
                           '([" #" "|#"]
                             [" #" ""]))
(test-parens-behavior/full 'delete-bars-with-hash
                           " |" "" "|#"
                           #\backspace
                           '([" " "|#"]
                             [" " "#"]))
(test-parens-behavior/full 'delete-one-bar-between-hashes-in-string
                           " \"#|" "" "|#\""
                           #\backspace
                           '([" \"#" "|#\""]
                             [" \"#" "|#\""]))
(test-parens-behavior/full 'delete-escaped-double-quote-in-string
                           "\"abcd \\\"" "" "\""
                           #\backspace
                           '(["\"abcd \\" "\""]
                             ["\"abcd \\" "\""])) ; don't delete the non-escaped double quote

;; test that backspace only removes one character in most cases in non-empty strings and comments
(for ([open '("(" "[" "{" "\"" "|")]
      [close '(")" "]" "}" "\"" "|")])
  (define single-delete-scenarios
    ;    description     before-cursor    after-cursor
    '(("in-line-comment"  ";; abc def "    " ghi ")
      ("in-block-comment" "#| abc def "  " ghi |#")
      ("in-string"        "\" abc def "  " ghi \"")
      ))
  (for ([s single-delete-scenarios]
        #:unless (and (string=? "\"" open)
                      (string=? "in-string" (car s))))
    (let* ([before (cadr s)]
           [after (string-append close (caddr s))]
           [before-and-open (string-append before open)]
           [result (list before after)])
      (test-parens-behavior/full (format "~a-~a" (string-append open close) (car s))
                                 before-and-open "" after #\backspace `(,result ,result)))))


#| for these, the key-event with meta-down doesn't seem to work... maybe a Mac OS
  issue; and may cause problems with these tests on another platform? .nah. |#
(when (equal? 'macosx (system-type))
  (test-parens-behavior/full 'meta-open-1
                             "abcd" "" "efg"
                             '(escape #\()   ; '((new key-event% [key-code #\(] [meta-down #t]))
                             '(["abcd(" ")efg"]  ["abcd(" ")efg"]))

  (test-parens-behavior/full 'meta-close-skip-1
                             "(define before (list 1 2" "" " 3 4)"
                             '(escape #\))   ; '((new key-event% [key-code #\)] [meta-down #t]))
                             '(["(define before (list 1 2 3 4)" ""]
                               ["(define before (list 1 2 3 4)" ""]))
  (test-parens-behavior/full
   'meta-close-skip-2
   "#lang racket\n(define before+afters `([\"\" abc \"efg\""
   ""
   " 12345 xyz] [84])"
   '(escape #\))    ;'((new key-event% [key-code #\)] [meta-down #t]))
   '(["#lang racket\n(define before+afters `([\"\" abc \"efg\" 12345 xyz]" " [84])"]
     ["#lang racket\n(define before+afters `([\"\" abc \"efg\" 12345 xyz]" " [84])"]))
  (test-parens-behavior/full 'meta-close-skip-3
                             "(define before" "" " (list 1 2 3 4)"
                             '(escape #\))   ; '((new key-event% [key-code #\)] [meta-down #t]))
                             '(["(define before (list 1 2 3 4)" ""]
                               ["(define before (list 1 2 3 4)" ""])))
