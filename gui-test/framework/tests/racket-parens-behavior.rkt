#lang racket/base

(require "private/util.rkt"
         rackunit
         racket/class
         framework
         framework/private/color-local-member-name
         racket/gui/base)

(module+ test
  (with-private-prefs
    (auto-parens-tests)))

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

(define (auto-parens-tests)
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

  (test-parens-behavior/full 'open-parens
                             "ab🏴‍☠️cd" "" "efg"  ; editor state: before, selected, after
                             #\(              ; key(s) pressed
                             '(["ab🏴‍☠️cd(" "efg"]  ; result state sep by cursor, no auto-parens
                               ["ab🏴‍☠️cd(" ")efg"]))

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

  (let ()
    (define fixup-open-parens (preferences:get 'framework:fixup-open-parens))
    (define k (new key-event%
                   [key-code #\[]
                   [control-down #t]))
    (preferences:set 'framework:fixup-open-parens #t)
    (test-parens-behavior/full 'open-parens
                               "abc" "def" "ghi"  ; editor state: before, selected, after
                               (list k)           ; key(s) pressed
                               '(["abc[" "ghi"]   ; result state sep by cursor, no auto-parens
                                 ["abc[" "def]ghi"])) ; result state with auto-parens
    (preferences:set 'framework:fixup-open-parens fixup-open-parens))

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
                                 ["(define before (list 1 2 3 4)" ""]))))


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
