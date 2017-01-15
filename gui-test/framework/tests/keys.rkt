#lang racket/gui
(require framework rackunit)

(check-equal?
 (let ([k (make-object keymap:aug-keymap%)])
   (send k add-function "abc" void)
   (send k map-function "c:k" "abc")
   (hash-map (send k get-map-function-table) list))
 '((c:k "abc")))

(check-equal?
 (let ([k (make-object keymap:aug-keymap%)]
       [ht (make-hasheq)])
   (send k add-function "abc" void)
   (send k map-function "c:k" "abc")
   (hash-set! ht 'c:k "def")
   (hash-map (send k get-map-function-table/ht ht) list))
 '((c:k "def")))

(check-equal?
 (let ([k (make-object keymap:aug-keymap%)]
       [k1 (make-object keymap:aug-keymap%)]
       [k2 (make-object keymap:aug-keymap%)])
   (send k1 add-function "abc-k1" void)
   (send k1 map-function "c:k" "abc-k1")
   (send k2 add-function "abc-k2" void)
   (send k2 map-function "c:k" "abc-k2")
   (send k chain-to-keymap k1 #t)
   (send k chain-to-keymap k2 #t)
   (hash-map (send k get-map-function-table) list))
 '((c:k "abc-k2")))

(check-equal?
 (let ([k (make-object keymap:aug-keymap%)]
       [k1 (make-object keymap:aug-keymap%)])
   (send k1 add-function "abc-k1" void)
   (send k1 map-function "c:k" "abc-k1")
   (send k add-function "abc-k" void)
   (send k map-function "c:k" "abc-k")
   (send k chain-to-keymap k1 #t)
   (hash-map (send k get-map-function-table) list))
 '((c:k "abc-k")))

(check-equal?
 (let ([k (make-object keymap:aug-keymap%)]
       [k1 (make-object keymap:aug-keymap%)])
   (send k1 add-function "abc-k1" void)
   (send k1 map-function "esc;p" "abc-k1")
   (send k add-function "abc-k2" void)
   (send k map-function "ESC;p" "abc-k2")
   (send k chain-to-keymap k1 #t)
   (hash-map (send k get-map-function-table) list))
 '((|esc;p| "abc-k2")))

(check-equal?
 (let ([k (make-object keymap:aug-keymap%)])
   (send k add-function "shift-em" void)
   (send k add-function "shift-ah" void)
   (send k map-function "s:m" "shift-em")
   (send k map-function "s:a" "shift-ah")
   (sort (hash-map (send k get-map-function-table) list)
         string<?
         #:key (lambda (x) (format "~s" x))))
 '((s:a "shift-ah") (s:m "shift-em")))

(check-equal?
 (let ()
   (define k0 (new keymap:aug-keymap%))
   (define k1 (new keymap:aug-keymap%))
   (define k2 (new keymap:aug-keymap%))
   (send k1 add-function "rectangle" void)
   (send k1 map-function "c:x;r;a" "rectangle")
   (send k2 add-function "swap if branches" void)
   (send k2 map-function "c:x;r" "swap if branches")
   (send k0 chain-to-keymap k1 #t)
   (send k0 chain-to-keymap k2 #t)
   (sort (hash-map (send k0 get-map-function-table) list)
         string<?
         #:key (lambda (x) (format "~s" x))))
 '((|c:x;r| "swap if branches")))

(check-equal? (keymap:canonicalize-keybinding-string "c:a") "c:a")
(check-equal? (keymap:canonicalize-keybinding-string "d:a") "d:a")
(check-equal? (keymap:canonicalize-keybinding-string "m:a") "m:a")
(check-equal? (keymap:canonicalize-keybinding-string "a:a") "a:a")
(check-equal? (keymap:canonicalize-keybinding-string "s:a") "s:a")
(check-equal? (keymap:canonicalize-keybinding-string "c:a") "c:a")
(check-equal? (keymap:canonicalize-keybinding-string "s:m:d:c:a:a") "a:c:d:m:s:a")
(check-equal? (keymap:canonicalize-keybinding-string "~s:~m:~d:~c:~a:a") "~a:~c:~d:~m:~s:a")
(check-equal? (keymap:canonicalize-keybinding-string ":a") "~a:~c:~d:~m:~s:a")
(check-equal? (keymap:canonicalize-keybinding-string ":d:a") "~a:~c:d:~m:~s:a")
(check-equal? (keymap:canonicalize-keybinding-string "esc;s:a") "esc;s:a")
(check-equal? (keymap:canonicalize-keybinding-string "s:a;esc") "s:a;esc")
(check-equal? (keymap:canonicalize-keybinding-string "ESC;p") "esc;p")
(check-equal? (keymap:canonicalize-keybinding-string "?:a:v") "?:a:v")
(check-equal? (keymap:canonicalize-keybinding-string "a:?:v") "?:a:v")
(check-equal? (keymap:canonicalize-keybinding-string "l:v") "l:v")
(check-equal? (keymap:canonicalize-keybinding-string "c:l:v") "c:l:v")

;; a key-spec is (make-key-spec buff-spec buff-spec (listof ?) (listof ?) (listof ?))
;; a key-spec represents a test case for a key; 'before' contains the
;; content of a buffer, and 'after' represents the desired content of the
;; buffer after the keypress.  The keypress(es) in question are specified
;; independently for the three platforms by the respective 'macos', 'unix',
;; and 'windows' fields.
(define-struct key-spec (before after macos unix windows) #:prefab)

;; an abstraction to use when all platforms have the same sequence of keys
(define (make-key-spec/allplatforms before after keys)
  (make-key-spec before after keys keys keys))

;; a buff-spec is (make-buff-spec string nat nat)
;; a buff-spec represents a buffer state; the content of the buffer,
;; and the start and end of the highlighted region.
;; the overwrite? field specifies if the overwrite mode is enabled during the test
;;    (its value is ignored for the result checking)
(define-struct buff-spec (string start end overwrite?) #:prefab)

(define (build-buff-spec string start end #:overwrite? [overwrite? #f])
  (make-buff-spec string start end overwrite?))

;; the keybindings test cases applied to frame:text% editors
(define global-specs
  (list
   (make-key-spec (build-buff-spec "abc" 1 1)
                  (build-buff-spec "abc" 2 2)
                  (list '((#\f control)) '((right)))
                  (list '((#\f control)) '((right)))
                  (list '((#\f control)) '((right))))

   (make-key-spec/allplatforms (build-buff-spec "\n\n\n\n" 2 2)
                               (build-buff-spec "\n" 0 0)
                               '(((#\x control) (#\o control))))
   (make-key-spec/allplatforms (build-buff-spec "  \n  \n  \n  \n" 7 7)
                               (build-buff-spec "  \n" 1 1)
                               '(((#\x control) (#\o control))))
   (make-key-spec/allplatforms (build-buff-spec "\n\n\n\n" 0 0)
                               (build-buff-spec "\n" 0 0)
                               '(((#\x control) (#\o control))))
   (make-key-spec/allplatforms (build-buff-spec "abcdef\n\n\n\nxyzpdq\n" 8 8)
                               (build-buff-spec "abcdef\n\nxyzpdq\n" 7 7)
                               '(((#\x control) (#\o control))))

   ;; TeX-compress tests
   (make-key-spec/allplatforms
    (build-buff-spec "\\ome" 4 4)
    (build-buff-spec "ω" 1 1)
    '(((#\\ control))))
   (make-key-spec/allplatforms
    (build-buff-spec "\\sub" 4 4)
    (build-buff-spec "\\subset" 7 7)
    '(((#\\ control))))
   (make-key-spec/allplatforms
    (build-buff-spec "\\subset" 7 7)
    (build-buff-spec "⊂" 1 1)
    '(((#\\ control))))
   (make-key-spec/allplatforms
    (build-buff-spec "\\sub" 4 4)
    (build-buff-spec "⊆" 1 1)
    '(((#\\ control) (#\e) (#\\ control))))))

(define (build-open-bracket-spec str pos char)
  (make-key-spec (build-buff-spec str pos pos)
                 (build-buff-spec
                  (string-append (substring str 0 pos)
                                 (string char)
                                 (substring str pos (string-length str)))
                  (+ pos 1)
                  (+ pos 1))
                 (list (list (list #\[)))
                 (list (list (list #\[)))
                 (list (list (list #\[)))))

(define (ascii-art-box-spec before after)
  (make-key-spec/allplatforms (build-buff-spec before 0 0)
                              (build-buff-spec after 0 0)
                              (list '((#\x control) (#\r) (#\a)))))

;; the keybindings test cases applied to racket:text% editors
(define scheme-specs
  (list
   (make-key-spec (build-buff-spec "(abc (def))" 4 4)
                  (build-buff-spec "(abc (def))" 10 10)
                  (list '((right alt)))
                  (list '((right alt)))
                  (list '((right alt))))
   (make-key-spec (build-buff-spec "'(abc (def))" 1 1)
                  (build-buff-spec "'(abc (def))" 12 12)
                  (list '((right alt)))
                  (list '((right alt)))
                  (list '((right alt))))
   #|
     (make-key-spec (build-buff-spec "'(abc (def))" 0 0)
                    (build-buff-spec "'(abc (def))" 12 12)
                    (list '(right alt))
                    (list '(right alt))
                    (list '(right alt)))
     (make-key-spec (build-buff-spec "'(abc (def))" 12 12)
                    (build-buff-spec "'(abc (def))" 0 0)
                    (list '(left alt))
                    (list '(left alt))
                    (list '(left alt)))
|#
   (build-open-bracket-spec "" 0 #\()
   (build-open-bracket-spec "(f cond " 8 #\()
   (build-open-bracket-spec "(f let (" 8 #\()
   (build-open-bracket-spec "(let (" 6 #\[)
   (build-open-bracket-spec "(let (" 5 #\()
   (build-open-bracket-spec "(provide/contract " 18 #\[)
   (build-open-bracket-spec "(kond " 5 #\()
   (build-open-bracket-spec "(cond " 5 #\[)
   (build-open-bracket-spec "(case-lambda " 13 #\[)
   (build-open-bracket-spec "(let ([]" 8 #\[)
   (build-open-bracket-spec "(let ({}" 8 #\{)
   (build-open-bracket-spec "()" 2 #\()
   (build-open-bracket-spec "(let (;;" 8 #\[)
   (build-open-bracket-spec ";" 1 #\[)
   (build-open-bracket-spec "\"" 1 #\[)
   (build-open-bracket-spec "\"\"" 1 #\[)
   (build-open-bracket-spec "||" 1 #\[)
   (build-open-bracket-spec "" 0 #\()
   (build-open-bracket-spec "(let (" 6 #\[)
   (build-open-bracket-spec "(new x% " 8 #\[)
   (build-open-bracket-spec "#\\" 2 #\[)
   (build-open-bracket-spec "#\\a" 2 #\[)
   (build-open-bracket-spec "(let ([let (" 12 #\()
   (build-open-bracket-spec "ab" 1 #\()
   (build-open-bracket-spec "|ab|" 2 #\[)
   (build-open-bracket-spec "(let loop " 10 #\()
   (build-open-bracket-spec "(let loop (" 11 #\[)
   (build-open-bracket-spec "(case x " 8 #\[)
   (build-open-bracket-spec "(case x [" 9 #\()
   (build-open-bracket-spec "(let ([])(" 10 #\()
   (build-open-bracket-spec "(local " 7 #\[)
   (build-open-bracket-spec "(local []" 9 #\()
   ;; test to show that multi-keystrokes works:
   (make-key-spec/allplatforms
    (build-buff-spec "" 0 0)
    (build-buff-spec "zx" 2 2)
    (list '((#\z) (#\x))))
   ;; remove-enclosing-parens :
   (make-key-spec/allplatforms
    (build-buff-spec "(abc def)" 1 1)
    (build-buff-spec "abc" 0 0)
    (list '((#\c control) (#\o control))))
   ;; (is this the desired behavior?):
   (make-key-spec/allplatforms
    (build-buff-spec "(abc def)" 2 3)
    (build-buff-spec "bc" 0 0)
    (list '((#\c control) (#\o control))))
   ;; insert-()-pair :
   (make-key-spec
    (build-buff-spec "abc" 0 0)
    (build-buff-spec "()abc" 1 1)
    (list '((escape) (#\()))
    (list '((#\( meta)))
    (list '((escape) (#\())))
   (make-key-spec
    (build-buff-spec "abc" 0 2)
    (build-buff-spec "(ab)c" 1 1)
    (list '((escape) (#\()))
    (list '((#\( meta)))
    (list '((escape) (#\())))
   ;; toggle-square-round-parens :
   ; () -> []
   (make-key-spec/allplatforms
    (build-buff-spec "(a)" 0 0)
    (build-buff-spec "[a]" 0 0)
    (list '((#\c control) (#\[ control))))
   ; [] -> ()
   (make-key-spec/allplatforms
    (build-buff-spec "[a]" 0 0)
    (build-buff-spec "(a)" 0 0)
    (list '((#\c control) (#\[ control))))
   ; enclosed sexps
   (make-key-spec/allplatforms
    (build-buff-spec "[a (def )b]" 0 0)
    (build-buff-spec "(a (def )b)" 0 0)
    (list '((#\c control) (#\[ control))))
   ; extra preceding whitespace
   (make-key-spec/allplatforms
    (build-buff-spec "  \n [a (def )b]" 0 0)
    (build-buff-spec "  \n (a (def )b)" 0 0)
    (list '((#\c control) (#\[ control))))
   ; cursor not at beginning of buffer
   (make-key-spec/allplatforms
    (build-buff-spec "  \n [a (def )b]" 1 1)
    (build-buff-spec "  \n (a (def )b)" 1 1)
    (list '((#\c control) (#\[ control))))
   ; intervening non-paren sexp
   (make-key-spec/allplatforms
    (build-buff-spec "  \nf [a (def )b]" 1 1)
    (build-buff-spec "  \nf [a (def )b]" 1 1)
    (list '((#\c control) (#\[ control))))
   ;; at end of buffer (hence sexp-forward returns #f):
   (make-key-spec/allplatforms
    (build-buff-spec "[a]" 3 3)
    (build-buff-spec "[a]" 3 3)
    (list '((#\c control) (#\[ control))))

   (make-key-spec/allplatforms
    (build-buff-spec "a" 0 0 #:overwrite? #t)
    (build-buff-spec "b" 1 1)
    (list '((#\b))))

   (make-key-spec/allplatforms
    (build-buff-spec "a" 0 0 #:overwrite? #t)
    (build-buff-spec "|" 1 1)
    (list '((#\|))))

   (make-key-spec/allplatforms
    (build-buff-spec "a" 0 0 #:overwrite? #t)
    (build-buff-spec "(" 1 1)
    (list '((#\())))

   (make-key-spec/allplatforms
    (build-buff-spec "a" 0 0 #:overwrite? #t)
    (build-buff-spec ")" 1 1)
    (list '((#\)))))

   ;; needs to be in auto-adjut open paren mode
   (make-key-spec/allplatforms
    (build-buff-spec "a" 0 0 #:overwrite? #t)
    (build-buff-spec "(" 1 1)
    (list '((#\[))))

   (ascii-art-box-spec "+" "═")
   (ascii-art-box-spec "x" "x")
   (ascii-art-box-spec "+-+" "═══")
   (ascii-art-box-spec "+\n|\n+\n" "║\n║\n║\n")
   (ascii-art-box-spec (string-append "+-+\n"
                                      "| |\n"
                                      "+-+\n")
                       (string-append "╔═╗\n"
                                      "║ ║\n"
                                      "╚═╝\n"))
   (ascii-art-box-spec (string-append "+---+\n"
                                      "| - |\n"
                                      "|+ ||\n"
                                      "+---+\n")
                       (string-append "╔═══╗\n"
                                      "║ - ║\n"
                                      "║+ |║\n"
                                      "╚═══╝\n"))
   (ascii-art-box-spec (string-append "+-+-+\n"
                                      "| | |\n"
                                      "+-+-+\n"
                                      "| | |\n"
                                      "+-+-+\n")
                       (string-append "╔═╦═╗\n"
                                      "║ ║ ║\n"
                                      "╠═╬═╣\n"
                                      "║ ║ ║\n"
                                      "╚═╩═╝\n"))))

(define automatic-scheme-specs
  (list (make-key-spec/allplatforms (build-buff-spec "" 0 0)
                                    (build-buff-spec "()" 1 1)
                                    '(((#\())))
        (make-key-spec/allplatforms (build-buff-spec "" 0 0)
                                    (build-buff-spec "[]" 1 1)
                                    '(((#\[))))
        (make-key-spec/allplatforms (build-buff-spec "" 0 0)
                                    (build-buff-spec "{}" 1 1)
                                    '(((#\{))))
        (make-key-spec/allplatforms (build-buff-spec "" 0 0)
                                    (build-buff-spec "\"\"" 1 1)
                                    '(((#\"))))
        (make-key-spec/allplatforms (build-buff-spec "" 0 0)
                                    (build-buff-spec "||" 1 1)
                                    '(((#\|))))))

(define (queue-callback/wait t)
  (define c (make-channel))
  (queue-callback (λ () (channel-put c (t))))
  (channel-get c))

(define (test-specs frame-name frame-class specs)
  (define f #f)
  (queue-callback/wait
   (λ ()
     (set! f (make-object frame-class frame-name))
     (send f show #t)))

  (for ([key-spec (in-list specs)]
        [i (in-naturals)])
    (define key-sequences
      ((case (system-type)
         [(macos macosx) key-spec-macos]
         [(unix) key-spec-unix]
         [(windows) key-spec-windows])
       key-spec))
    (define before (key-spec-before key-spec))
    (define after (key-spec-after key-spec))
    (for ([key-sequence (in-list key-sequences)])
      (define text-expect (buff-spec-string after))
      (define start-expect (buff-spec-start after))
      (define end-expect (buff-spec-end after))
      (queue-callback
       (λ ()
         (define frame (test:get-active-top-level-window))
         (define text (send frame get-editor))
         (send text set-overwrite-mode (buff-spec-overwrite? before))
         (send text erase)
         (send text insert (buff-spec-string before))
         (send text set-position (buff-spec-start before) (buff-spec-end before))))
      (for ([key (in-list key-sequence)])
        (test:keystroke (car key) (cdr key)))
      (check-equal?
       (queue-callback/wait
        (λ ()
          (define frame (test:get-active-top-level-window))
          (define text (send frame get-editor))
          (vector (send text get-text)
                  (send text get-start-position)
                  (send text get-end-position))))
       (vector text-expect start-expect end-expect)
       (~s (list key-sequence i)))))
  (queue-callback/wait (λ () (send f close))))

(let ([pref-ht (make-hash)])
  (parameterize ([test:use-focus-table #t]
                 [preferences:low-level-get-preference
                  (λ (sym [fail (λ () #f)])
                    (hash-ref pref-ht sym fail))]
                 [preferences:low-level-put-preferences
                  (λ (syms vals)
                    (for ([sym (in-list syms)]
                          [val (in-list vals)])
                      (hash-set! pref-ht sym val)))])
    ;; needs to be inside the test:use-focus-table setting
    (parameterize ([current-eventspace (make-eventspace)])

      (define dummy #f)
      (queue-callback
       (λ ()
         (set! dummy (make-object frame:basic% "dummy to trick frame group"))
         (send dummy show #t)))

      (preferences:set 'framework:fixup-open-parens #t)
      (preferences:set 'framework:automatic-parens #f)
      (test-specs "global keybindings test" frame:text% global-specs)
      (test-specs "racket mode keybindings test"
                  (class frame:editor%
                    (define/override (get-editor%) racket:text%)
                    (super-new))
                  scheme-specs)

      (preferences:set 'framework:automatic-parens #t)
      (preferences:set 'framework:fixup-open-parens #f)
      (test-specs "racket mode automatic-parens on keybindings test"
                  (class frame:editor%
                    (define/override (get-editor%) racket:text%)
                    (super-new))
                  automatic-scheme-specs)

      (queue-callback (λ () (send dummy show #f))))))
