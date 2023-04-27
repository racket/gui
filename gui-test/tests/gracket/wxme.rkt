#lang racket/base
(require racket/class
         racket/contract
         racket/file
         racket/draw
         (only-in racket/gui/base
                  color% 
                  font%
                  the-clipboard
                  clipboard-client%
                  key-event%
                  mouse-event%
                  [text% full-text%])
         racket/snip
         mred/private/wxme/mline
         mred/private/wxme/editor
         mred/private/wxme/text
         mred/private/wxme/pasteboard
         "test-editor-admin.rkt"
         mred/private/wxme/keymap
         mred/private/wxme/editor-snip
         (for-syntax racket/base))

(define wrong-cnt 0)
(define test-cnt 0)

(define-syntax (expect stx)
  (syntax-case stx ()
    [(_ a b #:extra-stuff e)
     #`(expect/proc #,(syntax-line stx) a b e)]
    [(_ a b)
     #`(expect/proc #,(syntax-line stx) a b #f)]))

(define (expect/proc line v v2 extra-stuff)
  (set! test-cnt (add1 test-cnt))
  (unless (equal? v v2)
    (set! wrong-cnt (add1 wrong-cnt))
    (eprintf "FAILED: line ~a\nexpected: ~s\n     got: ~s\n"
             line
             v2
             v)
    (when extra-stuff
      (eprintf "   extra: ~s\n" extra-stuff))))

(define (done)
  (printf "\n~a tests\n" test-cnt)
  (flush-output)
  (if (zero? wrong-cnt)
      (printf "all passed\n")
      (begin
        (eprintf "~s FAILED\n" wrong-cnt)
        (exit 1))))

;; ----------------------------------------
;; String snips and lines

(define s (make-object string-snip% "helko"))
(send s insert "cat " 4 2)
(void (send s get-text 0 (send s get-count)))
(send s set-flags (cons 'invisible (send s get-flags)))
(void (send s get-flags))
(void (send (send (get-the-snip-class-list) find "wxtext") get-classname))

(define root-box (box mline-NIL))
(define m20 (mline-insert #f root-box #t))
(expect (mline-get-line m20) 0)
(define m00 (mline-insert m20 root-box #t))
(expect (mline-get-line m00) 0)
(expect (mline-get-line m20) 1)
(expect (mline-get-position m00) 0)
(expect (mline-get-position m20) 0)
(void (mline-set-length m00 5 4))
(void (mline-set-length m20 20 8))
(expect (mline-get-position m00) 0)
(expect (mline-get-position m20) 5)
(expect (mline-get-grapheme-position m20) 4)
(expect (mline-grapheme-len m20) 8)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; Line inserts and deletes

(define m5 (mline-insert m20 root-box #t))
(mline-check-consistent (unbox root-box))

(void (mline-set-length m5 10 8))

(expect (mline-get-position m00) 0)
(expect (mline-get-position m5) 5)
(expect (mline-get-position m20) 15)
(expect (mline-get-grapheme-position m20) 12)

(mline-delete m5 root-box)
(expect (mline-get-position m20) 5)
(expect (mline-get-grapheme-position m20) 4)

(set! m5 (mline-insert m20 root-box #t))
(void (mline-set-length m5 8 7))

(expect (mline-get-position m00) 0)
(expect (mline-get-position m5) 5)
(expect (mline-get-position m20) 13)
(expect (mline-get-grapheme-position m20) 11)

(mline-delete m5 root-box)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; Line counts and positions

(define m30 (mline-insert m20 root-box #f))

(expect (mline-get-line m00) 0)
(expect (mline-get-line m20) 1)
(expect (mline-get-line m30) 2)

(expect (mline-get-position m00) 0)
(expect (mline-get-position m20) 5)
(expect (mline-get-position m30) 25)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; More line lines and positions

(define m05 (mline-insert m00 root-box #f))

(void (mline-set-length m05 2 2))

(expect (mline-get-line m00) 0)
(expect (mline-get-line m05) 1)
(expect (mline-get-line m20) 2)
(expect (mline-get-line m30) 3)

(expect (mline-get-position m00) 0)
(expect (mline-get-position m05) 5)
(expect (mline-get-position m20) 7)
(expect (mline-get-position m30) 27)

(mline-check-consistent (unbox root-box))

;; ----------------------------------------
;; Line inserts and deletes, radomized

(let ([added
       (let loop ([l (list m00 m05 m20 m30)]
                  [n 100])
         (let ([m (mline-insert (list-ref l (random (length l))) 
                                root-box
                                (zero? (random 2)))])
           (mline-check-consistent (unbox root-box))
           (if (zero? n)
               (cons m l)
               (loop (cons m l) (sub1 n)))))])
  (for-each (lambda (i)
              (mline-delete i root-box)
              (mline-check-consistent (unbox root-box)))
            (cdr added))
  (expect (mline-next (car added)) #f)
  (expect (mline-prev (car added)) #f)
  (expect (unbox root-box)
          (car added)))

;; ----------------------------------------
;; Styles, deltas, lists

(define d1 (new style-delta%))
(define d2 (new style-delta%))
(expect (send d1 get-underlined-on) #f)
(expect (send d1 equal? d2) #t)
(send d1 set-underlined-on #t)
(expect (send d1 equal? d2) #f)
(void (send d2 collapse d1))
(expect (send d2 get-underlined-on) #t)
(send d2 set-underlined-on #f)
(send d1 copy d2)
(expect (send d1 get-underlined-on) #f)

(define sl (new style-list%))
(expect #t (eq? (send sl basic-style) (send sl basic-style)))
(define s-plain (send sl find-or-create-style (send sl basic-style)
                      (new style-delta%)))
(expect (send sl find-or-create-style (send sl basic-style)
              (new style-delta%))
        s-plain)

(send d1 set-underlined-on #t)
(define s-underlined (send sl find-or-create-style s-plain d1))
(expect (send s-plain get-underlined) #f)
(expect (send s-underlined get-underlined) #t)

(send d2 set-underlined-off #t)
(send d2 set-smoothing-on 'partly-smoothed)
(define s-nonunderlined1 (send sl find-or-create-style s-underlined d2))
(expect (send s-nonunderlined1 get-underlined) #f)
(expect (send s-nonunderlined1 get-base-style) (send sl basic-style)) ; due to collpasing

(define s-named-underlined (send sl new-named-style "underlined" s-underlined))
(define s-nonunderlined (send sl find-or-create-style s-named-underlined d2))
(expect (send s-nonunderlined get-underlined) #f)
(expect (send s-nonunderlined get-base-style) s-named-underlined)

(send d1 set-family 'modern)
(define s-modern (send sl find-or-create-style s-plain d1))
(expect (send s-modern get-underlined) #t)
(expect (send s-modern get-family) 'modern)
(expect (send s-plain get-family) 'default)

(expect (send s-plain is-join?) #f)

(define s-modern+nonunderlined (send sl find-or-create-join-style
                                     s-modern
                                     s-nonunderlined))
(expect (send s-modern+nonunderlined get-underlined) #f)
(expect (send s-modern+nonunderlined get-smoothing) 'partly-smoothed)
(expect (send s-modern+nonunderlined get-family) 'modern)
(expect (send s-modern+nonunderlined is-join?) #t)

(send d2 set-smoothing-on 'base)
(send s-nonunderlined set-delta d2)
(expect (send s-nonunderlined get-smoothing) 'default)
(expect (send s-modern+nonunderlined get-smoothing) 'default)

(send d1 set-style-on 'italic)
(send s-modern set-delta d1)
(expect (send s-modern get-style) 'italic)
(expect (send s-modern+nonunderlined get-style) 'italic)

(expect (send s-plain get-alignment) 'bottom)
(expect (send (send s-plain get-background) red) 255)
(expect (send s-plain get-base-style) (send sl basic-style))
(expect (send s-modern+nonunderlined get-base-style) s-modern)
(expect (send s-plain get-face) #f)
(expect (send s-plain get-name) #f)
(expect (send s-plain get-shift-style) (send sl basic-style))
(expect (send s-modern+nonunderlined get-shift-style) s-nonunderlined)
(expect (send s-plain get-size-in-pixels) #f)
(expect (send s-plain get-transparent-text-backing) #t)
(expect (send s-plain get-weight) 'normal)

(expect (send s-nonunderlined get-base-style) s-named-underlined)
(send s-nonunderlined set-base-style s-modern+nonunderlined) ; would create cycle
(expect (send s-nonunderlined get-base-style) s-named-underlined)

(send s-modern+nonunderlined set-base-style s-plain)
(expect (send s-modern+nonunderlined get-family) 'default)
(expect (send s-modern+nonunderlined get-style) 'normal)

(send s-modern+nonunderlined set-shift-style s-modern+nonunderlined) ; would create cycle

(define sl2 (new style-list%))
(define s2-modern (send sl2 convert s-modern))
(expect (send s2-modern get-family) 'modern)

(let ()
  (define changes '())
  (define t%
    (class text%
      (define/override (style-has-changed s)
        (set! changes (cons s changes)))
      (super-new)))
  (define sl (new style-list%))
  (define t (new t%))
  (send t set-style-list sl)
  (define d1 (new style-delta%))
  (send d1 set-weight-on 'bold)
  (define d2 (new style-delta%))
  (send d2 set-underlined-on #t)
  (expect changes '())
  (define named-style1 (send sl new-named-style "named-style1" (send sl basic-style)))
  (define named-style2 (send sl new-named-style "named-style2" (send sl basic-style)))
  (set! changes '())

  (send named-style1 set-delta d1)
  (expect changes (list #f named-style1))
  (set! changes '())

  (send named-style2 set-delta d1)
  (expect changes (list #f named-style2))
  (set! changes '())

  (send sl begin-style-change-sequence)
  (send named-style1 set-delta d2)
  (expect changes '())
  (send named-style2 set-delta d2)
  (expect changes '())
  (send sl end-style-change-sequence)
  (expect (car changes) #f)
  (expect (length changes) 3)
  (expect (and (member named-style1 changes) #t) #t)
  (expect (and (member named-style2 changes) #t) #t)
  (set! changes '())

  (send sl begin-style-change-sequence)
  (send sl begin-style-change-sequence)
  (send named-style1 set-delta d1)
  (expect changes '())
  (send named-style2 set-delta d1)
  (expect changes '())
  (send sl end-style-change-sequence)
  (send sl end-style-change-sequence)
  (expect (length changes) 3)
  (expect (and (pair? changes) (equal? (car changes) #f)) #t)
  (expect (and (member named-style1 changes) #t) #t)
  (expect (and (member named-style2 changes) #t) #t)
  (set! changes '()))

;; ----------------------------------------
;; Lines, positions, paragraphs

(define t (new text%))
(expect (send t get-text) "")
(expect (send t last-position) 0)
(expect (send t get-start-position) 0)
(expect (send t get-end-position) 0)
(expect (send t position-line 0) 0)
(expect (send t position-paragraph 0) 0)

(send t insert "hello")
(expect (send t get-text) "hello")
(expect (send t get-text 3) "lo")
(expect (send t get-text 2 4) "ll")
(expect (send t last-position) 5)
(expect (send t last-line) 0)
(expect (send t get-start-position) 5)
(expect (send t get-end-position) 5)
(expect (send t get-character 1) #\e)
(expect (send t position-line 1) 0)
(expect (send t position-paragraph 1) 0)

(send t insert "!\nbye")
(expect (send t get-text) "hello!\nbye")
(expect (send t last-position) 10)
(expect (send t line-length 0) 7)
(expect (send t line-length 1) 3)
(expect (send t last-line) 1)
(expect (send t line-start-position 0) 0)
(expect (send t line-start-position 1) 7)
(expect (send t line-end-position 0) 6)
(expect (send t position-line 0) 0)
(expect (send t position-line 1) 0)
(expect (send t position-line 6) 0)
(expect (send t position-line 7 #t) 0)
(expect (send t position-line 7) 1)
(expect (send t position-line 10) 1)
(expect (send t position-paragraph 1) 0)
(expect (send t position-paragraph 6) 0)
(expect (send t position-paragraph 7 #t) 1) ; no eol ambiguity for paragraphs
(expect (send t position-paragraph 7) 1)
(expect (send t position-paragraph 8) 1)
(expect (send t get-start-position) 10)
(expect (send t get-end-position) 10)

(send t set-position 7 8)
(expect (send t get-start-position) 7)
(expect (send t get-end-position) 8)
(expect
 (let ([b (box 0)][e (box 0)])
   (list
    (begin (send t get-position b) (unbox b))
    (begin (send t get-position #f e) (list (unbox b) (unbox e)))))
 '(7 (7 8)))

(send t insert ".\t," 2 4)
(expect (send t get-text) "he.\t,o!\nbye")
(expect (send t get-start-position) 8)
(expect (send t get-end-position) 9)

(send t insert "\n3\n" 10)
(expect (send t get-text) "he.\t,o!\nby\n3\ne")
(expect (send t last-line) 3)
(expect (send t get-start-position) 8)
(expect (send t get-end-position) 9)
(send t set-position 100)
(expect (send t get-start-position) 14)
(expect (send t get-end-position) 14)
(send t set-position 14)
(expect (send t get-start-position) 14)
(expect (send t get-end-position) 14)

(send t delete (send t last-position))
(expect (send t get-text) "he.\t,o!\nby\n3\n")
(expect (send t last-line) 3)
(expect (send t get-start-position) 13)
(expect (send t get-end-position) 13)

(send t insert "4" (send t last-position))
(expect (send t get-text) "he.\t,o!\nby\n3\n4")
(expect (send t last-line) 3)
(send t delete 9 11)
(expect (send t last-line) 2)
(expect (send t get-text) "he.\t,o!\nb3\n4")

(send t set-position 2 4)
(send t delete)
(expect (send t get-text) "he,o!\nb3\n4")
(expect (send t last-line) 2)
(expect (send t get-start-position) 2)
(expect (send t get-end-position) 2)
(expect (send t position-line 6) 1)
(expect (send t position-line 7) 1)
(expect (send t position-line 12) 2)

(send t insert (make-object string-snip% "?") 2)
(expect (send t get-text) "he?,o!\nb3\n4")

(expect (send t find-string "o") 4)
(expect (send t find-string "q") #f)
(expect (send t find-string "\n") 6)
(expect (send t find-string "\n" 'forward) 6)
(expect (send t find-string "\n" 'forward 7) 9)
(expect (send t find-string "\n" 'backward 7) 7)
(expect (send t find-string "\n" 'backward 9) 7)
(expect (send t find-string-all "\n") '(6 9))
(expect (send t find-string-all "\n" 'forward 3 7) '(6))
(expect (send t find-string-all "\n" 'backward 8 4) '(7))
(expect (send t find-string-all "\n" 'backward 8 4 #f) '(6))
(expect (send t find-string "\n4") 9)
(expect (send t find-string "O") #f)
(expect (send t find-string "O" 'forward 0 20 #t #f) 4)

(expect (send t find-next-non-string-snip #f) #f)

(let ()
  (define (txt s)
    (define t (new text%))
    (send t insert s)
    (send t set-position 0 0)
    t)
  (define (kmp-search txt str all?)
    (send txt do-find-string-all str 'forward 0 (send txt last-position) (not all?) #t #t #f))
  
  (expect (kmp-search (txt "x") "x" #f) 0)
  (expect (kmp-search (txt "yx") "x" #f) 1)
  (expect (kmp-search (txt "yx") "yx" #f) 0)
  (expect (kmp-search (txt "zyx") "yx" #f) 1)
  (expect (kmp-search (txt "yyx") "yx" #f) 1)
  (expect (kmp-search (txt "qqq") "yx" #f) #f)
  (expect (kmp-search (txt "ABC ABCDAB ABCDABCDABDE") "ABCDABD" #f) 15)
  (expect (kmp-search (txt "xxxx") "y" #t) '())
  (expect (kmp-search (txt "xxxx") "x" #t) '(0 1 2 3))
  (expect (kmp-search (txt "xyxy") "x" #t) '(0 2))
  (expect (kmp-search (txt " x\n ") "x" #t) '(1))
  (expect (kmp-search (txt "") "x" #t) '())
  (expect (send (txt " x\n ") do-find-string-all "X" 'forward 0 'eof #f #t #f #f)
          '(1))
  (expect (send (txt "xXxXxX") do-find-string-all "x" 'forward 0 'eof #f #t #f #f)
          '(0 1 2 3 4 5))
  (expect (send (txt "xXxXxX") do-find-string-all "x" 'forward 2 4 #f #t #f #f)
          '(2 3))
  (expect (send (txt "xyxyxyxyxyx") do-find-string-all "xy" 'forward 2 5 #f #t #t #f)
          '(2))
  (expect (send (txt "abcdabcdabcd") do-find-string-all "abcd" 'forward 0 'eof #f #f #t #f)
          '(4 8 12))
  (expect (send (txt "qqabcdabcdabcd") do-find-string-all "abcd" 'forward 0 'eof #t #f #t #f)
          6)
  (expect (send (txt "qqabcdabcdabcd") do-find-string-all "abcd" 'forward 0 'eof #t #t #t #f)
          2)
  (expect (send (txt "abcdabcdabcd") do-find-string-all "abcd" 'backward 12 0 #f #t #t #f)
          '(12 8 4))
  (expect (send (txt "abcdabcdabcd") do-find-string-all "abcd" 'backward 12 0 #f #f #t #f)
          '(8 4 0))
  (expect (send (txt "abcd\nabcdabcd") do-find-string-all "abcd" 'backward 12 0 #f #t #t #f)
          '(9 4))
  (expect (send (txt "abcd\nabcdabcd") do-find-string-all "abcd" 'backward 13 0 #f #t #t #f)
          '(13 9 4))
  (expect (send (txt "abcdabcd\nabcd") do-find-string-all "abcd" 'backward 12 0 #f #t #t #f)
          '(8 4))
  (expect (send (txt "abcdabcd\nabcd") do-find-string-all "abcd" 'backward 13 0 #f #t #t #f)
          '(13 8 4))
  (expect (send (txt "abcdabcd\nabcd") do-find-string-all "abcd" 'backward 8 0 #f #t #t #f)
          '(8 4))
  (expect (send (txt "abcdabcd\nabcd") do-find-string-all "abcd" 'forward 4 13 #f #t #t #f)
          '(4 9))
  (expect (send (txt "xyz") do-find-string-all "xyz" 'backward 3 0 #t #f #t #f)
          0)
  (expect (send (txt "xyz") do-find-string-all "xyz" 'backward 3 0 #t #t #t #f)
          3)
  
  (let ([t (new text%)])
    (send t insert "abc")
    (send t insert "abc")
    (send t insert "abc")
    (expect (send t do-find-string-all "abc" 'forward 0 (send t last-position) #f #t #t #t)
            '(0 3 6))
    (expect (send t do-find-string-all "abc" 'backward (send t last-position) 0 #f #t #t #t)
            '(9 6 3))
    (expect (send t do-find-string-all "ca" 'forward 0 (send t last-position) #f #t #t #t)
            '(2 5))
    (expect (send t do-find-string-all "ca" 'backward (send t last-position) 0 #f #t #t #t)
            '(7 4)))
  
  (let ([t1 (new text%)]
        [t2 (new text%)])
    (send t1 insert "abc")
    (send t1 insert (new editor-snip% [editor t2]))
    (send t1 insert "abc")
    (send t2 insert "abc")
    (expect (send t1 do-find-string-all "abc" 'forward 0 (send t1 last-position) #f #t #t #t)
            (list 0 (list t2 0) 4))
    (expect (send t1 do-find-string-all "abc" 'backward (send t1 last-position) 0 #f #t #t #t)
            (list 7 (list t2 3) 3))
    (expect (send t1 do-find-string-all "abca" 'forward 0 (send t1 last-position) #f #t #t #t)
            '())
    (expect (send t1 do-find-string-all "cabc" 'forward 0 (send t1 last-position) #f #t #t #t)
            '()))
  
  (let ([t1 (new text%)]
        [t2 (new text%)])
    (send t1 insert "abc")
    (send t1 insert (new editor-snip% [editor t2]))
    (send t2 insert "abc")
    (expect (send t1 do-find-string-all "abc" 'forward 0 (send t1 last-position) #f #t #t #t)
            (list 0 (list t2 0)))
    (expect (send t1 do-find-string-all "abc" 'backward (send t1 last-position) 0 #f #t #t #t)
            (list (list t2 3) 3)))
  
  (let ([t1 (new text%)]
        [t2 (new text%)])
    (send t1 insert "abc")
    (send t1 insert (new editor-snip% [editor t2]))
    (send t1 insert "abcd")
    (send t2 insert "abc")
    (expect (send t1 do-find-string-all "abcd" 'forward 0 (send t1 last-position) #t #t #t #t)
            4))
  
  (let ([t1 (new text%)]
        [t2 (new text%)])
    (send t1 insert "abc")
    (send t1 insert (new editor-snip% [editor t2]))
    (send t1 insert "abc")
    (send t2 insert "abcd")
    (expect (send t1 do-find-string-all "abcd" 'forward 0 (send t1 last-position) #t #t #t #t)
            (cons t2 0)))
  
  (let ([t1 (new text%)]
        [t2 (new text%)]
        [pb (new pasteboard%)])
    (send t1 insert "abc")
    (send t1 insert (new editor-snip% [editor pb]))
    (send pb insert (new editor-snip% [editor t2]))
    (send t1 insert "abc")
    (send t2 insert "abcd")
    (expect (send t1 do-find-string-all "abcd" 'forward 0 (send t1 last-position) #t #t #t #t)
            (list* pb t2 0)))
  
  (let ([t1 (new text%)]
        [t2 (new text%)]
        [t3 (new text%)]
        [pb (new pasteboard%)])
    (send t1 insert "abc")
    (send t1 insert (new editor-snip% [editor pb]))
    (send pb insert (new editor-snip% [editor t2]))
    (send pb insert (new editor-snip% [editor t3]))
    (send t1 insert "abc")
    (send t2 insert "abcd")
    (send t3 insert "abcd")
    (expect (send t1 do-find-string-all "abcd" 'forward 0 (send t1 last-position) #f #t #t #t)
            (list (list pb (list t2 0) (list t3 0)))))
  
  (let ([t1 (new text%)])
    (send t1 insert "abc")
    (define es (new editor-snip%))
    (send t1 insert es)
    (send t1 insert "abc")
    (expect (send t1 do-find-string-all "abcd" 'forward 0 (send t1 last-position) #f #t #t #t)
            '())
    (expect (send t1 do-find-string-all "abca" 'forward 0 (send t1 last-position) #f #t #t #t)
            '())
    (expect (send t1 do-find-string-all "cabc" 'forward 0 (send t1 last-position) #f #t #t #t)
            '()))
  
  (let ([t1 (new text%)]
        [pb (new pasteboard%)])
    (send t1 insert "abc")
    (send t1 insert (new editor-snip% [editor pb]))
    (send t1 insert "abc")
    (send pb insert (new editor-snip%))
    (send pb insert (new editor-snip%))
    (expect (send t1 do-find-string-all "abcd" 'forward 0 (send t1 last-position) #f #t #t #t)
            '())
    (expect (send t1 do-find-string-all "abca" 'forward 0 (send t1 last-position) #f #t #t #t)
            '())
    (expect (send t1 do-find-string-all "cabc" 'forward 0 (send t1 last-position) #f #t #t #t)
            '()))
  
  (expect (send (txt "aaa") find-string-all "a") '(0 1 2))
  (expect (send (txt "aaa") find-string-all "aa") '(0 1))
  (expect (send (txt "aaa") find-string-all "aaa") '(0))
  (expect (send (txt "aaa") find-string-all "aaaa") '()))

(let ()
  (define (slow-string-search search-str text)
    (define result
      (let loop ([text text])
        (define lp (send text last-position))
        (cons/f text
                (for/or ([t-i (in-range (+ 1 lp))])
                  (define s (send text find-snip t-i 'after-or-none))
                  (cond
                    [(is-a? s editor-snip%)
                     (loop (send s get-editor))]
                    [else
                     (and (for/and ([c (in-string search-str)]
                                    [s-i (in-naturals)])
                            (define i (+ t-i s-i))
                            (and (i . <= . lp)
                                 (equal? c (send text get-character i))))
                          t-i)])))))
    (cond
      [result (cdr result)]
      [else result]))

  (define (cons/f txt result)
    (cond
      [result (cons txt result)]
      [else #f]))

  (define (fast-string-search search-str text)
    (send text find-string-embedded search-str 'forward 0))

  (define (make-random-thing-to-search-in)
    (define fuel 100)
    (let loop ([depth 2])
      (cond
        [(<= fuel 0) '()]
        [(zero? depth)
         (define size (random 10))
         (set! fuel (- fuel size))
         (list (random-string size))]
        [else
         (cond
           [(zero? (random 5))
            (set! fuel (- fuel 1))
            (cons (loop (- depth 1)) (loop depth))]
           [else
            (define size (random 20))
            (set! fuel (- fuel size))
            (cons (random-string size)
                  (loop depth))])])))

  (define (random-string len)
  (define s (make-string len))
  (for ([i (in-range len)])
    (string-set! s i (random-char)))
  s)

  (define (random-char) (integer->char (+ (random 4) (char->integer #\a))))

  (for ([__ (in-range 10000)])
    (define thing-to-search-in (make-random-thing-to-search-in))
    (define thing-to-search-for (random-string (+ 2 (random 3))))
    (define text-to-search-in (new text%))
    (let loop ([text text-to-search-in]
               [thing-to-search-in thing-to-search-in])
      (for ([ele (in-list thing-to-search-in)])
        (define lp (send text last-position))
        (cond
          [(list? ele)
           (define embedded-text (new text%))
           (send text insert (new editor-snip% [editor embedded-text]) lp lp)
           (loop embedded-text ele)]
          [else
           (send text insert ele lp lp)])))
    (expect (slow-string-search thing-to-search-for text-to-search-in)
            (fast-string-search thing-to-search-for text-to-search-in)
            #:extra-stuff (list thing-to-search-for thing-to-search-in))))

;; ----------------------------------------
;; Graphemes

(let ()
  (define t (new text%))
  (send t insert "he\u300llo")
  (expect (send t position-grapheme 5) 4)
  (expect (send t grapheme-position 5) 6)
  (send t insert "a" 1)
  (expect (send t position-grapheme 0) 0)
  (expect (send t position-grapheme 1) 1)
  (expect (send t position-grapheme 2) 2)
  (expect (send t position-grapheme 5) 4)
  (expect (send t grapheme-position 5) 6)
  (expect (send t last-position) 7)
  (send t set-position 4)
  (send t delete)
  (expect (send t last-position) 5)
  (send t insert "e\u300")
  (expect (send t last-position) 7)
  (expect (send t grapheme-position 6) 7)
  (expect (send t position-grapheme 2) 2)
  (expect (send t position-grapheme 3) 2)
  (expect (send t position-grapheme 4) 3)
  (expect (send t position-grapheme 7) 6)
  (send t insert "\n" 1)
  (expect (send t last-position) 8)
  (expect (send t position-grapheme 8) 7)
  (expect (send t position-grapheme 3) 3)
  (expect (send t position-grapheme 5) 4))

(let ()
  (define prog
    (apply string-append
           (map (lambda (l) (string-append l "\n"))
                '("#lang racket"
                  ""
                  "'🏴‍☠️"
                  "'(🏳️‍🌈 🇦🇩 📸 ☮️)"
                  "(list '🏴‍☠️"
                  "      '🏴‍☠️"
                  "      '🏴‍☠️"
                  "      '🏴‍☠️"
                  "      '⏰)"
                  ""
                  "#false"))))
  (define (check-prog t)
    (let loop ([snip (send t find-first-snip)])
      (when snip
        (define s (send snip get-text 0 (send snip get-count)))
        (expect (send snip get-grapheme-count) (string-grapheme-count s))
        (loop (send snip next))))
    (expect (send t last-position) (string-length prog))
    (expect (send t position-grapheme (send t last-position)) (string-grapheme-count prog))
    (let ([counts (make-vector (add1 (string-length prog)) 0)])
      (let loop ([n 0] [i 0])
        (cond
          [(= i (string-length prog))
           (vector-set! counts i n)]
          [else
           (define len (string-grapheme-span prog i))
           (for ([j (in-range len)])
             (vector-set! counts (+ i j) n))
           (loop (add1 n) (+ i len))]))
      (for ([i (in-range (string-length prog))])
        (unless (= (send t position-grapheme i) (vector-ref counts i))
          (printf "wrong at ~a: ~a ~a\n" i (send t position-grapheme i) (vector-ref counts i))))))
  ;; whole string
  (let ()
    (define t (new text%))
    (send t insert prog)
    (check-prog t))
  ;; char by char
  (let ()
    (define t (new text%))
    (for ([i (in-string prog)])
      (send t insert i))
    (check-prog t))
  ;; reverse chars
  (let ()
    (define t (new text%))
    (for ([i (in-list (reverse (string->list prog)))])
      (send t insert i 0 0))
    (check-prog t))
  ;; grapheme-by-grapheme
  (let ()
    (define t (new text%))
    (let loop ([i 0])
      (unless (= i (string-length prog))
        (define len (string-grapheme-span prog i))
        (send t insert (substring prog i (+ i len)))
        (loop (+ i len))))
    (check-prog t))
  ;; 3 bytes at a time, request merging, need for merging triggered by style change
  (let ()
    (define t (new text%))
    (define normal (send (send t get-style-list) basic-style))
    (define bold (send (send t get-style-list) find-or-create-style normal (make-object style-delta% 'change-bold)))
    (let loop ([i 0])
      (unless (= i (string-length prog))
        (define len (min 3 (- (string-length prog) i)))
        (define end (+ i len))
        (send t insert (substring prog i end) (send t get-end-position) 'same #f #t) ; <- `#t` here matters
        (define start (send t grapheme-position (send t position-grapheme i)))
        (send t change-style bold start end)
        (send t change-style normal end end)
        (loop (+ i len))))
    (check-prog t))
  
  (void))

(let ()
  ;; Regression test for a style change that involves a split
  ;; where the number of split-off characters matches the delta
  ;; between chars and graphemes
  (define t (new text%))

  (define normal (send (send t get-style-list) basic-style))
  (define bold (send (send t get-style-list) find-or-create-style normal (make-object style-delta% 'change-bold)))
  (define underline (send (send t get-style-list) find-or-create-style normal (make-object style-delta% 'change-underline)))

  (send t insert "(ความกว้าง 500)")
  (send t change-style underline 0 10)
  (send t change-style bold 1 1)
  (expect (for/list ([i (in-range (send t last-position))])
            (send t position-grapheme i))
          '(0 1 2 3 4 5 6 6 7 8 9 10 11 12 13))
  (expect (for/list ([i (in-range (send t last-position))])
            (send t grapheme-position i))
          '(0 1 2 3 4 5 6 8 9 10 11 12 13 14 15)))

(let ()
  ;; these strings are picked in a way to make the final insert
  ;; shift a buffer content leaving #\uFE0F just after the content
  (define pre0 "xx")
  (define pre1 "\ufe0f\ufe0f\ufe0f")
  (define pre (string-append pre0 pre1))
  (define str "\u200d\u2620\ufe0f")

  (define s (make-object string-snip% pre0))
  (send s insert pre1 (string-length pre1) (string-length pre1))
  (send s insert str (string-length str) (string-length pre))
  (define s2
    (let ([a (box #f)]
          [b (box #f)])
      (send s split (string-length pre) a b)
      (unbox b)))

  (send s2 insert "z" 1 (string-length str))

  (define bm (make-object bitmap% 10 10))
  (define dc (send bm make-dc))

  (expect
   (let ([w (box 0)])
     (send s2 get-extent dc 0 0 w)
     (unbox w))
   (send s2 partial-offset dc 0 0 (add1 (string-length str)))))

(let ()
  (define bts '(#"\360\237\217\264" #"\342\200\215" #"\342\230\240" #"\357\270\217"))
  (define txt (new text%))
  (send txt set-styles-sticky #f)
  (send txt insert "az")
  (for ([b (in-list bts)]
        [i (in-naturals)])
    (send txt insert (bytes->string/utf-8 b)
          (- (send txt last-position) 1)
          (- (send txt last-position) 1)
          #t #t)
    (when (= i 0)
      (send txt change-style (make-object style-delta% 'change-bold) 1 2)))
  (expect (for/list ([i (in-list '(0 1 2 3))])
            (send txt grapheme-position i))
          '(0 1 5 6)))


;; ----------------------------------------

;; Insert very long strings to test max-string-length handling
(send t delete 0 (send t last-position))
(send t insert (make-string 256 #\a))
(send t insert (make-string 256 #\a))
(send t insert (make-string 256 #\a))
(send t insert (make-string 256 #\a))
(send t insert (make-string 1024 #\a))
(expect (send t last-position) 2048)

;; ----------------------------------------
;; test how the insertion point moves during deletions

(let ()

  (define lines
    (list
     "(define (f x)\n"
     "  (+ x x)\n"
     "  (+ x x))\n"))

  (define init-position
    (+ (string-length (list-ref lines 0))
       (string-length (list-ref lines 1))
       -1 ;; before the newline
       ))

  (define second-position
    (+ (string-length (list-ref lines 0))
       2 ;; after the leading space
       ))

  (let ()
    (define t (new text%))

    (send t insert (apply string-append lines))
    (send t set-position init-position)
    (for ([_ (in-range (- init-position second-position))])
      (send t move-position 'left #t))
    (send t delete 16 23)
    (send t move-position 'home #t)
    (expect (send t get-start-position) 0)
    (expect (send t get-end-position) 16))

  (let ()
    (define t (new text%))

    (send t insert (apply string-append lines))
    (send t set-position init-position)
    (for ([_ (in-range (- init-position second-position))])
      (send t move-position 'left #f))
    (send t delete 16 23)
    (send t move-position 'home #t)
    (expect (send t get-start-position) 0)
    (expect (send t get-end-position) 16)))


;; ----------------------------------------
;; Moving and word boundaries

(send t delete 0 (send t last-position))
(send t insert "do you like\ngreen eggs and ham?")
(expect (send t position-paragraph 0) 0)
(expect (send t position-paragraph 12) 1)
(expect (send t paragraph-start-position 1) 12)
(expect (send t paragraph-start-position 2) 31)
(expect (send t find-newline 'forward 0) 12)
(expect (send t find-newline 'forward 12) 31)
(expect (send t get-text) "do you like\ngreen eggs and ham?")
(send t set-position 0)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 2)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 6)
(send t move-position 'left #f 'word)
(expect (send t get-start-position) 3)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 6)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 11)
(send t move-position 'right #f 'simple)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 17)
(send t set-position 11)
(send t move-position 'right #f 'word)
(expect (send t get-start-position) 17)

(define (check-positions graphics?)
  (define snips+counts
    (let loop ([snip (send t find-first-snip)])
      (if snip
          (cons (cons snip (send snip get-count))
                (loop (send snip next)))
          null)))

  (let ([x (box 0.0)]
        [y (box 0.0)])
    (let loop ([s+c snips+counts]
               [pos 0])
      (unless (null? s+c)
        (let ([p (send t get-snip-position (caar s+c))])
          (expect p pos)
          (let ([p2 (box 0)])
            (when graphics?
              (if (send t get-snip-position-and-location (caar s+c) p2 x y)
                  (expect (unbox p2) pos)
                  (expect #f #t)))
            (loop (cdr s+c) (+ pos (cdar s+c))))))))

  (for-each 
   (lambda (before)
     (let loop ([pos 0][s+c snips+counts][snip-pos 0])
       (if (null? s+c)
           (expect pos (add1 (send t last-position)))
           (let* ([s-pos (box 0)]
                  [s (send t find-snip pos before s-pos)])
             (let ([es (if (and (= pos 0) (eq? before 'before-or-none))
                           #f
                           (caar s+c))])
               (expect s es)
               (expect (unbox s-pos) snip-pos)
               (let ([next? (= pos (+ snip-pos (cdar s+c)))])
                 (loop (add1 pos)
                       (if next?
                           (cdr s+c)
                           s+c)
                       (if next?
                           (+ snip-pos (cdar s+c))
                           snip-pos))))))))
   '(before before-or-none))

  (for-each 
   (lambda (after)
     (let loop ([pos 0][s+c snips+counts][snip-pos 0][prev #f][prev-snip-pos 0])
       (let* ([s-pos (box 0)]
              [s (send t find-snip pos after s-pos)]
              [end? (null? s+c)]
              [es (if end?
                      (if (eq? after 'after-or-none)
                          #f
                          (car prev))
                      (caar s+c))]
              [ep (if end? (if es prev-snip-pos 0) snip-pos)])
         (expect s es)
         (expect (unbox s-pos) ep)
         (if end?
             (expect pos (send t last-position))
             (let ([next? (= (add1 pos) (+ snip-pos (cdar s+c)))])
               (loop (add1 pos)
                     (if next?
                         (cdr s+c)
                         s+c)
                     (if next?
                         (+ snip-pos (cdar s+c))
                         snip-pos)
                     (car s+c)
                     snip-pos))))))
   '(after after-or-none)))

(check-positions #f)

;; ----------------------------------------
;; Line flow

;; Every character is 10.0 high, 10.0 wide, 1.0 descent, 1.0 top space
(send t set-admin (new test-editor-admin%))

(define (check-simple-locations pl pt pr pb)
  (expect (let ([x (box 0.0)] [y (box 0.0)])
            (list (begin
                    (send t position-location 1 x y)
                    (list (unbox x) (unbox y)))
                  (begin
                    (send t position-location 1 x y #f)
                    (list (unbox x) (unbox y)))))
          (list (list (+ pl 10.0) (+ pt 0.0))
                (list (+ pl 10.0) (+ pt 10.0))))
  (expect (let ([x (box 0.0)] [y (box 0.0)])
            (list (begin
                    (send t position-location 14 x y)
                    (list (unbox x) (unbox y)))
                  (begin
                    (send t position-location 14 x y #f)
                    (list (unbox x) (unbox y)))))
          (list (list (+ pl 20.0) (+ pt 11.0))
                (list (+ pl 20.0) (+ pt 21.0))))
  (expect (let ([w (box 0.0)] [h (box 0.0)])
            (send t get-extent w h)
            (list (unbox w) (unbox h)))
          (list (+ 192.0 pl pr)
                (+ 22.0 pt pb))))
(check-simple-locations 0 0 0 0)

(send t set-padding 5.0 8.0 11.0 13.0)
(check-simple-locations 5 8 11 13)
(send t set-padding 0 0 0 0)

(expect (send t find-position 0.0 0.0) 0)
(expect (send t find-position 0.0 3.0) 0)
(expect (send t find-position 10.0 0.0) 1)
(expect (send t find-position 13.0 0.0) 1)
(expect (send t find-position 0.0 12.0) 12)
(expect (send t find-position 13.0 12.0) 13)
(expect (send t find-position 13.0 23.0) 31)
(expect (send t find-position 0.0 230.0) 31)
(expect (send t find-position 300.0 2.0) 11)
(expect (send t find-position -1.0 12.0) 12)
(expect (send t find-position 109.0 2.0) 10)
(expect (send t find-position 110.0 2.0) 11)
(expect (let ([b (box #f)])
          (send t find-position 1.0 12.0 #f b)
          (unbox b))
        #t)
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (send t find-position -1.0 12.0 #f b e)
          (list (unbox b) (unbox e)))
        '(#f 100.0))
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (list (send t find-position 109.0 2.0 #f b e)
                (unbox b) 
                (unbox e)))
        '(10 #t 1.0))
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (list (send t find-position 102.0 2.0 #f b e)
                (unbox b) 
                (unbox e)))
        '(10 #t -2.0))
(expect (let ([b (box #f)]
              [e (box 0.0)])
          (list (send t find-position 110.0 2.0 #f b e)
                (unbox b)
                (unbox e)))
        '(11 #f 100.0))
(expect (send t find-position-in-line 0 14.0) 1)
(expect (send t find-position-in-line 1 14.0) 13)

(send t set-position 1 1)
(send t move-position 'down #f 'line)
(expect (send t get-start-position) 13)
(send t move-position 'right #f 'simple)
(send t move-position 'up #f 'line)
(expect (send t get-start-position) 2)

(check-positions #t)

(send t set-max-width 71.0)

(define (check-ge&h-flow)
  (expect (send t last-line) 6)
  (expect (send t line-start-position 0) 0)
  (expect (send t line-start-position 1) 3)
  (expect (send t line-start-position 2) 7)
  (expect (send t line-start-position 3) 12)
  (expect (send t line-start-position 4) 18)
  (expect (send t line-start-position 5) 23)
  (expect (send t line-start-position 6) 27)
  (expect (send t last-paragraph) 1)
  (expect (send t paragraph-start-position 0) 0)
  (expect (send t paragraph-end-position 0) 11)
  (expect (send t paragraph-start-position 1) 12)
  (expect (send t paragraph-end-position 1) 31)
  (expect (send t paragraph-start-position 2) 31)
  (void))
(check-ge&h-flow)

(check-positions #t)

(send t set-max-width 200.0)
(expect (send t last-line) 1)

(send t set-max-width 71.0)
(check-ge&h-flow)

(send t insert "Sir: " 0)
(expect (send t last-line) 7)
(expect (send t line-start-position 7) 32)
(send t delete 0 5)
(check-ge&h-flow)

(define (check-line-starts)
  (let ([lens (let loop ([snip (send t find-first-snip)][len 0])
                (if snip
                    (let ([len (+ len (send snip get-count))])
                      (let ([s (send snip get-text 0 (send snip get-count))])
                        (when (regexp-match? #rx"\n" s)
                          (unless (and (memq 'hard-newline (send snip get-flags))
                                       (string=? s "\n"))
                            (error "embedded newline!")))
                        (if (or (memq 'newline (send snip get-flags))
                                (memq 'hard-newline (send snip get-flags)))
                            (cons len (loop (send snip next) 0))
                            (loop (send snip next) len))))
                    (list len)))])
    (for/fold ([pos 0]) ([i (in-range (add1 (send t last-line)))]
                         [len (in-list lens)])
      (expect (send t line-start-position i #f) pos)
      (expect (send t line-end-position i #f) (+ pos len))
      (+ pos len))))

(for-each
 (lambda (str)
   ;; (printf ">> ~s <<\n" str)
   (for ([i (in-range (add1 (send t last-position)))])
     (check-line-starts)
     (send t insert str i)
     (check-line-starts)
     ;; (printf "=> ~a ~s\n" i (send t get-text 0 'eof #t #t))
     (send t last-line)
     (send t delete i (+ i (string-length str)))
     (check-line-starts)
     ;; (printf "~a ~s <=\n" i (send t get-text 0 'eof #t #t))
     (check-ge&h-flow)))
 '(" a" "a " "qvzxw " " qvxzw" "qqq qqqq" "a\nb"))

;; ----------------------------------------
;; Undo

(send t set-modified #f)
(send t set-max-undo-history 100)
(send t delete 0 3)
(expect (send t get-text) "you like\ngreen eggs and ham?")
(expect (send t modified?) #t)
(send t undo)
(expect (send t get-text) "do you like\ngreen eggs and ham?")
(expect (send t modified?) #f)
(send t redo)
(expect (send t modified?) #t)
(expect (send t get-text) "you like\ngreen eggs and ham?")
(send t set-position 0)
(send t insert #\d)
(send t insert #\o)
(send t insert #\space)
(expect (send t get-text) "do you like\ngreen eggs and ham?")
(send t undo)
(expect (send t get-text) "you like\ngreen eggs and ham?")
(send t redo)
(expect (send t get-text) "do you like\ngreen eggs and ham?")

(send t begin-edit-sequence)
(send t delete 0 3)
(send t delete (- (send t last-position) 4) (send t last-position))
(send t end-edit-sequence)

(expect (send t get-text) "you like\ngreen eggs and ")
(send t delete 0 4)
(expect (send t get-text) "like\ngreen eggs and ")
(send t undo)
(send t undo)
(expect (send t get-text) "do you like\ngreen eggs and ham?")

;; ----------------------------------------
;; Save & load

(send t delete 0 (send t last-position))
(send t clear-undos)
(send t insert "one\ntwo\n")
(send t set-position 0 3)
(send t copy #f 0)
(send t set-position 8)
(send t paste 0) ;; probably uses the snip% `copy' method
(expect (send t get-text) "one\ntwo\none")
(define (move-to-serialized-clipboard)
  (let ([data (send the-clipboard get-clipboard-data "WXME" 0)])
    (send the-clipboard set-clipboard-client
          (new (class clipboard-client%
                 (inherit add-type)
                 (super-new)
                 (add-type "WXME")
                 (define/override (get-data format) data)))
          0)))
(move-to-serialized-clipboard)
(send t paste 0) ;; uses above clipboard
(expect (send t get-text) "one\ntwo\noneone")
(send the-clipboard set-clipboard-string "\u3BB" 0)
(send t paste 0)
(expect (send t get-text) "one\ntwo\noneone\u3BB")

(send t set-position 3 4)
(send t copy #f 0)
(send t set-position 4 7)
(send t copy #t 0)
(send t set-position (send t last-position))
(send t paste 0)
(expect (send t get-text) "one\ntwo\noneone\u3BB\ntwo")
(send t paste-next)
(expect (send t get-text) "one\ntwo\noneone\u3BBone")

(send t cut #f 0 0 4)
(expect (send t get-text) "two\noneone\u3BBone")

(define-values (in7 out7) (make-pipe))
(expect (send t save-port out7 'text) #t)
(close-output-port out7)
(expect (read-string 100 in7)  "two\noneone\u3BBone")

(define out8 (open-output-bytes))
(expect (send t save-port out8 'standard) #t)
(define in8 (open-input-bytes (get-output-bytes out8)))
(expect (peek-bytes 31 0 in8) #"#reader(lib\"read.ss\"\"wxme\")WXME")
(send t erase)
(expect (send t get-text) "")
(expect (send t insert-port in8) 'standard)
(expect (send t get-text) "two\noneone\u3BBone")

;; ----------------------------------------
;; Version and format checking

(let ()
  (define tmp (make-temporary-file))

  (define (test-unknown bstr what)
    (call-with-output-file*
     tmp
     #:exists 'truncate
     (lambda (o)
       (write-bytes bstr o)))

    (expect (regexp-match
             #rx"unknown .* in WXME file format"
             (with-handlers ([exn? exn-message])
               (send (new full-text%) load-file tmp)
               "read, but shouldn't"))
            (list (format "unknown ~a in WXME file format" what))))

  (test-unknown #"#reader(lib\"read.ss\"\"wxme\")WXME0199 ## "
                "version number")
  (test-unknown #"#reader(lib\"read.ss\"\"wxme\")WXME0201 ## "
                "format number")

  (delete-file tmp))

;; ----------------------------------------
;; Styles on text

(define (check-color pos r g b w)
  (let* ([s (send (send t find-snip pos 'after) get-style)]
         [c (send s get-foreground)]
         [f (send s get-font)])
    (expect (send c red) r)
    (expect (send c green) g)
    (expect (send c blue) b)
    (expect (send f get-weight) w)))

(send t erase)
(send t insert "red\nblue")
(check-color 0 0 0 0 'normal)
(let ([d (send (new style-delta%) set-delta-foreground (make-object color% 255 0 0))])
  (send d set-weight-on 'bold)
  (send t change-style d 0 3))
(send t change-style
      (send (new style-delta%) set-delta-foreground (make-object color% 0 0 255))
      4 8)
(check-color 0 255 0 0 'bold)
(check-color 4 0 0 255 'normal)

(define out9 (open-output-bytes))
(expect (send t save-port out9 'standard) #t)
(define in9 (open-input-bytes (get-output-bytes out9)))
(send t erase)
(expect (send t insert-port in9) 'standard)
(expect (send t get-text) "red\nblue")
(check-color 0 255 0 0 'bold)
(check-color 4 0 0 255 'normal)

(define (check-random-delta d)
  (expect (send d get-alignment-on) 'top)
  (expect (send d get-alignment-off) 'base)
  (expect (send (send d get-background-add) get-r) 25)
  (expect (send (send d get-background-add) get-g) 25)
  (expect (send (send d get-background-add) get-b) 25)
  (expect (send (send d get-background-mult) get-r) 0.5)
  (expect (send (send d get-background-mult) get-g) 0.5)
  (expect (send (send d get-background-mult) get-b) 0.5)
  (expect (send (send d get-foreground-add) get-r) 50)
  (expect (send (send d get-foreground-add) get-g) 50)
  (expect (send (send d get-foreground-add) get-b) 50)
  (expect (send (send d get-foreground-mult) get-r) 0.6)
  (expect (send (send d get-foreground-mult) get-g) 0.6)
  (expect (send (send d get-foreground-mult) get-b) 0.6)
  (expect (send d get-face) "Purty") 
  (expect (send d get-family) 'decorative)
  (expect (send d get-size-in-pixels-on) #t)
  (expect (send d get-size-in-pixels-off) #f)
  (expect (send d get-smoothing-off) 'smoothed)
  (expect (send d get-smoothing-on) 'base)
  (expect (send d get-style-on) 'italic)
  (expect (send d get-style-off) 'base)
  (expect (send d get-transparent-text-backing-on) #t)
  (expect (send d get-transparent-text-backing-off) #f)
  (expect (send d get-underlined-off) #t)
  (expect (send d get-underlined-on) #f)
  (expect (send d get-weight-on) 'light)
  (expect (send d get-weight-off) 'base))

(let ([d (new style-delta%)])
  (send d set-alignment-on 'top)
  (send (send d get-background-add) set 25 25 25)
  (send (send d get-background-mult) set 0.5 0.5 0.5)
  (send (send d get-foreground-add) set 50 50 50)
  (send (send d get-foreground-mult) set 0.6 0.6 0.6)
  (send d set-delta-face "Purty" 'decorative)
  (send d set-size-in-pixels-on #t)
  (send d set-smoothing-off 'smoothed)
  (send d set-style-on 'italic)
  (send d set-transparent-text-backing-on #t)
  (send d set-underlined-off #t)
  (send d set-weight-on 'light)

  (check-random-delta d)

  (let* ([sl (send t get-style-list)]
         [s (send sl find-or-create-style (send sl basic-style) d)])
    (send t change-style s 0 1)))

(define out10 (open-output-bytes))
(expect (send t save-port out10 'standard) #t)
(define in10 (open-input-bytes (get-output-bytes out10)))
(send t erase)
(expect (send t insert-port in10 'guess #t) 'standard)
(expect (send t get-text) "red\nblue")
(check-color 0 50 50 50 'light)
(check-color 1 255 0 0 'bold)
(check-color 4 0 0 255 'normal)

(let ([d (new style-delta%)])
  (send (send (send t find-first-snip) get-style) get-delta d)
  (check-random-delta d))

;; ----------------------------------------
;; Keymaps

(define km (new keymap%))
(define hit #f)
(define kevt (new key-event%))

(send km add-function "letter-a" (lambda (obj evt) (set! hit #\a)))
(send km add-function "letter-m" (lambda (obj evt) (set! hit #\m)))
(send km add-function "letter-n" (lambda (obj evt) (set! hit #\n)))
(send km add-function "letter-up" (lambda (obj evt) (set! hit 'up)))
(send km add-function "letter-UP" (lambda (obj evt) (set! hit 'UP)))
(send km add-function "letter-down" (lambda (obj evt) (set! hit 'down)))
(send km add-function "letter-DOWN" (lambda (obj evt) (set! hit 'DOWN)))

(send km map-function "a" "letter-a")
(send kevt set-key-code #\x)
(expect (send km handle-key-event 'obj kevt) #f)
(send kevt set-key-code #\a)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\a)

(send km map-function "up" "letter-up")
(send kevt set-key-code 'up)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'up)
(set! hit #f)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'up)

(send km map-function "s:up" "letter-UP")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'UP)

(send km map-function ":down" "letter-down")
(send kevt set-key-code 'down)
(send kevt set-shift-down #f)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'down)
(set! hit #f)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #f)

(send km map-function "s:down" "letter-DOWN")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'DOWN)

(expect (with-handlers ([values
                         (lambda (exn)
                           (and (regexp-match? #rx"mapped as a non-prefix key" (exn-message exn))
                                'bad-remap))])
          (send km map-function "s:down;z" "oops"))
        'bad-remap)

;; Check sequence
(set! hit #f)
(send km map-function "d;O" "letter-down")
(send kevt set-shift-down #f)
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send kevt set-key-code #\o)
(expect (send km handle-key-event 'obj kevt) #f)
(send kevt set-shift-down #f)
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'down)

;; Interrupt sequence
(set! hit #f)
(send kevt set-shift-down #f)
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send km break-sequence)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #f)
(expect hit #f)

;; Check success with alternate, then override with more specific non-alternate
(send kevt set-key-code #\m)
(send kevt set-other-shift-key-code #\n)
(send kevt set-shift-down #f)
(send km map-function "?:n" "letter-n")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\n)
(send km map-function "?:m" "letter-m")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\m)

(define km2 (new keymap%))
(send km chain-to-keymap km2 #t)

;; Chained keymap more specific overrides less specific
(send km2 add-function "letter-n2" (lambda (obj evt) (set! hit 'n2)))
(send km2 map-function "n" "letter-n2")
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\m)
(send kevt set-key-code #\n)
(send kevt set-other-shift-key-code #\p)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'n2)

;; Check sequence in chained keymap
(send km2 add-function "letter-t" (lambda (obj evt) (set! hit #\t)))
(send km2 map-function "c:x;t" "letter-t")
(send kevt set-key-code #\x)
(send kevt set-control-down #t)
(send kevt set-other-shift-key-code #f)
(set! hit #f)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send kevt set-control-down #f)
(send kevt set-key-code #\t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\t)

(let ()
  (define k (new keymap%))
  (send k add-function "swap if branches" void)
  (send k map-function "c:x;r" "swap if branches")
  (send k add-function "rectangle" void)
  (expect (regexp-match? (regexp-quote "map-function in keymap%: \"r\" is already mapped as a non-prefix key")
                         (with-handlers ([exn:fail? exn-message])
                           (send k map-function "c:x;r;a" "rectangle")))
          #t))

;; Chained keymap non-prefixed overrides prefixed
(send km2 add-function "letter-d" (lambda (obj evt) (set! hit #\d)))
(send km2 map-function "d" "letter-d")
(send kevt set-key-code #\d)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #\d)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #f)
(expect hit #\d)

;; Remove chained keymap
(send km remove-chained-keymap km2)
(send kevt set-key-code #\d)
(send kevt set-shift-down #f)
(set! hit #f)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit #f)
(send kevt set-key-code #\O)
(send kevt set-shift-down #t)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit 'down)

;; Key grab
(send kevt set-key-code #\m)
(send kevt set-shift-down #f)
(send km set-grab-key-function (lambda (str km-in ed evt)
                                 (expect km-in km)
                                 (expect evt kevt)
                                 (set! hit (list str ed))
                                 #t))
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit '("letter-m" obj))
(send kevt set-key-code #\p)
(expect (send km handle-key-event 'obj kevt) #t)
(expect hit '(#f obj))
(send km set-grab-key-function (lambda (str km-in ed evt)
                                 (expect str "letter-m")
                                 (expect ed 'obj2)
                                 (set! hit 'nope)
                                 #f))
(send kevt set-key-code #\m)
(expect (send km handle-key-event 'obj2 kevt) #t)
(expect hit #\m)
(send km set-grab-key-function (lambda (str km-in ed evt)
                                 (expect str #f)
                                 (expect ed 'obj3)
                                 (set! hit 'nope)
                                 #f))
(send kevt set-key-code #\p)
(expect (send km handle-key-event 'obj3 kevt) #f)
(expect hit 'nope)

;; Mouse events
(define mevt/l (new mouse-event% [event-type 'left-down]))
(send mevt/l set-left-down #t)
(send km add-function "mouse-right" (lambda (obj evt) (set! hit 'right)))
(send km add-function "mouse-left" (lambda (obj evt) (set! hit 'left)))
(send km add-function "mouse-left2" (lambda (obj evt) (set! hit 'left2)))

(expect (send km handle-mouse-event 'obj mevt/l) #f)
(send mevt/l set-time-stamp 501) ;; FIXME: depends on double-click time
(send km map-function "leftbutton" "mouse-left")
(send km map-function "leftbuttondouble" "mouse-left2")
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left)
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left2)
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left)
(send mevt/l set-time-stamp 10100)
(expect (send km handle-mouse-event 'obj mevt/l) #t)
(expect hit 'left)

(set! hit #f)
(send km map-function "rightbuttonseq" "mouse-right")
(define mevt/r (new mouse-event% [event-type 'right-down]))
(send mevt/r set-right-down #t)
(define mevt/r/up (new mouse-event% [event-type 'right-up]))
(expect (send km handle-mouse-event 'obj mevt/r) #t)
(expect hit 'right)
(set! hit #f)
(expect (send km handle-mouse-event 'obj mevt/r/up) #t)
(expect hit 'right)

(send km set-grab-mouse-function (lambda (str km-in ed evt)
                                   (set! hit 'm)
                                   #t))
(define mevt/m (new mouse-event% [event-type 'middle-down]))
(send mevt/m set-middle-down #t)
(expect (send km handle-mouse-event 'obj mevt/m) #t)
(expect hit 'm)
(send km remove-grab-mouse-function)
(expect (send km handle-mouse-event 'obj mevt/m) #f)

;; ----------------------------------------
;; editor snips, content

(define oe (new text%))
(define ie (new text%))
(define es (new editor-snip% [editor ie]))
(send ie insert "Hello")
(send oe insert es)

(expect (send oe get-text 0 'eof #f) ".")
(expect (send oe get-flattened-text) "Hello")

(send es show-border #t)
(expect (send es border-visible?) #t)
(send es set-margin 1 2 3 4)
(define (check-border es)
  (let ([l (box 0)][t (box 0)][r (box 0)][b (box 0)])
    (send es get-margin l t r b)
    (expect (list (unbox l) (unbox t) (unbox r) (unbox b))
            (list 1 2 3 4))))
(check-border es)

(send oe set-position 0 1)
(send oe copy #f 0)
(send oe set-position 1)
(send oe paste 0) ;; probably uses the snip% `copy' method
(expect (send oe last-position) 2)
(define es2 (send oe find-snip 1 'after-or-none))
(check-border es2)
(move-to-serialized-clipboard)
(send oe paste 0) ;; uses above clipboard
(define es3 (send oe find-snip 2 'after-or-none))
(check-border es3)
(expect (send es3 border-visible?) #t)
(expect (send es3 get-align-top-line) #f)

(send (send es2 get-editor) insert "zzz" 2 2)
(expect (send oe get-text 0 'eof #f) "...")
(expect (send oe get-flattened-text) "HelloHezzzlloHello")

(send oe insert "a\n" 0)
(send oe insert "\nb" (send oe last-position))
(expect (send oe get-flattened-text) "a\nHelloHezzzlloHello\nb")

;; ----------------------------------------
;; editor snips, locations

(send oe set-admin (new test-editor-admin%))
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send oe get-extent w h)
          (list (unbox w) (unbox h)))
        '(197.0 40.0))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (list (begin
                  (send oe position-location 0 x y)
                  (list (unbox x) (unbox y)))
                (begin
                  (send oe position-location 1 x y #f)
                  (list (unbox x) (unbox y)))))
        '((0.0 0.0) (10.0 10.0)))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (list (begin
                  (send oe position-location 2 x y)
                  (list (unbox x) (unbox y)))
                (begin
                  (send oe position-location 3 x y #f)
                  (list (unbox x) (unbox y)))))
        '((0.0 11.0) (55.0 28.0)))

(send (send es2 get-editor) insert "\nmore" 100)
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send oe get-extent w h)
          (list (unbox w) (unbox h)))
        '(197.0 51.0))

;; ----------------------------------------
;; Pasteboard

(define pb (new pasteboard%))
(expect (send pb find-first-snip) #f)
(expect (send pb find-snip 10.0 10.0) #f)
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send pb get-extent w h)
          (list (unbox w) (unbox h)))
        '(0.0 0.0))

(define ss1 (new string-snip%))
(send ss1 insert "one" 3)
(send pb insert ss1 12.0 17.5)
(expect (send pb find-first-snip) ss1)
(expect (send pb get-flattened-text) "one")

(define ss2 (new string-snip%))
(send ss2 insert "two!" 4)
(send pb insert ss2 ss1 32.0 7.5)
(expect (send pb find-first-snip) ss2)
(expect (send pb get-flattened-text) "two!one")
(send pb lower ss2)
(expect (send pb get-flattened-text) "onetwo!")
(send pb raise ss2)
(expect (send pb get-flattened-text) "two!one")

(send pb set-admin (new test-editor-admin%))
(expect (let ([w (box 0.0)] [h (box 0.0)])
          (send pb get-extent w h)
          (list (unbox w) (unbox h)))
        '(74.0 29.5))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #t)
          (list (unbox x) (unbox y)))
        '(72.0 17.5))
(send ss2 insert "more" 4 3)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #t)
          (list (unbox x) (unbox y)))
        '(112.0 17.5))
(expect (send pb get-flattened-text) "twomore!one")

(send pb no-selected)
(expect (send pb find-next-selected-snip #f) #f)
(send pb add-selected ss1)
(expect (send pb find-next-selected-snip #f) ss1)
(expect (send pb find-next-selected-snip ss1) #f)
(send pb no-selected)
(send pb add-selected 0.0 0.0 10.0 10.0)
(expect (send pb find-next-selected-snip #f) #f)
(send pb add-selected 10.0 10.0 20.0 20.0)
(expect (send pb find-next-selected-snip #f) ss1)
(expect (send pb find-next-selected-snip ss1) #f)
(send pb add-selected 10.0 10.0 40.0 40.0)
(expect (send pb find-next-selected-snip #f) ss2)
(expect (send pb find-next-selected-snip ss2) ss1)

(send pb set-max-undo-history 10)

(send pb move 3 4)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss1 x y #f)
          (list (unbox x) (unbox y)))
        '(15.0 21.5))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #f)
          (list (unbox x) (unbox y)))
        '(35.0 11.5))
(send pb undo)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss1 x y #f)
          (list (unbox x) (unbox y)))
        '(12.0 17.5))
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #f)
          (list (unbox x) (unbox y)))
        '(32.0 7.5))

(send pb remove-selected ss1)
(expect (send pb find-snip 15.0 20.0) ss1)
(expect (send pb find-snip 35.0 10.0) ss2)
(expect (send pb find-first-snip) ss2)
(send pb delete) ; "delete"
(expect (send pb find-first-snip) ss1)
(expect (send pb find-snip 15.0 20.0) ss1)
(expect (send pb find-snip 35.0 10.0) #f)
(send pb undo) ; "undo"
(expect (send pb find-first-snip) ss2)
(expect (send pb find-snip 35.0 10.0) ss2)
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb get-snip-location ss2 x y #f)
          (list (unbox x) (unbox y)))
        '(32.0 7.5))

(define out20 (open-output-bytes))
(expect (send pb save-port out20 'standard) #t)
(define in20 (open-input-bytes (get-output-bytes out20)))
(expect (peek-bytes 31 0 in20) #"#reader(lib\"read.ss\"\"wxme\")WXME")

(define t10 (make-object text%))
(expect (send t10 insert-port in20) 'standard)
(expect (send t10 get-flattened-text) "twomore!one")

(define in21 (open-input-bytes (get-output-bytes out20)))
(define pb2 (make-object pasteboard%))
(expect (send pb2 insert-port in21) 'standard)
(expect (send pb2 get-flattened-text) "twomore!one")
(expect (let ([x (box 0.0)] [y (box 0.0)])
          (send pb2 get-snip-location (send pb2 find-first-snip) x y #f)
          (list (unbox x) (unbox y)))
        '(32.0 7.5))

;; ----------------------------------------

(let ()
  (define (mk) (make-object image-snip% (collection-file-path "b-run.png" "icons") 'unknown #f #f))
  
  (define is (mk))
  (define copy-is
    (let ()
      (define sp (open-output-string))
      (define t (new text%))
      (send t insert (mk))
      (send t save-port sp)
      (define t2 (new text%))
      (send t2 insert-port (open-input-string (get-output-string sp)))
      (send t2 find-first-snip)))
  
  (expect (send (mk) get-filename)
          (send copy-is get-filename)))


;; ----------------------------------------
;; get-extend-start-position and
;; get-extend-end-position

(let ([t (new text%)])
  (send t insert (make-string 40 #\a))
  (send t set-position 10 20)
  
  (expect (send t get-start-position) 10)
  (expect (send t get-end-position) 20)
  (expect (send t get-extend-start-position) 10)
  (expect (send t get-extend-end-position) 20)
  
  (send t set-anchor #t)
  (send t set-position 5 25)
  
  (expect (send t get-start-position) 5)
  (expect (send t get-end-position) 25)
  (expect (send t get-extend-start-position) 5)
  (expect (send t get-extend-end-position) 25)
  
  (send t extend-position 30)
  (expect (send t get-start-position) 5)
  (expect (send t get-end-position) 30)
  (expect (send t get-extend-start-position) 5)
  (expect (send t get-extend-end-position) 25)

  (send t extend-position 0)
  (expect (send t get-start-position) 0)
  (expect (send t get-end-position) 25)
  (expect (send t get-extend-start-position) 5)
  (expect (send t get-extend-end-position) 25))

;; ----------------------------------------

(let ()
  (define t (new text%))
  (send t insert "1\n12\n123\n")
  (expect (send t paragraph-start-position 3) 9)
  (expect (send t paragraph-end-position 3) 9)
  (expect (send t line-end-position 3) 9))

(let ()
  (define t (new text%))
  (send t insert "1\n12\n123\n\n")
  (expect (send t paragraph-start-position 3) 9)
  (expect (send t paragraph-end-position 3) 9)
  (expect (send t line-end-position 3) 9))

;; ----------------------------------------
;; tabs

(let ([t1 (new text%)])
  (send t1 set-admin (new test-editor-admin%))  
  (send t1 set-tabs '(100 200 300 400 500 600 700 800 900 1000 100) 1 #t)
  (send t1 insert "Hello\tWorld")
  (send t1 get-extent (box 0) (box 0)))

;; ----------------------------------------
;; Overwrite mode
(let ([t (new text%)])
  (send t set-admin (new test-editor-admin%))
  (send t insert "abcdef")
  (send t set-position 3 3)
  (define (type c) (send t on-default-char (new key-event% [key-code c])))
  (send t set-overwrite-mode #t)
  (type #\z)
  (expect (send t get-start-position) 4)
  (expect (send t get-text) "abczef")
  (type #\backspace)
  (expect (send t get-start-position) 3)
  (expect (send t get-text) "abc ef")
  
  (send t set-position 1)
  (type #\backspace)
  (expect (send t get-start-position) 0)
  (expect (send t get-text) " bc ef")
  
  (type #\backspace)
  (expect (send t get-start-position) 0)
  (expect (send t get-text) " bc ef"))


;; ----------------------------------------
;; Identity and contracts

(let ([t (new text%)])
  (define s (new editor-snip%))
  (send t insert "x")
  (send t insert s)
  (define (check s)
    (expect (send t get-snip-location s) #t)
    (expect (send t get-snip-position s) 1))
  (check s)
  (define/contract s2 (object/c) s)
  (check s2))

(let ([t (new pasteboard%)])
  (define s (new editor-snip%))
  (send t insert (make-object string-snip% "x"))
  (send t insert s 13 14)
  (define (check s)
    (define x (box 0))
    (define y (box 0))
    (expect (send t get-snip-location s x y) #t)
    (expect (unbox x) 13.0)
    (expect (unbox y) 14.0))
  (check s)
  (define/contract s2 (object/c) s)
  (check s2)
  (send t delete s2)
  (expect (send t get-snip-location s) #f)
  (expect (send t get-snip-location s2) #f))

;; ----------------------------------------
;; Error reporting

(when (regexp-match? #rx"raise-type-error"
                     (with-handlers ([exn:fail? exn-message])
                       (send (new text%) get-text 1 'end #t)))
  (error "bad error message"))

;; ----------------------------------------

(done)
