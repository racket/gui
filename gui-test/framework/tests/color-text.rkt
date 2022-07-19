#lang racket
(require rackunit framework
         syntax-color/racket-lexer)

(define (get-colors t #:grapheme? [grapheme? #t])
  (define colors '())
  (define range-start #f)
  (define pending #f)
  (define (add-to-colors pending start end)
    (set! colors (cons (list pending
                             (if grapheme? (send t position-grapheme start) start)
                             (if grapheme? (send t position-grapheme end) end))
                       colors)))
  (for ([i (in-range (+ (send t last-position) 1))])
    (define p (send t classify-position i))
    (cond
      [(not range-start)
       (set! pending p)
       (set! range-start i)]
      [(and range-start (equal? pending p)) (void)]
      [(and range-start (not (equal? pending p)))
       (add-to-colors pending range-start i)
       (set! pending p)
       (set! range-start i)]))
  (when pending
    (add-to-colors pending range-start (send t last-position)))
  (reverse colors))

(define (token-sym->style s)
  (symbol->string s))

(define (start-racket-colorer t)
  (send t start-colorer
        symbol->string
        racket-lexer
        '((|(| |)|) (|[| |]|) (|{| |}|))))

(define (read-a-grapheme p)
  (define chars
    (let loop ([state 0])
      (define next-char (peek-char p))
      (cond
        [(eof-object? next-char) '()]
        [else
         (define-values (grapheme-terminated? new-state) (char-grapheme-step next-char state))
         (cond
           [grapheme-terminated?
            (if (zero? new-state)
                (list (read-char p))
                '())]
           [else
            (cons (read-char p)
                  (loop new-state))])])))
  (cond
    [(null? chars) eof]
    [else chars]))

(check-equal? (read-a-grapheme (open-input-string "")) eof)
(check-equal? (read-a-grapheme (open-input-string "a")) (list #\a))
(check-equal? (read-a-grapheme (open-input-string "\r\n")) (list #\return #\newline))
(check-equal? (read-a-grapheme (open-input-string "\n\r")) (list #\newline))
(check-equal? (read-a-grapheme (open-input-string "\n\r")) (list #\newline))
(check-equal? (let ([p (open-input-string "\r\n\n\r\na")])
                (list (read-a-grapheme p)
                      (read-a-grapheme p)
                      (read-a-grapheme p)
                      (read-a-grapheme p)
                      (read-a-grapheme p)
                      (read-a-grapheme p)))
              (list (list #\return #\newline)
                    (list #\newline)
                    (list #\return #\newline)
                    (list #\a)
                    eof
                    eof))
(check-equal? (read-a-grapheme (open-input-string "🏴‍☠️"))
              (string->list "\U1f3f4\u200d\u2620\ufe0f"))
(check-equal? (let ([p (open-input-string "\"🏴‍☠️\"")])
                (list (read-a-grapheme p)
                      (read-a-grapheme p)
                      (read-a-grapheme p)))
              (list (list #\")
                    (string->list "\U1f3f4\u200d\u2620\ufe0f")
                    (list #\")))

(define (backing-up-lexer port offset mode)
  (define-values (line col pos) (port-next-location port))
  (define c (read-a-grapheme port))
  (cond
    [(eof-object? c)
     (values "eof" 'eof #f #f #f #f #f)]
    [else
     (define peek-port (peeking-input-port port))
     (port-count-lines! peek-port)
     (read-a-grapheme peek-port)
     (define the-color (read-a-grapheme peek-port))
     (values (apply string c)
             (match the-color
               [(list #\a) 'symbol]
               [(list #\b) 'parenthesis]
               [(list #\c) 'constant]
               [_ 'no-color])
             #f
             pos
             (+ pos 1)
             2
             #f)]))

(define (start-backing-up-colorer t)
  (send t start-colorer symbol->string backing-up-lexer '()))

(let ()
  (define t (new color:text%))
  (start-racket-colorer t)
  (check-equal? (get-colors t)
                '()))

(let ()
  (define t (new color:text%))
  (send t insert "1")
  (start-racket-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t)
                '((constant 0 1))))

(let ()
  (define t (new color:text%))
  (send t insert "1 2")
  (start-racket-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (check-equal? (get-colors t)
                '((constant 0 1)
                  (white-space 1 2)
                  (constant 2 3))))

(let ()
  (define t (new color:text%))
  (send t insert "12 34")
  (start-racket-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (check-equal? (get-colors t)
                '((constant 0 2)
                  (white-space 2 3)
                  (constant 3 5))))

(let ()
  (define t (new color:text%))
  (send t insert "12 34")
  (start-racket-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  (send t insert " " 2 2)
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (check-equal? (get-colors t)
                '((constant 0 2)
                  (white-space 2 4)
                  (constant 4 6))))

(let ()
  (define t (new color:text%))
  (send t insert "12 34")
  (start-racket-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  (send t insert " " 3 3)
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (check-equal? (get-colors t)
                '((constant 0 2)
                  (white-space 2 4)
                  (constant 4 6))))

(let ()
  (define t (new color:text%))
  (send t insert "12 34")
  (start-racket-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  (send t insert " " 4 4)
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (check-equal? (get-colors t)
                '((constant 0 2)
                  (white-space 2 3)
                  (constant 3 4)
                  (white-space 4 5)
                  (constant 5 6))))

(let ()
  (define t (new color:text%))
  (send t insert "12 3 4")
  (start-racket-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  (send t insert " " 4 4)
  (send t delete 3 7)
  (send t insert "xyz" 3 3)
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t)
                '((constant 0 2)
                  (white-space 2 3)
                  (symbol 3 6))))

(let ()
  (define t (new color:text%))
  (start-racket-colorer t)
  (send t insert "x 1))")
  (send t freeze-colorer)
  (send t thaw-colorer)

  (send t insert "(+)\n((()) (+ y" 1 1)
  (send t delete 5 (+ 5 14))
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (define correct-result
    '((symbol 0 1)
      (parenthesis 1 2)
      (symbol 2 3)
      (parenthesis 3 4)
      (white-space 4 5)))
  
  (check-equal? (get-colors t) correct-result)

  (send t insert (send t get-text) 0 (send t last-position))
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t) correct-result))

(let ()
  (define t (new color:text%))
  (start-racket-colorer t)
  (send t insert "\"🏴‍☠️\"")
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t #:grapheme? #f) (list '(string 0 6))))

(let ()
  (define t (new color:text%))
  (start-racket-colorer t)
  (send t insert "\"🏴‍☠️\"")
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t) (list '(string 0 3))))

(let ()
  (define t (new color:text%))
  (start-racket-colorer t)
  (send t insert "(define 🏴‍☠️ 1)🏴‍☠️")
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t)
                '((parenthesis 0 1)
                  (symbol 1 7)
                  (white-space 7 8)
                  (symbol 8 9)
                  (white-space 9 10)
                  (constant 10 11)
                  (parenthesis 11 12)
                  (symbol 12 13))))

(let ()
  (define t (new color:text%))
  (send t insert "aaabc")
  (start-backing-up-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t)
                '((symbol 0 1)
                  (parenthesis 1 2)
                  (constant 2 3)
                  (no-color 3 5))))

(let ()
  (define t (new color:text%))
  (send t insert "aaabc")
  (start-backing-up-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  (send t insert "a" 4 4)

  (define correct-result
    '((symbol 0 1)
      (parenthesis 1 2)
      (symbol 2 3)
      (constant 3 4)
      (no-color 4 6)))
  
  (check-equal? (get-colors t) correct-result)

  (send t insert (send t get-text) 0 (send t last-position))
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t) correct-result))

(let ()
  (define t (new color:text%))
  (start-racket-colorer t)

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
