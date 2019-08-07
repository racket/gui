#lang racket
(require rackunit framework
         syntax-color/racket-lexer)

(define (get-colors t)
  (define colors '())
  (define range-start #f)
  (define pending #f)
  (for ([i (in-range (+ (send t last-position) 1))])
    (define p (send t classify-position i))
    (cond
      [(not range-start)
       (set! pending p)
       (set! range-start i)]
      [(and range-start (equal? pending p)) (void)]
      [(and range-start (not (equal? pending p)))
       (set! colors (cons (list pending range-start i) colors))
       (set! pending p)
       (set! range-start i)]))
  (when pending
    (set! colors (cons (list pending range-start (send t last-position))
                       colors)))
  (reverse colors))

(define (token-sym->style s)
  (symbol->string s))

(define (start-the-colorer t)
  (send t start-colorer
        symbol->string
        racket-lexer
        '((|(| |)|) (|[| |]|) (|{| |}|))))
  

(let ()
  (define t (new color:text%))
  (start-the-colorer t)
  (check-equal? (get-colors t)
                '()))

(let ()
  (define t (new color:text%))
  (send t insert "1")
  (start-the-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)

  (check-equal? (get-colors t)
                '((constant 0 1))))

(let ()
  (define t (new color:text%))
  (send t insert "1 2")
  (start-the-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (check-equal? (get-colors t)
                '((constant 0 1)
                  (white-space 1 2)
                  (constant 2 3))))

(let ()
  (define t (new color:text%))
  (send t insert "12 34")
  (start-the-colorer t)
  (send t freeze-colorer)
  (send t thaw-colorer)
  
  (check-equal? (get-colors t)
                '((constant 0 2)
                  (white-space 2 3)
                  (constant 3 5))))

(let ()
  (define t (new color:text%))
  (send t insert "12 34")
  (start-the-colorer t)
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
  (start-the-colorer t)
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
  (start-the-colorer t)
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
  (start-the-colorer t)
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
  (start-the-colorer t)
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
