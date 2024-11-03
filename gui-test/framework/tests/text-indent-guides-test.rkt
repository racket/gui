#lang racket/base
(require framework
         framework/private/guide-struct
         rackunit
         racket/gui/base
         racket/set
         racket/class
         data/skip-list)

(let ()
  (define t (new (text:indent-guides-mixin text%)))
  (send t insert #<<--
#lang racket

(define (f x)
  (cond
    [(empty? x)
     1]
    [else 2])
  (void))

--
        )
  (send t set-filename #f)

  (check-equal?
   (skip-list->list (send t get-guides))
   (list
    (cons 0 (guide 0 0 '()))
    (cons 1 (guide #f #f '()))
    (cons 2 (guide 0 0 '()))
    (cons 3 (guide 2 0 '()))
    (cons 4 (guide 4 0 '(2)))
    (cons 5 (guide 5 0 '(2 4)))
    (cons 6 (guide 4 0 '(2)))
    (cons 7 (guide 2 0 '()))
    (cons 8 (guide #f #f '()))))


  (define lines (set))
  (send t draw-the-lines
        (λ (x-in-editor-coordinates x y-start y-end)
          (set! lines (set-add lines (list x y-start y-end))))
        0
        (send t last-paragraph))
  (check-equal? lines
                (set '(2 4 6)
                     '(4 5 5))))

(let ()
  (define t (new (text:indent-guides-mixin text%)))
  (send t insert "\n abc\n   d\n   e\n\n   g\n   h\n")
  (send t set-filename #f)

  (check-equal?
   (skip-list->list (send t get-guides))
   (list
    (cons 0 (guide #f #f '()))
    (cons 1 (guide 1 0 '()))
    (cons 2 (guide 3 0 '(1)))
    (cons 3 (guide 3 0 '(1)))
    (cons 4 (guide #f #f '(1 3)))
    (cons 5 (guide 3 0 '(1)))
    (cons 6 (guide 3 0 '(1)))
    (cons 7 (guide #f #f '()))))

  (let ()
    (define lines (set))
    (send t draw-the-lines
          (λ (x-in-editor-coordinates x y-start y-end)
            (set! lines (set-add lines (list x y-start y-end))))
          0
          (send t last-paragraph))
    (check-equal? lines
                  (set '(1 2 6) '(3 4 4))))

  (let ()
    (define lines (set))
    (send t draw-the-lines
          (λ (x-in-editor-coordinates x y-start y-end)
            (set! lines (set-add lines (list x y-start y-end))))
          5 5)
    (check-equal? lines
                  (set '(1 5 5)))))

(let ()
  (define t (new (text:indent-guides-mixin text%)))
  (send t insert "abc\n  d\n   e\n     f\n  g\n")
  (send t set-filename #f)

  (check-equal?
   (skip-list->list (send t get-guides))
   (list
    (cons 0 (guide 0 0 '()))
    (cons 1 (guide 2 0 '()))
    (cons 2 (guide 3 0 '(2)))
    (cons 3 (guide 5 0 '(2 3)))
    (cons 4 (guide 2 0 '()))
    (cons 5 (guide #f #f '()))))

  (let ()
    (define lines (set))
    (send t draw-the-lines
          (λ (x-in-editor-coordinates x y-start y-end)
            (set! lines (set-add lines (list x y-start y-end))))
          0
          (send t last-paragraph))
    (check-equal? lines
                  (set '(3 3 3) '(2 2 3))))

  (let ()
    (define lines (set))
    (send t draw-the-lines
          (λ (x-in-editor-coordinates x y-start y-end)
            (set! lines (set-add lines (list x y-start y-end))))
          2
          4)
    (check-equal? lines
                  (set '(3 3 3) '(2 2 3)))))

(let ()
  (define t (new (text:indent-guides-mixin text%)))
  (send t insert "q\n  a\n    e\n")
  (send t set-filename #f)

  (check-equal?
   (skip-list->list (send t get-guides))
   (list
    (cons 0 (guide 0 0 '()))
    (cons 1 (guide 2 0 '()))
    (cons 2 (guide 4 0 '(2)))
    (cons 3 (guide #f #f '()))))

  (let ()
    (define lines (set))
    (send t draw-the-lines
          (λ (x-in-editor-coordinates x y-start y-end)
            (set! lines (set-add lines (list x y-start y-end))))
          0
          (send t last-paragraph))
    (check-equal? lines
                  (set '(2 2 2))))

  (let ()
    (define lines (set))
    (send t delete 6 12)
    (send t draw-the-lines
          (λ (x-in-editor-coordinates x y-start y-end)
            (set! lines (set-add lines (list x y-start y-end))))
          1
          2)
    (check-equal? lines
                  (set))))


(when (with-handlers ([exn:fail? (lambda (x) #f)])
        (collection-path "shrubbery"))
  (define t (new (text:indent-guides-mixin text%)))
  (send t insert
        (string-append
         "#lang shrubbery\n"
         "\n"
         "\n"
         "/*\n"
         "begin:\n"
         "  apple\n"
         " \n"
         "  banana\n"
         "  */\n"))
  (check-equal?
   (skip-list->list (send t get-guides))
   (list
    (cons 0 (guide 0 0 '()))
    (cons 1 (guide #f #f '()))
    (cons 2 (guide #f #f '()))
    (cons 3 (guide 0 0 '()))
    (cons 4 (guide 0 0 '()))
    (cons 5 (guide 2 0 '()))
    (cons 6 (guide #f #f '(2)))
    (cons 7 (guide 2 0 '()))
    (cons 8 (guide 2 0 '()))
    (cons 9 (guide #f #f '()))))

  (send t insert "\n\n" (send t paragraph-start-position 2))
  (check-equal?
   (skip-list->list (send t get-guides))
   (list
    (cons 0 (guide 0 0 '()))
    (cons 1 (guide #f #f '()))
    (cons 2 (guide #f #f '()))
    (cons 3 (guide #f #f '()))
    (cons 4 (guide #f #f '()))
    (cons 5 (guide 0 0 '()))
    (cons 6 (guide 0 0 '()))
    (cons 7 (guide 2 0 '()))
    (cons 8 (guide #f #f '(2)))
    (cons 9 (guide 2 0 '()))
    (cons 10 (guide 2 0 '()))
    (cons 11 (guide #f #f '())))))
