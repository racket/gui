#lang racket/base
(require racket/gui/base
         racket/class
         racket/contract
         2d/dir-chars)

(provide normalize-unicode-ascii-art-box
         widen-unicode-ascii-art-box
         heighten-unicode-ascii-art-box
         center-in-unicode-ascii-art-box)

(define (widen-unicode-ascii-art-box t orig-pos)
  (widen/highten-unicode-ascii-art-box t orig-pos #t))

(define (heighten-unicode-ascii-art-box t orig-pos)
  (widen/highten-unicode-ascii-art-box t orig-pos #f))

(define (widen/highten-unicode-ascii-art-box t orig-pos widen?)
  (define start-pos (scan-for-start-pos t orig-pos))
  (when start-pos 
    (send t begin-edit-sequence)
    (define-values (start-x start-y) (pos->xy t orig-pos))
    (define start-major (if widen? start-x start-y))
    (define min-minor #f)
    (define max-minor #f)
    (trace-unicode-ascii-art-box 
     t start-pos #f 
     (λ (pos x y i-up? i-dn? i-lt? i-rt?)
       (define minor (if widen? y x))
       (define major (if widen? x y))
       (when (= major start-major)
         (unless min-minor
           (set! min-minor minor)
           (set! max-minor minor))
         (set! min-minor (min minor min-minor))
         (set! max-minor (max minor max-minor)))))
    (cond
      [widen?
       (define to-adjust 0)
       (for ([minor (in-range max-minor (- min-minor 1) -1)])
         (define-values (pos char) (xy->pos t start-major minor))
         (when (< pos start-pos)
           (set! to-adjust (+ to-adjust 1)))
         (send t insert
               (cond
                 [(member char lt-chars) #\═]
                 [else #\space])
               pos pos))
       (send t set-position (+ orig-pos to-adjust 1) (+ orig-pos to-adjust 1))]
      [else
       (define-values (min-pos _1) (xy->pos t min-minor start-major))
       (define-values (max-pos _2) (xy->pos t max-minor start-major))
       (define para (send t position-paragraph max-pos))
       (define para-start (send t paragraph-start-position para))
       (define para-end (send t paragraph-end-position para))
       (send t insert "\n" para-end para-end)
       (for ([to-copy-pos (in-range para-start (+ max-pos 1))])
         (define to-insert-pos (+ para-end 1 (- to-copy-pos para-start)))
         (define char
           (cond
             [(< to-copy-pos min-pos) " "]
             [else
              (define above-char (send t get-character to-copy-pos))
              (if (and (member above-char dn-chars)
                       (member above-char double-barred-chars))
                  "║"
                  " ")]))
         (send t insert char to-insert-pos to-insert-pos))
       (void)])
    (send t end-edit-sequence)))

(define (normalize-unicode-ascii-art-box t pos)
  (define start-pos (scan-for-start-pos t pos))
  (when start-pos
    (send t begin-edit-sequence)
    (trace-unicode-ascii-art-box 
     t start-pos #f 
     (λ (pos x y i-up? i-dn? i-lt? i-rt?)
       (cond
         [(and i-up? i-dn? i-lt? i-rt?) (set-c t pos "╬")]
         [(and i-dn? i-lt? i-rt?)       (set-c t pos "╦")]
         [(and i-up? i-lt? i-rt?)       (set-c t pos "╩")]
         [(and i-up? i-dn? i-rt?)       (set-c t pos "╠")]
         [(and i-up? i-dn? i-lt?)       (set-c t pos "╣")]
         [(and i-up? i-lt?)             (set-c t pos "╝")]
         [(and i-up? i-rt?)             (set-c t pos "╚")]
         [(and i-dn? i-lt?)             (set-c t pos "╗")]
         [(and i-dn? i-rt?)             (set-c t pos "╔")]
         [(or i-up? i-dn?)              (set-c t pos "║")]
         [else                          (set-c t pos "═")])))
     (send t end-edit-sequence)))

(define (center-in-unicode-ascii-art-box txt insertion-pos)
  (define (find-something start-pos inc char-p?)
    (define-values (x y) (pos->xy txt start-pos))
    (let loop ([pos start-pos])
      (cond
        [(char-p? (send txt get-character pos))
         pos]
        [else
         (define new-pos (inc pos))
         (cond
           [(<= 0 new-pos (send txt last-position))
            (define-values (x2 y2) (pos->xy txt new-pos))
            (cond
              [(= y2 y)
               (loop new-pos)]
              [else #f])]
           [else #f])])))
  
  (define (adjust-space before-space after-space pos)
    (cond
      [(< before-space after-space) 
       (send txt insert (make-string (- after-space before-space) #\space) pos pos)]
      [(> before-space after-space) 
       (send txt delete pos (+ pos (- before-space after-space)))]))
  
  (define left-bar (find-something insertion-pos sub1 (λ (x) (equal? x #\║))))
  (define right-bar (find-something insertion-pos add1 (λ (x) (equal? x #\║))))
  (when (and left-bar right-bar (< left-bar right-bar))
    (define left-space-edge (find-something (+ left-bar 1) add1 (λ (x) (not (char-whitespace? x)))))
    (define right-space-edge (find-something (- right-bar 1) sub1 (λ (x) (not (char-whitespace? x)))))
    (when (and left-space-edge right-space-edge)
      (define before-left-space-count (- left-space-edge left-bar 1))
      (define before-right-space-count (- right-bar right-space-edge 1))
      (define tot-space (+ before-left-space-count before-right-space-count))
      (define after-left-space-count (floor (/ tot-space 2)))
      (define after-right-space-count (ceiling (/ tot-space 2)))
      (send txt begin-edit-sequence)
      (adjust-space before-right-space-count after-right-space-count (+ right-space-edge 1))
      (adjust-space before-left-space-count after-left-space-count (+ left-bar 1))
      (send txt end-edit-sequence))))

(define (trace-unicode-ascii-art-box t start-pos only-double-barred-chars? f)
  (define visited (make-hash))
  (let loop ([pos start-pos])
    (unless (hash-ref visited pos #f)
      (hash-set! visited pos #t)
      (define-values (x y) (pos->xy t pos))
      (define c (send t get-character pos))
      (define-values (up upc) (xy->pos t x (- y 1)))
      (define-values (dn dnc) (xy->pos t x (+ y 1)))
      (define-values (lt ltc) (xy->pos t (- x 1) y))
      (define-values (rt rtc) (xy->pos t (+ x 1) y))
      (define (interesting-dir? dir-c dir-chars) 
        (or (and (not only-double-barred-chars?)
                 (member dir-c adjustable-chars)
                 (member c dir-chars))
            (and (member dir-c double-barred-chars)
                 (member c double-barred-chars))))
      (define i-up? (interesting-dir? upc up-chars))
      (define i-dn? (interesting-dir? dnc dn-chars))
      (define i-lt? (interesting-dir? ltc lt-chars))
      (define i-rt? (interesting-dir? rtc rt-chars))
      (f pos x y i-up? i-dn? i-lt? i-rt?)
      (when i-up? (loop up))
      (when i-dn? (loop dn))
      (when i-lt? (loop lt))
      (when i-rt? (loop rt)))))

(define (scan-for-start-pos t pos)
  (define-values (x y) (pos->xy t pos))
  (findf
   (λ (p) (adj? t p))
   (for*/list ([xadj '(0 -1)]
               [yadj '(0 -1 1)])
     (define-values (d dc) (xy->pos t (+ x xadj) (+ y yadj)))
     d)))
       
(define (adj? t pos)
  (and pos 
       (member (send t get-character pos) 
               adjustable-chars)))

(define (set-c t pos s)
  (unless (equal? (string-ref s 0) (send t get-character pos))
    (send t delete pos (+ pos 1))
    (send t insert s pos pos)))

(define (pos->xy text pos)
  (define para (send text position-paragraph pos))
  (define start (send text paragraph-start-position para))
  (values (- pos start) para))

(define (xy->pos text x y)
  (cond
    [(and (<= 0 x) (<= 0 y (send text last-paragraph)))
     (define para-start (send text paragraph-start-position y))
     (define para-end (send text paragraph-end-position y))
     (define pos (+ para-start x))
     (define res-pos
       (and (< pos para-end)
            ;; the newline at the end of the
            ;; line is not on the line, so use this guard
            pos))
     (if res-pos
         (values res-pos (send text get-character res-pos))
         (values #f #f))]
    [else (values #f #f)]))

(module+ test
  (require rackunit 
           racket/gui/base)
  (define sa string-append)
  
  (define (first-value-xy->pos a b c)
    (define-values (d e) (xy->pos a b c))
    d)
  
  (let ([t (new text%)])
    (send t insert (sa "abc\n"
                       "d\n"
                       "ghi\n"))
    (check-equal? (first-value-xy->pos t 0 0) 0)
    (check-equal? (first-value-xy->pos t 1 0) 1)
    (check-equal? (first-value-xy->pos t 0 1) 4)
    (check-equal? (first-value-xy->pos t 3 0) #f)
    (check-equal? (first-value-xy->pos t 0 3) #f)
    (check-equal? (first-value-xy->pos t 1 1) #f)
    (check-equal? (first-value-xy->pos t 2 1) #f)
    (check-equal? (first-value-xy->pos t 0 2) 6)
    (check-equal? (first-value-xy->pos t 1 2) 7)
    (check-equal? (first-value-xy->pos t 2 -1) #f)
    (check-equal? (first-value-xy->pos t -1 0) #f)
    (check-equal? (first-value-xy->pos t 2 2) 8)
    (check-equal? (first-value-xy->pos t 2 3) #f))
  
  (let ([t (new text%)])
    (send t insert (sa "abc\n"
                       "d\n"
                       "ghi"))
    (check-equal? (first-value-xy->pos t 2 2) 8)
    (check-equal? (first-value-xy->pos t 2 3) #f))
  
  (let ([t (new text%)])
    (send t insert (string-append "+-+\n"
                                  "| |\n"
                                  "+-+\n"))
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═╗\n"
                   "║ ║\n"
                   "╚═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append "+=+\n"
                                  "| |\n"
                                  "+=+\n"))
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═╗\n"
                   "║ ║\n"
                   "╚═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append "+-+-+\n"
                                  "| | |\n"
                                  "+-+-+\n"
                                  "| | |\n"
                                  "+-+-+\n"))
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═╦═╗\n"
                   "║ ║ ║\n"
                   "╠═╬═╣\n"
                   "║ ║ ║\n"
                   "╚═╩═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═══╗\n"
                    "║ - ║\n"
                    "╚═══╝\n"))
    
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═══╗\n"
                   "║ - ║\n"
                   "╚═══╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═╦═╗\n"
                    "║ ║ ║\n"
                    "╠═╬═╣\n"
                    "║ ║ ║\n"
                    "╚═╩═╝\n"))
    (send t set-position 1 1)
    (widen-unicode-ascii-art-box t 1)
    (check-equal? (send t get-start-position) 2)
    (check-equal? (send t get-text)
                  (string-append
                   "╔══╦═╗\n"
                   "║  ║ ║\n"
                   "╠══╬═╣\n"
                   "║  ║ ║\n"
                   "╚══╩═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═╦═╗\n"
                    "║ ║ ║\n"
                    "╠═╬═╣\n"
                    "║ ║ ║\n"
                    "╚═╩═╝\n"))
    (send t set-position 8 8)
    (widen-unicode-ascii-art-box t 8)
    (check-equal? (send t get-start-position) 10)
    (check-equal? (send t get-text)
                  (string-append
                   "╔══╦═╗\n"
                   "║  ║ ║\n"
                   "╠══╬═╣\n"
                   "║  ║ ║\n"
                   "╚══╩═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═╦═╗\n"
                    "║ ║ ║\n"
                    "╠═╬═╣\n"
                    "║ ║ ║\n"))
    (send t set-position 8 8)
    (widen-unicode-ascii-art-box t 8)
    (check-equal? (send t get-text)
                  (string-append
                   "╔══╦═╗\n"
                   "║  ║ ║\n"
                   "╠══╬═╣\n"
                   "║  ║ ║\n")))

  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═╦═╗\n"
                    "║ ║ ║\n"
                    "╠═╬═╣\n"
                    "║ ║ ║\n"
                    "╚═╩═╝\n"))
    (send t set-position 8 8)
    (heighten-unicode-ascii-art-box t 8)
    (check-equal? (send t get-start-position) 8)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═╦═╗\n"
                   "║ ║ ║\n"
                   "║ ║ ║\n"
                   "╠═╬═╣\n"
                   "║ ║ ║\n"
                   "╚═╩═╝\n")))

  (let ([t (new text%)])
    (send t insert (string-append
                    "1 ╔═╦═╗\n"
                    "2 ║ ║ ║\n"
                    "3 ╠═╬═╣\n"
                    "4 ║ ║ ║\n"
                    "5 ╚═╩═╝\n"))
    (send t set-position 11 11)
    (heighten-unicode-ascii-art-box t 11)
    (check-equal? (send t get-text)
                  (string-append
                   "1 ╔═╦═╗\n"
                   "2 ║ ║ ║\n"
                   "  ║ ║ ║\n"
                   "3 ╠═╬═╣\n"
                   "4 ║ ║ ║\n"
                   "5 ╚═╩═╝\n")))

  (let ([t (new text%)])
    (send t insert (string-append
                    "1 ╔═╦═╗\n"
                    "2 ║ ║ ║\n"
                    "3 ╠═╬═╣\n"
                    "4 ║ ║ ║\n"
                    "5 ╚═╩═╝\n"))
    (send t set-position 19 19)
    (heighten-unicode-ascii-art-box t 19)
    (check-equal? (send t get-text)
                  (string-append
                   "1 ╔═╦═╗\n"
                   "2 ║ ║ ║\n"
                   "3 ╠═╬═╣\n"
                   "  ║ ║ ║\n"
                   "4 ║ ║ ║\n"
                   "5 ╚═╩═╝\n")))
  
  (let ([t (new text%)])
    (send t insert "║ x   ║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║  x  ║\n"))
  
  (let ([t (new text%)])
    (send t insert "║x    ║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║  x  ║\n"))
  
  (let ([t (new text%)])
    (send t insert "║    x║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║  x  ║\n"))
  
  (let ([t (new text%)])
    (send t insert "║abcde║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║abcde║\n"))
  
  (let ([t (new text%)])
    (send t insert "║║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║║\n"))
  
  (let ([t (new text%)])
    (send t insert "║abcde \n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║abcde \n"))
  
  (let ([t (new text%)])
    (send t insert " abcde║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  " abcde║\n")))

#;
(module+ main
  (require framework)
  (define f (new frame% [label ""] [width 500] [height 500]))
  (define t (new (ascii-art-enlarge-boxes-mixin racket:text%)))
  (send t set-overwrite-mode #t)
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send t insert
        (string-append
         "╔═╦═╗\n"
         "║ ║ ║\n"
         "║ ║ ║\n"
         "╠═╬═╣\n"
         "║ ║ ║\n"
         "╚═╩═╝\n"))
  (send t set-position 14 14)
  (send f show #t))

