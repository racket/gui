#lang racket

(require rackunit
         racket/file
         racket/gui/base
         framework
         framework/private/text-inline-overview)

(define (erase-rest-of-line.simple/slow bmp w x y)
  (for ([x (in-range x w)])
    (set-transparent-pixels bmp x y 1)))

(define (try-erase-end-of-line w h x y)
  (define bmp (make-bitmap w h))
  (define argb-bytes.1 (make-bytes (* w h 4) 48))
  (define argb-bytes.2 (make-bytes (* w h 4) 48))
  (send bmp set-argb-pixels 0 0 w h argb-bytes.1)
  (erase-rest-of-line bmp w x y)
  (send bmp get-argb-pixels 0 0 w h argb-bytes.1)
  (send bmp set-argb-pixels 0 0 w h argb-bytes.2)
  (erase-rest-of-line.simple/slow bmp w x y)
  (send bmp get-argb-pixels 0 0 w h argb-bytes.2)
  (equal? argb-bytes.1 argb-bytes.2))

(check-true (try-erase-end-of-line 1 1 0 0))
(check-true (try-erase-end-of-line 2 1 0 0))
(check-true (try-erase-end-of-line 2 1 1 0))
(check-true (try-erase-end-of-line 2 1 2 0))
(for ([width (in-range (* transparent-bytes-count 3))])
  (for ([start (in-range width)])
    (check-true (try-erase-end-of-line width 1 start 0))))

(define (get-strings t)
  (unless (send t get-inline-overview-enabled?)
    (error 'get-strings "inline overview isn't enabled"))
  (define bmp (send t get-primary-bmp))
  (define w (send bmp get-width))
  (define h (send bmp get-height))
  (define argb (make-bytes (* w h 4)))
  (send bmp get-argb-pixels 0 0 w h argb)

  (define strings-with-padding
    (for/list ([y (in-range h)])
      (apply
       string
       (for/list ([x (in-range w)])
         (define p (* 4 (+ (* y w) x)))
         (cond
           [(zero? (bytes-ref argb p))
            #\space]
           [(and (= 255 (bytes-ref argb (+ p 1)))
                 (= 255 (bytes-ref argb (+ p 2)))
                 (= 255 (bytes-ref argb (+ p 3))))
            #\space]
           [(and (= 255 (bytes-ref argb (+ p 1)))
                 (= 0 (bytes-ref argb (+ p 2)))
                 (= 0 (bytes-ref argb (+ p 3))))
            #\r]
           [(and (= 0 (bytes-ref argb (+ p 1)))
                 (= 255 (bytes-ref argb (+ p 2)))
                 (= 0 (bytes-ref argb (+ p 3))))
            #\g]
           [(and (= 0 (bytes-ref argb (+ p 1)))
                 (= 0 (bytes-ref argb (+ p 2)))
                 (= 255 (bytes-ref argb (+ p 3))))
            #\b]
           [else #\*])))))

  (define widest-width
    (apply
     max 0
     (for/list ([s (in-list strings-with-padding)])
       (let loop ([i 0] [m 0])
         (cond
           [(< i (string-length s))
            (cond
              [(equal? #\space (string-ref s i))
               (loop (+ i 1) m)]
              [else
               (loop (+ i 1) (+ i 1))])]
           [else m])))))

  (define dropped-blanks-at-end
    (reverse
     (let loop ([strings (reverse strings-with-padding)])
       (cond
         [(null? strings) '()]
         [else
          (cond
            [(regexp-match? #rx"^ *$" (car strings))
             (loop (cdr strings))]
            [else strings])]))))


  (for/list ([s (in-list dropped-blanks-at-end)])
    (substring s 0 widest-width)))
  
(define t% (class (text:inline-overview-mixin text%)
             (super-new)
             (send this set-inline-overview-enabled? #t)))
(define (red t start end) (color t start end "red"))
(define (green t start end) (color t start end "green"))
(define (blue t start end) (color t start end "blue"))
(define (color t start end c)
  (define bs (send (send t get-style-list) find-named-style "Basic"))
  (define sd (make-object style-delta%))
  (send sd set-delta-foreground c)
  (send t change-style sd start end))

(let ()
  (define t (new t%))
  (send t insert "a a")
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("* *")))

(let ()
  (define t (new t%))
  (send t insert "a a\n a")
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("* *"
                                  " * ")))

(let ()
  (define t (new t%))
  (send t insert "a a\n a")
  (send t do-all-of-the-work)
  (send t insert "b\nc\n" 4)
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("* *"
                                  "*  "
                                  "*  "
                                  " * ")))

(let ()
  (define t (new t%))
  (send t insert "a a\n a")
  (send t do-all-of-the-work)
  (send t delete (- (send t last-position) 2) (send t last-position))
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("* *")))

(let ()
  (define t (new t%))
  (send t insert "a a\n a")
  (send t do-all-of-the-work)
  (send t insert "b\nc\n" 4)
  (send t do-all-of-the-work)
  (send t delete 4 8)
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("* *"
                                  " * ")))

(let ()
  (define t (new t%))
  (send t insert "a a\n a")
  (send t do-all-of-the-work)
  (send t insert "b\nc\n" 4)
  (send t do-all-of-the-work)
  (send t delete 4 6)
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("* *"
                                  "*  "
                                  " * ")))

(let ()
  (define t (new (text:inline-overview-mixin text%)))
  (send t set-inline-overview-enabled? #t)
  (send t insert "a a\n a")
  (send t do-all-of-the-work)
  (send t insert "b\nc\n" 4)
  (send t do-all-of-the-work)
  (send t delete 4 6)
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("* *"
                                  "*  "
                                  " * ")))

(let ()
  (define t (new (text:inline-overview-mixin text%)))
  (send t set-inline-overview-enabled? #t)
  (send t insert "abcdef")
  (red t 0 1)
  (green t 1 2)
  (blue t 2 3)
  (send t do-all-of-the-work)
  (check-equal? (get-strings t) '("rgb***")))

(let ()
  (define t (new t%))
  (send t insert (make-string (* 2 maximum-bitmap-width) #\a))
  (send t do-all-of-the-work)
  (check-equal? (get-strings t)
                (list (make-string maximum-bitmap-width #\*))))

(let ()
  (define t (new t%))
  (send t insert (make-string (+ 10 maximum-bitmap-width 10) #\a))
  (red t 0 10)
  (green t 10 (+ 10 maximum-bitmap-width 10))
  (send t do-all-of-the-work)
  (check-equal? (get-strings t)
                (list (string-append
                       (make-string 10 #\r)
                       (make-string (- maximum-bitmap-width 10) #\g)))))

(let ()
  (define t (new t%))
  (send t insert (make-string (+ 10 maximum-bitmap-width 10) #\space))
  (send t do-all-of-the-work)
  (check-equal? (get-strings t)
                '()))

(let ()
  (define t (new (class t%
                   (define/augment (after-load-file success?)
                     (inner (void) after-load-file success?)
                     (send t delete 0 (send t paragraph-start-position 3)))
                   (super-new))))
  (define tmp (make-temporary-file "inline-overview-test~a.rkt"))
  (call-with-output-file tmp
    (λ (port)
      (for ([x (in-range 100)]) (fprintf port "~a\n" (modulo x 10))))
    #:exists 'truncate)
  (send t load-file tmp)
  (send t do-all-of-the-work)
  (check-equal? (get-strings t)
                (for/list ([i (in-range 97)])
                  "*")))
(let ()
  (define t (new t%))
  (send t insert (make-string 548 #\x))
  (send t do-all-of-the-work)
  (check-equal? (get-strings t)
                (list (make-string maximum-bitmap-width #\*))))
