(module cache-image-snip mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "string.ss"))

  (provide argb-vector->bitmap
           overlay-bitmap
           build-bitmap
           cache-image-snip%
           snipclass)
  
  #|

    The true meaning of an image is a vector of rationals,
    between 0 & 255, representing color and alpha channel
    information. The vector's contents are analagous to
    the last argument to the get-argb-pixels method. That is,
    there are (* 4 w h) entries in the vector for an image
    of width w and height h, and the entries represent the
    alpha, red, green, & blue channels, resp.

    When drawn to the screen, the rationals are rounded to
    their nearest integer, but the true meaning is kept inside
    the image.

  note to self:
    mask of zero means this image dominates
    mask of 255 means this image contributes nothing

    black is 0
    white is 255

    a cleared out bitmap is full of 255s (white)

    an alpha of 1 means the pixel value is 0
    an alpha of 0 means the pixel value is 255
 |#
  
  ;; type argb-vector = (vectorof rational[between 0 & 255])
  
  (define cache-image-snip%
    (class snip%
      
      ;; draw-proc : (union #f ((is-a?/c dc<%>) int[dx] int[dy] -> void))
      ;; used for direct drawing
      (init-field dc-proc)
      
      ;; bitmap-proc : ((vectorof rational[0 <= x <= 255]) int[dx] int[dy] -> void)
      ;; used for drawing into a bitmap
      (init-field argb-proc)
      
      (init-field (width #f)
                  (height #f))
      (define/public (get-size) 
        (values width height))
      
      ;; argb-vector : (union #f argb-vector)
      (init-field [argb-vector #f])

      ;; bitmap : (union #f (is-a?/c bitmap%))
      ;; the way that this image is be drawn, on its own
      (define bitmap #f)
      
      (define/override (copy)
        (new cache-image-snip% 
             (dc-proc dc-proc)
             (argb-proc argb-proc)
             (width width)
             (height height)
             (argb-vector argb-vector)))
      
      ;; this can use the draw proc, rather than the argb-vector.

      ;; get-bitmap : -> bitmap
      ;; returns a bitmap showing what the image would look like, 
      ;; if it were drawn
      (define/public (get-bitmap)
        (unless bitmap
          (set! bitmap (argb-vector->bitmap (get-argb-vector)
                                            (ceiling (inexact->exact width))
                                            (ceiling (inexact->exact height)))))
        bitmap)
      
      ;; get-argb-vector : -> argb-vector
      (define/public (get-argb-vector)
        (unless argb-vector
          (set! argb-vector (make-string (* 4 width height) #\000))
          (argb-proc argb-vector 0 0))
        argb-vector)
      
      (define/override (get-extent dc x y w h descent space lspace rspace)
        (set-box/f! w width)
        (set-box/f! h height)
        (set-box/f! descent 0)
        (set-box/f! space 0)
        (set-box/f! lspace 0)
        (set-box/f! rspace 0))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (cond
          [argb-vector
           (let ([bitmap (get-bitmap)])
             (send dc draw-bitmap bitmap x y))]
          [dc-proc
           (dc-proc dc x y)]
          [else (void)]))
      
      (define/override (write f)
        (let ([str (format "~s"
                           (list width
                                 height
                                 (get-argb-vector)))])
          (send f write str)))
      
      (super-new)
      (inherit set-snipclass)
      (set-snipclass snipclass)))
  
  (define cache-image-snip-class%
    (class snip-class%
      (define/override (read f)
        (let ([data (read-from-string (send f get-string)
                                      void
                                      (lambda (x) #f))])
          (if data
              (new cache-image-snip%
                   (width (car data))
                   (height (cadr data))
                   (argb-vector (caddr data))
                   (bitmap #f)
                   (argb-proc void)
                   (draw-proc void))
              (make-null-cache-image-snip))))
      (super-new)))
  
  (define (make-null-cache-image-snip)
    (define size 10)
    (define (draw dc dx dy)
      (with-pen/brush
       dc
       "black" 'solid
       "black" 'transparent
       (send dc draw-ellipse dx dy size size)
       (send dc draw-line dx dy (+ dx size -1) (+ dy size -1))))
    (define bm (build-bitmap (lambda (dc) (draw dc 0 0))
                             size
                             size))
    (new cache-image-snip%
         (width size)
         (height size)
         (draw-proc draw)
         (argb-proc 
          (lambda (argb-vector dx dy)
            (overlay-bitmap argb-vector dx dy bm bm)))))

  ;; argb-vector->bitmap : argb-vector int int -> bitmap
  ;; flattens the argb vector into a bitmap
  (define (argb-vector->bitmap argb-vector w h)
    (let* ([bm (make-object bitmap% w h)]
           [mask-bm (make-object bitmap% w h)]
           [bdc (new bitmap-dc% (bitmap bm))]
           [str (make-string (vector-length argb-vector) #\377)]
           [mask-str (make-string (vector-length argb-vector) #\377)])
      (let loop ([i (- (vector-length argb-vector) 1)])
        (cond
          [(zero? (modulo i 4))
           (let ([av (integer->char (round (vector-ref argb-vector i)))])
             (string-set! mask-str (+ i 1) av)
             (string-set! mask-str (+ i 2) av)
             (string-set! mask-str (+ i 3) av))]
          [else
           (string-set! str i (integer->char (round (vector-ref argb-vector i))))])
        (unless (zero? i)
          (loop (- i 1))))
      (send bdc set-argb-pixels 0 0 w h str)
      (send bdc set-bitmap mask-bm)
      (send bdc set-argb-pixels 0 0 w h mask-str)
      (send bdc set-bitmap #f)
      (send bm set-loaded-mask mask-bm)
      bm))
  
  ;; overlay-bitmap : argb-vector int int bitmap bitmap -> void
  ;; assumes that the mask bitmap only has greyscale in it
  ;; (ie, that looking at the red component of the mask is enough)
  (define (overlay-bitmap argb-vector dx dy color mask)
    (let* ([w (send color get-width)]
           [h (send color get-height)]
           [color-str (make-string (* w h 4) #\000)]
           [mask-str (make-string (* w h 4) #\000)]
           [dc (make-object bitmap-dc%)])
      (send dc set-bitmap color)
      (send dc get-argb-pixels 0 0 w h color-str)
      (send dc set-bitmap #f) ;; in case mask and color are the same bitmap....
      (send dc set-bitmap mask)
      (send dc get-argb-pixels 0 0 w h mask-str)
      (send dc set-bitmap #f)
      (let yloop ([y 0]
                  [str-i 0])
        (unless (= y h)
          (let xloop ([x 0]
                      [str-i str-i])
            (if (= x w)
                (yloop (add1 y) str-i)
                (let* ([argb-i (* 4 (+ (+ dx x) (* (+ dy y) w)))]
                       [m1 (vector-ref argb-vector argb-i)]
                       [m2 (char->integer (string-ref mask-str (+ str-i 1)))] ;; get red coordinate
                       [m3 (build-m3 m1 m2)]
                       [do-b
                        (lambda (off)
                          (vector-set! argb-vector
                                       (+ argb-i off)
                                       (build-b3 m1
                                                 (vector-ref argb-vector (+ argb-i off))
                                                 m2
                                                 (char->integer (string-ref color-str (+ str-i off)))
                                                 m3)))])
                  (vector-set! argb-vector argb-i m3)
                  (do-b 1)
                  (do-b 2)
                  (do-b 3)
                  (xloop (+ x 1) (+ str-i 4)))))))))
  
#|
From Matthew's computation in PR 6930:
> m3 is (m1+m2-m1*m2) and 
> b3 is (m1*b1*(1-m2) + m2*b2)/m3

but that's for values between 0 and 1 and we
need values between 0 and 255. Worse, the values
sense are reversed. That is,
1 above corresponds to 0 in pixel values and
0 above corresponds to 255.

;; the spec
(define (build-m3-0 big-m1 big-m2)
  (let ([m1 (- 1 (/ big-m1 255))]
        [m2 (- 1 (/ big-m2 255))])
    (let ([m3 (+ m1 m2 (- (* m1 m2)))])
      (* 255 (- 1 m3)))))

; = substitute in lets
(define (build-m3-1 m1 m2)
  (* 255 (- 1 (+ (- 1 (/ m1 255))
                 (- 1 (/ m2 255))
                 (- (* (- 1 (/ m1 255))
                       (- 1 (/ m2 255))))))))

;= multiply out last product
(define (build-m3-2 m1 m2)
  (* 255 (- 1 (+ (- 1 (/ m1 255))
                 (- 1 (/ m2 255))
                 (- (+ 1 
                       (- (/ m1 255))
                       (- (/ m2 255))
                       (* (- (/ m1 255)) (- (/ m2 255)))))))))

; = lift out the neagtives into topmost sum
(define (build-m3-3 m1 m2)
  (* 255 (- 1 (+ (- (/ m1 255))
                 1
                 (- (/ m2 255))
                 1
                 -1
                 (/ m1 255)
                 (/ m2 255)
                 (- (* (/ m1 255) (/ m2 255)))))))

; = push in topmost subtraction
(define (build-m3-4 m1 m2)
  (* 255 (+ 1
            (/ m1 255)
            -1
            (/ m2 255)
            -1
            1
            (- (/ m1 255))
            (- (/ m2 255))
            (* (/ m1 255) (/ m2 255)))))

; = simplify sum:

(define (build-m3-5 m1 m2)
  (* 255 (* (/ m1 255) (/ m2 255))))

; = distribute 255

(define (build-m3-6 m1 m2) (* m1 m2 1/255))

(define (test-m3 m1 m2)
  (values (build-m3-0 m1 m2)
          (build-m3-1 m1 m2)
          (build-m3-2 m1 m2)
          (build-m3-3 m1 m2)
          (build-m3-4 m1 m2)
          (build-m3-5 m1 m2)
          (build-m3-6 m1 m2)))

(test-m3 0 0)
(test-m3 255 255)
(test-m3 100 200)

for b3, we have:

(define (build-m3-6 m1 m2) (* m1 m2 1/255))

;; the spec
(define (build-b3-0 big-m1 big-b1 big-m2 big-b2 big-m3)
  (let ([m1 (- 1 (/ big-m1 255))]
        [b1 (- 1 (/ big-b1 255))]
        [m2 (- 1 (/ big-m2 255))]
        [b2 (- 1 (/ big-b2 255))]
        [m3 (- 1 (/ big-m3 255))])
    (let ([ans (/ (+ (* m1 b1 (- 1 m2)) (* m2 b2)) m3)])
      (* 255 (- 1 ans)))))

;; = substitute in for let.
(define (build-b3-1 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- 1 (/ m1 255)) (- 1 (/ b1 255)) (- 1 (- 1 (/ m2 255)))) 
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = simple substitution
(define (build-b3-2 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- 1 (/ m1 255)) (- 1 (/ b1 255)) (/ m2 255))
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = multiply out first part of first *
(define (build-b3-3 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (+ 1
                          (- (/ m1 255))
                          (- (/ b1 255))
                          (* (/ m1 255) (/ b1 255))) 
                       (/ m2 255))
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = distribute out newly created product 
(define (build-b3-4 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (/ m2 255)
                    (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    (* (- 1 (/ m2 255)) (- 1 (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = multiply out product of sum
(define (build-b3-5 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (/ m2 255)
                    (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    (+ 1
                       (- (/ m2 255))
                       (- (/ b2 255))
                       (* (/ m2 255) (/ b2 255))))
                 (- 1 (/ m3 255))))))

;; = flatten out sum of sum & simplify
(define (build-b3-6 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    1
                    (- (/ b2 255))
                    (* (/ m2 255) (/ b2 255)))
                 (- 1 (/ m3 255))))))

;; = rearrange denom
(define (build-b3-7 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- (/ m1 255)) (/ m2 255))
                    (* (- (/ b1 255)) (/ m2 255))
                    (* (/ m1 255) (/ b1 255) (/ m2 255))
                    1
                    (- (/ b2 255))
                    (* (/ m2 255) (/ b2 255)))
                 (/ (- 255 m3) 255)))))

;; = move 255 to numerator
(define (build-b3-8 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (* 255 (+ (* (- (/ m1 255)) (/ m2 255))
                           (* (- (/ b1 255)) (/ m2 255))
                           (* (/ m1 255) (/ b1 255) (/ m2 255))
                           1
                           (- (/ b2 255))
                           (* (/ m2 255) (/ b2 255))))
                 (- 255 m3)))))

;; cancel out 255s in numerator
(define (build-b3-9 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- m1) (/ m2 255))
                    (* (- b1) (/ m2 255))
                    (* m1 (/ b1 255) (/ m2 255))
                    255
                    (- b2)
                    (* m2 (/ b2 255)))
                 (- 255 m3)))))

;; rearrange numerator
(define (build-b3-10 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (/ (* (- m1) m2) 255)
                    (/ (* (- b1) m2) 255)
                    (/ (* m1 b1 (/ m2 255)) 255)
                    (/ (* 255 255) 255)
                    (/ (* 255 (- b2)) 255)
                    (/ (* m2 b2) 255))
                 (- 255 m3)))))

;; pull out 255 in num
(define (build-b3-11 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (/ (+ (* (- m1) m2)
                       (* (- b1) m2)
                       (* m1 b1 (/ m2 255))
                       (* 255 255)
                       (* 255 (- b2))
                       (* m2 b2))
                    255)
                 (- 255 m3)))))

;; push 255 into denom
(define (build-b3-12 m1 b1 m2 b2 m3)
  (* 255 (- 1 (/ (+ (* (- m1) m2)
                    (* (- b1) m2)
                    (* m1 b1 (/ m2 255))
                    (* 255 255)
                    (* 255 (- b2))
                    (* m2 b2))
                 (* 255 (- 255 m3))))))

;; turn 1 into (/ (* 255 (- 255 m3)) (* 255 (- 255 m3)))
;; and add into numerator
(define (build-b3-13 m1 b1 m2 b2 m3)
  (* 255 (/ (- (* 255 (- 255 m3))
               (+ (* (- m1) m2)
                  (* (- b1) m2)
                  (* m1 b1 (/ m2 255))
                  (* 255 255)
                  (* 255 (- b2))
                  (* m2 b2)))
            (* 255 (- 255 m3)))))

;; cancel out outer 255
(define (build-b3-14 m1 b1 m2 b2 m3)
  (/ (- (* 255 (- 255 m3))
        (+ (* (- m1) m2)
           (* (- b1) m2)
           (* m1 b1 (/ m2 255))
           (* 255 255)
           (* 255 (- b2))
           (* m2 b2)))
     (- 255 m3)))

;; push negative thru to make big sum in numerator
(define (build-b3-15 m1 b1 m2 b2 m3)
  (/ (+ (* 255 (- 255 m3))
        (* m1 m2)
        (* b1 m2)
        (- (* m1 b1 (/ m2 255)))
        (- (* 255 255))
        (* 255 b2)
        (- (* m2 b2)))
     (- 255 m3)))

;; distribute 255 in first num term
(define (build-b3-16 m1 b1 m2 b2 m3)
  (/ (+ (* 255 255)
        (- (* 255 m3))
        (* m1 m2)
        (* b1 m2)
        (- (* m1 b1 (/ m2 255)))
        (- (* 255 255))
        (* 255 b2)
        (- (* m2 b2)))
     (- 255 m3)))

;; simplify num
(define (build-b3-17 m1 b1 m2 b2 m3)
  (/ (+ (* m1 m2)
        (* b1 m2)
        (- (* m2 b2))
        (- (* m1 b1 m2 1/255))
        (* 255 b2)
        (- (* 255 m3)))
     (- 255 m3)))

;; simplify num, some more
(define (build-b3-18 m1 b1 m2 b2 m3)
  (/ (+ (* (+ m1 b1 (- b2)) m2) 
        (* m1 b1 m2 -1/255)
        (* 255 b2)
        (* -255 m3))
     (- 255 m3)))

(define (test-b3 m1 b1 m2 b2)
  (let ([m3 (build-m3-6 m1 m2)])
    (values (build-b3-0 m1 b1 m2 b2 m3)
            (build-b3-1 m1 b1 m2 b2 m3)
            (build-b3-2 m1 b1 m2 b2 m3)
            (build-b3-3 m1 b1 m2 b2 m3)
            (build-b3-4 m1 b1 m2 b2 m3)
            (build-b3-5 m1 b1 m2 b2 m3)
            (build-b3-6 m1 b1 m2 b2 m3)
            (build-b3-7 m1 b1 m2 b2 m3)
            (build-b3-8 m1 b1 m2 b2 m3)
            (build-b3-9 m1 b1 m2 b2 m3)
            (build-b3-10 m1 b1 m2 b2 m3)
            (build-b3-11 m1 b1 m2 b2 m3)
            (build-b3-12 m1 b1 m2 b2 m3)
            (build-b3-13 m1 b1 m2 b2 m3)
            (build-b3-14 m1 b1 m2 b2 m3)
            (build-b3-15 m1 b1 m2 b2 m3)
            (build-b3-16 m1 b1 m2 b2 m3)
            (build-b3-17 m1 b1 m2 b2 m3)
            (build-b3-18 m1 b1 m2 b2 m3)
            )))

(test-b3 255 100 0 250)
(test-b3 0 150 255 100)
(test-b3 100 200 75 150)


|#
  (define (build-m3 m1 m2) (* m1 m2 1/255))
  
  (define (build-b3 m1 b1 m2 b2 m3)
    (if (= m3 255)
        0
        (/ (+ (* (+ m1 b1 (- b2)) m2) 
              (* m1 b1 m2 -1/255)
              (* 255 b2)
              (* -255 m3))
           (- 255 m3))))
  
  (define snipclass (new cache-image-snip-class%))
  (send snipclass set-version 1)
  (send snipclass set-classname (format "~s" `(lib "cache-image-snip.ss" "mrlib")))
  (send (get-the-snip-class-list) add snipclass)  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; misc. utilities
  ;;
  
  (define (build-bitmap draw w h)
    (let* ([bm (make-object bitmap% w h)]
           [bdc (make-object bitmap-dc% bm)])
      (send bdc clear)
      (draw bdc)
      (send bdc set-bitmap #f)
      bm))

  (define-syntax (with-pen/brush stx)
    (syntax-case stx ()
      [(_ dc pen-color pen-style brush-color brush-style code ...)
       (syntax
        (let ([old-pen (send dc get-pen)]
              [old-brush (send dc get-brush)])
          (send dc set-pen (send the-pen-list find-or-create-pen pen-color 1 pen-style))
          (send dc set-brush (send the-brush-list find-or-create-brush brush-color brush-style))
          code ...
          (send dc set-pen old-pen)
          (send dc set-brush old-brush)))]))
  
  (define (set-box/f! b v) (when (box? b) (set-box! b v)))
  
  ;; --

    ;; bitmaps->cache-image-snip : bitmap bitmap -> cache-image-snip
  (define (bitmaps->cache-image-snip bmp bmp-mask)
    (new cache-image-snip%
         [dc-proc (lambda (dc dx dy)
                    (send dc draw-bitmap 
                          bmp 
                          dx
                          dy
                          'solid
                          (send the-color-database find-color "black")
                          bmp-mask))]
         [argb-proc (lambda (dc dx dy)
                      (send dc draw-bitmap 
                            bmp 
                            dx
                            dy
                            'solid
                            (send the-color-database find-color "black")
                            bmp-mask))]
         [width (send bmp get-width)]
         [height (send bmp get-height)]))
  
  )