(unit/sig framework:color-model^
  (import mzlib:function^)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                 ;;;
  ;;;           MATRIX OPS            ;;;
  ;;;                                 ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
  ;; Matrix inversion using Cramer's Rule

  ; submatrix : (list-of (list-of num)) int int -> (list-of (list-of num))
  ; submatrix "crosses out" row i and column j from the matrix, returning a new one
  
  (define (submatrix source i j)
    (let row-loop ([row 0])
      (cond 
        [(eq? row (length source)) null]
        [(eq? row i) (row-loop (+ row 1))]
        [else
         (cons 
          (let col-loop ([col 0])
            (cond 
              [(eq? col (length (car source))) null]
              [(eq? col j) (col-loop (+ col 1))]
              [else
               (cons (list-ref (list-ref source row) col)
                     (col-loop (+ col 1)))]))
          (row-loop (+ row 1)))])))
  
  ;(equal? (submatrix test-matrix 1 2)
  ;        '((1 2 6) (7 8 4)))
  
  ; det : (list-of (list-of num)) -> num
  
  (define (det matrix)
    (if (null? matrix)
        1
        (let loop ([row 0] [sign 1])
          (if (= row (length matrix))
              0
              (+ (* sign
                    (list-ref (list-ref matrix row) 0)
                    (det (submatrix matrix row 0)))
                 (loop (+ row 1) (- sign)))))))
  
  ;(define square-test-matrix '((3 20 3) (37 0 8) (2 1 4)))
  
  ;(= (det square-test-matrix) -2553)

  ; invert : (list-of (list-of num)) -> (list-of (list-of num))
  
  (define (matrix-invert matrix)
    (let-values ([(width height) (matrix-dimension matrix)])
      (when (not (= width height))
        (error 'invert "matrix is not square: ~s" matrix))
      (let ([delta-inv (/ 1 (det matrix))])
        (let row-loop ([row 0] [sign 1])
          (if (= row (length matrix))
              null
              (cons 
               (let col-loop ([col 0] [sign sign])
                 (if (= col (length (car matrix)))
                     null
                     (cons (* delta-inv
                              sign
                              (det (submatrix matrix col row)))
                           (col-loop (+ col 1) (- sign)))))
               (row-loop (+ row 1) (- sign))))))))
  
  ;(equal? (matrix-invert square-test-matrix)
  ;        '((8/2553 77/2553 -160/2553) (44/851 -2/851 -29/851) (-1/69 -1/69 20/69)))
  
  ; matrix-dimension : (list-of (list-of num)) -> (values num num)  
  ; takes a matrix, returns width and height
  
  (define (matrix-dimension matrix)
    (when (not (pair? matrix))
      (error 'matrix-dimension "matrix argument is not a list: ~s" matrix))
    (let ([height (length matrix)])
      (when (= height 0)
        (error 'matrix-dimension "matrix argument is empty: ~s" matrix))
      (when (not (pair? (car matrix)))
        (error 'matrix-dimension "matrix row is not a list: ~s" (car matrix)))
      (let ([width (length (car matrix))])
        (when (= width 0)
          (error 'matrix-dimension "matrix argument has width 0: ~s" matrix))
        (let loop ([rows matrix])
          (if (null? rows)
              (values width height)
              (begin
                (when (not (pair? (car rows)))
                  (error 'matrix-dimension "row is not a list: ~s" (car rows)))
                (when (not (= width (length (car rows))))
                  (error 'matrix-dimension "rows have different widths: ~s and ~s" width (length (car rows))))
                (loop (cdr rows))))))))
  
  ; transpose : (list-of (list-of num)) -> (list-of (list-of num))
  (define (transpose vector) (apply map list vector))
  
  
  ;; test code
  '(equal? (transpose '((3 2 1) (9 8 7))) '((3 9) (2 8) (1 7)))
  
  ; inner-product : (list-of num) (list-of num) -> num
  
  (define (inner-product a b)
    (foldl + 0 (map * a b)))
  
  ;; test code
  '(= (inner-product '(4 1 3) '(0 3 4))
     15)
  
  ; matrix-multiply: (list-of (list-of num)) (list-of (list-of num)) -> (list-of (list-of num))
  ; multiplies the two matrices.
  
  (define (matrix-multiply a b)
    (let-values ([(width-a height-a) (matrix-dimension a)]
                 [(width-b height-b) (matrix-dimension b)])
      (when (not (= width-a height-b))
        (error 'matrix-multiply "matrix dimensions do not match for multiplication"))
      (let ([b-t (transpose b)])
        (map (lambda (row)
               (map (lambda (col)
                      (inner-product row col))
                    b-t))
             a))))

  ;; test code
  '(equal? (matrix-multiply '((1 2 3 4) (9 8 3 2)) '((0) (2) (0) (3)))
          '((16) (22)))
  
  (void)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                 ;;;
  ;;;           COLOR MODEL           ;;;
  ;;;                                 ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; NTSC standard RGB phosphor constants:
  
  ;    red green blue
  ;x   0.67 0.21 0.14
  ;y   0.33 0.71 0.08
  ;
  ; white point: 
  ; C : x-w = 0.31, y-w = 0.316, big-y-w = 100.0
  
  (define x-r 0.67)
  (define y-r 0.33)
  (define x-g 0.21)
  (define y-g 0.71)
  (define x-b 0.14)
  (define y-b 0.08)
  
  (define z-r (- 1 x-r y-r))
  (define z-g (- 1 x-g y-g))
  (define z-b (- 1 x-b y-b))
  
  (define x-w 0.31)
  (define y-w 0.316)
  (define big-y-w 100.0)
  
  (define-struct XYZ (X Y Z))
  
  (define (xy-big-y->XYZ x y big-y)
    (let ([sigma (/ big-y y)])
      (make-XYZ
       (* x sigma)
       (* y sigma)
       (* (- 1 x y) sigma))))
  
  (define XYZ-white (xy-big-y->XYZ x-w y-w big-y-w))
  
  ;`((,(XYZ-X XYZ-white) ,x-r ,x-g ,x-b)
  ;  (,(XYZ-Y XYZ-white) ,y-r ,y-g ,y-b)
  ;  (,(XYZ-Z XYZ-white) ,z-r ,z-g ,z-b))
  
  ; sigmas were calculated by soving a set of linear equations based upon NTSC standard phosphors

  (define pre-matrix `((,x-r ,x-g ,x-b)
                       (,y-r ,y-g ,y-b)
                       (,z-r ,z-g ,z-b)))
  
  (define-values (sigma-r sigma-g sigma-b)
    (let* ([inversion 
            (matrix-invert pre-matrix)]
           [sigmas
            (matrix-multiply inversion `((,(XYZ-X XYZ-white))
                                         (,(XYZ-Y XYZ-white))
                                         (,(XYZ-Z XYZ-white))))])
      (apply values (car (transpose sigmas)))))
  
  (define big-x-r (* x-r sigma-r))
  (define big-y-r (* y-r sigma-r))
  (define big-z-r (* z-r sigma-r))
  
  (define big-x-g (* x-g sigma-g))
  (define big-y-g (* y-g sigma-g))
  (define big-z-g (* z-g sigma-g))
  
  (define big-x-b (* x-b sigma-b))
  (define big-y-b (* y-b sigma-b))
  (define big-z-b (* z-b sigma-b))
  
  (define rgb->xyz-matrix
    (map (lambda (row scalar)
           (map (lambda (row-elt) (* row-elt scalar 1/255)) row))
         pre-matrix
         `(,sigma-r ,sigma-g ,sigma-b)))
  
  ;(printf "rgb->xyz-matrix: ~n~s~n" rgb->xyz-matrix)
  
  (define xyz->rgb-matrix
    (matrix-invert rgb->xyz-matrix))
  
  ;(printf "xyz->rgb-matrix: ~n~s~n" xyz->rgb-matrix)
  
  (define (rgb->xyz r g b)
    (apply make-XYZ (car (transpose (matrix-multiply rgb->xyz-matrix (transpose `((,r ,g ,b))))))))
  
  (define (xyz->rgb x y z)
    (car (transpose (matrix-multiply xyz->rgb-matrix (transpose `((,x ,y ,z)))))))

  ;L* = 116(Y/big-y-n)^1/3 - 16, Y/big-y-n > 0.01
  ;u* = 13 L*(u-p - u-p-n)
  ;v* = 13 L*(v-p - v-p-n)
  ;
  ;u-p = (4X)/(X+15Y+3Z)   v-p = (9Y)/(X+15Y+3Z)
  ;u-p-n = (same but with -n) v-p-n = (same but with -n)
  
  (define-struct Luv (L u v))
  
  (define (XYZ-denom XYZ)
    (+ (XYZ-X XYZ) (* 15 (XYZ-Y XYZ)) (* 3 (XYZ-Z XYZ))))
  
  (define (XYZ-u-p XYZ)
    (/ (* 4 (XYZ-X XYZ)) (XYZ-denom XYZ)))
  
  (define (XYZ-v-p XYZ)
    (/ (* 9 (XYZ-Y XYZ)) (XYZ-denom XYZ)))
  
  (define (XYZ->Luv XYZ)
    (let* ([L (- (* 116 (expt (/ (XYZ-Y XYZ) (XYZ-Y XYZ-white))
                              1/3))
                 16)]
           [u-p (XYZ-u-p XYZ)]
           [u-p-white (XYZ-u-p XYZ-white)]
           [v-p (XYZ-v-p XYZ)]
           [v-p-white (XYZ-v-p XYZ-white)])
      (make-Luv L (* 13 L (- u-p u-p-white)) (* 13 L (- v-p v-p-white)))))
  
  (define (Luv-distance a b)
    (expt (+ (expt (- (Luv-L a) (Luv-L b)) 2)
             (expt (- (Luv-u a) (Luv-u b)) 2)
             (expt (- (Luv-v a) (Luv-v b)) 2))
          1/3))
  
  (define (rgb-color-distance r-a g-a b-a r-b g-b b-b)
    (let* ([Luv-a (XYZ->Luv (RGB->XYZ r-a g-a b-a))]
           [Luv-b (XYZ->Luv (RGB->XYZ r-b g-b b-b))])
      (Luv-distance Luv-a Luv-b))))