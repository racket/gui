
; This demonstrates adding your own snip classes to print arbitary
;  graphic objects in editor windows. In particular, when the result
;  of a scheme evalution in the console is a snip, then MrEd uses the
;  snip in the console rather than printing the textual representation.

(define car-try (lambda (list val) (if (null? list) val (car list))))
(define cdr-try (lambda (list val) (if (null? list) val (cdr list))))

; Here's a simple snip that just makes an empty square of a certain
; size. Try (make-object draw-snip 100 100) and you should get
; an empty box (100 pixels x 100 pixles) as the result
(define draw-snip%
  (make-class wx:snip%
    (inherit get-admin set-snipclass set-count)
    (public h w)
    (public
      (get-extent
       (lambda (dc x y wbox hbox descentbox spacebox
		   lspacebox rspacebox)
	 (if (not (null? hbox))
	     (set-box! hbox h))
	 (if (not (null? wbox))
	     (set-box! wbox w))
	 (if (not (null? descentbox))
	     (set-box! descentbox 0))
	 (if (not (null? spacebox))
	     (set-box! spacebox 0))
	 (if (not (null? rspacebox))
	     (set-box! rspacebox 0))
	 (if (not (null? lspacebox))
	     (set-box! lspacebox 0))))
      (draw
       (lambda (dc x y . other)
	 (let* ((xw (sub1 (+ x w)))
		(yh (sub1 (+ y h)))
		(x (add1 x))
		(y (add1 y)))
	   (send dc draw-line x y xw y)
	   (send dc draw-line xw y xw yh)
	   (send dc draw-line x yh xw yh)
	   (send dc draw-line x y x yh))))
      (copy
       (lambda ()
	 (make-object draw-snip% w h)))
      (write
       (lambda (stream)
	 (send stream << w)
	 (send stream << h)))
      (refresh
       (lambda ()
	 (let ([admin (get-admin)])
	   (unless (null? admin)
	     (send admin needs-update this 0 0 w h)))))
      (resized
       (lambda ()
	 (let ([admin (get-admin)])
	   (unless (null? admin)
	     (send admin resized this #t)))))
      (resize 
       (lambda (w-in h-in)
	 (set! w w-in)
	 (set! h h-in)
	 (resized)
	 #t)))
    (lambda (w-in h-in)
      (super-init)
      (set-snipclass (send (wx:get-the-snip-class-list) find "emptydrawbox"))
      (set-count 1)
      (set! h h-in)
      (set! w w-in))))

(define draw-snip-class
  (make-object 
   (make-class wx:snip-class%
     (inherit set-classname)
     (public
       [read
	(lambda (stream)
	  (let ([w-box (box 0)]
		[h-box (box 0)])
	    (send stream >> w-box)
	    (send stream >> h-box)
	    (make-object draw-snip% (unbox w-box) (unbox h-box))))])
     (lambda ()
       (super-init)
       (set-classname "emptydrawbox")))))

(send (wx:get-the-snip-class-list) add  draw-snip-class)

; Here's a snip class derived from draw-snip. It plots a function.
; There's a lot of code that tries to find the right part of the
; function to print or allows you to send it coordinate ranges - 
; I don't know if this code works. But it works fine if you give it
; a function on the unit square. (Say, (lambda (x) (* x x)).)

(define graph-snip%
  (make-class draw-snip%
    (inherit w h refresh resized set-snipclass)
    (rename [super-draw draw])
    (public [functions '()]
	    [shades '()]
	    x-start
	    x-end
	    y-start
	    y-end
	    [lmargin 5] [rmargin 5]
	    [tmargin 5] [bmargin 5]
	    [x-num-grid-lines 0]
	    [y-num-grid-lines 0]
	    [shade-brush
	     (send wx:the-brush-list find-or-create-brush
		   "BLACK" wx:const-cross-hatch)])
    (public
      (draw
       (lambda (dc x y . other)
	 (super-draw dc x y)
	 (let* ([bottom (- (+ h y) bmargin)]
		[top (+ y tmargin)]
		[right (- (+ x w) rmargin)]
		[left (+ x lmargin)]
		[graph-w (- w lmargin rmargin)]
		[graph-h (- h tmargin bmargin)]
		[x-scale (/ (- x-end x-start) graph-w)]
		[dx x-scale]
		[y-inv-scale (/ graph-h (- y-end y-start))]
		[dy (/ y-inv-scale)]
		[x-to-pos
		 (lambda (x)
		   (+ (/ (- x x-start) x-scale) left))]
		[y-to-pos
		 (lambda (y)
		   (- bottom (* y-inv-scale (- y y-start))))])

	   (if (<= x-start 0 x-end)
	       (let ([x-pos (x-to-pos 0)])
		 (send dc draw-line x-pos bottom x-pos top)))
	   (if (<= y-start 0 y-end)
	       (let ([y-pos (- bottom (* (- y-start) y-inv-scale))])
		 (send dc draw-line left y-pos right y-pos)))
	   (for-each
	    (lambda (f)
	      (let ([f (eval f)])
		(let loop ((i 0))
		  (if (< i graph-w)
		      (let* ((x0 (+ x-start (* i x-scale)))
			     (j (y-to-pos (f x0))))
			(if (and (> j y) (< j bottom))
			    (send dc draw-point (+ i left) j))
			(loop (add1 i)))))))
	    functions)
	   (for-each
	    (lambda (f)
	      (let ([f (eval f)]
		    [change-1 #f]
		    [change-1-time #f]
		    [change-2 #f]
		    [change-2-time #f])
		(catch escape
		  (let loop ([on (f x-start y-end)]
			     [xdir dx]
			     [ydir 0]
			     [x x-start]
			     [y y-end]
			     [time 0])
		    (let ([this-on (f x y)])
		      (if (not (eq? on this-on))
			  (let ([pair (cons
				       (if (< xdir 0)
					   (- x xdir)
					   x)
				       (if (< ydir 0)
					   (- y ydir)
					   y))])
			    (if change-1
				(begin
				  (set! change-2 pair)
				  (set! change-2-time time)
				  (escape this-on))
				(begin
				  (set! change-1 pair)
				  (set! change-1-time time)))))
		      (let ([newx (+ x xdir)]
			    [newy (+ y ydir)])
			(if (and (<= x-start newx x-end)
				 (<= y-start newy y-end))
			    (loop this-on xdir ydir newx newy time)
			    (cond
			      [(> xdir 0)
			       (loop this-on 0 (- dy) x-end y-end 1)]
			      [(< ydir 0)
			       (loop this-on(- dx) 0 x-end y-start 2)]
			      [(< xdir 0) 
			       (loop this-on 0 dy x-start y-start 3)]
			      [else (escape #f)]))))))
		(let ([points
		       (if change-2
			   ; Partial shade...
			   (let ([x1 (x-to-pos (car change-1))]
				 [x2 (x-to-pos (car change-2))]
				 [y1 (y-to-pos (cdr change-1))]
				 [y2 (y-to-pos (cdr change-2))]
				 [start-on (not (f x-start y-start))]
				 [pt (lambda (x y)
				       (make-object wx:point% x y))])
			     (send dc draw-line x1 y1 x2 y2)
			     (let ([end-time (if start-on 
						 change-1-time
						 change-2-time)]
				   [end-point (if start-on 
						  (pt x1 y1)
						  (pt x2 y2))])
			       (let loop ([time 
					   (if start-on 
					       change-2-time
					       change-1-time)]
					  [points
					   (list (if start-on 
						     (pt x2 y2)
						     (pt x1 y1)))])
				 (if (= time end-time)
				     (cons end-point points)
				     (loop
				      (modulo (add1 time) 4)
				      (cons
				       (case time
					 ((0) (pt right top))
					 ((1) (pt right bottom))
					 ((2) (pt left bottom))
					 ((3) (pt left top)))
				       points))))))
			   ; Shade everything or nothing
			   (if (f x-start y-start)
			       (list (pt right top)
				     (pt right bottom)
				     (pt left bottom)
				     (pt left top))
			       #f))])
		  (if points
		      (let ([old-brush (send dc get-brush)])
			(send dc set-brush shade-brush)
			(send dc draw-polygon points)
			(send dc set-brush old-brush))))))
	    shades))))
      (set-domain
       (opt-lambda (s [e x-end])
	 (set! x-start s)
	 (set! x-end e)
	 (refresh)))
      (set-range
       (opt-lambda (s [e y-end])
	 (set! y-start s)
	 (set! y-end e)
	 (refresh)))
      (plot
       (lambda (f)
	 (set! functions (cons f functions))
	 (refresh)))
      (affine-shade
       (lambda (f)
	 (set! shades (cons f shades))
	 (refresh)))
      (copy
       (lambda ()
	 (make-object graph-snip%
		      functions
		      (cons x-start x-end)
		      (cons w h)
		      (cons y-start y-end)
		      shades)))
      (write
       (lambda (stream)
	 (let ([INT (lambda (x)
		      (if (exact? x)
			  x
			  (inexact->exact x)))]
	       [FLOAT (lambda (x)
			(if (inexact? x)
			    x
			    (exact->inexact x)))])
	   (send stream << (expr->string functions)) 
	   (send stream << (FLOAT x-start))
	   (send stream << (FLOAT x-end))
	   (send stream << (INT w))
	   (send stream << (INT h))
	   (send stream << (FLOAT y-start))
	   (send stream << (FLOAT y-end))
	   (send stream << (expr->string shades))))))
    (opt-lambda ([fs '()]
		 [xaxes '()]
		 [size '()]
		 [yaxes '()]
		 [ss '()])
      (let* ((h (car-try size 200))
	     (w (cdr-try size 200)))
	(super-init h w)
	(set-snipclass (send (wx:get-the-snip-class-list) find "graph"))
	(set! functions fs)
	(set! shades ss)
	(set! x-start (car-try xaxes 0))
	(set! x-end (cdr-try xaxes 1))
	(let ([f (if (null? fs)
		     (lambda (x) x)
		     (eval (car fs)))])
	  (set! y-start (car-try yaxes (f x-start)))
	  (set! y-end (cdr-try yaxes (f x-end)))
	  (if (= y-start y-end)
	      (begin
		(set! y-start (- y-start 100))
		(set! y-end (+ y-end 100))))
	  (if (> y-start y-end)
	      (let ((start y-start))
		(set! y-start y-end)
		(set! y-end start))))))))

(define graph-snip-class
  (make-object 
   (make-class wx:snip-class%
     (inherit set-classname)
     (public
       [read
	(lambda (stream)
	  (let ([get-string
		 (lambda ()
		   (send stream get-string))]
		[get-integer
		 (lambda ()
		   (let ([n-box (box 0)])
		     (send stream >> n-box)
		     (unbox n-box)))]
		[get-float
		 (lambda ()
		   (let ([n-box (box 0.0)])
		     (send stream >> n-box)
		     (unbox n-box)))])
	    (let* ([functions (read-string (get-string))]
		   [x-start (get-float)]
		   [x-end (get-float)]
		   [w (get-integer)]
		   [h (get-integer)]
		   [y-start (get-float)]
		   [y-end (get-float)]
		   [shades (read-string (get-string))])
	      (make-object graph-snip%
			   functions
			   (cons x-start x-end)
			   (cons w h)
			   (cons y-start y-end)
			   shades))))])
     (lambda ()
       (super-init)
       (set-classname "graph")))))

(send (wx:get-the-snip-class-list) add graph-snip-class)

; Here's a helper function that will make the object for
; you. Try (graph (lambda (x) (* x x))).

(define graph
  (lambda (f)
    (make-object graph-snip% (list f))))

