
(define sys-path 
  (lambda (f)
    (build-path (collection-path "icons") f)))

(define local-path 
  (let ([d (current-load-relative-directory)])
    (lambda (f)
      (build-path d f))))

(define (get-icon)
  (make-object bitmap% (sys-path "mred.xbm") 'xbm))

(define (show-instructions file)
  (letrec ([f (make-object frame% file #f 400 400)]
	   [print (make-object button% "Print" f
			       (lambda (b ev)
				 (send e print)))]
	   [c (make-object editor-canvas% f)]
	   [e (make-object text%)])
    (send e load-file file)
    (send e lock #t)
    (send c set-editor e)
    (send f show #t)))

(let* ([f (make-object frame% "Graphics Test" #f 300 450)]
       [vp (make-object vertical-panel% f)]
       [hp0 (make-object horizontal-panel% vp)]
       [hp (make-object horizontal-panel% vp)]
       [hp2 hp]
       [bb (make-object bitmap% (sys-path "bb.gif") 'gif)]
       [return (let ([bm (make-object bitmap% (sys-path "return.xbm") 'xbm)]
		     [dc (make-object bitmap-dc%)])
		 (send dc set-bitmap bm)
		 (send dc draw-line 0 3 20 3)
		 (send dc set-bitmap #f)
		 bm)]
       [use-bitmap? #f]
       [depth-one? #f]
       [cyan? #f]
       [clip? #f])
  (send hp0 stretchable-height #f)
  (send hp stretchable-height #f)
  (make-object button% "What Should I See?" hp0
	       (lambda (b e)
		 (show-instructions (local-path "draw-info.txt"))))
  (let ([canvas
	 (make-object
	  (class canvas% args
	    (inherit get-dc)
	    (public
	     [no-bitmaps? #f]
	     [set-bitmaps (lambda (on?) (set! no-bitmaps? (not on?)) (on-paint))]
	     [no-stipples? #f]
	     [set-stipples (lambda (on?) (set! no-stipples? (not on?)) (on-paint))]
	     [scale 1]
	     [set-scale (lambda (s) (set! scale s) (on-paint))]
	     [offset 0]
	     [set-offset (lambda (o) (set! offset o) (on-paint))])
	    (override
	     [on-paint
	      (case-lambda
	       [() (on-paint #f)]
	       [(ps?)
		(let* ([can-dc (get-dc)]
		       [pen0s (make-object pen% "BLACK" 0 'solid)]
		       [pen1s (make-object pen% "BLACK" 1 'solid)]
		       [pen2s (make-object pen% "BLACK" 2 'solid)]
		       [pen0t (make-object pen% "BLACK" 0 'transparent)]
		       [pen1t (make-object pen% "BLACK" 1 'transparent)]
		       [pen2t (make-object pen% "BLACK" 2 'transparent)]
		       [brushs (make-object brush% "BLACK" 'solid)]
		       [brusht (make-object brush% "BLACK" 'transparent)]
		       [penr (make-object pen% "RED" 1 'solid)]
		       [brushb (make-object brush% "BLUE" 'solid)]
		       [mem-dc (if use-bitmap?
				   (make-object bitmap-dc%)
				   #f)]
		       [bm (if use-bitmap?
			       (make-object bitmap% (* scale 350) (* scale 300) depth-one?)
			       #f)]
		       [draw-series
			(lambda (dc pens pent size x y flevel last?)
			  (let* ([ofont (send dc get-font)]
				 [otfg (send dc get-text-foreground)]
				 [otbg (send dc get-text-background)]
				 [obm (send dc get-background-mode)])
			    (if (positive? flevel)
				(send dc set-font
				      (make-object font%
						   10 'decorative
						   'normal 
						   (if (> flevel 1)
						       'bold
						       'normal)
						   #t)))
			    (send dc set-pen pens)
			    (send dc set-brush brusht)
			    
			    ; Text should overlay this line (except for 2x2)
			    (send dc draw-line 
				  (+ x 3) (+ y 12)
				  (+ x 40) (+ y 12))

			    (send dc set-text-background (make-object color% "YELLOW"))
			    (when (= flevel 2)
			      (send dc set-text-foreground (make-object color% "RED"))
			      (send dc set-background-mode 'solid))

			    (send dc draw-text (string-append size " Pen")
				  (+ x 5) (+ y 8))
			    (send dc set-font ofont)
			    
			    (when (= flevel 2)
			      (send dc set-text-foreground otfg)
			      (send dc set-background-mode obm))
			    (send dc set-text-background otbg)
			    
			    (send dc draw-line
				  (+ x 5) (+ y 27) (+ x 10) (+ 27 y))
			    (send dc draw-rectangle
				  (+ x 5) (+ y 30) 5 5)
			    (send dc draw-line
				  (+ x 12) (+ y 30) (+ x 12) (+ y 35))
			    
			    (send dc draw-line
				  (+ x 5) (+ y 40) (+ x 10) (+ 40 y))
			    (send dc draw-rectangle
				  (+ x 5) (+ y 41) 5 5)
			    (send dc draw-line
				  (+ x 10) (+ y 41) (+ x 10) (+ 46 y))
			    
			    (send dc draw-line
				  (+ x 15) (+ y 25) (+ x 20) (+ 25 y))
			    (send dc draw-line
				  (+ x 20) (+ y 30) (+ x 20) (+ 25 y))
			    
			    (send dc draw-line
				  (+ x 30) (+ y 25) (+ x 25) (+ 25 y))
			    (send dc draw-line
				  (+ x 25) (+ y 30) (+ x 25) (+ 25 y))
			    
			    (send dc draw-line
				  (+ x 35) (+ y 30) (+ x 40) (+ 30 y))
			    (send dc draw-line
				  (+ x 40) (+ y 25) (+ x 40) (+ 30 y))
			    
			    (send dc draw-line
				  (+ x 50) (+ y 30) (+ x 45) (+ 30 y))
			    (send dc draw-line
				  (+ x 45) (+ y 25) (+ x 45) (+ 30 y))

			    ; Check line thickness with "X"
			    (send dc draw-line
				  (+ x 20) (+ y 45) (+ x 40) (+ 39 y))
			    (send dc draw-line
				  (+ x 20) (+ y 39) (+ x 40) (+ 45 y))
			    
			    (send dc draw-rectangle
				  (+ x 5) (+ y 50) 10 10)
			    (send dc draw-rounded-rectangle
				  (+ x 5) (+ y 65) 10 10 3)
			    (send dc draw-ellipse
				  (+ x 5) (+ y 80) 10 10)
			    
			    (send dc set-brush brushs)
			    (send dc draw-rectangle
				  (+ x 17) (+ y 50) 10 10)
			    (send dc draw-rounded-rectangle
				  (+ x 17) (+ y 65) 10 10 3)
			    (send dc draw-ellipse
				  (+ x 17) (+ y 80) 10 10)
			    
			    (send dc set-pen pent)
			    (send dc draw-rectangle
				  (+ x 29) (+ y 50) 10 10)
			    (send dc draw-rounded-rectangle
				  (+ x 29) (+ y 65) 10 10 3)
			    (send dc draw-ellipse
				  (+ x 29) (+ y 80) 10 10)
			    

			    (send dc set-pen pens)
			    (send dc draw-rectangle
				  (+ x 17) (+ y 95) 10 10)
			    (send dc set-logical-function 'clear)
			    (send dc draw-rectangle
				  (+ x 18) (+ y 96) 8 8)
			    (send dc set-logical-function 'copy)
			    
			    (send dc draw-rectangle
				  (+ x 29) (+ y 95) 10 10)
			    (send dc set-logical-function 'clear)
			    (send dc set-pen pent)
			    (send dc draw-rectangle
				  (+ x 30) (+ y 96) 8 8)

			    (send dc set-pen pens)
			    (send dc draw-rectangle
				  (+ x 5) (+ y 95) 10 10)
			    (send dc set-logical-function 'xor)
			    (send dc draw-rectangle
				  (+ x 5) (+ y 95) 10 10)
			    (send dc set-logical-function 'copy)
			    
			    (send dc draw-line
				  (+ x 5) (+ y 110) (+ x 8) (+ y 110))
			    (send dc draw-line
				  (+ x 8) (+ y 110) (+ x 11) (+ y 113))
			    (send dc draw-line
				  (+ x 11) (+ y 113) (+ x 11) (+ y 116))
			    (send dc draw-line
				  (+ x 11) (+ y 116) (+ x 8) (+ y 119))
			    (send dc draw-line
				  (+ x 8) (+ y 119) (+ x 5) (+ y 119))
			    (send dc draw-line
				  (+ x 5) (+ y 119) (+ x 2) (+ y 116))
			    (send dc draw-line
				  (+ x 2) (+ y 116) (+ x 2) (+ y 113))
			    (send dc draw-line
				  (+ x 2) (+ y 113) (+ x 5) (+ y 110))
			    
			    (send dc draw-lines
				  (list
				   (make-object point% 5 95)
				   (make-object point% 8 95)
				   (make-object point% 11 98)
				   (make-object point% 11 101)
				   (make-object point% 8 104)
				   (make-object point% 5 104)
				   (make-object point% 2 101)
				   (make-object point% 2 98)
				   (make-object point% 5 95))
				  (+ x 12) (+ y 15))

			    (send dc draw-point (+ x 35) (+ y 115))
			    
			    (send dc draw-line
				  (+ x 5) (+ y 125) (+ x 10) (+ y 125))
			    (send dc draw-line
				  (+ x 11) (+ y 125) (+ x 16) (+ y 125))

			    (send dc set-brush brusht)
			    (send dc draw-arc 
				  (+ x 20) (+ y 135)
				  (+ x 5) (+ y 150)
				  (+ x 20) (+ y 150))
			    (send dc draw-arc 
				  (+ x 35) (+ y 150)
				  (+ x 20) (+ y 135)
				  (+ x 20) (+ y 150))
			    (send dc set-brush brushs)
			    (send dc draw-arc 
				  (+ x 60) (+ y 135)
				  (+ x 36) (+ y 150)
				  (+ x 60) (+ y 150))		
			    (send dc set-brush brusht)      

			    (when last?
			      ; Splines
			      (define op (send dc get-pen))
			      (define (draw-ess dx dy)
				(send dc draw-spline 
				      (+ dx 200) (+ dy 10)
				      (+ dx 218) (+ dy 12)
				      (+ dx 220) (+ dy 20))
				(send dc draw-spline 
				      (+ dx 220) (+ dy 20)
				      (+ dx 222) (+ dy 28)
				      (+ dx 240) (+ dy 30)))
			      (send dc set-pen pen0s)
			      (draw-ess 0 0)
			      (send dc set-pen (make-object pen% "RED" 0 'solid))
			      (draw-ess -2 2)

			      ; Brush patterns:
			      (let ([pat-list (list 'bdiagonal-hatch
						    'crossdiag-hatch
						    'fdiagonal-hatch
						    'cross-hatch
						    'horizontal-hatch
						    'vertical-hatch)]
				    [b (make-object brush% "BLACK" 'solid)]
				    [ob (send dc get-brush)]
				    [obg (send dc get-background)]
				    [blue (make-object brush% "BLUE" 'solid)])
				(let loop ([x 245][y 10][l pat-list])
				  (unless (null? l)
				    (send b set-color "BLACK")
				    (send b set-style (car l))
				    (send dc set-brush b)
				    (send dc draw-rectangle x y 20 20)
				    (send dc set-brush ob)
				    (send b set-color "GREEN")
				    (send dc set-brush b)
				    (send dc draw-rectangle (+ x 25) y 20 20)
				    (send dc set-background blue)
				    (send dc draw-rectangle (+ x 50) y 20 20)
				    (send dc set-background obg)
				    (send dc set-brush ob)
				    (loop x (+ y 25) (cdr l)))))

			      (send dc set-pen op))

			    
			    (when (and (not no-bitmaps?) last?)
			      (let ([x 5] [y 165])
				(send dc draw-bitmap (get-icon) x y)
				(set! x (+ x (send (get-icon) get-width)))
				(let ([do-one
				       (lambda (bm mode)
					 (if (send bm ok?)
					     (begin
					       (let ([h (send bm get-height)]
						     [w (send bm get-width)])
						 (send dc draw-bitmap-region
						       bm x y 
						       0 0 w h
						       mode)
						 (set! x (+ x w 10))))
					     (printf "bad bitmap~n")))])
				  (do-one bb 'copy)
				  (do-one return 'copy)
				  (send dc set-pen penr)
				  (do-one return 'copy)
				  (do-one return 'color)
				  (do-one bb 'color)
				  (let ([bg (send dc get-background)])
				    (send dc set-background brushs)
				    (do-one return 'color)
				    (send dc set-background bg))
				  (send dc set-pen pens))))

			    (when (and (not no-stipples?) last?)
			      ; Blue box as background:
			      (send dc set-brush brushb)
			      (send dc draw-rectangle 80 200 125 40)
			      (when (send return ok?)
				(let ([b (make-object brush% "GREEN" 'stipple)])
				  (send b set-stipple return)
				  (send dc set-brush b)
				  ; First stipple (transparent background)
				  (send dc draw-rectangle 85 205 30 30)
				  (send dc set-brush brushs)
				  (send b set-style 'opaque-stipple)
				  (send dc set-brush b)
				  ; Second stipple (opaque)
				  (send dc draw-rectangle 120 205 30 30)
				  (send dc set-brush brushs)
				  (send b set-stipple bb)
				  (send dc set-brush b)
				  ; Third stipple (BB logo)
				  (send dc draw-rectangle 155 205 20 30)
				  (send dc set-brush brushs)
				  (send b set-stipple #f)
				  (send b set-style 'cross-hatch)
				  (send dc set-brush b)
				  ; Green cross hatch (white BG) on blue field
				  (send dc draw-rectangle 180 205 20 20)
				  (send dc set-brush brushs))))

			    (let ([styles (list 'solid
						'dot
						'long-dash
						'short-dash
						'dot-dash)]
				  [obg (send dc get-background)]
				  [red (make-object brush% "RED" 'solid)])
			      (let loop ([s styles][y 250])
				(unless (null? s)
				  (let ([p (make-object pen% "GREEN" flevel (car s))])
				    (send dc set-pen p)
				    (send dc draw-line (+ x 5) y (+ x 30) y)
				    (send dc set-background red)
				    (send dc draw-line (+ x 5) (+ 4 y) (+ x 30) (+ y 4))
				    (send dc set-background obg)
				    (send pens set-style (car s))
				    (send dc set-pen pens)
				    (send dc draw-line (+ x 30) y (+ x 55) y)
				    (send dc set-background red)
				    (send dc draw-line (+ x 30) (+ y 4) (+ x 55) (+ y 4))
				    (send dc set-background obg)
				    (send dc set-pen pent)
				    (send pens set-style 'solid)
				    (loop (cdr s) (+ y 8))))))

			    (if (not (or ps? (eq? dc can-dc)))
				(send can-dc draw-bitmap (send mem-dc get-bitmap) 0 0 'copy)))

			  'done)])

		  (send (get-dc) set-user-scale 1 1)
		  (send (get-dc) set-device-origin 0 0)

		  (let ([dc (if ps?
				(let ([dc (make-object post-script-dc%)])
				  (and (send dc ok?) dc))
				(if (and use-bitmap? (send bm ok?))
				    (begin
				      (send mem-dc set-bitmap bm)
				      mem-dc)
				    (get-dc)))])
		    (when dc
		      (send dc start-doc "Draw Test")
		      (send dc start-page)

		      (send dc set-user-scale scale scale)
		      (send dc set-device-origin offset offset)
		      
		      (send dc set-background
			    (if cyan?
				(make-object brush% "CYAN" 'solid)
				(make-object brush% "WHITE" 'solid)))

		      (send dc destroy-clipping-region)
		      (send dc clear)

		      (when clip?
			(send dc set-clipping-region 100 -25 10 400))
		      
		      ; check default pen/brush:
		      (send dc draw-rectangle 0 0 5 5)
		      (send dc draw-line 0 0 20 6)
		      
		      (draw-series dc pen0s pen0t "0 x 0" 5 0 0 #f)
		      
		      (draw-series dc pen1s pen1t "1 x 1" 70 0 1 #f)
		      
		      (draw-series dc pen2s pen2t "2 x 2" 135 0 2 #t)

		      (let ([x (box 0)]
			    [y (box 0)]
			    [w (box 0)]
			    [h (box 0)])
			(send dc get-clipping-region x y w h)
			(unless (equal? (map unbox (list x y w h))
					(if clip?
					    '(100. -25. 10. 400.)
					    '(0. 0. -1. -1.)))
			  (error 'draw-test "clipping region changed badly: ~a" (list x y w h))))

		      (let ([w (box 0)]
			    [h (box 0)])
			(send dc get-size w h)
			(let ([w (unbox w)]
			      [h (unbox h)])
			  (unless (cond
				   [ps? #t]
				   [use-bitmap? (and (= w (* scale 350)) (= h (* scale 300)))]
				   [else (= w (send this get-width)) (= h (send this get-height))])
			    (error 'x "wrong size reported by get-size: ~a ~a; w & h is ~a ~a" 
				   w h (send this get-width) (send this get-height)))))

		      (send dc end-page)
		      (send dc end-doc)))
		  
		  'done)])])
	    (sequence (apply super-init args)))
	  vp)])
    (make-object radio-box% #f '("Canvas" "Pixmap" "Bitmap") hp0
		 (lambda (self event)
		   (set! use-bitmap? (< 0 (send self get-selection)))
		   (set! depth-one? (< 1 (send self get-selection)))
		   (send canvas on-paint))
		 '(horizontal))
    (make-object button% "Hide" hp0
		 (lambda (self event) (send vp change-children (lambda (l) (list canvas)))))
    (make-object button% "PostScript" hp
		 (lambda (self event)
		   (send canvas on-paint #t)))
    (make-object check-box% "*2" hp
		 (lambda (self event)
		   (send canvas set-scale (if (send self get-value) 2 1))))
    (make-object check-box% "+10" hp
		 (lambda (self event)
		   (send canvas set-offset (if (send self get-value) 10 0))))
    (make-object check-box% "Clip" hp
		 (lambda (self event)
		   (set! clip? (send self get-value))
		   (send canvas on-paint)))
    (make-object check-box% "Cyan" hp
		 (lambda (self event)
		   (set! cyan? (send self get-value))
		   (send canvas on-paint)))
    (send (make-object check-box% "Icons" hp2
		       (lambda (self event)
			 (send canvas set-bitmaps (send self get-value))))
	  set-value #t)
    (send (make-object check-box% "Stipples" hp2
		       (lambda (self event)
			 (send canvas set-stipples (send self get-value))))
	  set-value #t))

  (send f show #t))

;; Still to do:

; set-logical-function

; Canvas, Pixmaps, and Bitmaps:
;  get-pixel
;  begin-set-pixel
;  end-set-pixel
;  set-pixel
