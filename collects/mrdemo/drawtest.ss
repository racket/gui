
(let ([f (make-object wx:frame% ()
		      "Graphics Test"
		      -1 -1 300 350)]
      [use-bitmap? #f])
  (let ([canvas
	 (make-object
	  (make-class wx:canvas%
	    (inherit get-dc)
	    (public
	     [on-paint
	      (lambda ()
		(let* ([can-dc (get-dc)]
		       [pen0s (make-object wx:pen% "BLACK" 0 wx:const-solid)]
		 [pen1s (make-object wx:pen% "BLACK" 1 wx:const-solid)]
		 [pen2s (make-object wx:pen% "BLACK" 2 wx:const-solid)]
		 [pen0t (make-object wx:pen% "BLACK" 0 wx:const-transparent)]
		 [pen1t (make-object wx:pen% "BLACK" 1 wx:const-transparent)]
		 [pen2t (make-object wx:pen% "BLACK" 2 wx:const-transparent)]
		 [brushs (make-object wx:brush% "BLACK" wx:const-solid)]
		 [brusht (make-object wx:brush% "BLACK" wx:const-transparent)]
		 [mem-dc (if use-bitmap?
			     (make-object wx:memory-dc%)
			     #f)]
		 [bm (if use-bitmap?
			 (make-object wx:bitmap% 300 300)
			 #f)]
		 [draw-series
		  (lambda (pens pent size x y)
		    (let ([dc (if (and use-bitmap?
				       (send bm ok?)
				       (send mem-dc ok?))
				  mem-dc
				  can-dc)])
		      (send dc set-font
			    (make-object wx:font%
					 10 wx:const-decorative
					 wx:const-normal wx:const-normal #t))

		      (send dc set-pen pens)
		      (send dc set-brush brusht)
		      
		      (send dc draw-text (string-append size " Pen")
			    (+ x 5) (+ y 8))
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
		      (send dc set-logical-function wx:const-clear)
		      (send dc draw-rectangle
			    (+ x 18) (+ y 96) 8 8)
		      (send dc set-logical-function wx:const-copy)
		      
		      (send dc draw-rectangle
			    (+ x 29) (+ y 95) 10 10)
		      (send dc set-logical-function wx:const-clear)
		      (send dc set-pen pent)
		      (send dc draw-rectangle
			    (+ x 30) (+ y 96) 8 8)

		      (send dc set-pen pens)
		      (send dc draw-rectangle
			    (+ x 5) (+ y 95) 10 10)
		      (send dc set-logical-function wx:const-xor)
		      (send dc draw-rectangle
			    (+ x 5) (+ y 95) 10 10)
		      (send dc set-logical-function wx:const-copy)
		      
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
			     (make-object wx:point% 5 95)
			     (make-object wx:point% 8 95)
			     (make-object wx:point% 11 98)
			     (make-object wx:point% 11 101)
			     (make-object wx:point% 8 104)
			     (make-object wx:point% 5 104)
			     (make-object wx:point% 2 101)
			     (make-object wx:point% 2 98)
			     (make-object wx:point% 5 95))
			    (+ x 12) (+ y 15))

		      (send dc draw-line
			    (+ x 5) (+ y 125) (+ x 10) (+ y 125))
		      (send dc draw-line
			    (+ x 11) (+ y 125) (+ x 16) (+ y 125))

		      (send dc draw-icon
			    mred:icon 5 135)
		      
		      (if (not (eq? dc can-dc))
			  (send can-dc blit 0 0 300 300
				mem-dc 0 0 wx:const-copy)))

		    'done)])

		  (when (and use-bitmap? (send bm ok?))
			(send mem-dc select-object bm)
			(when (send mem-dc ok?)
			      (send mem-dc set-brush brushs)
			      (send mem-dc set-logical-function wx:const-clear)
			      (send mem-dc draw-rectangle 0 0 300 300)
			      (send mem-dc set-logical-function wx:const-copy)))

		  (draw-series pen0s pen0t "0 x 0" 5 0)

		  (draw-series pen1s pen1t "1 x 1" 70 0)
		  
		  (draw-series pen2s pen2t "2 x 2" 135 0)
		  
		  'done))]))
	  f 0 50 300 300)])
    (let ([p (make-object wx:panel%
			  f 0 0 300 50)])
      (make-object wx:check-box% p
		   (lambda (self event)
		     (set! use-bitmap? (send event checked?))
		     (send canvas on-paint))
		   "Use Bitmap?")
      (make-object wx:button% p
		   (lambda ignored
		     (send f show #f))
		   "Close")))
  (send f show #t))


