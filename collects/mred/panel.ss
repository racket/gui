
  (unit/sig mred:panel^
    (import [mred:constants : mred:constants^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    mzlib:function^)

    (mred:debug:printf 'invoke "mred:panel@")

    (define make-edit-panel%
      (lambda (super%)
	(class-asi super%
	  (rename [super-change-children change-children])
	  (inherit get-parent change-children children)
	  (public [get-canvas% (lambda () mred:canvas:frame-title-canvas%)])
	  (public
	    [collapse
	     (lambda (canvas)
	       (letrec* ([media (send canvas get-media)]
			 [helper
			  (lambda (canvas/panel)
			    (if (eq? canvas/panel this)
				(begin (cond
					 [(and (= (length children) 1)
					       (eq? canvas (car children)))
					  (void)]
					 [(member canvas children)
					  (change-children (lambda (l) (list canvas)))]
					 [else
					  (change-children
					   (lambda (l)
					     (let ([c (make-object (object-class canvas) this)])
					       (send c set-media media)
					       (list c))))])
				       (wx:bell))
				(let* ([parent (send canvas/panel get-parent)]
				       [parents-children (ivar parent children)]
				       [num-children (length parents-children)])
				  (if (<= num-children 1)
				      (helper parent)
				      (begin (send parent delete-child canvas/panel)
					     (send (car (ivar parent children)) set-focus))))))])
		 (send media remove-canvas canvas)
		 (helper canvas)))]

	    [split
	     (opt-lambda (canvas [panel% mred:container:horizontal-panel%])
	       (let* ([frame (ivar canvas frame)]
		      [media (send canvas get-media)]
		      [canvas% (object-class canvas)]
		      [parent (send canvas get-parent)]
		      [new-panel #f]
		      [left-split #f]
		      [right-split #f]
		      [before #t])
		 (dynamic-wind
		  (lambda () 
		    (set! before (send frame delay-updates))
		    (send frame delay-updates #t))
		  (lambda () 
		    (set! new-panel (make-object panel% parent))
		    (set! left-split (make-object canvas% new-panel))
		    (set! right-split (make-object canvas% new-panel))
		    (send parent change-children
			  (lambda (l)
			    (let ([before (remq new-panel l)])
			      (map (lambda (x) (if (eq? x canvas)
						   new-panel
						   x))
				   before)))))
		  (lambda () (send frame delay-updates before)))
		 (send* media (remove-canvas canvas)
			(add-canvas left-split)
			(add-canvas right-split))
		 (send* left-split (set-media media) (set-focus))
		 (send* right-split (set-media media))))]))))
    
    (define horizontal-edit-panel%
      (make-edit-panel% mred:container:horizontal-panel%))    
    (define vertical-edit-panel%
      (make-edit-panel% mred:container:vertical-panel%)))