(unit/sig framework:panel^
  (import mred^
	  [mzlib:function : mzlib:function^])
  
  (rename [-editor<%> editor<%>])

  (define single<%> (interface (area-container<%>) active-child))
  (define single-mixin
    (mixin (area-container<%>) (single<%>) args
      (inherit get-alignment)
      (rename [super-after-new-child after-new-child])
      (override
	[after-new-child
	 (lambda (c)
	   (if current-active-child
	       (send c show #f)
	       (set! current-active-child c)))]
       [container-size
	(lambda (l)
	  (if (null? l)
	      (values 0 0)
	      (values (apply max (map car l)) (apply max (map cadr l)))))]
       [place-children
	(lambda (l width height)
	  (let-values ([(h-align-spec v-align-spec) (get-alignment)])
	    (let ([align
		   (lambda (total-size spec item-size)
		     (floor
		      (case spec
			[(center) (- (/ total-size 2) (/ item-size 2))]
			[(left top) 0]
			[(right bottom) (- total-size item-size)]
			[else (error 'place-children "alignment spec is unknown ~a~n" spec)])))])
	      (map (lambda (l) 
		     (let*-values ([(min-width min-height v-stretch? h-stretch?) (apply values l)]
				   [(x this-width) (if h-stretch?
						       (values 0 width)
						       (values (align width h-align-spec min-width) min-width))]
				   [(y this-height) (if v-stretch?
							(values 0 height)
							(values (align height v-align-spec min-height) min-height))])
		       (list x y this-width this-height)))
		   l))))])
      
      (inherit get-children)
      (private [current-active-child #f])
      (public
	[active-child
	 (case-lambda
	  [() current-active-child]
	  [(x) 
	   (unless (eq? x current-active-child)
	     (for-each (lambda (x) (send x show #f))
		       (get-children))
	     (set! current-active-child x)
	     (send current-active-child show #t))])])
      (sequence
	(apply super-init args))))

  (define single-window<%> (interface (single<%>)))
  (define single-window-mixin
    (mixin (single<%> window<%>) (single-window<%>) args
      (inherit get-client-size get-size)
      (rename [super-container-size container-size])
      (override
       [container-size
	(lambda (l)
	  (let-values ([(super-width super-height) (super-container-size l)]
		       [(client-width client-height) (get-client-size)]
		       [(window-width window-height) (get-size)]
		       [(calc-size)
			(lambda (super client window)
			  (+ super (max 0 (- window client))))])
			 
	    (values
	     (calc-size super-width client-width window-width)
	     (calc-size super-height client-height window-height))))])
      (sequence
	(apply super-init args))))

  (define single% (single-window-mixin (single-mixin panel%)))
  (define single-pane% (single-mixin pane%)))