(unit/sig framework:panel^
  (import mred-interfaces^
	  [mzlib:function : mzlib:function^])
  
  (rename [-editor<%> editor<%>])

  (define single<%> (interface (area-container<%>) active-child))
  (define single-mixin
    (mixin (area-container<%>) (single<%>) args
      (inherit get-alignment)
      (override
       [container-size
	(lambda (l)
	  (values (apply max (map car l)) (apply max (map cadr l))))]
       [place-children
	(lambda (l width height)
	  (printf "place children~n")
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
  
  (define single% (single-mixin panel%))
  (define single-pane% (single-mixin pane%))
  
  (define -editor<%>
    (interface ()
      get-canvas%
      collapse
      split))
  
  (define editor-mixin
    (mixin (panel<%>) (-editor<%>) args
      (rename [super-change-children change-children])
      (inherit get-parent change-children get-children)
      (public [get-canvas% (lambda () editor-canvas%)])
      (private
	[split-edits null])
      (public
	[collapse
	 (lambda (canvas)
	   (let ([media (send canvas get-media)])
	     (if (memq media split-edits)
		 (letrec ([helper
			   (lambda (canvas/panel)
			     (if (eq? canvas/panel this)
				 (let ([children (get-children)])
				   (cond
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
					(bell))
				 (let* ([parent (send canvas/panel get-parent)]
					[parents-children (send parent get-children)]
					[num-children (length parents-children)])
				   (if (<= num-children 1)
				       (helper parent)
				       (begin (send parent delete-child canvas/panel)
					      (send (car (send parent get-children)) focus))))))])
		   (send media remove-canvas canvas)
		   (helper canvas))
		 (bell))))]
	[split
	 (opt-lambda (canvas [panel% horizontal-panel%])
	   (let* ([frame (ivar canvas frame)]
		  [media (send canvas get-media)]
		  [canvas% (object-class canvas)]
		  [parent (send canvas get-parent)]
		  [new-panel #f]
		  [left-split #f]
		  [right-split #f]
		  [before #t])
	     (set! split-edits (cons media split-edits))
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
			(let ([before (mzlib:function:remq new-panel l)])
			  (map (lambda (x) (if (eq? x canvas)
					       new-panel
					       x))
			       before)))))
	      (lambda () (send frame delay-updates before)))
	     (send* media (remove-canvas canvas)
	       (add-canvas left-split)
	       (add-canvas right-split))
	     (send* left-split (set-media media) (focus))
	     (send* right-split (set-media media))))])
      (sequence (apply super-init args))))
  
  (define horizontal-editor%
    (editor-mixin horizontal-panel%))    
  (define vertical-editor%
    (editor-mixin vertical-panel%)))