(define mred:panel@
  (unit/sig mred:panel^
    (import [mred:debug : mred:debug^]
	    [mred:container : mred:container^]
	    [mred:canvas : mred:canvas^]
	    mzlib:function^)

    (mred:debug:printf 'invoke "mred:panel@")

    (define make-edit-panel%
      (lambda (super%)
	(class-asi super%
	  (rename [super-change-children change-children])
	  (inherit children get-parent)
	  (public [get-canvas% (lambda () mred:canvas:editor-canvas%)])
	  (private
	    [edit-mapping (make-hash-table)]
	    [bind
	     (lambda (panel edit)
	       (hash-table-put! edit-mapping edit panel))]
	    [lookup/add
	     (lambda (child)
	       (if (is-a? child wx:media-edit%)
		   (let ([add-new
			  (lambda ()
			    (let ([p (make-object (get-canvas%) this)])
			      (send p set-media child)
			      (send child add-canvas p)
			      (bind p child)
			      p))])
		     (hash-table-get edit-mapping child add-new))
		   child))])
						     
	  (public

	    ; this contains the edits and panels that are children of
	    ; this panel, but the canvases of these edits are not
	    ; necessarily immediate children, since they may be split.
	    [actual-children null]
	   

	    [collapse
	     (lambda (canvas)
	       (letrec* ([media (send canvas get-media)]
			 [helper
			  (lambda (canvas/panel)
			    (printf "collapse; canvas/panel: ~a~n" canvas/panel)
			    (let* ([parent (send canvas/panel get-parent)])
			      (if (is-a? parent wx:frame%)
				  (begin (send canvas/panel change-children
					       (lambda (l) (list media)))
					 (wx:bell))
				  (let* ([parents-children (ivar parent children)]
					 [num-children (length parents-children)])
				    (printf "collapse; parent: ~a num-children: ~a~n" parent num-children)
				    (if (<= num-children 1)
					(collapse parent)
					(begin (send parent delete-child canvas/panel)
					       (send (car (ivar parent children)) set-focus)))))))])
		 (send media remove-canvas canvas)
		 (helper canvas)))]

	    [split
	     (opt-lambda (canvas [panel% mred:container:horizontal-panel%])
	       (let ([frame (ivar canvas frame)])
		 (dynamic-wind
		  (lambda () (send frame set-perform-updates #f))
		  (lambda () (letrec* ([media (send canvas get-media)]
				       [canvas% (object-class canvas)]
				       [parent (send canvas get-parent)]
				       [new-panel (make-object panel% parent)]
				       [left-split (make-object canvas% new-panel)]
				       [right-split (make-object canvas% new-panel)])
			       (send parent change-children
				     (lambda (l)
				       (let ([before (remq new-panel l)])
					 (map (lambda (x) (if (eq? x canvas)
							      new-panel
							      x))
					      before))))
			       (send* media (remove-canvas canvas)
				            (add-canvas left-split)
					    (add-canvas right-split))
			       (send* left-split (set-media media) (set-frame frame) (set-focus))
			       (send* right-split (set-media media) (set-frame frame))
			       (when (eq? this parent)
				 (bind media new-panel))))
		  (lambda () (send frame set-perform-updates #t)))))]
	    [change-children
	     (lambda (f)
	       (let ([new-children (f actual-children)])
		 (super-change-children (lambda (l)
					  (map lookup/add
					       new-children)))
		 (set! actual-children new-children)))]))))
    
    (define horizontal-edit-panel%
      (make-edit-panel% mred:container:horizontal-panel%))    
    (define vertical-edit-panel%
      (make-edit-panel% mred:container:vertical-panel%))))