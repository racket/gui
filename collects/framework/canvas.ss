(dunit/sig framework:canvas^
  (import mred-interfaces^
	  [preferences : framework:preferences^])
  
  (define wide-snip<%> (interface (editor-canvas<%>)
			 add-wide-snip
			 add-tall-snip))

  ;; wx: this need to collude with
  ;;     the edit, since the edit has the right callbacks.
  (define wide-snip-mixin
    (mixin (editor-canvas<%>) (wide-snip<%>) args
      (inherit get-editor)
      (rename [super-on-size on-size])
      (private
	[wide-snips null]
	[tall-snips null]
	[update-snip-size
	 (lambda (width?)
	   (lambda (s)
	     (let* ([width (box 0)]
		    [height (box 0)]
		    [leftm (box 0)]
		    [rightm (box 0)]
		    [topm (box 0)]
		    [bottomm (box 0)]
		    [left-edge-box (box 0)]
		    [top-edge-box (box 0)]
		    [snip-media (send s get-this-media)]
		    [edit (get-editor)])
	       (when edit
		 (send edit
		       run-after-edit-sequence
		       (lambda ()
			 (let ([admin (send edit get-admin)])
			   (send admin get-view #f #f width height)
			   (send s get-margin leftm topm rightm bottomm)


			   ;; when the width is to be maximized and there is a
			   ;; newline just behind the snip, we know that the left
			   ;; edge is zero. Special case for efficiency in the 
			   ;; console printer
			   (let ([fallback
				  (lambda ()
				    (send edit get-snip-position-and-location
					  s #f left-edge-box top-edge-box))])
			     (cond
			       [(not width?) (fallback)]
			       [(let ([prev (send s previous)])
				  (and (not prev
					    (member 'hard-newline (send prev get-flags)))))
				(set-box! left-edge-box 0)]
			       [else (fallback)]))


			   (if width?
			       (let ([snip-width (- (unbox width)
						    (unbox left-edge-box)
						    (unbox leftm)
						    (unbox rightm)
						    
						    ;; this two is the space that 
						    ;; the caret needs at the right of
						    ;; a buffer.
						    2)])
				 (send* s 
					(set-min-width snip-width)
					(set-max-width snip-width))
				 (when snip-media
				   (send snip-media set-max-width
					 (if (send snip-media auto-wrap)
					     snip-width
					     0))))
			       (let ([snip-height (- (unbox height)
						     (unbox top-edge-box)
						     (unbox topm)
						     (unbox bottomm))])
				 (send* s 
					(set-min-height snip-height)
					(set-max-height snip-height)))))))))))])
      (public
	[add-wide-snip
	 (lambda (snip)
	   (set! wide-snips (cons snip wide-snips))
	   ((update-snip-size #t) snip))]
	[add-tall-snip
	 (lambda (snip)
	   (set! tall-snips (cons snip tall-snips))
	   ((update-snip-size #f) snip))])
      (override
	[on-size
	 (lambda (width height)
	   (super-on-size width height)
	   (for-each (update-snip-size #t) wide-snips)
	   (for-each (update-snip-size #f) tall-snips))])
      (sequence
	(apply super-init args))))

  (define wide-snip% (wide-snip-mixin editor-canvas%)))