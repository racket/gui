(module canvas mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   "../macro.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide canvas@)

  (define canvas@
    (unit/sig framework:canvas^
      (import mred^
	      [preferences : framework:preferences^]
	      [frame : framework:frame^])
      
      (define basic<%> (interface ((class->interface editor-canvas%))))
      (define basic-mixin
	(mixin ((class100->interface editor-canvas%)) (basic<%>) args
	  (sequence
	    (apply super-init args))))
      
      (define info<%> (interface (basic<%>)))
      ;; (basic<%> -> (class (is-a? (send this get-top-level-window) frame:info<%>)))
      (define info-mixin 
	(mixin (basic<%>) (info<%>) (parent [editor #f] . args)
	  (inherit has-focus? get-top-level-window)
	  (rename [super-on-focus on-focus]
		  [super-set-editor set-editor])
	  (override
	   [on-focus
	    (lambda (on?)
	      (super-on-focus on?)
	      (send (get-top-level-window) set-info-canvas (and on? this))
	      (when on?
		(send (get-top-level-window) update-info)))]
	   [set-editor
	    (lambda (m)
	      (super-set-editor m)
	      (let ([tlw (get-top-level-window)])
		(when (eq? this (send tlw get-info-canvas))
		  (send tlw update-info))))])
	  (sequence
	    (apply super-init parent editor args)

	    (unless (is-a? (get-top-level-window) frame:info<%>)
	      (error 'canvas:text-info-mixin
		     "expected to be placed into a frame or dialog implementing frame:info<%>, got: ~e" 
		     (get-top-level-window)))

	    (when (has-focus?)
	      (send (get-top-level-window) update-info)))))

      (define wide-snip<%> (interface (basic<%>)
			     recalc-snips
			     add-wide-snip
			     add-tall-snip))

      ;; wx: this need to collude with
      ;;     the edit, since the edit has the right callbacks.
      (define wide-snip-mixin
	(mixin (basic<%>) (wide-snip<%>) args
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
			[snip-media (send s get-editor)]
			[edit (get-editor)]        
			[get-width
			 (let ([bl (box 0)]
			       [br (box 0)])
			   (lambda (s)
			     (send edit get-snip-location s bl #f #f)
			     (send edit get-snip-location s br #f #t)
			     (- (unbox br) (unbox bl))))]
			[calc-after-width
			 (lambda (s)
			   (+ 4 ;; this is compensate for an autowrapping bug
			      (let loop ([s s])
				(cond
				 [(not s) 0]
				 [(member 'hard-newline (send s get-flags)) 0]
				 [(member 'newline (send s get-flags)) 0]
				 [else
				  (if s
				      (+ (get-width s)
					 2 ;; for the caret
					 (loop (send s next)))
				      0)]))))])
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
					(send edit get-snip-location
					      s left-edge-box top-edge-box))])
				 (cond
				  [(not width?) (fallback)]
				  [(let ([prev (send s previous)])
				     (and prev
					  (member 'hard-newline (send prev get-flags))))
				   (set-box! left-edge-box 0)]
				  [else (fallback)]))

			       (if width?
				   (let* ([after-width (calc-after-width (send s next))]
					  [snip-width (max 0 (- (unbox width)
								(unbox left-edge-box)
								(unbox leftm)
								(unbox rightm)
								after-width
								;; this two is the space that 
								;; the caret needs at the right of
								;; a buffer.
								2))])
				     (send* s 
					    (set-min-width snip-width)
					    (set-max-width snip-width))
				     (when snip-media
				       (send snip-media set-max-width
					     (if (send snip-media auto-wrap)
						 snip-width
						 0))))
				   (let ([snip-height (max 0 (- (unbox height)
								(unbox top-edge-box)
								(unbox topm)
								(unbox bottomm)))])
				     (send* s 
					    (set-min-height snip-height)
					    (set-max-height snip-height)))))))))))])
	  (public
	    [recalc-snips
	     (lambda ()
	       (for-each (update-snip-size #t) wide-snips)
	       (for-each (update-snip-size #f) tall-snips))])
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
	      (recalc-snips)
	      (super-on-size width height))])
	  (sequence
	    (apply super-init args))))

      (define basic% (basic-mixin editor-canvas%))
      (define info% (info-mixin basic%))
      (define wide-snip% (wide-snip-mixin basic%)))))
