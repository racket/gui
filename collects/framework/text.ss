(unit/sig framework:text^
  (import mred^
	  [editor : framework:editor^]
	  [preferences : framework:preferences^]
	  [keymap : framework:keymap^])

  ;; wx: `default-wrapping?', add as the initial value for auto-wrap bitmap,
  ;; unless matthew makes it primitive

  (define basic<%>
    (interface ()
      highlight-range      
      styles-fixed?
      set-styles-fixed
      move/copy-to-edit
      autowrap-bitmap))

  (define make-basic%
    (mixin (interface (editor:basic<%> text<%>)) basic<%> args
      (inherit canvases get-max-width get-admin split-snip get-snip-position
	       delete find-snip invalidate-bitmap-cache
	       set-autowrap-bitmap get-keymap mode set-mode-direct
	       set-file-format get-file-format get-frame
	       get-style-list modified? change-style set-modified
	       position-location get-extent)
      
      (private
	[b1 (box 0)]
	[b2 (box 0)]
	[b3 (box 0)]
	[b4 (box 0)]
	[range-rectangles null]
	[recompute-range-rectangles
	 (lambda ()
	   (let ([new-rectangles
		  (lambda (range)
		    (let* ([start (range-start range)]
			   [end (range-end range)]
			   [b/w-bitmap (range-b/w-bitmap range)]
			   [color (range-color range)]
			   [caret-space? (range-caret-space? range)]
			   [start-eol? #f]
			   [end-eol? (if (= start end)
					 start-eol?
					 #t)])
		      (let-values ([(start-x top-start-y)
				    (begin 
				      (position-location start b1 b2 #t start-eol? #t)
				      (values (if caret-space?
						  (+ 1 (unbox b1))
						  (unbox b1))
					      (unbox b2)))]
				   [(end-x top-end-y)
				    (begin (position-location end b1 b2 #t end-eol? #t)
					   (values (unbox b1) (unbox b2)))]
				   [(bottom-start-y)
				    (begin (position-location start b1 b2 #f start-eol? #t)
					   (unbox b2))]
				   [(bottom-end-y)
				    (begin (position-location end b1 b2 #f end-eol? #t)
					   (unbox b2))])
			(cond
			  [(= top-start-y top-end-y)
			   (list 
			    (make-rectangle start-x
					    top-start-y
					    (if (= end-x start-x)
						(+ end-x 1)
						end-x)
					    bottom-start-y
					    b/w-bitmap 
					    color))]
			  [else
			   (list
			    (make-rectangle start-x
					    top-start-y
					    'right-edge
					    bottom-start-y
					    b/w-bitmap
					    color)
			    (make-rectangle 'left-edge
					    bottom-start-y
					    'max-width
					    top-end-y
					    b/w-bitmap
					    color)
			    (make-rectangle 'left-edge
					    top-end-y
					    end-x
					    bottom-end-y
					    b/w-bitmap
					    color))]))))]

		 [invalidate-rectangles
		  (lambda (rectangles)
		    (let-values ([(min-left max-right)
				  (let loop ([left #f]
					     [right #f]
					     [canvases canvases])
				    (cond
				      [(null? canvases)
				       (values left right)]
				      [else
				       (let-values ([(this-left this-right)
						     (send (car canvases)
							   call-as-primary-owner
							   (lambda ()
							     (send (get-admin) get-view b1 b2 b3 b4)
							     (let* ([this-left (unbox b1)]
								    [this-width (unbox b3)]
								    [this-right (+ this-left this-width)])
							       (values this-left
								       this-right))))])
					 (if (and left right)
					     (loop (min this-left left)
						   (max this-right right)
						   (cdr canvases))
					     (loop this-left
						   this-right
						   (cdr canvases))))]))])
		      (let loop ([left #f]
				 [top #f]
				 [right #f]
				 [bottom #f]
				 [rectangles rectangles])
			(cond
			  [(null? rectangles)
			   (when left
			     (invalidate-bitmap-cache left top (- right left) (- bottom top)))]
			  [else (let* ([r (car rectangles)]

				       [rleft (rectangle-left r)]
				       [rright (rectangle-right r)]
				       [rtop (rectangle-top r)]
				       [rbottom (rectangle-bottom r)]

				       [this-left (if (number? rleft)
						      rleft
						      min-left)]
				       [this-right (if (number? rright)
						       rright
						       max-right)]
				       [this-bottom rbottom]
				       [this-top rtop])
				  (if (and left top right bottom)
				      (loop (min this-left left)
					    (min this-top top)
					    (max this-right right)
					    (max this-bottom bottom)
					    (cdr rectangles))
				      (loop this-left 
					    this-top
					    this-right
					    this-bottom
					    (cdr rectangles))))]))))]
		 [old-rectangles range-rectangles])
	     
	     (set! range-rectangles 
		   (mzlib:function:foldl (lambda (x l) (append (new-rectangles x) l))
					 null ranges))
	     (invalidate-rectangles (append old-rectangles
					    range-rectangles))))]
	[ranges null]
	[pen (make-object pen% "BLACK" 0 'stipple)]
	[brush (make-object brush% "black" 'stipple)])
      (public
	;; the bitmap is used in b/w and the color is used in color.
	[highlight-range
	 (opt-lambda (start end color bitmap [caret-space? #f])
	   (let ([l (make-range start end bitmap color caret-space?)])
	     (set! ranges (cons l ranges))
	     (recompute-range-rectangles)
	     (lambda ()
	       (set! ranges 
		     (let loop ([r ranges])
		       (cond
			 [(null? r) r]
			 [else (if (eq? (car r) l)
				   (cdr r)
				   (cons (car r) (loop (cdr r))))])))
	       (recompute-range-rectangles))))])
      (rename [super-on-paint on-paint])
      (override
	[on-paint
	 (lambda (before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
	   (super-on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
	   (for-each
	    (lambda (rectangle)
	      (let-values ([(view-x view-y view-width view-height)
			    (begin 
			      (send (get-admin) get-view b1 b2 b3 b4)
			      (values (unbox b1)
				      (unbox b2)
				      (unbox b3)
				      (unbox b4)))])
		(let* ([old-pen (send dc get-pen)]
		       [old-brush (send dc get-brush)]
		       [old-logical-function (send dc get-logical-function)]
		       [b/w-bitmap (rectangle-b/w-bitmap rectangle)]
		       [color (let* ([rc (rectangle-color rectangle)]
				     [tmpc (make-object color% 0 0 0)])
				(if rc
				    (begin (send dc try-colour rc tmpc)
					   (and (<= (max (abs (- (send rc red) (send tmpc red)))
							 (abs (- (send rc blue) (send tmpc blue)))
							 (abs (- (send rc green) (send tmpc green))))
						    15)
						rc))
				    rc))]
		       [first-number (lambda (x y) (if (number? x) x y))]
		       [left (max left-margin (first-number (rectangle-left rectangle) view-x))]
		       [top (max top-margin (rectangle-top rectangle))]
		       [right (min right-margin
				   (if (number? (rectangle-right rectangle))
				       (rectangle-right rectangle)
				       (+ view-x view-width)))]
		       [bottom (min bottom-margin (rectangle-bottom rectangle))]
		       [width (max 0 (- right left))]
		       [height (max 0 (- bottom top))])
		  (let/ec k
		    (cond
		      [(and before color)
		       (send pen set-style 'solid)
		       (send brush set-style 'solid)
		       (send pen set-colour color)
		       (send brush set-colour color)
		       (send dc set-logical-function 'copy)]
		      [(and (not before) (not color) b/w-bitmap)
		       (send pen set-stipple b/w-bitmap)
		       (send pen set-style 'stipple)
		       (send brush set-stipple b/w-bitmap)
		       (send brush set-style 'stipple)
		       (send dc set-logical-function 'and)]
		      [else (k (void))])
		    (send dc set-pen pen)
		    (send dc set-brush brush)
		    (send dc draw-rectangle 
			  (+ left dx)
			  (+ top dy)
			  width
			  height)
		    (send dc set-logical-function old-logical-function)
		    (send dc set-pen old-pen)
		    (send dc set-brush old-brush)))))
	    range-rectangles))])


      (private
	[styles-fixed-edit-modified? #f])
      (public
	[styles-fixed? #f]
	[set-styles-fixed (lambda (b) (set! styles-fixed? b))])
      (rename
	[super-on-change-style on-change-style]
	[super-after-change-style after-change-style]
	[super-after-insert after-insert])
      (override
	[on-change-style
	 (lambda (start len)
	   (when styles-fixed?
	     (set! styles-fixed-edit-modified? (modified?)))
	   (super-on-change-style start len))]
	[after-insert
	 (lambda (start len)
	   (when styles-fixed?
	     (change-style (send (get-style-list) find-named-style "Standard")
			   start
			   (+ start len)))
	   (super-after-insert start len))]
	[after-change-style
	 (lambda (start len)
	   (super-after-change-style start len)
	   (when styles-fixed?
	     (set-modified styles-fixed-edit-modified?)))])
      
      (public
	[move/copy-to-edit
	 (lambda (dest-edit start end dest-position)
	   (let ([insert-edit (ivar dest-edit insert)])
	     (split-snip start)
	     (split-snip end)
	     (let loop ([snip (find-snip end 'before)])
	       (cond
		 [(or (null? snip) (< (get-snip-position snip) start))
		  (void)]
		 [else
		  (let ([prev (send snip previous)]
			[released/copied (if (send snip release-from-owner)
					     snip
					     (let* ([copy (send snip copy)]
						    [snip-start (get-snip-position snip)]
						    [snip-end (+ snip-start (send snip get-count))])
					       (delete snip-start snip-end)
					       snip))])
		    (insert-edit released/copied dest-position dest-position)
		    (loop prev))]))))])


      (public
	[autowrap-bitmap #f])
      (sequence
	(apply super-init args)
	(set-autowrap-bitmap autowrap-bitmap)
	(let ([keymap (get-keymap)])
	  (keymap:set-keymap-error-handler keymap)
	  (keymap:set-keymap-implied-shifts keymap)
	  (send keymap chain-to-keymap keymap:global-keymap #f)))))
  
  (define make-clever-file-format%
    (mixin text<%> text<%> args
      (inherit get-file-format set-file-format find-snip)
      (rename [super-on-save-file on-save-file]
	      [super-after-save-file after-save-file])
      
      (private [restore-file-format void])
      
      (public
	[after-save-file
	 (lambda (success)
	   (restore-file-format)
	   (super-after-save-file success))]
	[on-save-file
	 (let ([has-non-text-snips 
		(lambda ()
		  (let loop ([s (find-snip 0 'after)])
		    (cond
		      [(null? s) #f]
		      [(is-a? s text-snip%)
		       (loop (send s next))]
		      [else #t])))])
	   (lambda (name format)
	     (when (and (or (eq? format 'same)
			    (eq? format 'copy))
			(not (eq? (get-file-format) 
				  'std)))
	       (cond
		 [(eq? format 'copy)
		  (set! restore-file-format 
			(let ([f (get-file-format)])
			  (lambda ()
			    (set! restore-file-format void)
			    (set-file-format f))))
		  (set-file-format 'std)]
		 [(and (has-non-text-snips)
		       (or (not (preferences:get-preference 'framework:verify-change-format))
			   (gui-utils:get-choice "Save this file as plain text?" "No" "Yes")))
		  (set-file-format 'std)]
		 [else (void)]))
	     (or (super-on-save-file name format)
		 (begin 
		   (restore-file-format)
		   #f))))])
      (sequence (apply super-init args))))

  (define searching<%>
    (interface ()
      find-string-embedded))
  (define make-searching%
    (mixin (interface (editor:basic<%> text<%>)) searching<%> args
      (inherit get-end-position get-start-position last-position 
	       find-string get-snip-position get-admin find-snip
	       get-keymap)
      (public
	[find-string-embedded
	 (opt-lambda (str [direction 1] [start -1]
			  [end -1] [get-start #t]
			  [case-sensitive? #t] [pop-out? #f])
	   (let/ec k
	     (let* ([start (if (= -1 start)
			       (if (= direction 1)
				   (get-end-position)
				   (get-start-position))
			       start)]
		    [end (if (= -1 end)
			     (if (= direction 1)
				 (last-position)
				 0)
			     end)]
		    [flat (find-string str direction
				       start end get-start
				       case-sensitive?)]
		    [end-test
		     (lambda (snip)
		       (cond
			 [(null? snip) flat]
			 [(and (not (= -1 flat))
			       (let* ([start (get-snip-position snip)]
				      [end (+ start (send snip get-count))])
				 (if (= direction 1)
				     (and (<= start flat)
					  (< flat end))
				     (and (< start flat)
					  (<= flat end)))))
			  flat]
			 [else #f]))]			       
		    [pop-out
		     (lambda ()
		       (let ([admin (get-admin)])
			 (if (is-a? admin editor-snip-editor-admin%)
			     (let* ([snip (send admin get-snip)]
				    [edit-above (send (send snip get-admin) get-media)]
				    [pos (send edit-above get-snip-position snip)])
			       (send edit-above
				     find-string-embedded 
				     str
				     direction 
				     (if (= direction 1) (add1 pos) pos)
				     -1 get-start
				     case-sensitive? pop-out?))
			     (values this -1))))])
	       (let loop ([current-snip (find-snip start
						   (if (= direction 1)
						       'after-or-none
						       'before-or-none))])
		 (let ([next-loop
			(lambda ()
			  (if (= direction 1)
			      (loop (send current-snip next))
			      (loop (send current-snip previous))))])
		   (cond
		     [(end-test current-snip) => 
					      (lambda (x)
						(if (and (= x -1) pop-out?)
						    (pop-out)
						    (values this x)))]
		     [(is-a? current-snip editor-snip%)
		      (let-values ([(embedded embedded-pos)
				    (let ([media (send current-snip get-this-media)])
				      (and (not (null? media))
					   (send media find-string-embedded str
						 direction
						 (if (= 1 direction)
						     0
						     (send media last-position))
						 -1
						 get-start case-sensitive?)))])
			(if (= -1 embedded-pos)
			    (next-loop)
			    (values embedded embedded-pos)))]
		     [else (next-loop)]))))))])
      (sequence
	(apply super-init args)
	(let ([keymap (get-keymap)])
	  (keymap:set-keymap-error-handler keymap)
	  (keymap:set-keymap-implied-shifts keymap)
	  (send keymap chain-to-keymap keymap:global-search-keymap #f)))))
  
  (define make-return%
    (mixin text<%> text<%> args
      (rename [super-on-local-char on-local-char])
      (override
	[on-local-char
	 (lambda (key)
	   (let ([cr-code 13]
		 [lf-code 10]
		 [code (send key get-key-code)])
	     (or (and (or (= lf-code code)
			  (= cr-code code))
		      (return))
		 (super-on-local-char key))))])
      (sequence
	(apply super-init args))))

  (define make-info%
    (mixin (interface (editor:basic<%> text<%>)) (interface (editor:basic<%> text<%>)) args
      (inherit get-frame get-start-position get-end-position
	       run-after-edit-sequence)
      (rename [super-after-set-position after-set-position]
	      [super-after-edit-sequence after-edit-sequence]
	      [super-on-edit-sequence on-edit-sequence]
	      [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-set-overwrite-mode set-overwrite-mode]
	      [super-set-anchor set-anchor])
      (private
	[enqueue-for-frame
	 (lambda (ivar-sym tag)
	   (run-after-edit-sequence
	    (rec from-enqueue-for-frame
		 (lambda ()
		   (let ([frame (get-frame)])
		     (when frame
		       ((ivar/proc frame ivar-sym))))))
	    tag))])
      (override
	[set-anchor
	 (lambda (x)
	   (super-set-anchor x)
	   (enqueue-for-frame 'anchor-status-changed
			      'framework:anchor-status-changed))]
	[set-overwrite-mode
	 (lambda (x)
	   (super-set-overwrite-mode x)
	   (enqueue-for-frame 'overwrite-status-changed
			      'framework:overwrite-status-changed))]
	[after-set-position
	 (lambda ()
	   (super-after-set-position)
	   (enqueue-for-frame 'edit-position-changed
			      'framework:edit-position-changed))]
	[after-insert
	 (lambda (start len)
	   (super-after-insert start len)
	   (enqueue-for-frame 'edit-position-changed
			      'framework:edit-position-changed))]
	[after-delete
	 (lambda (start len)
	   (super-after-delete start len)
	   (enqueue-for-frame 'edit-position-changed
			      'framework:edit-position-changed))])))


  (define basic% (make-basic (editor:make-basic% text%)))
  (define return% (make-return% basic%))
  (define file% (editor:make-file% basic%))
  (define clever-file-format% (editor:make-clever-file-format% file%))
  (define backup-autosave% (editor:make-backup-autosave% clever-file-format%))
  (define searching% (make-searching backup-autosave%))
  (define info% (make-info% (editor:make-info% searching%))))