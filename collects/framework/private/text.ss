(module text mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
	   "sig"
	   "../macro.ss"
	   "../gui-utils-sig.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "list.ss")
	   (lib "etc.ss"))
  (provide text@)

  (define text@
(unit/sig framework:text^
  (import mred^
	  [icon : framework:icon^]
	  [editor : framework:editor^]
	  [preferences : framework:preferences^]
	  [keymap : framework:keymap^]
          [gui-utils : framework:gui-utils^]
	  [color-model : framework:color-model^]
	  [frame : framework:frame^])
  
  (rename [-keymap% keymap%])
  
  (define-struct range (start end b/w-bitmap color caret-space?))
  (define-struct rectangle (left top right bottom b/w-bitmap color))
  
  ;; wx: `default-wrapping?', add as the initial value for auto-wrap bitmap,
  ;; unless matthew makes it primitive
  
  (define basic<%>
    (interface (editor:basic<%> (class->interface text%))
      highlight-range      
      get-styles-fixed
      set-styles-fixed
      move/copy-to-edit
      initial-autowrap-bitmap))
  
  (define basic-mixin
    (mixin (editor:basic<%> (class->interface text%)) (basic<%>) args
	   (inherit get-canvases get-admin split-snip get-snip-position
		    begin-edit-sequence end-edit-sequence
		    set-autowrap-bitmap
		    delete find-snip invalidate-bitmap-cache
		    set-file-format get-file-format
		    get-style-list is-modified? change-style set-modified
		    position-location get-extent)
	   
	   (private
	     [b1 (box 0)]
	     [b2 (box 0)]
	     [b3 (box 0)]
	     [b4 (box 0)]
	     [range-rectangles null]

             [invalidate-rectangles
              (lambda (rectangles)
                (let-values
		    ([(min-left max-right)
		      (let loop ([left #f]
				 [right #f]
				 [canvases (get-canvases)])
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
		  (when (and min-left max-right)
		    (let loop ([left #f]
			       [top #f]
			       [right #f]
			       [bottom #f]
			       [rectangles rectangles])
		      (cond
		       [(null? rectangles)
			(when left
			  (let ([width (- right left)]
				[height (- bottom top)])
			    (when (and (> width 0)
				       (> height 0))
			      (invalidate-bitmap-cache left top width height))))]
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
					 (cdr rectangles))))])))))]

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
		      [old-rectangles range-rectangles])
		  
		  (set! range-rectangles 
			(foldl (lambda (x l) (append (new-rectangles x) l))
			       null ranges))))]
	     [ranges null]
	     [pen (make-object pen% "BLACK" 0 'solid)]
	     [brush (make-object brush% "black" 'solid)])
	   (public
	     ;; the bitmap is used in b/w and the color is used in color.
	     [highlight-range
	      (opt-lambda (start end color bitmap [caret-space? #f] [priority 'low])
		(let ([l (make-range start end bitmap color caret-space?)])
                  (unless (or (eq? priority 'high) (eq? priority 'low))
		    (error 'highlight-range "expected last argument to be either 'high or 'low, got: ~e"
                           priority))
		  (invalidate-rectangles range-rectangles)
		  (set! ranges (if (eq? priority 'high) (cons l ranges) (append ranges (list l))))
		  (recompute-range-rectangles)
		  (invalidate-rectangles range-rectangles)
		  (lambda ()
		    (let ([old-rectangles range-rectangles])
		      (set! ranges
			    (let loop ([r ranges])
			      (cond
			       [(null? r) r]
			       [else (if (eq? (car r) l)
					 (cdr r)
					 (cons (car r) (loop (cdr r))))])))
		      (recompute-range-rectangles)                    
		      (invalidate-rectangles old-rectangles)))))])
	   (rename [super-on-paint on-paint])
	   (override
	    [on-paint
	     (lambda (before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
	       (super-on-paint before dc left-margin top-margin right-margin bottom-margin dx dy draw-caret)
               (recompute-range-rectangles)
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
			   [b/w-bitmap (rectangle-b/w-bitmap rectangle)]
			   [color (let* ([rc (rectangle-color rectangle)]
					 [tmpc (make-object color% 0 0 0)])
                                     (if rc
                                         (begin (send dc try-color rc tmpc)
                                                (if (<= (color-model:rgb-color-distance
                                                         (send rc red)
                                                         (send rc green)
                                                         (send rc blue)
                                                         (send tmpc red)
                                                         (send tmpc green)
                                                         (send tmpc blue))
                                                        18)
                                                    rc
                                                    #f))
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
			   (send pen set-color color)
			   (send brush set-color color)]
			  [(and (not before) (not color) b/w-bitmap)
			   (send pen set-stipple b/w-bitmap)
			   (send brush set-stipple b/w-bitmap)]
			  [else (k (void))])
			(send dc set-pen pen)
			(send dc set-brush brush)
			(send dc draw-rectangle 
			      (+ left dx)
			      (+ top dy)
			      width
			      height)
			(send dc set-pen old-pen)
			(send dc set-brush old-brush)))))
		range-rectangles))])
	   
	   (private
	     [styles-fixed? #f]
	     [styles-fixed-edit-modified? #f])
	   (public
	     [get-styles-fixed (lambda () styles-fixed?)]
	     [set-styles-fixed (lambda (b) (set! styles-fixed? b))])
	   (rename
	     [super-on-change-style on-change-style]
	     [super-after-change-style after-change-style]
	     [super-on-insert on-insert]
	     [super-after-insert after-insert])
	   (override
	    [on-change-style
	     (lambda (start len)
	       (when styles-fixed?
		 (set! styles-fixed-edit-modified? (is-modified?)))
	       (super-on-change-style start len))]
	    [on-insert
	     (lambda (start len)
	       (begin-edit-sequence)
	       (super-on-insert start len))]
	    [after-insert
	     (lambda (start len)
	       (when styles-fixed?
		 (change-style (send (get-style-list) find-named-style "Standard")
			       start
			       (+ start len)))
	       (super-after-insert start len)
	       (end-edit-sequence))]
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
		      [(or (not snip) (< (get-snip-position snip) start))
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
	     [initial-autowrap-bitmap (lambda () (icon:get-autowrap-bitmap))])
	   
	   (sequence
	     (apply super-init args)
	     (set-autowrap-bitmap (initial-autowrap-bitmap)))))

  (define searching<%> (interface (editor:keymap<%> basic<%>)))
  (define searching-mixin
    (mixin (editor:keymap<%> basic<%>) (searching<%>) args
	   (inherit get-end-position get-start-position last-position 
		    find-string get-snip-position get-admin find-snip)
	   ;(rename [super-on-new-box on-new-box])
	   ;(override
	   ; [on-new-box
	   ;  (lambda (type)
	   ;    (if (eq? type 'text)
	;	   (make-object editor-snip% (make-object searching%))
	;	   (super-on-new-box)))])
	   
	   (rename [super-get-keymaps get-keymaps])
	   (override
	    [get-keymaps
	     (lambda ()
	       (cons (keymap:get-search) (super-get-keymaps)))])
	   
	   (sequence
	     (apply super-init args))))
  
  (define return<%> (interface ((class->interface text%))))
  
  (define return-mixin
    (mixin ((class->interface text%)) (return<%>) (return . args)
	   (rename [super-on-local-char on-local-char])
	   (override
	    [on-local-char
	     (lambda (key)
	       (let ([cr-code #\return]
		     [lf-code #\newline]
		     [code (send key get-key-code)])
		 (or (and (char? code)
			  (or (char=? lf-code code)
			      (char=? cr-code code))
			  (return))
		     (super-on-local-char key))))])
	   (sequence
	     (apply super-init args))))
  
  (define info<%> (interface (basic<%>)))
  
  (define info-mixin
    (mixin (editor:keymap<%> basic<%>) (info<%>) args
	   (inherit get-start-position get-end-position get-canvas
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
			(let ([canvas (get-canvas)])
			  (when canvas
			    (let ([frame (send canvas get-top-level-window)])
			      (when (is-a? frame frame:text-info<%>)
				((ivar/proc frame ivar-sym))))))))
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
	       (enqueue-for-frame 'editor-position-changed
				  'framework:editor-position-changed))]
	    [after-insert
	     (lambda (start len)
	       (super-after-insert start len)
	       (enqueue-for-frame 'editor-position-changed
				  'framework:editor-position-changed))]
	    [after-delete
	     (lambda (start len)
	       (super-after-delete start len)
	       (enqueue-for-frame 'editor-position-changed
				  'framework:editor-position-changed))])
	   (sequence
	     (apply super-init args))))
  
  (define clever-file-format<%> (interface ((class->interface text%))))
  
  (define clever-file-format-mixin
    (mixin ((class->interface text%)) (clever-file-format<%>) args
      (inherit get-file-format set-file-format find-first-snip)
      (rename [super-on-save-file on-save-file])
      (override
       [on-save-file
	(let ([all-string-snips 
	       (lambda ()
		 (let loop ([s (find-first-snip)])
		   (cond
		    [(not s) #t]
		    [(is-a? s string-snip%)
		     (loop (send s next))]
		    [else #f])))])
	  (lambda (name format)
	    (let ([all-strings? (all-string-snips)])
	      (cond
	       [(and all-strings?
		     (or (eq? format 'same) (eq? format 'copy))
		     (eq? 'standard (get-file-format))
		     (or (not (preferences:get 'framework:verify-change-format))
			 (gui-utils:get-choice
			  "Save this file as plain text?" "Yes" "No")))
		(set-file-format 'text)]
	       [(and (not all-strings?)
		     (or (eq? format 'same) (eq? format 'copy))
		     (eq? 'text (get-file-format))
		     (or (not (preferences:get 'framework:verify-change-format))
			 (gui-utils:get-choice
			  "Save this file in drscheme-specific non-text format?" "Yes" "No")))
		(set-file-format 'standard)]
	       [else (void)]))
	    (super-on-save-file name format)))])
      (sequence
	(apply super-init args))))
  
  (define basic% (basic-mixin (editor:basic-mixin text%)))
  (define -keymap% (editor:keymap-mixin basic%))
  (define return% (return-mixin -keymap%))
  (define autowrap% (editor:autowrap-mixin -keymap%))
  (define file% (editor:file-mixin autowrap%))
  (define clever-file-format% (clever-file-format-mixin file%))
  (define backup-autosave% (editor:backup-autosave-mixin clever-file-format%))
  (define searching% (searching-mixin backup-autosave%))
  (define info% (info-mixin (editor:info-mixin searching%))))))
