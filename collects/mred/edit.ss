(unit/sig mred:edit^
  (import mred:wx^
	  [mred:constants : mred:constants^]
	  [mred:connections : mred:connections^]
	  [mred:autosave : mred:autosave^]
	  [mred:finder : mred:finder^]
	  [mred:path-utils : mred:path-utils^]
	  [mred:mode : mred:mode^]
	  [mred:frame : mred:frame^]
	  [mred:scheme-paren : mred:scheme-paren^]
	  [mred:keymap : mred:keymap^]
	  [mred:icon : mred:icon^]
	  [mred:preferences : mred:preferences^]
	  [mred:gui-utils : mred:gui-utils^]
	  [mzlib:function : mzlib:function^])
  
  (mred:debug:printf 'invoke "mred:edit@")
  
  (define-struct range (start end b/w-bitmap color))
  (define-struct rectangle (left top right bottom b/w-bitmap color))
  
  (mred:preferences:set-preference-default 'mred:verify-change-format #f 
					   (lambda (x)
					     (or (not x)
						 (eq? x #t))))
  
  (mred:preferences:set-preference-default 'mred:auto-set-wrap? #f
					   (lambda (x)
					     (or (not x)
						 (eq? x #t))))
  
  (define make-snip%
    (let ([sl (make-object wx:style-list%)])
      (send sl new-named-style "Standard" (send sl find-named-style "Basic"))
      (let ([std (send sl find-named-style "Standard")])
	(lambda (snip%)
	  (class snip% args
	    (inherit set-style)
	    (public [edit% media-edit%])
	    (sequence
	      (cond
		[(null? args)
		 (super-init (make-object edit%))]
		[(null? (car args))
		 (apply super-init (make-object edit%) (cdr args))]
		[else (apply super-init args)])
	      (set-style std)))))))
  
  (define media-snip% (make-snip% wx:media-snip%))
  (define snip% (make-snip% wx:snip%))
  
  (define make-std-buffer%
    (lambda (buffer%)
      (class buffer% args
	(sequence (mred:debug:printf 'creation "creating a buffer"))
	(inherit modified? get-filename save-file canvases
		 get-max-width get-admin)
	(rename [super-set-modified set-modified]
		[super-on-save-file on-save-file]
		[super-on-focus on-focus]
		[super-lock lock])
	
	(public [editing-this-file? #f])
	
	(public
	  [locked? #f]
	  [lock 
	   (lambda (x)
	     (set! locked? x)
	     (super-lock x))]
	  [do-close (lambda () (void))]
	  
	  [get-edit-snip
	   (lambda () (make-object media-snip%
				   (make-object edit%)))]
	  [get-pasteboard-snip
	   (lambda () (make-object media-snip%
				   (make-object pasteboard%)))]
	  [on-new-box
	   (lambda (type)
	     (cond
	       [(= type wx:const-edit-buffer)
		(get-edit-snip)]
	       [else (get-pasteboard-snip)]))])
	
	(public
	  [get-file (lambda (d) 
		      (let ([v (mred:finder:get-file d)])
			(if v
			    v
			    null)))]
	  [put-file (lambda (d f) (let ([v (mred:finder:put-file f d)])
				    (if v
					v
					null)))]
	  [mode #f]
	  [set-mode-direct (lambda (v) (set! mode v))]
	  [set-mode
	   (lambda (m)
	     #f)])
	(sequence
	  (apply super-init args)))))
  
  (define make-pasteboard% make-std-buffer%)
  
  (define make-media-edit%
    (lambda (super%)
      (class super% args
	(inherit canvases get-max-width get-admin split-snip get-snip-position
		 delete find-snip set-filename invalidate-bitmap-cache
		 begin-edit-sequence end-edit-sequence
		 set-autowrap-bitmap get-keymap mode set-mode-direct
		 set-file-format get-file-format get-frame
		 get-style-list modified? change-style set-modified
		 position-location get-extent)
	
	(rename [super-on-focus on-focus]
		[super-on-local-event on-local-event]
		
		[super-on-set-focus on-set-focus]
		[super-on-kill-focus on-kill-focus]
		
		[super-after-set-position after-set-position]
		
		[super-on-edit-sequence on-edit-sequence]
		[super-on-change-style on-change-style]
		[super-on-insert on-insert]
		[super-on-delete on-delete]
		[super-on-set-size-constraint on-set-size-constraint]
		
		[super-after-edit-sequence after-edit-sequence]
		[super-after-change-style after-change-style]
		[super-after-insert after-insert]
		[super-after-delete after-delete]
		[super-after-set-size-constraint after-set-size-constraint]
		
		[super-set-max-width set-max-width]
		[super-load-file load-file]
		[super-on-paint on-paint])
	
	(private [styles-fixed-edit-modified? #f])
	
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
			     [start-eol? #f]
			     [end-eol? (if (= start end)
					   start-eol?
					   #t)])
			(let-values ([(start-x top-start-y)
				      (begin (position-location start b1 b2 #t start-eol? #t)
					     (values (unbox b1) (unbox b2)))]
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
					   (if left
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
				    (if left
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
	  [pen (make-object wx:pen%
			    "BLACK"
			    0
			    wx:const-stipple)]
	  [brush (make-object wx:brush%
			      "black"
			      wx:const-stipple)])
	(public
	  ;; the bitmap is used in b/w and the color is used in color.
	  [highlight-range
	   (opt-lambda (start end color [bitmap #f])
	     (mred:debug:printf 'highlight-range "highlight-range: adding range: ~a ~a" start end)
	     (let ([l (make-range start end bitmap color)])
	       (set! ranges (cons l ranges))
	       (recompute-range-rectangles)
	       (lambda ()
		 (mred:debug:printf 'highlight-range "highlight-range: removing range: ~a ~a" start end)
		 (set! ranges 
		       (let loop ([r ranges])
			 (cond
			   [(null? r) r]
			   [else (if (eq? (car r) l)
				     (cdr r)
				     (cons (car r) (loop (cdr r))))])))
		 (recompute-range-rectangles))))]
	  
	  [on-paint
	   (lambda (before dc left top right bottom dx dy draw-caret)
	     (super-on-paint before dc left top right bottom dx dy draw-caret)
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
				       [tmpc (make-object wx:colour% 0 0 0)])
				  (if rc
				      (begin (send dc try-colour rc tmpc)
					     (and (<= (max (abs (- (send rc red) (send tmpc red)))
							   (abs (- (send rc blue) (send tmpc blue)))
							   (abs (- (send rc green) (send tmpc green))))
						      15)
						  rc))
				      rc))]
			 [first-number (lambda (x y) (if (number? x) x y))]
			 [left (first-number (rectangle-left rectangle) view-x)]
			 [top (rectangle-top rectangle)]
			 [right (if (number? (rectangle-right rectangle))
				    (rectangle-right rectangle)
				    (+ view-x view-width))]
			 [bottom (rectangle-bottom rectangle)])
		    (let/ec k
		      (cond
			[(and (not before)
			      color
			      (not (eq? wx:platform 'unix))
			      (<= 8 (wx:display-depth)))
			 (send pen set-style wx:const-solid)
			 (send brush set-style wx:const-solid)
			 (send pen set-colour color)
			 (send brush set-colour color)
			 (send dc set-logical-function wx:const-and)]
			[(and before
			      color
			      (<= 8 (wx:display-depth)))
			 (send* pen (set-style wx:const-solid)
			   (set-colour color))
			 (send* brush (set-style wx:const-solid)
			   (set-colour color))
			 (send dc set-logical-function wx:const-copy)]
			[(and (not before)
			      (< (wx:display-depth) 8)
			      b/w-bitmap
			      (eq? wx:platform 'unix))
			 (send pen set-stipple b/w-bitmap)
			 (send brush set-stipple b/w-bitmap)]
			[else (k (void))])
		      (send dc set-pen pen)
		      (send dc set-brush brush)
		      (send dc draw-rectangle 
			    (+ left dx)
			    (+ top dy)
			    (- right left)
			    (- bottom top))
		      (send dc set-logical-function old-logical-function)
		      (send dc set-pen old-pen)
		      (send dc set-brush old-brush)))))
	      range-rectangles))])
	
	(public
	  [on-kill-focus
	   (lambda ()
	     (super-on-kill-focus)
	     (let ([frame (get-frame)])
	       (when (and frame
			  (is-a? frame mred:frame:empty-frame%))
		 (send (get-keymap)
		       remove-chained-keymap
		       (ivar frame keymap)))))]
	  [on-set-focus
	   (lambda ()
	     (super-on-set-focus)
	     (let ([frame (get-frame)])
	       (when (and frame
			  (is-a? frame mred:frame:empty-frame%))
		 (send (get-keymap)
		       chain-to-keymap
		       (ivar frame keymap)
		       #t))))])
	
	(public
	  [set-mode
	   (lambda (m)
	     (if mode
		 (send mode deinstall this))
	     (if (is-a? m mred:mode:mode%)
		 (begin
		   (set-mode-direct m)
		   (set-file-format (ivar m file-format))
		   (send m install this)
		   (let ([new-delta (ivar m standard-style-delta)])
		     (when new-delta
		       (send (send (get-style-list) 
				   find-named-style "Standard")
			     set-delta new-delta))))
		 (begin
		   (set-mode-direct #f)
		   (send (send (get-style-list) 
			       find-named-style "Standard")
			 set-delta (make-object wx:style-delta%)))))]
	  [styles-fixed? #f]
	  [set-styles-fixed (lambda (b) (set! styles-fixed? b))])
	
	(public
	  [on-focus
	   (lambda (on?)
	     (super-on-focus on?)
	     (when mode
	       (send mode on-focus this on?)))]
	  [on-local-event
	   (lambda (mouse)
	     (if (or (not mode)
		     (not (send mode on-event this mouse)))
		 (super-on-local-event mouse)))]
	  [on-insert
	   (lambda (start len)
	     (if (or (not mode) (send mode on-insert this start len))
		 (super-on-insert start len)))]
	  [on-delete
	   (lambda (start len)
	     (if (or (not mode) (send mode on-delete this start len))
		 (super-on-delete start len)))]
	  [on-change-style
	   (lambda (start len)
	     (when styles-fixed?
	       (set! styles-fixed-edit-modified? (modified?)))
	     (and (or (not mode) 
		      (send mode on-change-style this start len))
		  (super-on-change-style start len)))]
	  [on-edit-sequence
	   (lambda ()
	     (when mode
	       (send mode on-edit-sequence this))
	     (super-on-edit-sequence))]
	  [on-set-size-constraint
	   (lambda ()
	     (and (or (not mode) (send mode on-set-size-constraint this))
		  (super-on-set-size-constraint)))]
	  
	  [after-insert
	   (lambda (start len)
	     (when styles-fixed?
	       (change-style (send (get-style-list) find-named-style "Standard")
			     start
			     (+ start len)))
	     (when mode (send mode after-insert this start len))
	     (super-after-insert start len))]
	  [after-delete
	   (lambda (start len)
	     (if mode (send mode after-delete this start len))
	     (super-after-delete start len))]
	  [after-change-style
	   (lambda (start len)
	     (when mode (send mode after-change-style this start len))
	     (super-after-change-style start len)
	     (when styles-fixed?
	       (set-modified styles-fixed-edit-modified?)))]
	  [after-edit-sequence
	   (lambda ()
	     (when mode
	       (send mode after-edit-sequence this))
	     (super-after-edit-sequence))]
	  [after-set-size-constraint
	   (lambda ()
	     (when mode
	       (send mode after-set-size-constraint this))
	     (super-after-set-size-constraint))]
	  [after-set-position
	   (lambda ()
	     (when mode 
	       (send mode after-set-position this))
	     (super-after-set-position))])
	
	(public
	  [set-max-width
	   (lambda (x)
	     (mred:debug:printf 'rewrap "rewrap: set-max-width: ~a" x)
	     (super-set-max-width x))]
	  [auto-set-wrap? (mred:preferences:get-preference 'mred:auto-set-wrap?)]
	  [set-auto-set-wrap
	   (lambda (v)
	     (mred:debug:printf 'rewrap 
				"rewrap: set-auto-set-wrap: ~a (canvases ~a)"
				v canvases)
	     (set! auto-set-wrap? v)
	     (for-each (lambda (c) (send c resize-edit)) canvases))]
	  
	  [rewrap
	   (lambda ()
	     (if auto-set-wrap?
		 (let* ([current-width (get-max-width)]
			[w-box (box 0)]
			[new-width
			 (mzlib:function:foldl
			  (lambda (canvas sofar)
			    (send canvas call-as-primary-owner
				  (lambda ()
				    (send (get-admin)
					  get-view null null
					  w-box (box 0))))
			    (max (unbox w-box) sofar))
			  0
			  canvases)])
		   (mred:debug:printf 'rewrap "rewrap: new-width ~a  current-width ~a" 
				      new-width current-width)
		   (when (and (not (= current-width new-width))
			      (< 0 new-width))
		     (set-max-width new-width)
		     (mred:debug:printf 'rewrap "rewrap: attempted to wrap to: ~a actually wrapped to ~a" 
					new-width (get-max-width))))
		 (begin
		   (mred:debug:printf 'rewrap "rewrap: wrapping to -1")
		   (set-max-width -1))))])
	
	(public
	  [move/copy-to-edit
	   (lambda (dest-edit start end dest-position)
	     (let ([insert-edit (ivar dest-edit insert)])
	       (split-snip start)
	       (split-snip end)
	       (let loop ([snip (find-snip end wx:const-snip-before)])
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
		      '(wx:message-box (format "before: ~a" (eq? snip released/copied)))
		      (insert-edit released/copied dest-position dest-position)
		      '(wx:message-box (format "after: ~a" (eq? snip released/copied)))
		      (loop prev))]))))])
	
	(public
	  [load-file
	   (opt-lambda ([filename null] [format wx:const-media-ff-guess])
	     (let ([filename (if (null? filename)
				 (mred:finder:get-file)
				 filename)])
	       (and filename
		    (if (file-exists? filename)
			(super-load-file filename format)
			(set-filename filename)))))])
	(public
	  [autowrap-bitmap null])
	(sequence
	  (apply super-init args)
	  (set-autowrap-bitmap autowrap-bitmap)
	  (let ([keymap (get-keymap)])
	    (mred:keymap:set-keymap-error-handler keymap)
	    (mred:keymap:set-keymap-implied-shifts keymap)
	    (send keymap chain-to-keymap mred:keymap:global-keymap #f))))))
  
  (define make-searching-edit%
    (lambda (super%)
      (class super% args
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
			   (if (is-a? admin wx:media-snip-media-admin%)
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
							 wx:const-snip-after-or-null 
							 wx:const-snip-before-or-null))])
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
		       [(is-a? current-snip wx:media-snip%)
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
	    (mred:keymap:set-keymap-error-handler keymap)
	    (mred:keymap:set-keymap-implied-shifts keymap)
	    (send keymap chain-to-keymap mred:keymap:global-search-keymap #f))))))
  
  (define make-file-buffer%
    (lambda (super%)
      (class super% args
	(inherit get-keymap find-snip  
		 get-filename lock get-style-list 
		 modified? change-style set-modified 
		 get-frame)
	(rename [super-after-save-file after-save-file]
		[super-after-load-file after-load-file])
	
	(public [editing-this-file? #t])
	(private
	  [check-lock
	   (lambda ()
	     (let* ([filename (get-filename)]
		    [lock? (and (not (null? filename))
				(file-exists? filename)
				(not (member
				      'write
				      (file-or-directory-permissions
				       filename))))])
	       (mred:debug:printf 'permissions 
				  "locking: ~a (filename: ~a)"
				  lock?
				  filename)
	       (lock lock?)))])
	(public
	  [after-save-file
	   (lambda (success)
	     (when success
	       (check-lock))
	     (super-after-save-file success))]
	  
	  [after-load-file
	   (lambda (sucessful?)
	     (when sucessful?
	       (check-lock))
	     (super-after-load-file sucessful?))]
	  [autowrap-bitmap (mred:icon:get-autowrap-bitmap)])
	(sequence
	  (apply super-init args)
	  (let ([keymap (get-keymap)])
	    (mred:keymap:set-keymap-error-handler keymap)
	    (mred:keymap:set-keymap-implied-shifts keymap)
	    (send keymap chain-to-keymap mred:keymap:global-file-keymap #f))))))
  
  (define make-clever-file-format-edit%
    (lambda (super%)
      (class-asi super%
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
		    (let loop ([s (find-snip 0 wx:const-snip-after)])
		      (cond
			[(null? s) #f]
			[(is-a? s wx:text-snip%)
			 (loop (send s next))]
			[else #t])))])
	     (lambda (name format)
	       (when (and (or (= format wx:const-media-ff-same)
			      (= format wx:const-media-ff-copy))
			  (not (= (get-file-format) 
				  wx:const-media-ff-std)))
		 (cond
		   [(= format wx:const-media-ff-copy)
		    (set! restore-file-format 
			  (let ([f (get-file-format)])
			    (lambda ()
			      (set! restore-file-format void)
			      (set-file-format f))))
		    (set-file-format wx:const-media-ff-std)]
		   [(and (has-non-text-snips)
			 (or (not (mred:preferences:get-preference 'mred:verify-change-format))
			     (mred:gui-utils:get-choice "Save this file as plain text?" "No" "Yes")))
		    (set-file-format wx:const-media-ff-std)]
		   [else (void)]))
	       (or (super-on-save-file name format)
		   (begin 
		     (restore-file-format)
		     #f))))]))))
  
  (define make-backup-autosave-buffer%
    (lambda (super-edit%)
      (class super-edit% args
	(inherit modified? get-filename save-file)
	(rename [super-on-save-file on-save-file]
		[super-on-change on-change]
		[super-do-close do-close]
		[super-set-modified set-modified])
	(private
	  [auto-saved-name #f]
	  [auto-save-out-of-date? #t]
	  [auto-save-error? #f])
	(public
	  [backup? #t]
	  [on-save-file
	   (lambda (name format)
	     (set! auto-save-error? #f)
	     (and (super-on-save-file name format)
		  (begin
		    (when (and backup?
			       (not (= format wx:const-media-ff-copy))
			       (file-exists? name))
		      (let ([back-name (mred:path-utils:generate-backup-name name)])
			(copy-file name back-name)))
		    #t)))]
	  [do-close
	   (lambda ()
	     (super-do-close)
	     (remove-autosave)
	     (set! auto-save? #f))]
	  [on-change
	   (lambda ()
	     (super-on-change)
	     (set! auto-save-out-of-date? #t))]
	  [auto-save? #t]
	  [set-modified
	   (lambda (modified?)
	     (when auto-saved-name
	       (if (not modified?)
		   (begin
		     (delete-file auto-saved-name)
		     (set! auto-saved-name #f))
		   (set! auto-save-out-of-date? #t)))
	     (super-set-modified modified?))]
	  [do-autosave
	   (lambda ()
	     (when (and auto-save?
			(not auto-save-error?)
			(modified?)
			(or (not auto-saved-name)
			    auto-save-out-of-date?))
	       (let* ([orig-name (get-filename)]
		      [auto-name (mred:path-utils:generate-autosave-name orig-name)]
		      [success (save-file auto-name wx:const-media-ff-copy)])
		 (if success
		     (begin
		       (when auto-saved-name
			 (delete-file auto-saved-name))
		       (set! auto-saved-name auto-name)
		       (set! auto-save-out-of-date? #f))
		     (begin
		       (wx:message-box 
			(format "Error autosaving ~s.~n~a~n~a"
				(if (null? orig-name) "Untitled" orig-name)
				"Autosaving is turned off"
				"until the file is saved.")
			"Warning")
		       (set! auto-save-error? #t))))))]
	  [remove-autosave
	   (lambda ()
	     (when auto-saved-name
	       (delete-file auto-saved-name)
	       (set! auto-saved-name #f)))])
	(sequence
	  (apply super-init args)
	  (mred:autosave:register-autosave this)))))
  
  (define make-return-edit%
    (lambda (super%)
      (class super% (return . args)
	(rename [super-on-local-char on-local-char])
	(public
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
	  (apply super-init args)))))
  
  (define make-info-edit%
    (lambda (super-info-edit%)
      (class-asi super-info-edit%
	(inherit get-frame get-start-position get-end-position
		 position-line line-start-position)
	(rename [super-after-set-position after-set-position]
		[super-after-edit-sequence after-edit-sequence]
		[super-on-edit-sequence on-edit-sequence]
		[super-after-insert after-insert]
		[super-after-delete after-delete]
		[super-lock lock]
		[super-set-overwrite-mode set-overwrite-mode]
		[super-set-anchor set-anchor])
	(private
	  [edit-sequence-depth 0]
	  [position-needs-updating #f]
	  [lock-needs-updating #f]
	  [anchor-needs-updating #f]
	  [overwrite-needs-updating #f]
	  [maybe-update-anchor
	   (lambda ()
	     (if (= edit-sequence-depth 0)
		 (let ([frame (get-frame)])
		   (when frame
		     (send frame anchor-status-changed)))
		 (set! anchor-needs-updating #t)))]
	  [maybe-update-overwrite
	   (lambda ()
	     (if (= edit-sequence-depth 0)
		 (let ([frame (get-frame)])
		   (when frame
		     (send frame overwrite-status-changed)))
		 (set! overwrite-needs-updating #t)))]
	  [maybe-update-lock-icon
	   (lambda ()
	     (if (= edit-sequence-depth 0)
		 (let ([frame (get-frame)])
		   (when frame
		     (send frame lock-status-changed)))
		 (set! lock-needs-updating #t)))]
	  [maybe-update-position-edit
	   (lambda ()
	     (if (= edit-sequence-depth 0)
		 (update-position-edit)
		 (set! position-needs-updating #t)))]
	  [update-position-edit
	   (lambda ()
	     (let ([frame (get-frame)])
	       (when frame
		 (send frame edit-position-changed))))])
	
	(public
	  [set-anchor
	   (lambda (x)
	     (super-set-anchor x)
	     (maybe-update-anchor))]
	  [set-overwrite-mode
	   (lambda (x)
	     (super-set-overwrite-mode x)
	     (maybe-update-overwrite))]
	  [lock
	   (lambda (x)
	     (super-lock x)
	     (maybe-update-lock-icon))]
	  [after-set-position
	   (lambda ()
	     (maybe-update-position-edit)
	     (super-after-set-position))]
	  [after-insert
	   (lambda (start len)
	     (maybe-update-position-edit)
	     (super-after-insert start len))]
	  [after-delete
	   (lambda (start len)
	     (maybe-update-position-edit)
	     (super-after-delete start len))]
	  [after-edit-sequence
	   (lambda ()
	     (super-after-edit-sequence)
	     (set! edit-sequence-depth (sub1 edit-sequence-depth))
	     (when (= 0 edit-sequence-depth)
	       (let ([frame (get-frame)])
		 (when anchor-needs-updating
		   (set! anchor-needs-updating #f)
		   (send frame overwrite-status-changed))
		 (when position-needs-updating
		   (set! position-needs-updating #f)
		   (update-position-edit))
		 (when lock-needs-updating
		   (set! lock-needs-updating #f)
		   (send frame lock-status-changed)))))]
	  [on-edit-sequence
	   (lambda ()
	     (set! edit-sequence-depth (add1 edit-sequence-depth))
	     (super-on-edit-sequence))]))))
  
  
  (define media-edit% (make-media-edit%
		       (make-std-buffer% 
			mred:connections:connections-media-edit%)))
  (define searching-edit% (make-searching-edit% media-edit%))
  
  (define info-edit% (make-info-edit% searching-edit%))
  (define clever-file-format-edit% (make-clever-file-format-edit% info-edit%))
  (define file-edit% (make-file-buffer% clever-file-format-edit%))
  (define backup-autosave-edit% (make-backup-autosave-buffer% file-edit%))
  
  (define edit% file-edit%)
  
  (define return-edit% (make-return-edit% media-edit%))
  
  (define pasteboard% (make-pasteboard%
		       mred:connections:connections-media-pasteboard%))
  (define file-pasteboard% (make-file-buffer% pasteboard%))
  (define backup-autosave-pasteboard% (make-backup-autosave-buffer% 
				       file-pasteboard%)))
