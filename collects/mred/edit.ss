(define mred:edit@
  (unit/sig mred:edit^
    (import [mred:debug : mred:debug^]
	    [mred:finder : mred:finder^]
	    [mred:path-utils : mred:path-utils^]
	    [mred:mode : mred:mode^]
	    [mred:scheme-paren : mred:scheme-paren^]
	    [mred:keymap : mred:keymap^]
	    [mred:icon : mred:icon^]
	    [mred:preferences : mred:preferences^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mzlib:function : mzlib:function^])
	    
    (mred:debug:printf 'invoke "mred:edit@")

    (mred:preferences:set-preference-default 'mred:verify-change-format #f)

    (define-struct range (start end b/w-bitmap color))
    (define-struct rectangle (left top width height b/w-bitmap color))

    (define make-std-buffer%
      (lambda (buffer%)
	(class buffer% args
	  (inherit modified? get-filename save-file set-max-width)
	  (rename
	    [super-set-filename set-filename]
	    [super-set-modified set-modified]
	    [super-on-change on-change]
	    [super-on-save-file on-save-file]
	    [super-on-focus on-focus]
	    [super-lock lock])
	  (private
	    [auto-saved-name #f]
	    [auto-save-out-of-date? #t]
	    [auto-save-error? #f])
	  (public
	    [get-file (lambda (d) 
			(let ([v (mred:finder:get-file d)])
			  (if v
			      v
			      '())))]
	    [put-file (lambda (d f) (let ([v (mred:finder:put-file f d)])
				      (if v
					  v
					  '())))]
	    
	    [auto-set-wrap? #f]
	    [set-auto-set-wrap
	     (lambda (v)
	       (set! auto-set-wrap? v)
	       (if (not v)
		   (set-max-width -1)))]
	    
	    [active-canvas #f]
	    [set-active-canvas
	     (lambda (c)
	       (set! active-canvas c))]
	    
	    [canvases '()]
	    [add-canvas
	     (lambda (canvas)
	       (set! canvases (cons canvas canvases)))]
	    [remove-canvas
	     (lambda (canvas)
	       (set! canvases (mzlib:function:remove canvas canvases)))]
	    
	    [mode #f]
	    [set-mode
	     (lambda (m)
	       #f)]
	    
	    [set-modified
	     (lambda (modified?)
	       (if auto-saved-name
		   (if (not modified?)
		       (begin
			 (delete-file auto-saved-name)
			 (set! auto-saved-name #f))
		       (set! auto-save-out-of-date? #t)))
	       (super-set-modified modified?)
	       (for-each (lambda (canvas) (send canvas edit-modified modified?))
			 canvases))]
	    [set-filename
	     (opt-lambda (name [temp? #f])
	       (super-set-filename name temp?)
	       (for-each (lambda (canvas) (send canvas edit-renamed name))
			 canvases))]
	    
	    [on-change
	     (lambda ()
	       (super-on-change)
	       (set! auto-save-out-of-date? #t))]
	    [auto-save? #t]
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
			 (if auto-saved-name
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
		 (set! auto-saved-name #f)))]
	    
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
			  (unless (file-exists? back-name)
			    (rename-file name back-name))))
		      #t)))]
	    
	    [get-canvas
	     (lambda ()
	       (cond
		 [(and active-canvas
		       (member active-canvas canvases))
		  active-canvas]
		 [(null? canvases) #f]
		 [else (car canvases)]))]
	    [get-frame
	     (lambda ()
	       (let ([c (get-canvas)])
		 (if c
		     (let ([f (ivar c frame)])
		       (if (null? f)
			   #f
			   f))
		     #f)))])
	  (sequence
	    (apply super-init args)))))

    (define make-edit%
      (lambda (super%)
	(class (make-std-buffer% super%) args
	  (inherit mode canvases get-file-format
		   change-style save-file
		   invalidate-bitmap-cache
		   begin-edit-sequence end-edit-sequence
		   flash-on get-keymap get-start-position
		   on-default-char on-default-event 
		   set-file-format get-style-list
		   set-autowrap-bitmap
		   get-snip-location find-snip get-max-width
		   modified? set-modified
		   lock get-filename)
	  
	  (rename [super-on-focus on-focus]
		  [super-on-paint on-paint]
		  [super-on-local-event on-local-event]
		  [super-on-local-char on-local-char]

		  [super-on-save-file on-save-file]
		  
		  [super-after-set-position after-set-position]
		  
		  [super-on-edit-sequence on-edit-sequence]
		  [super-on-change-style on-change-style]
		  [super-on-insert on-insert]
		  [super-on-delete on-delete]
		  [super-on-set-size-constraint on-set-size-constraint]
		  
		  [super-after-load-file after-load-file]

		  [super-after-edit-sequence after-edit-sequence]
		  [super-after-change-style after-change-style]
		  [super-after-insert after-insert]
		  [super-after-delete after-delete]
		  [super-after-set-size-constraint after-set-size-constraint])
	  (private
	    [styles-fixed-edit-modified? #f])
	  (public
	    [on-save-file
	     (let ([skip-check #f]
		   [has-non-text-snips 
		    (lambda ()
		      (let loop ([s (find-snip 0 wx:const-snip-after)])
			(cond
			  [(null? s) #f]
			  [(is-a? s wx:text-snip%)
			   (loop (send s next))]
			  [else #t])))])
	       (lambda (name format)
		 (unless (or skip-check
			     (= format wx:const-media-ff-std)
			     (and (or (= format wx:const-media-ff-same)
				      (= format wx:const-media-ff-guess))
				  (= (get-file-format) 
				     wx:const-media-ff-std)))
		   (dynamic-wind
		    (lambda () (set! skip-check #t))
		    (lambda ()
		      (when (and (has-non-text-snips)
				 (or (not (mred:preferences:get-preference 'mred:verify-change-format))
				     (mred:gui-utils:get-choice "Save this file as plain text?" "No" "Yes")))
			(save-file name wx:const-media-ff-std)))
		    (lambda () (set! skip-check #f))))
		 (super-on-save-file name format)))]
	 


	    [autowrap-bitmap mred:icon:autowrap-bitmap]
	    [after-load-file
	     (lambda (sucessful?)
	       (when sucessful?
		 (lock (not (member 'write (file-or-directory-permissions (get-filename))))))
	       (super-after-load-file sucessful?))]

	    [set-mode
	     (lambda (m)
	       (if mode
		   (send mode deinstall this))
	       (if (is-a? m mred:mode:mode%)
		   (begin
		     (set! mode m)
		     (set-file-format (ivar m file-format))
		     (send (send (get-style-list) 
				 find-named-style "Standard")
			   set-delta (ivar m standard-style-delta))
		     (send m install this))
		   (begin
		     (set! mode #f)
		     (send (send (get-style-list) 
				 find-named-style "Standard")
			   set-delta (make-object wx:style-delta%)))))]
	    [styles-fixed? #f]
	    [set-styles-fixed (lambda (b) (set! styles-fixed? b))]

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
	       (if (or (not mode) (send mode on-change-style this start len))
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
	       (when mode (send mode after-insert this start len))
	       (super-after-insert start len)
	       (when styles-fixed?
		 (change-style (send (get-style-list) find-named-style "Standard")
			       start
			       (+ start len))))]
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
	    
	  (private
	    [range-rectangles null]
	    [recompute-range-rectangles
	     (lambda ()
	       (let ([new-rectangles
		      (lambda (range)
			(let ([start (range-start range)]
			      [end (range-end range)]
			      [b/w-bitmap (range-b/w-bitmap range)]
			      [color (range-color range)]
			      [buffer-width (box 0)]
			      [start-x (box 0)]
			      [top-start-y (box 0)]
			      [bottom-start-y (box 0)]
			      [end-x (box 0)]
			      [top-end-y (box 0)]
			      [bottom-end-y (box 0)])
			  (send this get-extent buffer-width null)
			  (send this position-location start start-x top-start-y    #t #f #t)
			  (send this position-location end end-x top-end-y          #t #t #t)
			  (send this position-location start start-x bottom-start-y #f #f #t)
			  (send this position-location end end-x bottom-end-y       #f #t #t)
			  (cond
			    [(= (unbox top-start-y) (unbox top-end-y))
			     (list (make-rectangle (unbox start-x)
						   (unbox top-start-y)
						   (- (unbox end-x) (unbox start-x))
						   (- (unbox bottom-start-y) (unbox top-start-y))
						   b/w-bitmap color))]
			    [else
			     (list
			      (make-rectangle (unbox start-x)
					      (unbox top-start-y)
					      (- (unbox buffer-width) (unbox start-x))
					      (- (unbox bottom-start-y) (unbox top-start-y))
					      b/w-bitmap color)
			      (make-rectangle 0
					      (unbox bottom-start-y)
					      (unbox buffer-width)
					      (- (unbox top-end-y) (unbox bottom-start-y))
					      b/w-bitmap color)
			      (make-rectangle 0
					      (unbox top-end-y)
					      (unbox end-x)
					      (- (unbox bottom-end-y) (unbox top-end-y))
					      b/w-bitmap color))])))]
		     [invalidate-rectangle 
		      (lambda (r)
			(invalidate-bitmap-cache (rectangle-left r)
						 (rectangle-top r)
						 (rectangle-width r)
						 (rectangle-height r)))]
		     [old-rectangles range-rectangles])
		 
		 (set! range-rectangles 
		       (mzlib:function:foldl (lambda (x l) (append (new-rectangles x) l))
					     null ranges))
		 (begin-edit-sequence)
		 (for-each invalidate-rectangle old-rectangles)
		 (for-each invalidate-rectangle range-rectangles)
		 (end-edit-sequence)))]
	    [ranges null])

	  (public
	    ;; the bitmap is used in b/w and the color is used in color.
	    [highlight-range
	     (opt-lambda (start end color [bitmap #f])
	       (let ([l (make-range start end bitmap color)])
		 (set! ranges (cons l ranges))
		 (recompute-range-rectangles)
		 (lambda () (set! ranges 
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
	       (unless before
		 (for-each (lambda (rectangle)
			     (let ([pen (make-object wx:pen% "black" 1 wx:const-stipple)]
				   [brush (make-object wx:brush% "black" wx:const-stipple)]
				   [old-pen (send dc get-pen)]
				   [old-brush (send dc get-brush)]
				   [old-logical-function (send dc get-logical-function)]
				   [b/w-bitmap (rectangle-b/w-bitmap rectangle)]
				   [color (rectangle-color rectangle)]
				   [left (rectangle-left rectangle)]
				   [top (rectangle-top rectangle)]
				   [width (rectangle-width rectangle)]
				   [height (rectangle-height rectangle)])
			       (cond
				 [(and color
				       (not (eq? wx:platform 'unix))
				       (<= 8 (wx:display-depth)))	
				  (send pen set-style wx:const-solid)
				  (send brush set-style wx:const-solid)
				  (send pen set-colour color)
				  (send brush set-colour color)
				  (send dc set-logical-function wx:const-and)]
				 [(and b/w-bitmap
				       (eq? wx:platform 'unix))
				  (send pen set-stipple b/w-bitmap)
				  (send brush set-stipple b/w-bitmap)]
				 [else (send dc set-logical-function wx:const-xor)
				       (send pen set-style wx:const-solid)
				       (send brush set-style wx:const-solid)])
			       (send dc set-pen pen)
			       (send dc set-brush brush)
			       (unless (or (zero? width)
					   (zero? height))
				 (send dc draw-rectangle (+ left dx) (+ top dy) width height))
			       (send dc set-logical-function old-logical-function)
			       (send dc set-pen old-pen)
			       (send dc set-brush old-brush)))
			   range-rectangles)))])
	  (sequence
	    (apply super-init args)
	    (set-autowrap-bitmap autowrap-bitmap)
	    (let ([keymap (get-keymap)])
	      (mred:keymap:set-keymap-error-handler keymap)
	      (mred:keymap:set-keymap-implied-shifts keymap)
	      (send keymap chain-to-keymap mred:keymap:global-keymap #f))))))

    (define edit% (make-edit% wx:media-edit%))

    (define make-pasteboard% make-std-buffer%)
    (define pasteboard% (make-pasteboard% wx:media-pasteboard%))))


