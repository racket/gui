(define-sigfunctor (mred:edit@ mred:edit^)
  (import mred:debug^ mred:finder^ mred:path-utils^ mred:mode^ mred:scheme-paren^ 
	  mred:keymap^ mzlib:function^)

  (define first car)
  (define second cadr)
  (define third caddr)
  (define fourth cadddr)

  (define make-std-buffer%
    (lambda (buffer%)
      (class buffer% args
	(inherit modified? get-filename save-file set-max-width)
	(rename
	 [super-set-filename set-filename]
	 [super-set-modified set-modified]
	 [super-on-change on-change]
	 [super-on-save-file on-save-file])
	(private
	 [auto-saved-name #f]
	 [auto-save-out-of-date? #t]
	 [auto-save-error? #f])
	(public
	 [get-file (lambda (d) (let ([v (mred:finder^:get-file d)])
				 (if v
				     v
				     '())))]
	 [put-file (lambda (d f) (let ([v (mred:finder^:put-file f d)])
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
	    (set! canvases (mzlib:function^:remove canvas canvases)))]
	 
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
		     [auto-name (mred:path-utils^:generate-autosave-name orig-name)]
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
	    (if (super-on-save-file name format)
		(begin
		  (if (and backup?
			   (not (= format wx:const-media-ff-copy)))
		      (if (file-exists? name)
			  (let ([back-name (mred:path-utils^:generate-backup-name name)])
			    (unless (file-exists? back-name)
				    (rename-file name back-name)))))
		  #t)
		#f))]

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
		  (let ([f (send c get-parent)])
		    (if (null? f)
			#f
			f))
		  #f)))])
	(sequence
	  (apply super-init args)))))

  (define edits%
    (class-asi wx:snip%
      (private
	[edits null])
      (public
	[add
	 (lambda (edit)
	   (unless (let loop ([e edits])
		     (cond
		       [(null? e) #f]
		       [else (if (eq? this (car e))
				 #t
				 (loop (cdr e)))]))
	    (set! edits (cons edit edits))))])))

  (define edits (make-object edits%))

  (define make-edit%
    (lambda (super%)
      (class (make-std-buffer% super%) args
        (inherit mode canvases
		 flash-on get-keymap get-start-position
		 on-default-char on-default-event 
		 set-file-format get-style-list)
	(rename [super-on-focus on-focus]
		[super-on-paint on-paint]
		[super-after-set-position after-set-position]
		[super-on-local-event on-local-event]
		[super-on-local-char on-local-char]
		[super-on-insert on-insert]
		[super-on-delete on-delete]
		[super-after-insert after-insert]
		[super-after-delete after-delete])
	(public
	 [set-mode
	  (lambda (m)
	    (if mode
		(send mode deinstall this))
	    (if (is-a? m mred:mode^:mode%)
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
	 
	 [after-insert
	  (lambda (start len)
	    (if mode (send mode after-insert this start len))
	    (super-after-insert start len))]
	 [after-delete
	  (lambda (start len)
	    (if mode (send mode after-delete this start len))
	    (super-after-delete start len))]
    
	 [after-set-position
	  (lambda ()
	    (when mode 
	      (send mode after-set-position this))
	    (super-after-set-position))]

	[ranges (list (list 4 24 '... '...))]
	[add-range 
	 (lambda (start end b/w-pattern color-pattern)
	   (let ([l (list start end b/w-pattern color-pattern)])
	     (set! ranges (cons l ranges))
	     (lambda () (set! ranges 
			      (let loop ([r ranges])
				(cond
				  [(null? r) r]
				  [else (if (eq? (car r) l)
					    (cdr r)
					    (cons (car r) (loop (cdr r))))]))))))]
	[range-rectangles null]
	[recompute-range-rectangles
	 (lambda ()
	   (let ([new-rectangles 
		  (lambda (start end b/w color)
		    (let ([left 0]
			  [right 1000]
			  [top-start-x (box 0)]
			  [top-start-y (box 0)]
			  [bottom-start-x (box 0)]
			  [bottom-start-y (box 0)]
			  [top-end-x (box 0)]
			  [top-end-y (box 0)]
			  [bottom-end-x (box 0)]
			  [bottom-end-y (box 0)])
		      (send this position-location start top-start-x top-start-y #t #f #t)
		      (send this position-location end top-end-x top-end-y #t #t #t)
		      (send this position-location start bottom-start-x bottom-start-y #f #f #t)
		      (send this position-location end bottom-end-x bottom-end-y #f #t #t)
		      (cond
			[(= (unbox top-start-y) (unbox top-end-y))
			 (list (list (unbox top-start-x) (unbox top-start-y)
				     (- (unbox bottom-end-x) (unbox top-start-x))
				     (- (unbox bottom-end-y) (unbox top-start-y))))]
			[else
			 (list (list (unbox top-start-x) (unbox top-start-y)
				     (- right (unbox top-start-x))
				     (- (unbox bottom-start-y) (unbox top-start-y)))
			       (list (unbox bottom-start-x) left
				     (- right left) (- (unbox bottom-start-x) (unbox top-end-x)))
			       (list left (unbox top-end-y)
				     (- (unbox top-end-x) left)
				     (- (unbox bottom-end-y) (unbox top-end-y))))])))])
	     (set! range-rectangles (map (lambda (x) (apply new-rectangles x)) ranges))
	     (printf "~a~n" range-rectangles)))]
	[on-paint
	 (lambda (before dc left top right bottom dx dy draw-caret)
	   (when #f
	     (for-each (lambda (rlist)
			 (for-each (lambda (rectangle)
				     (let ([pen (make-object wx:pen% "black" 1 1)]
					   [brush (make-object wx:brush% "black" wx:const-transparent)]
					   [old-pen (send dc get-pen)]
					   [old-brush (send dc get-brush)])
				       (send dc set-pen pen)
				       (send dc set-brush brush)
				       (send dc draw-rectangle
					     (+ (first rectangle) dx)
					     (+ (second rectangle) dy)
					     (+ (third rectangle) dx)
					     (+ (fourth rectangle) dy))
				       (send dc set-pen old-pen)
				       (send dc set-brush old-brush)))
				   rlist))
		       range-rectangles))
	   (super-on-paint before dc left top right bottom dx dy draw-caret))])
	(sequence
	  (apply super-init args)
	  (send edits add this)
	  (let ([keymap (get-keymap)])
	    (mred:keymap^:set-keymap-error-handler keymap)
	    (mred:keymap^:set-keymap-implied-shifts keymap)
	    (send keymap chain-to-keymap mred:keymap^:global-keymap #f))))))

  (define edit% (make-edit% wx:media-edit%))

  (define make-pasteboard% make-std-buffer%)
  (define pasteboard% (make-pasteboard% wx:media-pasteboard%)))


