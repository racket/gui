
  (unit/sig mred:edit^
    (import [wx : mred:wx^]
	    [mred:constants : mred:constants^]
	    [mred:connections : mred:connections^]
	    [mred:autosave : mred:autosave^]
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

    (mred:preferences:set-preference-default 'mred:auto-set-wrap? #f)

    (define make-snip%
      (let ([sl (make-object wx:style-list%)])
	(send sl new-named-style "Standard" (send sl find-named-style "Basic"))
	(let ([std (send sl find-named-style "Standard")])
	  (lambda (snip%)
	    (class snip% args
	      (inherit set-style)
	      (sequence
		(apply super-init args)
		(set-style std)))))))

    (define media-snip% (make-snip% wx:media-snip%))
    (define snip% (make-snip% wx:snip%))

    (define make-std-buffer%
      (lambda (buffer%)
	(class buffer% args
	  (sequence (mred:debug:printf 'creation "creating a buffer"))
	  (inherit modified? get-filename save-file canvases
		   get-max-width get-admin)
	  (rename
	    [super-set-modified set-modified]
	    [super-on-change on-change]
	    [super-on-save-file on-save-file]
	    [super-on-focus on-focus]
	    [super-set-max-width set-max-width]
	    [super-lock lock])

	  (public
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

	  (private
	    [auto-saved-name #f]
	    [auto-save-out-of-date? #t]
	    [auto-save-error? #f])
	  (public
	    [set-max-width
	     (lambda (x)
	       (mred:debug:printf 'rewrap "set-max-width: ~a" x)
	       (super-set-max-width x))]
	    [get-file (lambda (d) 
			(let ([v (mred:finder:get-file d)])
			  (if v
			      v
			      '())))]
	    [put-file (lambda (d f) (let ([v (mred:finder:put-file f d)])
				      (if v
					  v
					  '())))]
	    
	    [auto-set-wrap? (mred:preferences:get-preference 'mred:auto-set-wrap?)]
	    [set-auto-set-wrap
	     (lambda (v)
	       (mred:debug:printf 'rewrap "set-auto-set-wrap: ~a" v)
	       (set! auto-set-wrap? v)
	       (for-each (lambda (c) (send c resize-edit)) canvases))]
	    
	    [rewrap
	     (let ([do-wrap
		    (lambda (new-width)
		      (let ([current-width (get-max-width)])
			(mred:debug:printf 'rewrap "do-wrap: new-width ~a  current-width ~a" new-width current-width)
			(when (and (not (= current-width new-width))
				   (< 0 new-width))
			  (set-max-width new-width)
			  (mred:debug:printf 'rewrap "attempted to wrap to: ~a actually wrapped to ~a" 
					     new-width (get-max-width)))))])
	       (lambda ()
		 (if auto-set-wrap?
		     (let* ([w-box (box 0)]
			    [h-box (box 0)]
			    [update-box
			     (lambda ()
			       (send (get-admin)
				     get-view null null
				     w-box h-box))])
		       (do-wrap
			(mzlib:function:foldl
			 (lambda (canvas sofar)
			   (begin
			     (send canvas call-as-primary-owner update-box)
			     (max (unbox w-box) sofar)))
			 0
			 canvases)))
		     (do-wrap 0))))]
	    [mode #f]
	    [set-mode-direct (lambda (v) (set! mode v))]
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
	       (super-set-modified modified?))]
	    
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
		      #t)))])
	  (sequence
	    (apply super-init args)
	    (mred:autosave:register-autosave this)))))

    (define make-edit%
      (lambda (super%)
	(class (make-std-buffer% super%) args
	  (inherit mode set-mode-direct canvases get-file-format 
		   set-filename find-string get-snip-position
		   change-style save-file get-admin
		   invalidate-bitmap-cache split-snip
		   begin-edit-sequence end-edit-sequence
		   flash-on get-keymap get-start-position
		   get-end-position last-position
		   on-default-char on-default-event 
		   set-file-format get-style-list
		   set-autowrap-bitmap delete
		   get-snip-location find-snip get-max-width
		   modified? set-modified
		   lock get-filename)
	  
	  (rename [super-on-focus on-focus]
		  [super-on-paint on-paint]
		  [super-on-local-event on-local-event]
		  [super-on-local-char on-local-char]

		  [super-on-save-file on-save-file]
		  [super-after-save-file after-save-file]
		  
		  [super-after-set-position after-set-position]
		  
		  [super-on-edit-sequence on-edit-sequence]
		  [super-on-change-style on-change-style]
		  [super-on-insert on-insert]
		  [super-on-delete on-delete]
		  [super-on-set-size-constraint on-set-size-constraint]
		  
		  [super-after-load-file after-load-file]
		  [super-load-file load-file]

		  [super-after-edit-sequence after-edit-sequence]
		  [super-after-change-style after-change-style]
		  [super-after-insert after-insert]
		  [super-after-delete after-delete]
		  [super-after-set-size-constraint after-set-size-constraint])

	  (private
	    [styles-fixed-edit-modified? #f]
	    [restore-file-format void])	  

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
		       #f))))]
	    [after-save-file
	     (lambda (success)
	       (super-after-save-file success)
	       (restore-file-format))]

	    [autowrap-bitmap mred:icon:autowrap-bitmap]
	    [load-file
	     (opt-lambda ([filename null] [format wx:const-media-ff-guess])
	       (if (file-exists? filename)
		   (super-load-file filename format)
		   (set-filename filename)))]
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
		     (set-mode-direct m)
		     (set-file-format (ivar m file-format))
		     (send m install this)
		     (send (send (get-style-list) 
				 find-named-style "Standard")
			   set-delta (ivar m standard-style-delta)))
		   (begin
		     (set-mode-direct #f)
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

	  (private
	    [range-rectangles null]
	    [recompute-range-rectangles
	     (lambda ()
	       (let ([new-rectangles
		      (lambda (range)
			(let* ([start (range-start range)]
			       [end (range-end range)]
			       [b/w-bitmap (range-b/w-bitmap range)]
			       [color (range-color range)]
			       [buffer-width (box 0)]
			       [start-x (box 0)]
			       [top-start-y (box 0)]
			       [bottom-start-y (box 0)]
			       [end-x (box 0)]
			       [top-end-y (box 0)]
			       [bottom-end-y (box 0)]
			       [start-eol? #f]
			       [end-eol? (if (= start end)
					     start-eol?
					     #t)])
			  (send this get-extent buffer-width null)
			  (send this position-location start start-x top-start-y
				#t start-eol? #t)
			  (send this position-location end end-x top-end-y
				#t end-eol? #t)
			  (send this position-location start start-x bottom-start-y
				#f start-eol? #t)
			  (send this position-location end end-x bottom-end-y
				#f end-eol? #t)
			  (cond
			    [(= (unbox top-start-y) (unbox top-end-y))
			     (list (make-rectangle (unbox start-x)
						   (unbox top-start-y)
						   (max 1 (- (unbox end-x) (unbox start-x)))
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
		  (let ([pen (make-object wx:pen% "black" 1 wx:const-stipple)]
			[brush (make-object wx:brush% "black" wx:const-stipple)]
			[old-pen (send dc get-pen)]
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
			[left (rectangle-left rectangle)]
			[top (rectangle-top rectangle)]
			[width (rectangle-width rectangle)]
			[height (rectangle-height rectangle)])
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
		      (send dc draw-rectangle (+ left dx) (+ top dy)
			    width height)
		      (send dc set-logical-function old-logical-function)
		      (send dc set-pen old-pen)
		      (send dc set-brush old-brush))))
		range-rectangles))])

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
	    (set-autowrap-bitmap autowrap-bitmap)
	    (let ([keymap (get-keymap)])
	      (mred:keymap:set-keymap-error-handler keymap)
	      (mred:keymap:set-keymap-implied-shifts keymap)
	      (send keymap chain-to-keymap mred:keymap:global-keymap #f))))))

    (define edit% (make-edit% mred:connections:connections-media-edit%))

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

    (define return-edit% (make-return-edit% edit%))
		  
    (define make-pasteboard% make-std-buffer%)
    (define pasteboard% (make-pasteboard% mred:connections:connections-media-pasteboard%)))
