(unit/sig framework:group^
  (import mred^
	  [application : framework:application^]
	  [frame : framework:frame^]
	  [preferences : framework:preferences^]
	  [mzlib:function : mzlib:function^]
	  [mzlib:file : mzlib:file^])

  (define-struct frame (frame id))
  
  (define mdi-parent #f)

  (define %
    (class object% ()
      (private
	[active-frame #f]
	[frame-counter 0]
	[frames null]
	[todo-to-new-frames void]
	[empty-close-down (lambda () (void))]
	[empty-test (lambda () #t)]
	
	[windows-menus null])
      
      (private
	[get-windows-menu
	 (lambda (frame)
	   (let ([menu-bar (send frame get-menu-bar)])
	     (and menu-bar
		  (let ([menus (send menu-bar get-items)])
		    (ormap (lambda (x)
			     (if (string=? "&Windows" (send x get-label))
				 x
				 #f))
			   menus)))))]
	[insert-windows-menu
	 (lambda (frame)
	   (let ([menu (get-windows-menu frame)])
	     (when menu
	       (set! windows-menus (cons menu windows-menus)))))]
	[remove-windows-menu
	 (lambda (frame)
	   (let* ([menu (get-windows-menu frame)])
	     (set! windows-menus
		   (mzlib:function:remove
		    menu
		    windows-menus
		    eq?))))]

	[update-windows-menus
	 (lambda ()
	   (let* ([windows (length windows-menus)]
		  [default-name "Untitled"]
		  [get-name 
		   (lambda (frame)
		     (let ([label (send frame get-label)])
		       (if (string=? label "")
			   (if (ivar-in-interface? 'get-entire-label (object-interface frame))
			       (let ([label (send frame get-entire-label)])
				 (if (string=? label "")
				     default-name
				     label))
			       default-name)
			   label)))]
		  [sorted-frames
		   (mzlib:function:quicksort
		    frames
		    (lambda (f1 f2)
		      (string-ci<=? (get-name (frame-frame f1))
				    (get-name (frame-frame f2)))))])
	      (for-each
	       (lambda (menu)
		 (for-each (lambda (item) (send item delete))
			   (send menu get-items))
		 (for-each
		  (lambda (frame)
		    (let ([frame (frame-frame frame)])
		      (make-object menu-item% (get-name frame)
				   menu
				   (lambda (_1 _2)
				     (send frame show #t)))))
		  sorted-frames))
	       windows-menus)))])
      
      (private
	[update-close-menu-item-state
	 (lambda ()
	   (let* ([set-close-menu-item-state! 
		   (lambda (frame state)
		     (when (is-a? frame frame:standard-menus<%>)
		       (let ([close-menu-item (ivar frame file-menu:close-menu)])
			 (when close-menu-item
			   (send close-menu-item enable state)))))])
	     (if (eq? (length frames) 1)
		 (set-close-menu-item-state! (car frames) #f)
		 (for-each (lambda (a-frame)
			     (set-close-menu-item-state! a-frame #t))
			   frames))))])
      (public

	[get-mdi-parent
	 (lambda ()
	   (if (and (eq? (system-type) 'windows)
		    (preferences:get 'framework:windows-mdi))
	       (begin
		 (set! get-mdi-parent (lambda () mdi-parent))
		 (set! mdi-parent (make-object frame% (application:current-app-name)
					       #f #f #f #f #f
					       '(mdi-parent)))
		 (send mdi-parent show #t)
		 mdi-parent)
	       (begin
		 (set! get-mdi-parent (lambda () #f))
		 #f)))]

	[set-empty-callbacks
	 (lambda (test close-down) 
	   (set! empty-test test)
	   (set! empty-close-down close-down))]
	[get-frames (lambda () (map frame-frame frames))]
	
	[frame-label-changed
	 (lambda (frame)
	   (when (member frame (map frame-frame frames))
	     (update-windows-menus)))]
	
	[for-each-frame
	 (lambda (f)
	   (for-each (lambda (x) (f (frame-frame x))) frames)
	   (set! todo-to-new-frames
		 (let ([old todo-to-new-frames])
		   (lambda (frame) (old frame) (f frame)))))]
	[get-active-frame
	 (lambda ()
	   (cond
	     [active-frame active-frame]
	     [(null? frames) #f]
	     [else (frame-frame (car frames))]))]
	[set-active-frame
	 (lambda (f)
	   (set! active-frame f))]
	[insert-frame
	 (lambda (f)
	   (set! frame-counter (add1 frame-counter))
	   (let ([new-frames (cons (make-frame f frame-counter)
				   frames)])
	     (set! frames new-frames)
	     (update-close-menu-item-state)
	     (insert-windows-menu f)
	     (update-windows-menus))
	   (todo-to-new-frames f))]
	
	[can-remove-frame?
	 (opt-lambda (f)
	   (let ([new-frames 
		  (mzlib:function:remove
		   f frames
		   (lambda (f fr) (eq? f (frame-frame fr))))])
	     (if (null? new-frames)
                 (empty-test)
		 #t)))]
	[remove-frame
	 (opt-lambda (f)
	   (when (eq? f active-frame)
	     (set! active-frame #f))
	   (let ([new-frames
		  (mzlib:function:remove
		   f frames
		   (lambda (f fr) (eq? f (frame-frame fr))))])
	     (set! frames new-frames)
	     (update-close-menu-item-state)
	     (remove-windows-menu f)
	     (update-windows-menus)
	     (when (null? frames)
	       (empty-close-down))))]
	[clear
	 (lambda ()
	   (and (empty-test)
		(begin (set! frames null)
		       (empty-close-down)
		       #t)))]
	[on-close-all
	 (lambda ()
	   (for-each (lambda (f)
		       (let ([frame (frame-frame f)])
			 (send frame on-close) 
			 (send frame show #f)))
		     frames))]
	[can-close-all?
	 (lambda ()
	   (andmap (lambda (f)
		     (let ([frame (frame-frame f)])
		       (send frame can-close?)))
		   frames))]
	[locate-file
	 (lambda (name)
	   (let* ([normalized
		   ;; allow for the possiblity of filenames that are urls
		   (with-handlers ([(lambda (x) #t)
				    (lambda (x) name)])
		     (mzlib:file:normalize-path name))]
		  [test-frame
		   (lambda (frame)
		     (and (is-a? frame frame:editor<%>)
			  (let* ([editor (send frame get-editor)]
				 [filename (send editor get-filename)])
			    (and (string? filename)
				 (string=? normalized
					   (with-handlers ([(lambda (x) #t)
							    (lambda (x) filename)])
					     (mzlib:file:normalize-path 
					      filename)))))))])
	     (let loop ([frames frames])
	       (cond
		 [(null? frames) #f]
		 [else
		  (let* ([frame (frame-frame (car frames))])
		    (if (test-frame frame)
			frame
			(loop (cdr frames))))]))))])
      (sequence
	(super-init))))
  
  (define the-frame-group #f)
  
  (define get-the-frame-group
    (lambda ()
      (set! the-frame-group (make-object %))
      (set! get-the-frame-group (lambda () the-frame-group))
      (get-the-frame-group))))