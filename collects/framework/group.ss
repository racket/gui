(dunit/sig framework:group^
  (import mred-interfaces^
	  [exit : framework:exit^]
	  [frame : framework:frame^]
	  [mzlib:function : mzlib:function^]
	  [mzlib:file : mzlib:file^])

  (define-struct frame (frame id))
  
  (define %
    (class null ()
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
	   (and (ivar-in-class? 'windows-menu (object-class frame))
		(ivar frame windows-menu)))]
	[insert-windows-menu
	 (lambda (frame)
	   (let ([menu (get-windows-menu frame)])
	     (when menu
	       (set! windows-menus (cons (list menu) windows-menus)))))]
	[remove-windows-menu
	 (lambda (frame)
	   (let* ([menu (get-windows-menu frame)])
	     (set! windows-menus
		   (mzlib:function:remove
		    menu
		    windows-menus
		    (lambda (x y)
		      (eq? x (car y)))))))]

	[update-windows-menus
	 (lambda ()
	   (let* ([windows (length windows-menus)]
		  [get-name (lambda (frame) (send (frame-frame frame) get-label))]
		  [sorted-frames
		   (mzlib:function:quicksort
		    frames
		    (lambda (f1 f2)
		      (string-ci<=? (get-name f1)
				    (get-name f2))))])
	     (set!
	      windows-menus
	      (map
	       (lambda (menu-list)
		 (let ([menu (car menu-list)]
		       [old-ids (cdr menu-list)])
		   (for-each (lambda (id) (send menu delete id))
			     old-ids)
		   (let ([new-ids
			  (map
			   (lambda (frame)
			     (let ([frame (frame-frame frame)]
				   [default-name "Untitled"])
			       (send menu append-item
				     (let ([label (send frame get-label)])
				       (if (string=? label "")
					   (if (ivar-in-class? 'get-entire-label (object-class frame))
					       (let ([label (send frame get-entire-label)])
						 (if (string=? label "")
						     default-name
						     label))
					       default-name)
					   label))
				     (lambda ()
				       (send frame show #t)))))
			   sorted-frames)])
		     (cons menu new-ids))))
	       windows-menus))))])
      
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
	[close-all
	 (lambda ()
	   (let/ec escape
	     (for-each (lambda (f)
			 (let ([frame (frame-frame f)])
			   (if (send frame on-close)
			       (send frame show #f)
			       (escape #f))))
		       frames)
	     #t))]
	[locate-file
	 (lambda (name)
	   (let* ([normalized
		   ;; allow for the possiblity of filenames that are urls
		   (with-handlers ([(lambda (x) #t)
				    (lambda (x) name)])
		     (mzlib:file:normalize-path name))]
		  [test-frame
		   (lambda (frame)
		     (and (ivar-in-class? 'get-edit (object-class frame))
			  (let* ([edit (send frame get-edit)]
				 [filename (send edit get-filename)])
			    (and (send edit editing-this-file?)
				 (string? filename)
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
			(loop (cdr frames))))]))))])))
  
  (define the-frame-group (make-object %)))