      (private [get-standard-menu-close-item 
		(lambda (frame)
		  (let* ([close-string (if (eq? (system-type) 'windows)
					   "&Close"
					   "Close")]
			 [file-menu (ivar frame file-menu)])
		    (if file-menu 
			(send file-menu find-item close-string)
			#f)))]
	       [set-close-menu-item-state! 
		(lambda (frame state)
		  (when (is-a? frame frame:standard-menus<%>)
		    (let ([close-menu-item 
			   (get-standard-menu-close-item frame)])
		      (when close-menu-item
			(send (ivar frame file-menu) 
			      enable close-menu-item state)))))])

when removing a frame, do this:


	   (let ([frames (send mred:group:the-frame-group 
			       get-frames)])

	     ; disable File|Close if remaining frame is singleton

	     (when (eq? (length frames) 1)
	       (set-close-menu-item-state! (car frames) #f)))

when adding a frame, do this:

	    (let ([frames (send mred:group:the-frame-group get-frames)])

	      (if (eq? (length frames) 1)

		  ; disable File|Close if frame is singleton

		  (set-close-menu-item-state! this #f)

		  ; otherwise, enable for all frames

		  (send mred:group:the-frame-group
			for-each-frame
			(lambda (a-frame)
			  (set-close-menu-item-state! a-frame #t)))))


