
(module group mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
           "../gui-utils-sig.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "list.ss")
	   (lib "file.ss"))

  (provide group@)

  (define group@
    (unit/sig framework:group^
      (import mred^
	      [application : framework:application^]
	      [frame : framework:frame^]
	      [preferences : framework:preferences^]
              [gui-utils : framework:gui-utils^]
              [text : framework:text^]
              [canvas : framework:canvas^])

      (define-struct frame (frame id))
      
      (define mdi-parent #f)

      (define %
	(class object%

	  [define active-frame #f]
	  [define frame-counter 0]
	  [define frames null]
	  [define todo-to-new-frames void]
	  [define empty-close-down (lambda () (void))]
	  [define empty-test (lambda () #t)]
	  
	  [define windows-menus null]
	  
	  [define get-windows-menu
	    (lambda (frame)
	      (let ([menu-bar (send frame get-menu-bar)])
		(and menu-bar
		     (let ([menus (send menu-bar get-items)])
		       (ormap (lambda (x)
				(if (string=? (string-constant windows-menu-label) (send x get-label))
				    x
				    #f))
			      menus)))))]
	  [define insert-windows-menu
	    (lambda (frame)
	      (let ([menu (get-windows-menu frame)])
		(when menu
		  (set! windows-menus (cons menu windows-menus)))))]
	  [define remove-windows-menu
	    (lambda (frame)
	      (let* ([menu (get-windows-menu frame)])
		(map (lambda (i) (send i delete)) (send menu get-items))
		(set! windows-menus
		      (remove
		       menu
		       windows-menus
		       eq?))))]

	  [define (update-windows-menus)
	    (let* ([windows (length windows-menus)]
		   [default-name (string-constant untitled)]
		   [get-name 
		    (lambda (frame)
		      (let ([label (send frame get-label)])
			(if (string=? label "")
			    (if (method-in-interface? 'get-entire-label (object-interface frame))
				(let ([label (send frame get-entire-label)])
				  (if (string=? label "")
				      default-name
				      label))
				default-name)
			    label)))]
		   [sorted/visible-frames
		    (quicksort
		     (filter (lambda (x) (send (frame-frame x) is-shown?)) frames)
		     (lambda (f1 f2)
		       (string-ci<=? (get-name (frame-frame f1))
				     (get-name (frame-frame f2)))))])
	      (for-each
	       (lambda (menu)
		 (for-each (lambda (item) (send item delete))
			   (send menu get-items))
                 (instantiate menu-item% ()
                   (label (string-constant bring-frame-to-front...))
                   (parent menu)
                   (callback (lambda (x y) (choose-a-frame (send (send menu get-parent) get-frame))))
                   (shortcut #\h))
                 (make-object separator-menu-item% menu)
		 (for-each
		  (lambda (frame)
		    (let ([frame (frame-frame frame)])
		      (make-object menu-item% (get-name frame)
				   menu
				   (lambda (_1 _2)
				     (send frame show #t)))))
		  sorted/visible-frames))
	       windows-menus))]
	  
	  [define update-close-menu-item-state
	    (lambda ()
	      (let* ([set-close-menu-item-state! 
		      (lambda (frame state)
			(when (is-a? frame frame:standard-menus<%>)
			  (let ([close-menu-item (send frame file-menu:get-close-menu)])
			    (when close-menu-item
			      (send close-menu-item enable state)))))])
		(if (eq? (length frames) 1)
		    (set-close-menu-item-state! (car frames) #f)
		    (for-each (lambda (a-frame)
				(set-close-menu-item-state! a-frame #t))
			      frames))))]
	  (public get-mdi-parent set-empty-callbacks frame-label-changed for-each-frame
		  get-active-frame set-active-frame insert-frame can-remove-frame?
		  remove-frame clear on-close-all can-close-all? locate-file get-frames
		  frame-shown/hidden)
	  [define get-mdi-parent
	    (lambda ()
	      (when (and (eq? (system-type) 'windows)
			 (preferences:get 'framework:windows-mdi)
			 (not mdi-parent))
		(set! mdi-parent (make-object frame% (application:current-app-name)
					      #f #f #f #f #f
					      '(mdi-parent)))
		(send mdi-parent show #t))
	      mdi-parent)]

	  [define set-empty-callbacks
	    (lambda (test close-down) 
	      (set! empty-test test)
	      (set! empty-close-down close-down))]
	  [define get-frames (lambda () (map frame-frame frames))]
	  
	  [define frame-label-changed
	    (lambda (frame)
	      (when (memq frame (map frame-frame frames))
		(update-windows-menus)))]

	  [define frame-shown/hidden
	    (lambda (frame)
	      (when (memq frame (map frame-frame frames))
	        (update-windows-menus)))]
	  
	  [define for-each-frame
	    (lambda (f)
	      (for-each (lambda (x) (f (frame-frame x))) frames)
	      (set! todo-to-new-frames
		    (let ([old todo-to-new-frames])
		      (lambda (frame) (old frame) (f frame)))))]
	  [define get-active-frame
	    (lambda ()
	      (cond
		[active-frame active-frame]
		[(null? frames) #f]
		[else (frame-frame (car frames))]))]
	  [define set-active-frame
	    (lambda (f)
	      (set! active-frame f))]
	  [define insert-frame
	    (lambda (f)
	      (set! frame-counter (add1 frame-counter))
	      (let ([new-frames (cons (make-frame f frame-counter)
				      frames)])
		(set! frames new-frames)
		(update-close-menu-item-state)
		(insert-windows-menu f)
		(update-windows-menus))
	      (todo-to-new-frames f))]
	  
	  [define can-remove-frame?
	    (lambda (f)
	      (let ([new-frames 
		     (remove
		      f frames
		      (lambda (f fr) (eq? f (frame-frame fr))))])
		(if (null? new-frames)
		    (empty-test)
		    #t)))]
	  [define remove-frame
	    (lambda (f)
	      (when (eq? f active-frame)
		(set! active-frame #f))
	      (let ([new-frames
		     (remove
		      f frames
		      (lambda (f fr) (eq? f (frame-frame fr))))])
		(set! frames new-frames)
		(update-close-menu-item-state)
		(remove-windows-menu f)
		(update-windows-menus)
		(when (null? frames)
		  (empty-close-down))))]
	  [define clear
	    (lambda ()
	      (and (empty-test)
		   (begin (set! frames null)
			  (empty-close-down)
			  #t)))]
	  [define on-close-all
	    (lambda ()
	      (for-each (lambda (f)
			  (let ([frame (frame-frame f)])
			    (send frame on-close) 
			    (send frame show #f)))
			frames))]
	  [define can-close-all?
	    (lambda ()
	      (andmap (lambda (f)
			(let ([frame (frame-frame f)])
			  (send frame can-close?)))
		      frames))]
	  [define locate-file
	    (lambda (name)
	      (let* ([normalized
		      ;; allow for the possiblity of filenames that are urls
		      (with-handlers ([(lambda (x) #t)
				       (lambda (x) name)])
			(normal-case-path
			 (normalize-path name)))]
		     [test-frame
		      (lambda (frame)
			(and (is-a? frame frame:basic<%>)
			     (let* ([filename (send frame get-filename)])
			       (and (string? filename)
				    (string=? normalized
					      (with-handlers ([(lambda (x) #t)
							       (lambda (x) filename)])
						(normal-case-path
						 (normalize-path 
						  filename))))))))])
		(let loop ([frames frames])
		  (cond
		    [(null? frames) #f]
		    [else
		     (let* ([frame (frame-frame (car frames))])
		       (if (test-frame frame)
			   frame
			   (loop (cdr frames))))]))))]
	  
          (super-instantiate ())))
      
      (define (choose-a-frame parent)
        (letrec ([sorted-frames
                  (quicksort
                   (send (get-the-frame-group) get-frames)
                   (lambda (x y) (string-ci<=? (send x get-label) (send y get-label))))]
                 [d (make-object dialog% (string-constant bring-frame-to-front) parent 400 600)]
                 [lb (instantiate gui-utils:alphabetic-list-box% () 
                       (label #f)
                       (choices (map (lambda (x) (send x get-label)) sorted-frames))
                       (callback (lambda (x y) (listbox-callback y)))
                       (parent d))]
                 [t (instantiate text:hide-caret/selection% ())]
                 [ec (instantiate canvas:basic% ()
                       (parent d)
                       (stretchable-height #f))]
                 [bp (instantiate horizontal-panel% ()
                       (parent d)
                       (stretchable-height #f)
                       (alignment '(right center)))]
                 [cancelled? #t]
                 [ok (instantiate button% ()
                       (label (string-constant ok))
                       (parent bp)
                       (callback (lambda (x y)
                                   (set! cancelled? #f)
                                   (send d show #f)))
                       (style '(border)))]
                 [cancel (instantiate button% ()
                           (label (string-constant cancel))
                           (parent bp) 
                           (callback 
                            (lambda (x y)
                              (send d show #f))))]
                 [listbox-callback
                  (lambda (evt)
                    (case (send evt get-event-type)
                      [(list-box)
                       
                       (send ok enable (pair? (send lb get-selections)))
                       
                       (let ([full-name
                              (let ([sels (send lb get-selections)])
                                (and (pair? sels)
                                     (let ([fr (list-ref sorted-frames (car sels))])
                                       (and (is-a? fr frame:basic%)
                                            (send fr get-filename)))))])
                         (send t begin-edit-sequence)
                         (send t erase)
                         (when full-name
                           (send t insert full-name))
                         (send t end-edit-sequence))]
                      [(list-box-dclick)
                       (set! cancelled? #f)
                       (send d show #f)]))])
          (send ec set-line-count 3)
          (send ec set-editor t)
          (send t auto-wrap #t)
          (let ([fr (car sorted-frames)])
            (when (and (is-a? fr frame:basic<%>)
                       (send fr get-filename))
              (send t insert (send (car sorted-frames) get-filename)))
            (send lb set-selection 0))
          (send d show #t)
          (unless cancelled?
            (let ([sels (send lb get-selections)])
              (unless (null? sels)
                (send (list-ref sorted-frames (car sels)) show #t))))))
          

      (define (internal-get-the-frame-group)
	(let ([the-frame-group (make-object %)])
	  (set! internal-get-the-frame-group (lambda () the-frame-group))
	  (internal-get-the-frame-group)))
      
      (define (get-the-frame-group)
	(internal-get-the-frame-group)))))
