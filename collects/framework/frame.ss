(unit/sig framework:frame^
  (import mred-interfaces^
	  [group : framework:group^]
	  [preferences : framework:preferences^]
	  [icon : framework:icon^]
	  [handler : framework:handler^]
	  [application : framework:application^]
	  [panel : framework:panel^]
	  [gui-utils : framework:gui-utils^]
	  [exit : framework:exit^]
	  [finder : framework:finder^]
	  [keymap : framework:keymap^]
	  [text : framework:text^]
	  [pasteboard : framework:pasteboard^]
	  [editor : framework:editor^]
	  [mzlib:function : mzlib:function^])

  (rename [-editor<%> editor<%>]
	  [-pasteboard% pasteboard%]
	  [-pasteboard<%> pasteboard<%>]
	  [-text% text%]
	  [-text<%> text<%>])

  (define frame-width 600)
  (define frame-height 650)
  (let ([window-trimming-upper-bound-width 20]
	[window-trimming-upper-bound-height 50])
    (let-values ([(w h) (get-display-size)])
      (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
      (set! frame-height (min frame-height (- h window-trimming-upper-bound-height)))))

  (define basic<%> (interface (frame<%>)
		     get-area-container%
		     get-area-container
		     get-menu-bar%
		     make-root-area-container))
  (define basic-mixin
    (mixin (frame<%>) (basic<%>) args
      (rename [super-can-close? can-close?]
	      [super-on-close on-close]
	      [super-on-focus on-focus])
      (override
	[can-close?
	 (lambda ()
	   (and (super-can-close?)
		(send (group:get-the-frame-group)
		      can-remove-frame?
		      this)))]
	[on-close
	 (lambda ()
	   (super-on-close)
	   (send (group:get-the-frame-group)
		 remove-frame
		 this))]
	[on-focus
	 (lambda (on?)
	   (super-on-focus on?)
	   (when on?
	     (send (group:get-the-frame-group) set-active-frame this)))])
      (public
	[get-area-container% (lambda () vertical-panel%)]
	[get-menu-bar% (lambda () menu-bar%)]
	[make-root-area-container
	 (lambda (% parent)
	   (make-object % parent))])
      (sequence
	(apply super-init args)
	(send (group:get-the-frame-group) insert-frame this)
	(make-object (get-menu-bar%) this))
      (private
	[panel (make-root-area-container (get-area-container%) this)])
      (public
	[get-area-container (lambda () panel)])))

  (include "standard-menus.ss")

  (define -editor<%> (interface (standard-menus<%>)
		       get-init-width
		       get-init-height
		       get-entire-label
		       get-label-prefix
		       set-label-prefix

		       get-canvas%
		       get-editor%
		       get-editor<%>
		       make-editor
		       save-as		      
		       get-canvas
		       get-editor))

  (define editor-mixin
    (mixin (standard-menus<%>) (-editor<%>) (file-name)
      (inherit get-area-container get-client-size set-icon show get-edit-target-window get-edit-target-object)
      (rename [super-on-close on-close]
	      [super-set-label set-label])
      (public
	[get-init-width (lambda () frame-width)]
	[get-init-height (lambda () frame-height)])
	     
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (send (get-editor) on-close))]
	[get-area-container%  (lambda () panel:vertical-editor%)])
      (private
	[label (let-values ([(base name dir?) (split-path file-name)])
		 (or name
		     file-name))]
	[label-prefix (application:current-app-name)]
	[do-label
	 (lambda ()
	   (super-set-label (get-entire-label))
	   (send (group:get-the-frame-group) frame-label-changed this))])
	     
      (public
	[get-entire-label
	 (lambda ()
	   (if (or (string=? "" label)
		   (string=? "" label-prefix))
	       (string-append label-prefix label)
	       (string-append label " - " label-prefix)))]
	[get-label-prefix (lambda () label-prefix)]
	[set-label-prefix
	 (lambda (s)
	   (when (and (string? s)
		      (not (string=? s label-prefix)))
	     (set! label-prefix s)
	     (do-label)))])
      (override
	[get-label (lambda () label)]
	[set-label
	 (lambda (t)
	   (when (and (string? t)
		      (not (string=? t label)))
	     (set! label t)
	     (do-label)))])
      (public
	[get-canvas% (lambda () editor-canvas%)]
	[get-editor% (lambda () (error 'editor-frame% "no editor% class specified"))]
	[get-editor<%> (lambda () editor<%>)]
	[make-editor (lambda ()
		       (let ([% (get-editor%)]
			     [<%> (get-editor<%>)])
			 (unless (implementation? % <%>)
			   (let ([name (inferred-name this)])
			     (error (or name 'frame:editor%)
				    "result of get-editor% method must match ~e interface; got: ~e"
				    <%> %)))
			 (make-object %)))])
				  
	     
      (public
	[save-as
	 (opt-lambda ([format 'same])
	   (let ([file (parameterize ([finder:dialog-parent-parameter this])
			 (finder:put-file))])
	     (when file
	       (send (get-editor) save-file file format))))])
      (inherit get-menu-item%)
      (override
	[file-menu:revert 
	 (lambda (item control)
	   (let* ([b (box #f)]
		  [edit (get-editor)]
		  [filename (send edit get-filename b)])
	     (if (or (not filename) (unbox b))
		 (bell)
		 (let-values ([(start end)
			       (if (is-a? edit original:text%)
				   (values (send edit get-start-position)
					   (send edit get-end-position))
				   (values #f #f))])
		   (send edit begin-edit-sequence)
		   (let ([status (send edit load-file
				       filename
				       'same
				       #f)])
		     (if status
			 (begin
			   (when (is-a? edit original:text%)
			     (send edit set-position start end))
			   (send edit end-edit-sequence))
			 (begin
			   (send edit end-edit-sequence)
			   (message-box
			    "Error Reverting"
			    (format "could not read ~a" filename)))))))
	     #t))]
	[file-menu:save (lambda (item control)
			  (send (get-editor) save-file)
			  #t)]
	[file-menu:save-as (lambda (item control) (save-as) #t)]
	[file-menu:between-print-and-close
	 (lambda (file-menu)
	   (make-object separator-menu-item% file-menu)
	   (let ([split
		  (lambda (panel%)
		    (lambda (item control)
		      (let ([win (get-edit-target-object)])
			(when (and win
				   (is-a? win canvas<%>))
			  (send (get-area-container) split win panel%)))))])
	     (make-object (get-menu-item%) "Split Horizontally" file-menu (split horizontal-panel%))
	     (make-object (get-menu-item%) "Split Vertically" file-menu (split vertical-panel%))
	     (make-object (get-menu-item%) "Collapse" file-menu
		   (lambda (item control)
		     (let ([canvas (get-edit-target-window)])
		       (when canvas
			 (send (get-area-container) collapse canvas))))))
	   (make-object separator-menu-item% file-menu))]
	[file-menu:print (lambda (item control)
			   (send (get-editor) print
				 #t
				 #t
				 (preferences:get 'framework:print-output-mode))
			   #t)])
	     
      (private
	[edit-menu:do (lambda (const)
			(lambda (menu evt)
			  (let ([edit (get-edit-target-object)])
			    (when (and edit
				       (is-a? edit editor<%>))
			      (send edit do-edit-operation const)))
			  #t))])
	     
      (override
	[edit-menu:undo (edit-menu:do 'undo)]
	[edit-menu:redo (edit-menu:do 'redo)]
	[edit-menu:cut (edit-menu:do 'cut)]
	[edit-menu:clear (edit-menu:do 'clear)]
	[edit-menu:copy (edit-menu:do 'copy)]
	[edit-menu:paste (edit-menu:do 'paste)]
	[edit-menu:select-all (edit-menu:do 'select-all)]
	
	[edit-menu:between-find-and-preferences
	 (lambda (edit-menu)
	   (make-object separator-menu-item% edit-menu)
	   (make-object (get-menu-item%) "Insert Text Box" edit-menu
			(edit-menu:do 'insert-text-box))
	   (make-object (get-menu-item%) "Insert Graphic Box" edit-menu
			(edit-menu:do 'insert-graphic-box))
	   (make-object (get-menu-item%) "Insert Image..." edit-menu
			(edit-menu:do 'insert-image))
	   (make-object (get-menu-item%) "Toggle Wrap Text" edit-menu
			(lambda (item event)
			  (let ([edit (get-edit-target-object)])
			    (when (and edit
				       (is-a? edit editor<%>))
			      (send edit auto-wrap (not (send edit auto-wrap)))))))
	   (make-object separator-menu-item% edit-menu))])
	     
      (override
	[help-menu:about (lambda (menu evt) (message-box (format "Welcome to ~a" (application:current-app-name))))]
	[help-menu:about-string (lambda () (application:current-app-name))])
	     
      (sequence (super-init (get-entire-label) #f (get-init-width) (get-init-height)))
	     
      (public
	[get-canvas (let ([c #f])
		      (lambda () 
			(unless c
			  (set! c (make-object (get-canvas%) (get-area-container)))
			  (send c set-editor (get-editor)))
			c))]
	[get-editor (let ([e #f])
		      (lambda () 
			(unless e 
			  (set! e (make-editor))
			  (send (get-canvas) set-editor e))
			e))])
      (sequence
	(let ([icon (icon:get)])
	  (when (send icon ok?)
	    (set-icon icon)))
	(do-label)
	(let ([canvas (get-canvas)])
	  (send (get-editor) load-file file-name 'guess #f)
	  (send canvas focus)))))
  
  (define -text<%> (interface (-editor<%>)))
  (define text-mixin
    (mixin (-editor<%>) (-text<%>) args
      (override
        [get-editor<%> (lambda () text<%>)]
	[get-editor% (lambda () text:keymap%)])
      (sequence (apply super-init args))))

  (define -pasteboard<%> (interface (-editor<%>)))
  (define pasteboard-mixin
    (mixin (-editor<%>) (-pasteboard<%>) args
      (override
        [get-editor<%> (lambda () pasteboard<%>)]
	[get-editor% (lambda () pasteboard:keymap%)])
      (sequence (apply super-init args))))

  (define searchable<%> (interface (-text<%>)
			  get-text-to-search
			  hide-search
			  unhide-search
			  set-search-direction
			  replace&search
			  replace-all
			  replace
			  toggle-search-focus
			  move-to-search-or-search
			  move-to-search-or-reverse-search
			  search))
  (define search-anchor 0)
  (define searching-direction 'forward)
  (define old-search-highlight void)
  (define get-active-embedded-edit
    (lambda (edit)
      (let loop ([edit edit])
	(let ([snip (send edit get-focus-snip)])
	  (if (or (not snip)
		  (not (is-a? snip original:editor-snip%)))
	      edit
	      (loop (send snip get-this-media)))))))
  (define clear-search-highlight
    (lambda ()
      (begin (old-search-highlight)
	     (set! old-search-highlight void))))
  (define reset-search-anchor
    (let ([color (make-object color% "BLUE")])
      (lambda (edit)
	(old-search-highlight)
	(let ([position 
	       (if (eq? 'forward searching-direction)
		   (send edit get-end-position)
		   (send edit get-start-position))])
	  (set! search-anchor position)
	  (set! old-search-highlight
		(send edit highlight-range position position color #f))))))

  (define find-text%
    (class-asi text%
      (inherit get-text)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-on-focus on-focus])
      (public
	[searching-frame #f]
	[set-searching-frame
	 (lambda (frame)
	   (set! searching-frame frame))]
	[get-searching-edit
	 (lambda ()
	   (get-active-embedded-edit
	    (send searching-frame get-text-to-search)))]
	[search
	 (opt-lambda ([reset-search-anchor? #t] [beep? #t] [wrap? #t])
	   (when searching-frame
	     (let* ([string (get-text)]
		    [searching-edit (get-searching-edit)]
		    [not-found
		     (lambda (found-edit)
		       (send found-edit set-position search-anchor)
		       (when beep?
			 (bell))
		       #f)]
		    [found
		     (lambda (edit first-pos)
		       (let ([last-pos (+ first-pos (* (if (eq? searching-direction 'forward) 1 -1)
						       (string-length string)))])
			 (send* edit 
				(set-caret-owner #f 'display)
				(set-position
				 (min first-pos last-pos)
				 (max first-pos last-pos)))
			 #t))])
	       (when reset-search-anchor?
		 (reset-search-anchor searching-edit))
	       (let-values ([(found-edit first-pos)
			     (send searching-edit
				   find-string-embedded
				   string
				   searching-direction
				   search-anchor
				   'eof #t #t #t)])
		 (cond
		   [(not first-pos)
		    (if wrap?
			(let-values ([(found-edit pos)
				      (send searching-edit
					    find-string-embedded
					    string 
					    searching-direction
					    (if (eq? 'forward searching-direction)
						0
						(send searching-edit last-position)))])
			  (if (not pos)
			      (not-found found-edit)
			      (found found-edit 
				     ((if (eq? searching-direction 'forward)
					  +
					  -)
				      pos
				      (string-length string)))))
			(not-found found-edit))]
		   [else
		    (found found-edit first-pos)])))))])
      (override
	[on-focus
	 (lambda (on?)
	   (when on?
	     (reset-search-anchor (get-searching-edit)))
	   (super-on-focus on?))]
	[after-insert
	 (lambda args
	   (apply super-after-insert args)
	   (search #f))]
	[after-delete
	 (lambda args
	   (apply super-after-delete args)
	   (search #f))])))

  (define find-edit #f)
  (define replace-edit #f)

  (define searchable-canvas% 
    (class editor-canvas% (parent)
      (inherit get-top-level-window set-line-count)
      (rename [super-on-focus on-focus])
      (override
	[on-focus
	 (lambda (x)
	   (when x
	     (send find-edit set-searching-frame (get-top-level-window)))
	   (super-on-focus x))])
      (sequence
	(super-init parent #f)
	(set-line-count 2))))

  (define (init-find/replace-edits)
    (unless find-edit
      (set! find-edit (make-object find-text%))
      (set! replace-edit (make-object text%))
      (for-each (lambda (keymap)
		  (send keymap chain-to-keymap
			(keymap:get-search)
			#t))
		(list (send find-edit get-keymap)
		      (send replace-edit get-keymap)))))

  (define searchable-mixin
    (mixin (-text<%>) (searchable<%>) args
      (sequence (init-find/replace-edits))
      (inherit get-editor)
      (rename [super-make-root-area-container make-root-area-container]
	      [super-on-activate on-activate]
	      [super-on-close on-close])
      (private
	[super-root 'unitiaialized-super-root])
      (override
        [get-editor<%> (lambda () text:searching<%>)]
	[get-editor% (lambda () text:searching%)]
	[edit-menu:find (lambda (menu evt) (search))])
      (override
	[make-root-area-container
	 (lambda (% parent)
	   (let* ([s-root (super-make-root-area-container
			   vertical-panel%
			   parent)]
		  [root (make-object % s-root)])
	     (set! super-root s-root)
	     root))])
      (override
	[on-activate
	 (lambda (on?)
	   (unless hidden?
	     (if on?
		 (reset-search-anchor (get-text-to-search))
		 (clear-search-highlight)))
	   (super-on-activate on?))])
      (public
	[get-text-to-search
	 (lambda () 
	   (get-editor))]
	[hide-search
	 (opt-lambda ([startup? #f])
	   (send super-root delete-child search-panel)
	   (clear-search-highlight)
	   (unless startup?
	     (send 
	      (send (get-text-to-search) get-canvas) 
	      focus))
	   (set! hidden? #t))]
	[unhide-search
	 (lambda ()
	   (when hidden?
	     (set! hidden? #f)
	     (send super-root add-child search-panel)
	     (reset-search-anchor (get-text-to-search))))])
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (let ([close-canvas
		  (lambda (canvas edit)
		    (send edit remove-canvas canvas)
		    (send canvas set-editor #f))])
	     (close-canvas find-canvas find-edit)
	     (close-canvas replace-canvas replace-edit))
	   (when (eq? this (ivar find-edit searching-frame))
	     (send find-edit set-searching-frame #f)))])
      (public
	[set-search-direction 
	 (lambda (x) 
	   (set! searching-direction x)
	   (send dir-radio set-selection (if (= x 1) 0 1)))]
	[replace&search
	 (lambda ()
	   (when (replace)
	     (search)))]
	[replace-all
	 (lambda ()
	   (let* ([replacee-edit (get-text-to-search)]
		  [pos (if (eq? searching-direction 'forward)
			   (send replacee-edit get-start-position)
			   (send replacee-edit get-end-position))]
		  [get-pos 
		   (if (eq? searching-direction 'forward)
		       (ivar replacee-edit get-end-position)
		       (ivar replacee-edit get-start-position))]
		  [done? (if (eq? 'forward searching-direction)
			     (lambda (x) (>= x (send replacee-edit last-position)))
			     (lambda (x) (<= x 0)))])
	     (send* replacee-edit 
		    (begin-edit-sequence)
		    (set-position pos))
	     (when (search)
	       (send replacee-edit set-position pos)
	       (let loop ()
		 (when (send find-edit search #t #f #f)
		   (replace)
		   (loop))))
	     (send replacee-edit end-edit-sequence)))]
	[replace
	 (lambda ()
	   (let* ([search-text (send find-edit get-text)]
		  [replacee-edit (get-text-to-search)]
		  [replacee-start (send replacee-edit get-start-position)]
		  [new-text (send replace-edit get-text)]
		  [replacee (send replacee-edit get-text
				  replacee-start
				  (send replacee-edit get-end-position))])
	     (if (string=? replacee search-text)
		 (begin (send replacee-edit insert new-text)
			(send replacee-edit set-position
			      replacee-start
			      (+ replacee-start (string-length new-text)))
			#t)
		 #f)))]
	[toggle-search-focus
	 (lambda ()
	   (unhide-search)
	   (send (cond
		   [(send find-canvas has-focus?)
		    replace-canvas]
		   [(send replace-canvas has-focus?)
		    (send (get-text-to-search) get-canvas)]
		   [else
		    find-canvas])
		 focus))]
	[move-to-search-or-search
	 (lambda ()
	   (unhide-search)
	   (if (or (send find-canvas has-focus?)
		   (send replace-canvas has-focus?))
	       (search 1)
	       (send find-canvas focus)))]
	[move-to-search-or-reverse-search
	 (lambda ()
	   (unhide-search)
	   (if (or (send find-canvas has-focus?)
		   (send replace-canvas has-focus?))
	       (search -1)
	       (send find-canvas focus)))]
	[search
	 (opt-lambda ([direction searching-direction] [beep? #t])
	   
	   (send find-edit set-searching-frame this)
	   (unhide-search)
	   (set-search-direction direction)
	   (send find-edit search #t beep?))])
      (sequence
	(apply super-init args))
      (private
	[search-panel (make-object horizontal-panel% super-root)]
	
	[left-panel (make-object vertical-panel% search-panel)]
	[find-canvas (make-object searchable-canvas% left-panel)]
	[replace-canvas (make-object searchable-canvas% left-panel)]
	
	[middle-left-panel (make-object vertical-panel% search-panel)]
	[middle-middle-panel (make-object vertical-panel% search-panel)]
	[middle-right-panel (make-object vertical-panel% search-panel)]
	
	[search-button (make-object button% 
			 "Search"
			 middle-left-panel
			 (lambda args (search)))]
	
	[replace&search-button (make-object button% 
				 "Replace && Search"
				 middle-middle-panel 
				 (lambda x (replace&search)))]
	[replace-button (make-object button% "Replace" middle-left-panel (lambda x (replace)))]
	[replace-all-button (make-object button% 
			      "Replace To End"
			      middle-middle-panel
			      (lambda x (replace-all)))]
	
	[dir-radio (make-object radio-box%
		     #f
		     (list "Forward" "Backward")
		     middle-right-panel
		     (lambda (dir-radio evt)
		       (let ([forward (if (= 0 (send evt get-command-int))
					  'forward
					  'backward)])
			 (set-search-direction forward)
			 (reset-search-anchor (get-text-to-search)))))]
	[close-button (make-object button% "Hide"
				   middle-right-panel
				   (lambda args (hide-search)))]
	[hidden? #f])
      (sequence
	(let ([align
	       (lambda (x y)
		 (let ([m (max (send x get-width)
			       (send y get-width))])
		   (send x min-width m)
		   (send y min-width m)))])
	  (align search-button replace-button)
	  (align replace&search-button replace-all-button))
	(for-each (lambda (x) (send x set-alignment 'center 'center))
		  (list middle-left-panel middle-middle-panel))
	(for-each (lambda (x) (send x stretchable-height #f))
		  (list search-panel left-panel middle-left-panel middle-middle-panel middle-right-panel))
	(for-each (lambda (x) (send x stretchable-width #f))
		  (list middle-left-panel middle-middle-panel middle-right-panel))
	(send find-canvas set-editor find-edit)
	(send replace-canvas set-editor replace-edit) 
	(send find-edit add-canvas find-canvas)
	(send replace-edit add-canvas replace-canvas)
	(hide-search #t))))
  
  (define info<%> (interface (-editor<%>)
		    determine-width
		    get-info-editor
		    lock-status-changed
		    update-info
		    get-info-panel))

  (define time-edit (make-object text%))
  (define time-semaphore (make-semaphore 1))
  (define wide-time "00:00pm")
  (send time-edit lock #t)
  (define update-time
    (lambda ()
      (dynamic-wind
       (lambda ()
	 (semaphore-wait time-semaphore)
	 (send time-edit lock #f))
       (lambda ()
	 (send* time-edit 
		(erase)
		(insert 
		 (let* ([date (seconds->date
			       (current-seconds))]
			[hours (date-hour date)]
			[minutes (date-minute date)])
		   (format "~a:~a~a~a"
			   (cond
			     [(= hours 0) 12]
			     [(<= hours 12) hours]
			     [else (- hours 12)])
			   (quotient minutes 10)
			   (modulo minutes 10)
			   (if (< hours 12) "am" "pm"))))))
       (lambda ()
	 (send time-edit lock #t)
	 (semaphore-post time-semaphore)))))
  (define time-thread
    (thread
     (rec loop
	  (lambda ()
	    (update-time)
	    (sleep 30)
	    (loop)))))

  (define info-mixin
    (mixin (-editor<%>) (info<%>) args
      (rename [super-make-root-area-container make-root-area-container])
      (private
	[rest-panel 'uninitialized-root]
	[super-root 'uninitialized-super-root])
      (override
	[make-root-area-container
	 (lambda (% parent)
	   (let* ([s-root (super-make-root-area-container
			   vertical-panel%
			   parent)]
		  [r-root (make-object % s-root)])
	     (set! super-root s-root)
	     (set! rest-panel r-root)
	     r-root))])
      
      (override
        [get-editor<%> (lambda () editor:info<%>)]
        [get-editor% (lambda () text:info%)])

      (public
	[determine-width
	 (let ([magic-space 25])
	   (lambda (string canvas edit)
	     (send edit set-autowrap-bitmap #f)
	     (send canvas call-as-primary-owner
		   (lambda ()
		     (let ([lb (box 0)]
			   [rb (box 0)])
		       (send edit erase)
		       (send edit insert string)
		       (send edit position-location 
			     (send edit last-position)
			     rb)
		       (send edit position-location 0 lb)
		       (send canvas min-width 
			     (+ magic-space (- (inexact->exact (floor (unbox rb)))
					       (inexact->exact (floor (unbox lb)))))))))))])
      
      (rename [super-on-close on-close])
      (private
	[close-panel-callback
	 (preferences:add-callback
	  'framework:show-status-line
	  (lambda (p v)
	    (if v 
		(register-gc-blit)
		(unregister-collecting-blit gc-canvas))
	    (send super-root change-children
		  (lambda (l)
		    (if v
			(list rest-panel (get-info-panel))
			(list rest-panel))))))])
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (send time-canvas set-editor #f)
	   (unregister-collecting-blit gc-canvas)
	   (close-panel-callback))])
      
      (inherit get-editor)
      (public
	[get-info-editor
	 (lambda ()
	   (and (procedure? get-editor)
		(get-editor)))])
      
      (public
	[lock-status-changed
	 (let ([icon-currently-locked? #f])
	   (lambda ()
	     (let ([info-edit (get-info-editor)])
	       (when info-edit
		 (let ([locked-now? (ivar info-edit locked?)])
		   (unless (eq? locked-now? icon-currently-locked?)
		     (set! icon-currently-locked? locked-now?)
		     (let ([label
			    (if locked-now?
				(icon:get-lock-bitmap)
				(icon:get-unlock-bitmap))])
		       (when (object? lock-message)
			 (send lock-message
			       set-label
			       (if (send label ok?)
				   label
				   (if locked-now? "Locked" "Unlocked")))))))))))])
      (public
	[update-info
	 (lambda ()
	   (lock-status-changed))])
      (sequence 
	(apply super-init args))

      (public
	[get-info-panel
	 (let ([info-panel (make-object horizontal-panel% super-root)])
	   (lambda ()
	     info-panel))])
      (private
	[lock-message (make-object message%
			(let ([b (icon:get-unlock-bitmap)])
			  (if (and #f (send b ok?))
			      b
			      "Unlocked"))
			(get-info-panel))]
	[time-canvas (make-object editor-canvas% (get-info-panel) #f '(no-hscroll no-vscroll))]
	[_ (send time-canvas set-line-count 1)]
	[gc-canvas (make-object canvas% (get-info-panel) '(border))]
	[register-gc-blit
	 (lambda ()
	   (let ([onb (icon:get-gc-on-bitmap)]
		 [offb (icon:get-gc-off-bitmap)])
	     (when (and (send onb ok?)
			(send offb ok?))
	       (register-collecting-blit gc-canvas 
					 0 0
					 (send onb get-width)
					 (send onb get-height)
					 onb offb))))])
      
      (sequence
	(unless (preferences:get 'framework:show-status-line)
	  (send super-root change-children
		(lambda (l)
		  (list rest-panel))))
	(register-gc-blit)
	
	(let* ([gcb (icon:get-gc-on-bitmap)]
	       [gc-width (if (send gcb ok?)
			     (send gcb get-width)
			     10)]
	       [gc-height (if (send gcb ok?)
			      (send gcb get-height)
			      10)])
	  (send* gc-canvas
		  (min-client-width (max (send gc-canvas min-width) gc-width))
		  (min-client-height (max (send gc-canvas min-height) gc-height))
		  (stretchable-width #f)
		  (stretchable-height #f)))
	(send* (get-info-panel) 
	       (set-alignment 'right 'center)
	       (stretchable-height #f)
	       (spacing 3)
	       (border 3))
	(send* time-canvas 
	       (set-editor time-edit)
	       (stretchable-width #f)
	       (stretchable-height #f))
	(semaphore-wait time-semaphore)
	(determine-width wide-time time-canvas time-edit)
	(semaphore-post time-semaphore)
	(update-time))))

  (define text-info<%> (interface (info<%>)
			   overwrite-status-changed
			   anchor-status-changed
			   editor-position-changed))
  (define text-info-mixin
    (mixin (info<%>) (text-info<%>) args
      (inherit get-info-editor)
      (rename [super-on-close on-close])
      (private
	[remove-pref-callback
	 (let ([one
		(preferences:add-callback
		 'framework:line-offsets
		 (lambda (p v)
		   (editor-position-changed-offset/numbers
		    v
		    (preferences:get 'framework:display-line-numbers))
		   #t))]
	       [two
		(preferences:add-callback
		 'framework:display-line-numbers
		 (lambda (p v)
		   (editor-position-changed-offset/numbers
		    (preferences:get 'framework:line-offsets)
		    v)
		   #t))])
	   (lambda ()
	     (one)
	     (two)))])
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (remove-pref-callback))])
      
      (private
	[editor-position-changed-offset/numbers
	 (let ([last-start #f]
	       [last-end #f])
	   (lambda (offset? line-numbers?)
	     (let* ([edit (get-info-editor)]
		    [make-one
		     (lambda (pos)
		       (let* ([line (send edit position-line pos)]
			      [line-start (send edit line-start-position line)]
			      [char (- pos line-start)])
			 (if line-numbers?
			     (format "~a:~a"
				     (if offset?
					 (add1 line)
					 line)
				     (if offset?
					 (add1 char)
					 char))
			     (format "~a"
				     (if offset?
					 (+ pos 1)
					 pos)))))])
	       (when edit
		 (let ([start (send edit get-start-position)]
		       [end (send edit get-end-position)])
		   (unless (and last-start
				(= last-start start)
				(= last-end end))
		     (set! last-start start)
		     (set! last-end end)
		     (when (object? position-edit)
		       (send* position-edit
			      (lock #f)
			      (erase)
			      (insert 
			       (if (= start end)
				   (make-one start)
				   (string-append (make-one start)
						  "-"
						  (make-one end))))
			      (lock #t)))))))))])
      (public
	[anchor-status-changed
	 (let ([last-state? #f])
	   (lambda ()
	     (let ([info-edit (get-info-editor)])
	       (when info-edit
		 (let ([anchor-now? (send info-edit get-anchor)])
		   (unless (eq? anchor-now? last-state?)
		     (when (object? anchor-message)
		       (send anchor-message
			     show
			     anchor-now?))
		     (set! last-state? anchor-now?)))))))]
	[editor-position-changed
	 (lambda ()
	   (editor-position-changed-offset/numbers
	    (preferences:get 'framework:line-offsets)
	    (preferences:get 'framework:display-line-numbers)))]
	[overwrite-status-changed
	 (let ([last-state? #f])
	   (lambda ()
	     (let ([info-edit (get-info-editor)])
	       (when info-edit
		 (let ([overwrite-now? (send info-edit get-overwrite-mode)])
		   (unless (eq? overwrite-now? last-state?)
		     (when (object? overwrite-message)
		       (send overwrite-message
			     show
			     overwrite-now?))
		     (set! last-state? overwrite-now?)))))))])
      (rename [super-update-info update-info])
      (override
	[update-info
	 (lambda ()
	   (super-update-info)
	   (overwrite-status-changed)
	   (anchor-status-changed)
	   (editor-position-changed))])
      (sequence 
	(apply super-init args))
      
      (inherit get-info-panel)
      (private
	[anchor-message 
	 (make-object message%
	   (let ([b (icon:get-anchor-bitmap)])
	     (if (and #f (send b ok?))
		 b
		 "Anchor"))
	   (get-info-panel))]
	[overwrite-message 
	 (make-object message%
	   "Overwrite"
	   (get-info-panel))]
	[position-canvas (make-object editor-canvas% (get-info-panel) #f '(no-hscroll no-vscroll))]
	[position-edit (make-object text%)])
      
      (inherit determine-width)
      (sequence
	(let ([move-front
	       (lambda (x l)
		 (cons x (mzlib:function:remq x l)))])
	  (send (get-info-panel) change-children
		(lambda (l)
		  (move-front
		   anchor-message
		   (move-front
		    overwrite-message
		    (move-front
		     position-canvas
		     l))))))
	(send anchor-message show #f)
	(send overwrite-message show #f)
	(send* position-canvas
	       (set-line-count 1)
	       (set-editor position-edit)
	       (stretchable-width #f)
	       (stretchable-height #f))
	(determine-width "0000:000-0000:000" 
			 position-canvas
			 position-edit)
	(editor-position-changed)
	(send position-edit lock #t))))
  
  (define file<%> (interface (-editor<%>)))
  (define file-mixin
    (mixin (-editor<%>) (file<%>) args
      (inherit get-editor)
      (rename [super-can-close? can-close?])
      (override
	[can-close?
	 (lambda ()
	   (let* ([edit (get-editor)]
		  [user-allowed-or-not-modified
		   (or (not (send edit is-modified?))
		       (case (gui-utils:unsaved-warning
			      (let ([fn (send edit get-filename)])
				(if (string? fn)
				    fn
				    "Untitled"))
			      "Close"
			      #t)
			 [(continue) #t]
			 [(save) (send edit save-file)]
			 [else #f]))])
	     (and user-allowed-or-not-modified
		  (super-can-close?))))])
      (sequence (apply super-init args))))

  (define basic% (basic-mixin frame%))
  (define standard-menus% (standard-menus-mixin basic%))
  (define editor% (editor-mixin standard-menus%))

  (define -text% (text-mixin editor%))
  (define searchable% (searchable-mixin -text%))
  (define text-info% (text-info-mixin (info-mixin searchable%)))
  (define text-info-file% (file-mixin text-info%))

  (define -pasteboard% (pasteboard-mixin editor%))
  (define pasteboard-info% (info-mixin -pasteboard%))
  (define pasteboard-info-file% (file-mixin pasteboard-info%)))
