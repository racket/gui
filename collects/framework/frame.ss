(unit/sig framework:frame^
  (import mred^
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
	  [canvas : framework:canvas^]
	  [mzlib:function : mzlib:function^])

  (rename [-editor<%> editor<%>]
	  [-pasteboard% pasteboard%]
	  [-text% text%])

  (define (reorder-menus frame)
    (let* ([items (send (send frame get-menu-bar) get-items)]
	   [move-to-back
	    (lambda (name items)
	      (let loop ([items items]
			 [back null])
		(cond
		 [(null? items) back]
		 [else (let ([item (car items)])
			 (if (string=? (send item get-plain-label) name)
			     (loop (cdr items)
				   (cons item back))
			     (cons item (loop (cdr items) back))))])))]
	   [move-to-front
	    (lambda (name items)
	      (reverse (move-to-back name (reverse items))))]
	   [re-ordered
	    (move-to-front
	     "File"
	     (move-to-front
	      "Edit"
	      (move-to-back
	       "Help"
	       items)))])
      (for-each (lambda (item) (send item delete)) items)
      (for-each (lambda (item) (send item restore)) re-ordered)))

  (define frame-width 600)
  (define frame-height 650)
  (let ([window-trimming-upper-bound-width 20]
	[window-trimming-upper-bound-height 50])
    (let-values ([(w h) (get-display-size)])
      (set! frame-width (min frame-width (- w window-trimming-upper-bound-width)))
      (set! frame-height (min frame-height (- h window-trimming-upper-bound-height)))))

  (define basic<%> (interface ((class->interface frame%))
		     get-area-container%
		     get-area-container
		     get-menu-bar%
		     make-root-area-container
		     close))
  (define basic-mixin
    (mixin ((class->interface frame%)) (basic<%>)
	   (label [parent #f] [width #f] [height #f] [x #f] [y #f] [style null])
      (rename [super-can-close? can-close?]
	      [super-on-close on-close]
	      [super-on-focus on-focus])
      (private
	[after-init? #f])
      (override
	[can-close?
	 (lambda ()
           (let ([super (super-can-close?)]
                 [group
                  (send (group:get-the-frame-group)
                        can-remove-frame?
                        this)])
             (and super group)))]
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
	     (send (group:get-the-frame-group) set-active-frame this)))]
        
        [on-drop-file
         (lambda (filename)
           (handler:edit-file filename))])

      (inherit change-children)
      (override
	[after-new-child
	 (lambda (child)
	   (when after-init?
	     (change-children (lambda (l) (mzlib:function:remq child l)))
	     (error 'frame:basic-mixin
		    "do not add children directly to a frame:basic (unless using make-root-area-container); use the get-area-container method instead"
		    )))])

      (inherit show)
      (public
	[get-area-container% (lambda () vertical-panel%)]
	[get-menu-bar% (lambda () menu-bar%)]
	[make-root-area-container
	 (lambda (% parent)
	   (make-object % parent))]
	[close
	 (lambda ()
	   (when (can-close?)
	     (on-close)
	     (show #f)))])
      
      (inherit accept-drop-files)
      (sequence
	(let ([mdi-parent (send (group:get-the-frame-group) get-mdi-parent)])
	  (super-init label
		      (or parent mdi-parent)
		      width height x y
		      (cond
		       [parent style]
		       [mdi-parent (cons 'mdi-child style)]
		       [else style])))

        (accept-drop-files #t)

	(make-object menu% "&Windows" (make-object (get-menu-bar%) this))
	(reorder-menus this)
	(send (group:get-the-frame-group) insert-frame this))
      (private
	[panel (make-root-area-container (get-area-container%) this)])
      (public
	[get-area-container (lambda () panel)])
      (sequence
	(set! after-init? #t))))

  (include "standard-menus.ss")

  (define -editor<%> (interface (standard-menus<%>)
		       get-entire-label
		       get-label-prefix
		       set-label-prefix

		       get-canvas%
		       get-canvas<%>
		       get-editor%
		       get-editor<%>
		       
		       make-editor
		       save-as		      
		       get-canvas
		       get-editor))

  (define editor-mixin
    (mixin (standard-menus<%>) (-editor<%>) (file-name
					     [parent #f]
					     [width frame-width]
					     [height frame-height]
					     .
					     args)
      (inherit get-area-container get-client-size 
	       show get-edit-target-window get-edit-target-object)
      (rename [super-on-close on-close]
	      [super-set-label set-label])
	     
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (send (get-editor) on-close))])
      (private
	[label (if file-name
		   (let-values ([(base name dir?) (split-path file-name)])
		     (or name
			 file-name))
		   (gui-utils:next-untitled-name))]
	[label-prefix (application:current-app-name)]
	[do-label
	 (lambda ()
	   (super-set-label (get-entire-label))
	   (send (group:get-the-frame-group) frame-label-changed this))])
	     
      (public
	[get-entire-label
	 (lambda ()
	   (cond
	    [(string=? "" label)
	     label-prefix]
	    [(string=? "" label-prefix)
	     label]
	    [else 
	     (string-append label " - " label-prefix)]))]
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
	[get-canvas<%> (lambda () (class->interface editor-canvas%))]
	[make-canvas (lambda ()
		       (let ([% (get-canvas%)]
			     [<%> (get-canvas<%>)])
			 (unless (implementation? % <%>)
				 (error 'frame:editor%
					"result of get-canvas% method must match ~e interface; got: ~e"
					<%> %))
			 (make-object % (get-area-container))))]
	[get-editor% (lambda () (error 'editor-frame% "no editor% class specified"))]
	[get-editor<%> (lambda () editor<%>)]
	[make-editor (lambda ()
		       (let ([% (get-editor%)]
			     [<%> (get-editor<%>)])
			 (unless (implementation? % <%>)
				 (error 'frame:editor%
					"result of get-editor% method must match ~e interface; got: ~e"
					<%> %))
			 (make-object %)))])
				  
	     
      (public
	[save-as
	 (opt-lambda ([format 'same])
	   (let* ([name (send (get-editor) get-filename)]
                  [file (parameterize ([finder:dialog-parent-parameter this])
                          (finder:put-file name))])
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
		 (let ([start
                        (if (is-a? edit text%)
                            (send edit get-start-position)
                            #f)])
		   (send edit begin-edit-sequence)
		   (let ([status (send edit load-file
				       filename
				       'same
				       #f)])
		     (if status
			 (begin
			   (when (is-a? edit text%)
			     (send edit set-position start start))
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
	[edit-menu:between-select-all-and-find
	 (lambda (edit-menu)
	   (make-object separator-menu-item% edit-menu)
	   (make-object (get-menu-item%) "Insert Text Box" edit-menu
			(edit-menu:do 'insert-text-box))
	   (make-object (get-menu-item%) "Insert Pasteboard Box" edit-menu
			(edit-menu:do 'insert-pasteboard-box))
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
        [help-menu:about 
	 (lambda (menu evt) 
	   (message-box (application:current-app-name)
			(format "Welcome to ~a" (application:current-app-name))))]
	[help-menu:about-string (lambda () (application:current-app-name))])
	     
      (sequence (apply super-init
		       (get-entire-label)
		       parent
		       width
		       height
		       args))
	     
      (public
	[get-canvas (let ([c #f])
		      (lambda () 
			(unless c
			  (set! c (make-canvas))
			  (send c set-editor (get-editor)))
			c))]
	[get-editor (let ([e #f])
		      (lambda () 
			(unless e 
			  (set! e (make-editor))
			  (send (get-canvas) set-editor e))
			e))])
      (sequence
	(do-label)
	(cond
	 [(and file-name (file-exists? file-name))
	  (send (get-editor) load-file file-name 'guess #f)]
	 [file-name
	  (send (get-editor) set-filename file-name)]
	 [else (void)])
	(let ([canvas (get-canvas)])
	  (send canvas focus)))))
  
  (define text<%> (interface (-editor<%>)))
  (define text-mixin
    (mixin (-editor<%>) (text<%>) args
      (override
        [get-editor<%> (lambda () (class->interface text%))]
	[get-editor% (lambda () text:keymap%)])
      (sequence (apply super-init args))))

  (define pasteboard<%> (interface (-editor<%>)))
  (define pasteboard-mixin
    (mixin (-editor<%>) (pasteboard<%>) args
      (override
        [get-editor<%> (lambda () (class->interface pasteboard%))]
	[get-editor% (lambda () pasteboard:keymap%)])
      (sequence (apply super-init args))))

  (define (search-dialog frame)
    (init-find/replace-edits)
    (keymap:call/text-keymap-initializer
     (lambda ()
       (let* ([to-be-searched-text (send frame get-text-to-search)]
	      [to-be-searched-canvas (send to-be-searched-text get-canvas)]

	      [dialog (make-object dialog% "Find and Replace" frame)]

	      [copy-text
	       (lambda (from to)
		 (send to erase)
		 (let loop ([snip (send from find-first-snip)])
		   (when snip
		     (send to insert (send snip copy))
		     (loop (send snip next)))))]
	      

	      [find-panel (make-object horizontal-panel% dialog)]
	      [find-message (make-object message% "Find" find-panel)]
	      [find-field (make-object text-field% #f find-panel void)]
	      [f-text (send find-field get-editor)]

	      [replace-panel (make-object horizontal-panel% dialog)]
	      [replace-message (make-object message% "Replace" replace-panel)]
	      [replace-field (make-object text-field% #f replace-panel void)]
	      [r-text (send replace-field get-editor)]

	      [button-panel (make-object horizontal-panel% dialog)]
	      [pref-check (make-object check-box%
			    "Use separate dialog for searching"
			    dialog
			    (lambda (pref-check evt)
			      (preferences:set
			       'framework:search-using-dialog?
			       (send pref-check get-value))))]

	      [update-texts
	       (lambda ()
		 (send find-edit stop-searching)
		 (copy-text f-text find-edit)
		 (send find-edit start-searching)
		 (copy-text r-text replace-edit))]

	      [find-button (make-object button% "Find" button-panel 
					(lambda x
					  (update-texts)
					  (send frame search-again))
					'(border))]
	      [replace-button (make-object button% "Replace" button-panel
					   (lambda x
					     (update-texts)
					     (send frame replace)))]
	      [replace-button (make-object button% "Replace && Find Again" button-panel
					   (lambda x
					     (update-texts)
					     (send frame replace&search)))]
	      [replace-button (make-object button% "Replace to End" button-panel
					   (lambda x
					     (update-texts)
					     (send frame replace-all)))]
	      [close-button (make-object button% "Close" button-panel
					 (lambda x
					   (send to-be-searched-canvas force-display-focus #f)
					   (send dialog show #f)))])
	 (copy-text find-edit f-text)
	 (copy-text replace-edit r-text)
	 (send find-field min-width 400)
	 (send replace-field min-width 400)
	 (let ([msg-width (max (send find-message get-width)
			       (send replace-message get-width))])
	   (send find-message min-width msg-width)
	   (send replace-message min-width msg-width))
	 (send find-field focus)
	 (send (send find-field get-editor) set-position
	       0
	       (send (send find-field get-editor) last-position))
	 (send pref-check set-value (preferences:get 'framework:search-using-dialog?))
	 (send button-panel set-alignment 'right 'center)
	 (send dialog center 'both)
	 (send to-be-searched-canvas force-display-focus #t)
	 (send dialog show #t)))))
      
  (define searchable<%> (interface (text<%>)
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
			  search-again))
  (define search-anchor 0)
  (define searching-direction 'forward)
  (define (set-searching-direction x) 
    (unless (or (eq? x 'forward)
		(eq? x 'backward))
      (error 'set-searching-direction "expected ~e or ~e, got ~e" 'forward 'backward x))
    (set! searching-direction x))

  (define old-search-highlight void)
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
	  
	  ;; don't draw the anchor
	  '(set! old-search-highlight
		(send edit highlight-range position position color #f))))))

  (define find-string-embedded
    (let ([default-direction 'forward]
	  [default-start 'start]
	  [default-end 'eof]
	  [default-get-start #t]
	  [default-case-sensitive? #t]
	  [default-pop-out? #f])
      (case-lambda
       [(edit str)
	(find-string-embedded edit str default-direction default-start default-end default-get-start default-case-sensitive? default-pop-out?)]
       [(edit str direction) 
	(find-string-embedded edit str direction default-start default-end default-get-start default-case-sensitive? default-pop-out?)]
       [(edit str direction start) 
	(find-string-embedded edit str direction start default-end default-get-start default-case-sensitive? default-pop-out?)]
       [(edit str direction start end) 
	(find-string-embedded edit str direction start end default-get-start default-case-sensitive? default-pop-out?)]
       [(edit str direction start end get-start) 
	(find-string-embedded edit str direction start end get-start default-case-sensitive? default-pop-out?)]
       [(edit str direction start end get-start case-sensitive?) 
	(find-string-embedded edit str direction start end get-start case-sensitive? default-pop-out?)]
       [(edit str direction start end get-start case-sensitive? pop-out?)
	(unless (member direction '(forward backward))
	  (error 'find-string-embedded
		 "expected ~e or ~e as first argument, got: ~e" 'forward 'backward direction))
	(let/ec k
	  (let* ([start (if (eq? start 'start) 
			    (send edit get-start-position)
			    start)]
		 [end (if (eq? 'eof end)
			  (if (eq? direction 'forward)
			      (send edit last-position)
			      0)
			  end)]
		 [flat (send edit find-string str direction
			     start end get-start
			     case-sensitive?)]
		 [pop-out
		  (lambda ()
		    (let ([admin (send edit get-admin)])
		      (if (is-a? admin editor-snip-editor-admin<%>)
			  (let* ([snip (send admin get-snip)]
				 [edit-above (send (send snip get-admin) get-editor)]
				 [pos (send edit-above get-snip-position snip)]
				 [pop-out-pos (if (eq? direction 'forward) (add1 pos) pos)])
			    (find-string-embedded
			     edit-above
			     str
			     direction 
			     pop-out-pos
			     (if (eq? direction 'forward) 'eof 0)
			     get-start
			     case-sensitive?
			     pop-out?))
			  (values edit #f))))])
	    (let loop ([current-snip (send edit find-snip start
					   (if (eq? direction 'forward)
					       'after-or-none
					       'before-or-none))])
	      (let ([next-loop
		     (lambda ()
		       (if (eq? direction 'forward)
			   (loop (send current-snip next))
			   (loop (send current-snip previous))))])
		(cond
		  [(or (not current-snip)
		       (and flat
			    (let* ([start (send edit get-snip-position current-snip)]
				   [end (+ start (send current-snip get-count))])
			      (if (eq? direction 'forward)
				  (and (<= start flat)
				       (< flat end))
				  (and (< start flat)
				       (<= flat end))))))
		   (if (and (not flat) pop-out?)
		       (pop-out)
		       (values edit flat))]
		  [(is-a? current-snip editor-snip%)
		   (let-values ([(embedded embedded-pos)
				 (let ([media (send current-snip get-editor)])
				   (if (and media
					    (is-a? media text%))
				       (begin
					 (find-string-embedded 
					  media 
					  str
					  direction
					  (if (eq? 'forward direction)
					      0
					      (send media last-position))
					  'eof
					  get-start case-sensitive?))
				       (values #f #f)))])
		     (if (not embedded-pos)
			 (next-loop)
			 (values embedded embedded-pos)))]
		  [else (next-loop)])))))])))
  

  (define searching-frame #f)
  (define (set-searching-frame frame)
    (set! searching-frame frame))

  (define find-text%
    (class-asi text:keymap%
      (inherit get-text)
      (rename [super-after-insert after-insert]
	      [super-after-delete after-delete]
	      [super-on-focus on-focus])
      (private
	[get-searching-edit
	 (lambda ()
           (and searching-frame
                (send searching-frame get-text-to-search)))])
      (public
	[search
	 (opt-lambda ([reset-search-anchor? #t] [beep? #t] [wrap? #t])
	   (when searching-frame
	     (let* ([string (get-text)]
		    [top-searching-edit (get-searching-edit)]

		    [searching-edit (let ([focus-snip (send top-searching-edit get-focus-snip)])
				      (if focus-snip
					  (send focus-snip get-editor)
					  top-searching-edit))]
					
		    [not-found
		     (lambda (found-edit)
		       (send found-edit set-position search-anchor)
		       (when beep?
			 (bell))
		       #f)]
		    [found
		     (lambda (edit first-pos)
		       (let ([last-pos ((if (eq? searching-direction 'forward) + -)
					first-pos (string-length string))])
			 (send* edit 
			   (set-caret-owner #f 'display)
			   (set-position
			    (min first-pos last-pos)
			    (max first-pos last-pos)
			    #f #t 'local))
			 #t))])
	       (unless (string=? string "")
		 (when reset-search-anchor?
		   (reset-search-anchor searching-edit))
		 (let-values ([(found-edit first-pos)
			       (find-string-embedded
				searching-edit
				string
				searching-direction
				search-anchor
				'eof #t #t #t)])
		   (cond
		     [(not first-pos)
		      (if wrap?
			  (let-values ([(found-edit pos)
					(find-string-embedded
					 top-searching-edit
					 string 
					 searching-direction
					 (if (eq? 'forward searching-direction)
					     0
					     (send searching-edit last-position)))])
			    (if (not pos)
				(not-found found-edit)
				(found found-edit pos)))
			  (not-found found-edit))]
		     [else
		      (found found-edit first-pos)]))))))])
      (private
	[dont-search #f])
      (public
	[stop-searching
	 (lambda ()
	   (set! dont-search #t))]
	[start-searching
	 (lambda ()
	   (set! dont-search #f))])

      (override
	[on-focus
	 (lambda (on?)
	   (when on?
             (let ([edit (get-searching-edit)])
               (when edit
                 (reset-search-anchor (get-searching-edit)))))
	   (super-on-focus on?))]
	[after-insert
	 (lambda args
	   (apply super-after-insert args)
	   (unless dont-search
	     (search #f)))]
	[after-delete
	 (lambda args
	   (apply super-after-delete args)
	   (unless dont-search
	     (search #f)))])))

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
	     (set-searching-frame (get-top-level-window)))
	   (super-on-focus x))])
      (sequence
	(super-init parent #f '(hide-hscroll hide-vscroll))
	(set-line-count 2))))

  (define (init-find/replace-edits)
    (unless find-edit
      (set! find-edit (make-object find-text%))
      (set! replace-edit (make-object text:keymap%))
      (for-each (lambda (keymap)
		  (send keymap chain-to-keymap
			(keymap:get-search)
			#t))
		(list (send find-edit get-keymap)
		      (send replace-edit get-keymap)))))

  (define searchable-mixin
    (mixin (text<%>) (searchable<%>) args
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
	[edit-menu:find (lambda (menu evt) (move-to-search-or-search) #t)]
	[edit-menu:find-again (lambda (menu evt) (search-again) #t)])
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
	   (send super-root change-children
                 (lambda (l)
                   (mzlib:function:remove search-panel l)))
	   (clear-search-highlight)
	   (unless startup?
	     (send 
	      (send (get-text-to-search) get-canvas) 
	      focus))
	   (set! hidden? #t))]
	[unhide-search
	 (lambda ()
	   (when (and hidden?
                      (not (preferences:get 'framework:search-using-dialog?)))
	     (set! hidden? #f)
	     (send search-panel focus)
	     (send super-root add-child search-panel)
	     (reset-search-anchor (get-text-to-search))))])
      (private
	[remove-callback
	 (preferences:add-callback
	  'framework:search-using-dialog?
	  (lambda (p v)
	    (when p
	      (hide-search))))])
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (remove-callback)
	   (let ([close-canvas
		  (lambda (canvas edit)
		    (send edit remove-canvas canvas)
		    (send canvas set-editor #f))])
	     (close-canvas find-canvas find-edit)
	     (close-canvas replace-canvas replace-edit))
	   (when (eq? this searching-frame)
	     (set-searching-frame #f)))])
      (public
	[set-search-direction 
	 (lambda (x) 
	   (set-searching-direction x)
	   (send dir-radio set-selection (if (eq? x 'forward) 0 1)))]
	[replace&search
	 (lambda ()
	   (when (replace)
	     (search-again)))]
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
	     (when (search-again)
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
	   (set-searching-frame this)
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
	   (set-searching-frame this)
	   (unhide-search)
           (cond
             [(preferences:get 'framework:search-using-dialog?)
              (search-dialog this)]
             [else
              (if (or (send find-canvas has-focus?)
                      (send replace-canvas has-focus?))
                  (search-again 'forward)
                  (send find-canvas focus))]))]
	[move-to-search-or-reverse-search
	 (lambda ()
	   (set-searching-frame this)
	   (unhide-search)
	   (if (or (send find-canvas has-focus?)
		   (send replace-canvas has-focus?))
	       (search-again 'backward)
	       (send find-canvas focus)))]
	[search-again
	 (opt-lambda ([direction searching-direction] [beep? #t])
	   (set-searching-frame this)
	   (unhide-search)
	   (set-search-direction direction)
           (send find-edit search #t beep?))])
      (sequence
	(apply super-init args))
      (private
	[search-panel (make-object horizontal-panel% super-root '(border))]
	
	[left-panel (make-object vertical-panel% search-panel)]
	[find-canvas (make-object searchable-canvas% left-panel)]
	[replace-canvas (make-object searchable-canvas% left-panel)]
	
	[middle-left-panel (make-object vertical-pane% search-panel)]
	[middle-middle-panel (make-object vertical-pane% search-panel)]
	[middle-right-panel (make-object vertical-pane% search-panel)]
	
	[search-button (make-object button% 
			 "Search"
			 middle-left-panel
			 (lambda args (search-again)))]
	
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
		       (let ([forward (if (= (send dir-radio get-selection) 0)
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
		    lock-status-changed
		    update-info
		    set-info-canvas
		    get-info-canvas
		    get-info-editor
		    get-info-panel))

  (define memory-text (make-object text%))

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
       [get-canvas<%>
	(lambda () canvas:info<%>)]
       [get-canvas%
	(lambda () canvas:info%)])

      (private
       [info-canvas #f])
      (public
        [get-info-canvas
	 (lambda ()
	   info-canvas)]
	[set-info-canvas
	 (lambda (c)
	   (set! info-canvas c))]
	[get-info-editor
	 (lambda ()
	   (and info-canvas
		(send info-canvas get-editor)))])

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
	[outer-info-panel 'top-info-panel-uninitialized]
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
			(list rest-panel outer-info-panel)
			(list rest-panel))))))])
      (private
	[memory-cleanup void]) ;; only for PLTers; used with memory-text
      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (unregister-collecting-blit gc-canvas)
	   (close-panel-callback)
	   (memory-cleanup))])
      
      (public
	[lock-status-changed
	 (let ([icon-currently-locked? #f])
	   (lambda ()
	     (let ([info-edit (get-info-editor)])
	       (cond
		[(not (object? lock-message))
		 (void)]
		[info-edit
		 (unless (send lock-message is-shown?)
			 (send lock-message show #t))
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
				   (if locked-now? "Locked" "Unlocked")))))))]
		[else
		 (when (send lock-message is-shown?)
		       (send lock-message show #f))]))))])
      (public
	[update-info
	 (lambda ()
	   (lock-status-changed))])
      (sequence 
	(apply super-init args))

      (public
	[get-info-panel
	 (begin
	   (set! outer-info-panel (make-object horizontal-panel% super-root))
	   (let ([info-panel (make-object horizontal-panel% outer-info-panel)]
		 [spacer (make-object grow-box-spacer-pane% outer-info-panel)])
	     (lambda ()
	       (send outer-info-panel stretchable-height #f)
	       info-panel)))])
      (sequence
        ;; only for PLTers
        (when (directory-exists? (build-path (collection-path "framework") "CVS"))
          (let* ([panel (make-object horizontal-panel% (get-info-panel) '(border))]
                 [update-text
                  (lambda ()
                    (send memory-text begin-edit-sequence)
                    (send memory-text erase)
                    (send memory-text insert (number->string (current-memory-use)))
                    (send memory-text end-edit-sequence))]
                 [button (make-object button% "Memory" panel 
                           (lambda x
                             (collect-garbage)(collect-garbage)(collect-garbage)
                             (update-text)))]
                 [ec (make-object editor-canvas% panel memory-text '(no-hscroll no-vscroll))])
            (determine-width "000000000" ec memory-text)
            (update-text)
	    (set! memory-cleanup
		  (lambda ()
		    (send memory-text remove-canvas ec)
		    (send ec set-editor #f)))
            (send panel stretchable-width #f))))
      (private
	[lock-message (make-object message%
			(let ([b (icon:get-unlock-bitmap)])
			  (if (and #f (send b ok?))
			      b
			      "Unlocked"))
			(get-info-panel))]
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
	       (border 3)))))

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
        [get-editor<%> (lambda () editor:info<%>)]
        [get-editor% (lambda () text:info%)])

      (override
	[on-close
	 (lambda ()
	   (super-on-close)
	   (remove-pref-callback))])
      
      (private
	[editor-position-changed-offset/numbers
	 (let ([last-start #f]
	       [last-end #f]
               [last-params #f])
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
	       (cond
		[(not (object? position-canvas))
		 (void)]
		[edit
		 (unless (send position-canvas is-shown?)
			 (send position-canvas show #t))
		 (let ([start (send edit get-start-position)]
		       [end (send edit get-end-position)])
		   (unless (and last-start
                                (equal? last-params (list offset? line-numbers?))
				(= last-start start)
				(= last-end end))
                     (set! last-params (list offset? line-numbers?))
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
			      (lock #t)))))]
		[else
		 (when (send position-canvas is-shown?)
		       (send position-canvas show #f))]))))])
      (public
	[anchor-status-changed
	 (let ([last-state? #f])
	   (lambda ()
	     (let ([info-edit (get-info-editor)]
		   [failed
		    (lambda ()
		      (unless (eq? last-state? #f)
			(set! last-state? #f)
			(send anchor-message show #f)))])
	       (cond
		[info-edit
		 (let ([anchor-now? (send info-edit get-anchor)])
		   (unless (eq? anchor-now? last-state?)
		     (cond
		      [(object? anchor-message)
		       (send anchor-message
			     show
			     anchor-now?)
		       (set! last-state? anchor-now?)]
		      [else (failed)])))]
		[else
		 (failed)]))))]
	[editor-position-changed
	 (lambda ()
	   (editor-position-changed-offset/numbers
	    (preferences:get 'framework:line-offsets)
	    (preferences:get 'framework:display-line-numbers)))]
	[overwrite-status-changed
	 (let ([last-state? #f])
	   (lambda ()
	     (let ([info-edit (get-info-editor)]
		   [failed
		    (lambda ()
		      (set! last-state? #f)
		      (send overwrite-message show #f))])
	       (cond
		[info-edit
		 (let ([overwrite-now? (send info-edit get-overwrite-mode)])
		   (unless (eq? overwrite-now? last-state?)
		     (cond
		      [(object? overwrite-message)
		       (send overwrite-message
			     show
			     overwrite-now?)
		       (set! last-state? overwrite-now?)]
		      [else
		       (failed)])))]
		[else
		 (failed)]))))])
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
		 "Auto-extend Selection"))
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
  
  (define pasteboard-info<%> (interface (info<%>)))
  (define pasteboard-info-mixin
    (mixin (info<%>) (pasteboard-info<%>) args
      (override
        [get-editor% (lambda () pasteboard:info%)])
      (sequence
	(apply super-init args))))

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
  (define pasteboard-info% (pasteboard-info-mixin (info-mixin -pasteboard%)))
  (define pasteboard-info-file% (file-mixin pasteboard-info%))
  
  )
