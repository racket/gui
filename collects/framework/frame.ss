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
	  [mzlib:function : mzlib:function^])

  (rename [-editor<%> editor<%>]
	  [-pasteboard% pasteboard%]
	  [-pasteboard<%> pasteboard<%>]
	  [-text% text%]
	  [-text<%> text<%>])

  (define frame-width 600)
  (define frame-height 650)
  (let-values ([(w h) (get-display-size)])
    (when (< w frame-width)
      (set! frame-width (- (unbox w) 65)))
    (when (< w frame-height)
      (set! frame-height (- (unbox h) 65))))

  (define basic<%> (interface ()
		     get-panel%
		     make-root-panel))
  (define make-basic%
    (mixin (frame<%>) (basic<%>) args
      (rename [super-on-activate on-activate])

      (override
	[can-close?
	 (lambda ()
	   (send group:the-frame-group
		 can-remove-frame?
		 this))]
	[on-close
	 (lambda ()
	   (send group:the-frame-group
		 remove-frame
		 this))])
      (public
	[get-panel% (lambda () vertical-panel%)]
	[get-menu-bar% (lambda () menu-bar%)]
	[make-root-panel
	 (lambda (% parent)
	   (make-object % parent))])
      (rename [super-show show])
      (override
       [show 
	(lambda (on?)
	  (super-show on?)
	  (when on?
	    '(unless (member this (send group:the-frame-group 
				       get-frames))
	      (send group:the-frame-group 
		    insert-frame this))))]
	[on-activate
	 (lambda (active?)
	   (super-on-activate active?)
	   '(when active?
	     (send group:the-frame-group set-active-frame this)))])

      (sequence
	(apply super-init args))
      (public
	[menu-bar (make-object (get-menu-bar%) this)]
	[panel (make-root-panel (get-panel%) this)])))

  (define standard-menus<%>
    (interface (basic<%>)
      get-menu%
      get-menu-item%

      edit-menu
      edit-menu:after-standard-items
      edit-menu:between-clear-and-select-all
      edit-menu:between-copy-and-paste
      edit-menu:between-cut-and-copy
      edit-menu:between-paste-and-clear
      edit-menu:between-redo-and-cut
      edit-menu:between-find-and-preferences
      edit-menu:between-select-all-and-find
      edit-menu:clear
      edit-menu:clear-help-string
      edit-menu:clear-menu
      edit-menu:clear-string
      edit-menu:copy
      edit-menu:copy-help-string
      edit-menu:copy-menu
      edit-menu:copy-string
      edit-menu:cut
      edit-menu:cut-help-string
      edit-menu:cut-menu
      edit-menu:cut-string
      edit-menu:find
      edit-menu:find-help-string
      edit-menu:find-menu
      edit-menu:find-string
      edit-menu:paste
      edit-menu:paste-help-string
      edit-menu:paste-menu
      edit-menu:paste-string
      edit-menu:preferences
      edit-menu:preferences-help-string
      edit-menu:preferences-menu
      edit-menu:redo
      edit-menu:redo-help-string
      edit-menu:redo-menu
      edit-menu:redo-string
      edit-menu:select-all
      edit-menu:select-all-help-string
      edit-menu:select-all-menu
      edit-menu:select-all-string
      edit-menu:undo
      edit-menu:undo-help-string
      edit-menu:undo-menu
      edit-menu:undo-string
      file-menu
      file-menu:after-quit
      file-menu:between-close-and-quit
      file-menu:between-new-and-open
      file-menu:between-open-and-revert
      file-menu:between-revert-and-save
      file-menu:between-print-and-close
      file-menu:between-save-and-print
      file-menu:close
      file-menu:close-help-string
      file-menu:close-menu
      file-menu:close-string
      file-menu:new
      file-menu:new-help-string
      file-menu:new-menu
      file-menu:new-string
      file-menu:open
      file-menu:open-help-string
      file-menu:open-menu
      file-menu:open-string
      file-menu:print
      file-menu:print-help-string
      file-menu:print-menu
      file-menu:print-string
      file-menu:quit
      file-menu:quit-help-string
      file-menu:quit-menu
      file-menu:quit-string
      file-menu:revert
      file-menu:revert-help-string
      file-menu:revert-menu
      file-menu:revert-string
      file-menu:save
      file-menu:save-as
      file-menu:save-as-help-string
      file-menu:save-as-menu
      file-menu:save-as-string
      file-menu:save-help-string
      file-menu:save-menu
      file-menu:save-string
      help-menu
      help-menu:about
      help-menu:about-help-string
      help-menu:about-menu
      help-menu:about-string
      help-menu:after-about
      windows-menu))

  (define make-standard-menus%
    (begin-elaboration-time
     (let-struct between (menu name procedure)
       (let-struct an-item (name help-string proc key menu-string-before menu-string-after)
	 (letrec ([build-id
		   (lambda (name post)
		     (let* ([name-string (symbol->string name)]
			    [answer (string->symbol (string-append name-string post))])
		       answer))]
		  [menu-name->id
		   (lambda (name-string)
		     (let ([file-menu? (string=? (substring name-string 0 9) "file-menu")]
			   [edit-menu? (string=? (substring name-string 0 9) "edit-menu")]
			   [windows-menu? (string=? (substring name-string 0 9) "windows-m")]
			   [help-menu? (string=? (substring name-string 0 9) "help-menu")])
		       (cond
			[file-menu? 'file-menu]
			[edit-menu? 'edit-menu]
			[windows-menu? 'windows-menu]
			[help-menu? 'help-menu]
			[else (printf "WARNING: defaulting item to file-menu ~s~n" name-string)
			      'file-menu])))]

		  [build-fill-in-item-clause
		   (lambda (item)
		     (let ([name (an-item-name item)]
			   [help-string (an-item-help-string item)]
			   [proc (an-item-proc item)])
		       `(public 
			  [,name ,proc]
			  [,(build-id name "-string") ""]
			  [,(build-id name "-help-string") ,help-string])))]
		  [build-fill-in-between-clause
		   (lambda (between)
		     (let ([menu (between-menu between)]
			   [name (between-name between)]
			   [procedure (between-procedure between)])
		       `(public
			  [,(string->symbol
			     (string-append
			      (symbol->string menu)
			      ":"
			      (symbol->string name)))
			   ,procedure])))]
		  [build-item-menu-clause
		   (lambda (item)
		     (let* ([name (an-item-name item)]
			    [name-string (symbol->string name)]
			    [menu-before-string (an-item-menu-string-before item)]
			    [menu-after-string (an-item-menu-string-after item)]
			    [key (an-item-key item)]
			    [join (lambda (base special suffix)
				    (if (string=? special "")
					(string-append base suffix)
					(string-append base " " special suffix)))])
		       `(public
			  [,(build-id name "-menu")
			   (and ,name
				(make-object
				    (get-menu-item%)
				  (,join ,menu-before-string
					 ,(build-id name "-string")
					 ,menu-after-string)
				  ,(menu-name->id name-string)
				  ,name
				  ,key
				  ,(build-id name "-help-string")))])))]
		  [build-between-menu-clause
		   (lambda (between)
		     `(sequence
			(,(string->symbol
			   (string-append
			    (symbol->string (between-menu between))
			    ":"
			    (symbol->string (between-name between))))
			 ,(between-menu between))))]
		  [items
		   (let ([between-nothing (lambda (menu) (void))]
			 [between-separator (lambda (menu) (make-object separator-menu-item% menu))])
		     (list (make-an-item 'file-menu:new "Open a new file"
					 '(lambda (item control) (handler:edit-file #f) #t)
					 #\n "&New" "")
			   (make-between 'file-menu 'between-new-and-open between-nothing)
			   (make-an-item 'file-menu:open "Open a file from disk"
					 '(lambda (item control) (handler:open-file) #t)
					 #\o "&Open" "...")
			   (make-between 'file-menu 'between-open-and-revert between-nothing)
			   (make-an-item 'file-menu:revert 
					 "Revert this file to the copy on disk"
					 #f #f "&Revert" "")
			   (make-between 'file-menu 'between-revert-and-save between-nothing)
			   (make-an-item 'file-menu:save "" #f "s" "&Save" "")
			   (make-an-item 'file-menu:save-as "" #f #f "Save" " &As...")
			   (make-between 'file-menu 'between-save-and-print between-separator)
			   (make-an-item 'file-menu:print "" #f "p" "&Print" "...")
			   (make-between 'file-menu 'between-print-and-close between-separator)
			   (make-an-item 'file-menu:close "" 
					 '(lambda (item control) (when (on-close) (show #f)) #t)
					 #\w "&Close" "")
			   (make-between 'file-menu 'between-close-and-quit between-nothing)
			   (make-an-item 'file-menu:quit "" '(lambda (item control) (exit:exit))
					 #\q
					 '(if (eq? (system-type) 'windows) "E&xit" "Quit")
					 "")
			   (make-between 'file-menu 'after-quit between-nothing)
			   
			   (make-an-item 'edit-menu:undo "" #f #\z "&Undo" "")
			   (make-an-item 'edit-menu:redo "" #f #\y "&Redo" "")
			   (make-between 'edit-menu 'between-redo-and-cut between-nothing)
			   (make-an-item 'edit-menu:cut "" #f #\x "Cu&t" "")
			   (make-between 'edit-menu 'between-cut-and-copy between-nothing)
			   (make-an-item 'edit-menu:copy "" #f #\c "&Copy" "")
			   (make-between 'edit-menu 'between-copy-and-paste between-nothing)
			   (make-an-item 'edit-menu:paste "" #f #\v "&Paste" "")
			   (make-between 'edit-menu 'between-paste-and-clear between-nothing)
			   (make-an-item 'edit-menu:clear "" #f #f
					 '(if (eq? (system-type) 'macos)
					      "Clear"
					      "&Delete")
					 "")
			   (make-between 'edit-menu 'between-clear-and-select-all between-nothing)
			   (make-an-item 'edit-menu:select-all "" #f #\a "Select A&ll" "")
			   (make-between 'edit-menu 'between-select-all-and-find between-nothing)
			   (make-an-item 'edit-menu:find "Search for a string in the buffer"
					 '(lambda (item control) (send this move-to-search-or-search) #t)
					 #\f "Find" "")
			   (make-between 'edit-menu 'between-find-and-preferences between-separator)
			   (make-an-item 'edit-menu:preferences "Configure your preferences"
					 '(lambda (item control) (preferences:show-dialog) #t)
					 #f "Preferences..." "")
			   (make-between 'edit-menu 'after-standard-items between-nothing)
			   
			   (make-an-item 'help-menu:about "About this application"
					 #f
					 #f
					 "About "
					 "...")
			   (make-between 'help-menu 'after-about between-nothing)))])
	   `(mixin (basic<%>) (standard-menus<%>) args
	      (inherit menu-bar on-close show)
	      (public [get-menu% (lambda () menu%)]
		      [get-menu-item% (lambda () menu-item%)])
	      ,@(append 
		 (map (lambda (x)
			(if (between? x)
			    (build-fill-in-between-clause x)
			    (build-fill-in-item-clause x)))
		      items)
		 (list `(sequence (apply super-init args))
		       `(public
			  [file-menu (make-object (get-menu%)
				       (if (eq? (system-type) 'windows)
					   "&File" "F&ile")
				       menu-bar)]
			  
			  [edit-menu (make-object (get-menu%) "&Edit" menu-bar)]
			  [windows-menu (make-object (get-menu%) "&Windows" menu-bar)]
			  [help-menu (make-object (get-menu%) "&Help" menu-bar)]))
		 (map (lambda (x)
			(if (between? x)
			    (build-between-menu-clause x)
			    (build-item-menu-clause x)))
		      items))))))))

  (define -editor<%> (interface (standard-menus<%>)
		       WIDTH
		       HEIGHT
		       get-entire-label
		       get-label-prefix
		       set-label-prefix

		       get-canvas%
		       get-editor%
		       make-edit
		       save-as		      
		       get-canvas
		       get-editor))

  (define make-editor%
    (mixin (standard-menus<%>) (-editor<%>) (file-name)
      (inherit panel get-client-size set-icon get-menu-bar
	       make-menu show active-edit active-canvas)
      (rename [super-can-close? can-close?]
	      [super-make-menu-bar make-menu-bar]
	      [super-set-label set-label])
      (public
	[WIDTH frame-width]
	[HEIGHT frame-height])
	     
      (override
	[can-close?
	 (lambda ()
	   (and (send (get-editor) do-close)
		(super-can-close?)))]
	[get-panel%  (lambda () panel:vertical-edit%)])
      (private
	[label file-name]
	[label-prefix (application:current-app-name)]
	[do-label
	 (lambda ()
	   (super-set-label (get-entire-label))
	   (send group:the-frame-group frame-label-changed this))])
	     
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
	[make-editor (lambda () (make-object (get-editor%)))])
	     
      (public
	[save-as
	 (opt-lambda ([format 'same])
	   (let ([file (parameterize ([finder:dialog-parent-parameter this])
			 (finder:put-file))])
	     (when file
	       (send (get-editor) save-file file format))))])
      (override
	[file-menu:revert 
	 (lambda () 
	   (let* ([b (box #f)]
		  [edit (get-editor)]
		  [filename (send edit get-filename b)])
	     (if (or (not filename) (unbox b))
		 (bell)
		 (let-values ([(start end)
			       (if (is-a? edit text%)
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
			   (when (is-a? edit text%)
			     (send edit set-position start end))
			   (send edit end-edit-sequence))
			 (begin
			   (send edit end-edit-sequence)
			   (message-box
			    "Error Reverting"
			    (format "could not read ~a" filename)))))))
	     #t))]
	[file-menu:save (lambda ()
			  (send (get-editor) save-file)
			  #t)]
	[file-menu:save-as (lambda () (save-as) #t)]
	[file-menu:between-print-and-close
	 (lambda (file-menu)
	   (send file-menu append-separator)
	   (let ([split
		  (lambda (panel%)
		    (lambda ()
		      (when (active-canvas)
			(send panel split (active-canvas) panel%))))])
	     (send file-menu append-item "Split Horizontally" (split horizontal-panel%))
	     (send file-menu append-item "Split Vertically" (split vertical-panel%))
	     (send file-menu append-item "Collapse"
		   (lambda ()
		     (when (active-canvas)
		       (send panel collapse (active-canvas))))))
	   (send file-menu append-separator))]
	[file-menu:print (lambda ()
			   (send (get-editor) print
				 #f
				 #t
				 #t
				 (preferences:get 'framework:print-output-mode))
			   #t)])
	     
      (private
	[edit-menu:do (lambda (const)
			(lambda (menu evt)
			  (let ([edit (active-edit)])
			    (when edit
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
	   (send edit-menu append-separator)
	   (send edit-menu append-item "Insert Text Box"
		 (edit-menu:do 'insert-text-box))
	   (send edit-menu append-item "Insert Graphic Box"
		 (edit-menu:do 'insert-graphic-box))
	   (send edit-menu append-item "Insert Image..."
		 (edit-menu:do 'insert-image))
	   (send edit-menu append-item "Toggle Wrap Text"
		 (lambda ()
		   (let ([edit (active-edit)])
		     (when edit
		       (send edit auto-wrap (not (send edit auto-wrap)))))))
	   (send edit-menu append-separator))])
	     
      (override
	[help-menu:about (lambda (menu evt) (message-box (format "Welcome to ~a" (application:current-app-name))))]
	[help-menu:about-string (application:current-app-name)])
	     
      (sequence (super-init (get-entire-label) #f WIDTH HEIGHT))
	     
      (public
	[get-canvas (let ([c #f])
		      (lambda () 
			(unless c
			  (set! c (make-object (get-canvas%) panel))
			  (send c set-media (get-editor)))
			c))]
	[get-editor (let ([e #f])
		      (lambda () 
			(unless e 
			  (set! e (make-editor))
			  (send (get-canvas) set-media e))
			e))])
      (sequence
	(let ([icon (icon:get)])
	  (when (send icon ok?)
	    (set-icon icon)))
	(do-label)
	(let ([canvas (get-canvas)])
	  (send (get-editor) load-file file-name)
	  (send canvas focus)))))
  
  (define make-text/pasteboard%
    (lambda (% <%>)
      (mixin (editor<%>) (<%>) args
	(override
	  [get-editor% (lambda () %)])
	(sequence (apply super-init args)))))
  (define -text<%> (interface (editor<%>)))
  (define make-text% (make-text/pasteboard% -text% -text<%>))
  (define -pasteboard<%> (interface (pasteboard<%>)))
  (define make-pasteboard% (make-text/pasteboard% -pasteboard% -pasteboard<%>))


  (define searchable<%> (interface ()
			  get-text-to-search
			  hide-search
			  unhide-search
			  set-search-direction
			  replace&search
			  replace-all
			  replace
			  toggle-search-focus
			  move-to-search-or-show-search
			  move-to-search-or-reverse-search
			  search))
  (define make-searchable%
    (let* ([anchor 0]
	   [searching-direction 1]
	   [old-highlight void]
	   [get-active-embedded-edit
	    (lambda (edit)
	      (let loop ([edit edit])
		(let ([snip (send edit get-focus-snip)])
		  (if (or (not snip)
			  (not (is-a? snip editor-snip%)))
		      edit
		      (loop (send snip get-this-media))))))]
	   [clear-highlight
	    (lambda ()
	      (begin (old-highlight)
		     (set! old-highlight void)))]
	   [reset-anchor
	    (let ([color (make-object color% "BLUE")])
	      (lambda (edit)
		(old-highlight)
		(let ([position 
		       (if (= 1 searching-direction)
			   (send edit get-end-position)
			   (send edit get-start-position))])
		  (set! anchor position)
		  (set! old-highlight
			(send edit highlight-range position position color #f)))))]
	   [replace-edit (make-object text%)]
	   [find-edit
	    (make-object
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
		  (opt-lambda ([reset-anchor? #t] [beep? #t] [wrap? #t])
		    (when searching-frame
		      (let* ([string (get-text)]
			     [searching-edit (get-searching-edit)]
			     [not-found
			      (lambda (found-edit)
				(send found-edit set-position anchor)
				(when beep?
				  (bell))
				#f)]
			     [found
			      (lambda (edit first-pos)
				(let ([last-pos (+ first-pos (* searching-direction 
								(string-length string)))])
				  (send* edit 
				    (set-caret-owner #f 'display)
				    (set-position
				     (min first-pos last-pos)
				     (max first-pos last-pos)))
				  #t))])
			(when reset-anchor?
			  (reset-anchor searching-edit))
			(let-values ([(found-edit first-pos)
				      (send searching-edit
					    find-string-embedded
					    string
					    searching-direction
					    anchor
					    -1 #t #t #t)])
			  (cond
			    [(= -1 first-pos)
			     (if wrap?
				 (let-values ([(found-edit pos)
					       (send searching-edit
						     find-string-embedded
						     string 
						     searching-direction
						     (if (= 1 searching-direction)
							 0
							 (send searching-edit last-position)))])
				   (if (= -1 pos)
				       (not-found found-edit)
				       (found found-edit 
					      ((if (= searching-direction 1)
						   +
						   -)
					       pos
					       (string-length string)))))
				 (not-found found-edit))]
			    [else
			     (found found-edit first-pos)])))))]
		 [on-focus
		  (lambda (on?)
		    (when on?
		      (reset-anchor (get-searching-edit)))
		    (super-on-focus on?))]
		 [after-insert
		  (lambda args
		    (apply super-after-insert args)
		    (search #f))]
		 [after-delete
		  (lambda args
		    (apply super-after-delete args)
		    (search #f))])))]
	   [canvas% 
	    (class editor-canvas% args
	      (inherit get-parent frame set-line-count)
	      (rename [super-on-set-focus on-set-focus])
	      (override
		[style-flags '(h-scroll)]
		[on-set-focus
		 (lambda ()
		   (send find-edit set-searching-frame frame)
		   (super-on-set-focus))])
	      (sequence
		(apply super-init args)
		(set-line-count 2)))])
      (for-each (lambda (keymap)
		  (send keymap chain-to-keymap
			keymap:search
			#t))
		(list (send find-edit get-keymap)
		      (send replace-edit get-keymap)))
      (mixin (-text<%>) (searchable<%>) args
	(inherit active-edit active-canvas get-editor)
	(rename [super-make-root-panel make-root-panel]
		[super-on-activate on-activate]
		[super-do-close do-close])
	(private
	  [super-root 'unitiaialized-super-root])
	(override
	 [edit-menu:find (lambda (menu evt) (search))])
	(override
	  [make-root-panel
	   (lambda (% parent)
	     (let* ([s-root (super-make-root-panel
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
		   (reset-anchor (get-text-to-search))
		   (clear-highlight)))
	     (super-on-activate on?))])
	(public
	  [get-text-to-search
	   (lambda () 
	     (get-editor))]
	  [hide-search
	   (opt-lambda ([startup? #f])
	     (send super-root delete-child search-panel)
	     (clear-highlight)
	     (unless startup?
	       (send 
		(send (get-text-to-search) get-canvas) 
		focus))
	     (set! hidden? #t))]
	  [unhide-search
	   (lambda ()
	     (set! hidden? #f)
	     (send super-root add-child search-panel)
	     (reset-anchor (get-text-to-search)))])
	(override
	  [do-close
	   (lambda ()
	     (super-do-close)
	     (let ([close-canvas
		    (lambda (canvas edit)
		      (send edit remove-canvas canvas)
		      (send canvas set-media #f))])
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
		    [pos (if (= searching-direction 1)
			     (send replacee-edit get-start-position)
			     (send replacee-edit get-end-position))]
		    [get-pos 
		     (if (= searching-direction 1)
			 (ivar replacee-edit get-end-position)
			 (ivar replacee-edit get-start-position))]
		    [done? (if (= 1 searching-direction)
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
	     (when hidden?
	       (unhide-search))
	     (send (cond
		     [(send find-canvas is-focus-on?)
		      replace-canvas]
		     [(send replace-canvas is-focus-on?)
		      (send (get-text-to-search) get-canvas)]
		     [else
		      find-canvas])
		   focus))]
	  [move-to-search-or-search
	   (lambda ()
	     (when hidden?
	       (unhide-search))
	     (if (or (send find-canvas is-focus-on?)
		     (send replace-canvas is-focus-on?))
		 (search 1)
		 (send find-canvas focus)))]
	  [move-to-search-or-reverse-search
	   (lambda ()
	     (when hidden?
	       (unhide-search))
	     (if (or (send find-canvas is-focus-on?)
		     (send replace-canvas is-focus-on?))
		 (search -1)
		 (send find-canvas focus)))]
	  [search
	   (opt-lambda ([direction searching-direction] [beep? #t])
	     
	     (send find-edit set-searching-frame this)
	     (when hidden?
	       (unhide-search))
	     (set-search-direction direction)
	     (send find-edit search #t beep?))])
	(sequence
	  (apply super-init args))
	(private
	  [search-panel (make-object horizontal-panel% super-root)]
	  
	  [left-panel (make-object vertical-panel% search-panel)]
	  [find-canvas (make-object canvas% left-panel)]
	  [replace-canvas (make-object canvas% left-panel)]
	  
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
					    1
					    -1)])
			   (set-search-direction forward)
			   (reset-anchor (get-text-to-search)))))]
	  [close-button (make-object button% middle-right-panel
				     (lambda args (hide-search)) "Hide")]
	  [hidden? #f])
	(sequence
	  (let ([align
		 (lambda (x y)
		   (let ([m (max (send x get-width)
				 (send y get-width))])
		     (send x user-min-width m)
		     (send y user-min-width m)))])
	    (align search-button replace-button)
	    (align replace&search-button replace-all-button))
	  (for-each (lambda (x) (send x major-align-center))
		    (list middle-left-panel middle-middle-panel))
	  (for-each (lambda (x) (send x stretchable-in-y #f))
		    (list search-panel left-panel middle-left-panel middle-middle-panel middle-right-panel))
	  (for-each (lambda (x) (send x stretchable-in-x #f))
		    (list middle-left-panel middle-middle-panel middle-right-panel))
	  (send find-canvas set-media find-edit)
	  (send replace-canvas set-media replace-edit) 
	  (send find-edit add-canvas find-canvas)
	  (send replace-edit add-canvas replace-canvas)
	  (hide-search #t)))))
  
  (define info<%> (interface (editor<%>)
		    determine-width
		    get-info-edit
		    lock-status-changed
		    update-info
		    info-panel))
  (define make-info%
    (let* ([time-edit (make-object text%)]
	   [time-semaphore (make-semaphore 1)]
	   [wide-time "00:00pm"]
	   [_ (send time-edit lock #t)]
	   [update-time
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
		 (semaphore-post time-semaphore))))]
	   [time-thread
	    (thread
	     (rec loop
		  (lambda ()
		    (update-time)
		    (sleep 30)
		    (loop))))])
      (mixin (-editor<%>) (info<%>) args
	(rename [super-make-root-panel make-root-panel])
	(private
	  [rest-panel 'uninitialized-root]
	  [super-root 'uninitialized-super-root])
	(override
	  [make-root-panel
	   (lambda (% parent)
	     (let* ([s-root (super-make-root-panel
			     vertical-panel%
			     parent)]
		    [r-root (make-object % s-root)])
	       (set! super-root s-root)
	       (set! rest-panel r-root)
	       r-root))])
	
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
			       (+ magic-space (- (unbox rb) (unbox lb)))))))))])
	
	(rename [super-do-close do-close])
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
			  (list rest-panel info-panel)
			  (list rest-panel))))))])
	(override
	  [do-close
	   (lambda ()
	     (super-do-close)
	     (send time-canvas set-media #f)
	     (unregister-collecting-blit gc-canvas)
	     (close-panel-callback))])
	
	(inherit get-editor)
	(public
	  [get-info-edit
	   (lambda ()
	     (and (procedure? get-editor)
		  (get-editor)))])
	
	(public
	  [lock-status-changed
	   (let ([icon-currently-locked? #f])
	     (lambda ()
	       (let ([info-edit (get-info-edit)])
		 (when info-edit
		   (let ([locked-now? (ivar info-edit locked?)])
		     (unless (eq? locked-now? icon-currently-locked?)
		       (set! icon-currently-locked? locked-now?)
		       (let ([label
			      (if locked-now?
				  (cons (icon:get-lock-mdc)
					(icon:get-lock-bitmap))
				  (cons (icon:get-unlock-mdc)
					(icon:get-unlock-bitmap)))])
			 (send lock-message
			       set-label
			       (if (send (car label) ok?)
				   label
				   (if locked-now? "Locked" "Unlocked"))))))))))])
	(public
	  [update-info
	   (lambda ()
	     (lock-status-changed))])
	(sequence 
	  (apply super-init args))
	
	(public
	  [info-panel (make-object horizontal-panel% 
				   super-root)])
	(private
	  [lock-message (make-object message%
				     (let ([b (icon:get-unlock-bitmap)])
				       (if (send b ok?)
					   (cons (icon:get-unlock-mdc) b)
					   "Unlocked")) 
				     info-panel
				     '(border))]
	  [time-canvas (make-object editor-canvas% info-panel)]
	  [_ (send time-canvas set-line-count 1)]
	  [gc-canvas (make-object canvas% info-panel '(border))]
	  [register-gc-blit
	   (lambda ()
	     (let ([mdc (icon:get-gc-on-dc)])
	       (when (send mdc ok?)
		 (register-collecting-blit gc-canvas 
					   0 0
					   (icon:get-gc-width)
					   (icon:get-gc-height)
					   (icon:get-gc-on-dc)
					   (icon:get-gc-off-dc)))))])
	
	(sequence
	  (unless (preferences:get 'framework:show-status-line)
	    (send super-root change-children
		  (lambda (l)
		    (list rest-panel))))
	  (register-gc-blit)
	  
	  (let ([bw (box 0)]
		[bh (box 0)]
		[gc-width (icon:get-gc-width)]
		[gc-height (icon:get-gc-height)])
	    (send* gc-canvas
	      (set-size 0 0 gc-width gc-height)
	      (get-client-size bw bh))
	    (send* gc-canvas
	      (user-min-client-width gc-width)
	      (user-min-client-height gc-height)
	      (stretchable-in-x #f)
	      (stretchable-in-y #f)))
	  (send* info-panel 
	    (major-align-right)
	    (stretchable-in-y #f)
	    (spacing 3)
	    (border 3))
	  (send* time-canvas 
	    (set-media time-edit)
	    (stretchable-in-x #f))
	  (semaphore-wait time-semaphore)
	  (determine-width wide-time time-canvas time-edit)
	  (semaphore-post time-semaphore)
	  (update-time)))))

  (define edit-info<%> (interface (info<%>)
			 overwrite-status-changed
			 anchor-status-changed
			 edit-position-changed-offset
			 edit-position-changed))
  (define make-edit-info%
    (mixin (info<%>) (edit-info<%>) args
      (inherit get-info-edit)
      (rename [super-do-close do-close])
      (private
	[remove-pref-callback
	 (preferences:add-callback
	  'framework:line-offsets
	  (lambda (p v)
	    (edit-position-changed-offset v)
	    #t))])
      (override
	[do-close
	 (lambda ()
	   (super-do-close)
	   (remove-pref-callback))])
      
      (public
	[overwrite-status-changed
	 (let ([last-state? #f])
	   (lambda ()
	     (let ([info-edit (get-info-edit)])
	       (when info-edit
		 (let ([overwrite-now? (send info-edit get-overwrite-mode)])
		   (unless (eq? overwrite-now? last-state?)
		     (send overwrite-message
			   show
			   overwrite-now?)
		     (set! last-state? overwrite-now?)))))))]
	[anchor-status-changed
	 (let ([last-state? #f])
	   (lambda ()
	     (let ([info-edit (get-info-edit)])
	       (when info-edit
		 (let ([anchor-now? (send info-edit get-anchor)])
		   (unless (eq? anchor-now? last-state?)
		     (send anchor-message
			   show
			   anchor-now?)
		     (set! last-state? anchor-now?)))))))]
	
	[edit-position-changed-offset
	 (let ([last-start #f]
	       [last-end #f])
	   (lambda (offset?)
	     (let* ([edit (get-info-edit)]
		    [make-one
		     (lambda (pos)
		       (let* ([line (send edit position-line pos)]
			      [line-start (send edit line-start-position line)]
			      [char (- pos line-start)])
			 (if (preferences:get 'framework:display-line-numbers)
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
			      (lock #t)))))))))]
	[edit-position-changed
	 (lambda ()
	   (edit-position-changed-offset
	    (preferences:get 'framework:line-offsets)))])
      (rename [super-update-info update-info])
      (override
	[update-info
	 (lambda ()
	   (super-update-info)
	   (overwrite-status-changed)
	   (anchor-status-changed)
	   (edit-position-changed))])
      (sequence 
	(apply super-init args))
      
      (inherit info-panel)
      (private
	[anchor-message 
	 (make-object message%
	   (let ([b (icon:get-anchor-bitmap)])
	     (if (send b ok?)
		 (cons (icon:get-anchor-mdc) b)
		 "Anchor"))
	   info-panel '(border))]
	[overwrite-message 
	 (make-object message%
	   "Overwrite"
	   info-panel
	   '(border))]
	[position-canvas (make-object editor-canvas% info-panel)]
	[_2 (send position-canvas set-line-count 1)]
	[position-edit (make-object text%)])
      
      (inherit determine-width)
      (sequence
	(let ([move-front
	       (lambda (x l)
		 (cons x (mzlib:function:remq x l)))])
	  (send info-panel change-children
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
	       (set-media position-edit)
	       (stretchable-in-x #f))
	(determine-width "0000:000-0000:000" 
			 position-canvas
			 position-edit)
	(edit-position-changed)
	(send position-edit lock #t))))
  
  (define file<%> (interface (-editor<%>)))
  (define make-file%
    (mixin (editor<%>) (file<%>) args
      (inherit get-editor)
      (rename [super-can-close? can-close?])
      (override
	[on-close?
	 (lambda ()
	   (let* ([edit (get-editor)]
		  [user-allowed-or-not-modified
		   (or (not (send edit modified?))
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

  (define empty% (make-basic% frame%))
  (define standard-menus% (make-standard-menus% empty%))
  (define editor% (make-editor% standard-menus%))

  (define -text% (make-text% editor%))
  (define searchable% (make-searchable% editor%))
  (define text-info% (make-info% searchable%))
  (define text-info-file% (make-file% text-info%))

  (define -pasteboard% (make-pasteboard% editor%))
  (define pasteboard-info% (make-info% -pasteboard%))
  (define pasteboard-info-file% (make-file% pasteboard-info%)))
  
  