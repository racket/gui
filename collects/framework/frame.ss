(unit/sig framework:frame^
  (import [group framework:group^])

  (define empty<%> (interface ()
		     get-panel%
		     make-root-panel))
  (define standard-menus<%>
    (interface () 
      get-menu%
      get-menu-item%

      edit-menu
      edit-menu:after-standard-items
      edit-menu:between-clear-and-select-all
      edit-menu:between-copy-and-paste
      edit-menu:between-cut-and-copy
      edit-menu:between-paste-and-clear
      edit-menu:between-redo-and-cut
      edit-menu:between-replace-and-preferences
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
      edit-menu:replace
      edit-menu:replace-help-string
      edit-menu:replace-menu
      edit-menu:replace-string
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
      file-menu:between-open-and-save
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
      file-menu:open-url
      file-menu:open-url-help-string
      file-menu:open-url-menu
      file-menu:open-url-string
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

  (define empty-standard-menus<%> (interface (standard-menus<%> empty<%>)))
  (define edit<%> (interface () FILL-ME-IN))
  (define searchable<%> (interface ()))
  (define pasteboard<%> (interface ()))
  (define info<%> (interface ()))
  (define info-file<%> (interface ()))

  (define frame-width 600)
  (define frame-height 650)
  (let ([w (box 0)]
	[h (box 0)])
    (wx:display-size w h)
    (when (< (unbox w) frame-width)
      (set! frame-width (- (unbox w) 65)))
    (when (< (unbox h) frame-height)
      (set! frame-height (- (unbox h) 65))))

  (define make-empty%
    (mixin frame% empty<%> args
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
					 '(lambda (item control) (mred:handler:edit-file #f) #t)
					 #\n "&New" "")
			   (make-between 'file-menu 'between-new-and-open between-nothing)
			   (make-an-item 'file-menu:open "Open a file from disk"
					 '(lambda (item control) (mred:handler:open-file) #t)
					 #\o "&Open" "...")
			   (make-an-item 'file-menu:open-url "Open a Uniform Resource Locater"
					 '(lambda (item control) (mred:handler:open-url) #t)
					 #f "Open &URL" "...")
			   (make-an-item 'file-menu:revert 
					 "Revert this file to the copy on disk"
					 #f #f "&Revert" "")
			   (make-between 'file-menu 'between-open-and-save between-nothing)
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
			   (make-an-item 'edit-menu:replace "Search and replace a string in the buffer"
					 #f #f "Replace" "")
			   (make-between 'edit-menu 'between-replace-and-preferences between-separator)
			   (make-an-item 'edit-menu:preferences "Configure your preferences"
					 '(lambda (item control) (mred:preferences:show-preferences-dialog) #t)
					 #f "Preferences..." "")
			   (make-between 'edit-menu 'after-standard-items between-nothing)
			   
			   (make-an-item 'help-menu:about "About this application"
					 '(lambda (item control) (mred:console:credits))
					 #f
					 "About "
					 "...")
			   (make-between 'help-menu 'after-about between-nothing)))])
	   `(mixin empty<%> standard-menus<%> args
	      (inherit menu-bar on-close)
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

  (define make-edit%
    (mixin empty-standard-menus<%> frame:simple-menu<%> ([name (mred:application:current-app-name)])
      (inherit panel get-client-size set-icon get-menu-bar
	       make-menu show active-edit active-canvas)
      (rename [super-can-close? can-close?]
	      [super-make-menu-bar make-menu-bar]
	      [super-set-title set-title])
      (public
	[WIDTH frame-width]
	[HEIGHT frame-height])
	     
      (override
	[can-close?
	 (lambda ()
	   (and (send (get-edit) do-close)
		(super-can-close?)))]
	[get-panel%  (lambda () mred:panel:vertical-edit-panel%)])
      (public
	[title-prefix name])
	     
      (private
	[label ""]
	
	[do-label
	 (lambda ()
	   (super-set-label (get-entire-label))
	   (send group:the-frame-group frame-title-changed this))])
	     
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
	[get-canvas% (lambda () mred:canvas:frame-title-canvas%)]
	[get-edit% (lambda () mred:edit:media-edit%)]
	[make-edit (lambda () (make-object (get-edit%)))])
	     
      (public
	[save-as
	 (opt-lambda ([format wx:const-media-ff-same])
	   (let ([file (parameterize ([mred:finder:dialog-parent-parameter
				       this])
			 (mred:finder:put-file))])
	     (when file
	       (send (get-edit) save-file file format))))]
	[file-menu:revert 
	 (lambda () 
	   (let* ([b (box #f)]
		  [edit (get-edit)]
		  [filename (send edit get-filename b)])
	     (if (or (null? filename) (unbox b))
		 (wx:bell)
		 (let-values ([(start end)
			       (if (is-a? edit wx:media-edit%)
				   (values (send edit get-start-position)
					   (send edit get-end-position))
				   (values #f #f))])
		   (send edit begin-edit-sequence)
		   (let ([status (send edit load-file
				       filename
				       wx:const-media-ff-same
				       #f)])
		     (if status
			 (begin
			   (when (is-a? edit wx:media-edit%)
			     (send edit set-position start end))
			   (send edit end-edit-sequence))
			 (begin
			   (send edit end-edit-sequence)
			   (mred:gui-utils:message-box
			    (format "could not read ~a" filename)
			    "Error Reverting"))))))
	     #t))]
	[file-menu:save (lambda ()
			  (send (get-edit) save-file)
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
	     (send file-menu append-item "Split Horizontally" (split mred:container:horizontal-panel%))
	     (send file-menu append-item "Split Vertically" (split mred:container:vertical-panel%))
	     (send file-menu append-item "Collapse"
		   (lambda ()
		     (when (active-canvas)
		       (send panel collapse (active-canvas))))))
	   (send file-menu append-separator))]
	[file-menu:print (lambda ()
			   (send (get-edit) print
				 '()
				 #t
				 #t
				 (mred:preferences:get-preference 'mred:print-output-mode))
			   #t)])
	     
      (private
	[edit-menu:do (lambda (const)
			(lambda (menu evt)
			  (let ([edit (active-edit)])
			    (when edit
			      (send edit do-edit const)))
			  #t))])
	     
      (public
	[edit-menu:undo (edit-menu:do wx:const-edit-undo)]
	[edit-menu:redo (edit-menu:do wx:const-edit-redo)]
	[edit-menu:cut (edit-menu:do wx:const-edit-cut)]
	[edit-menu:clear (edit-menu:do wx:const-edit-clear)]
	[edit-menu:copy (edit-menu:do wx:const-edit-copy)]
	[edit-menu:paste (edit-menu:do wx:const-edit-paste)]
	[edit-menu:select-all (edit-menu:do wx:const-edit-select-all)]
	[edit-menu:replace (lambda (menu evt)
			     (when (active-canvas)
			       (mred:find-string:find-string
				(active-canvas)
				(active-edit)
				-1 -1 (list 'replace 'ignore-case))))]
	
	[edit-menu:between-replace-and-preferences
	 (lambda (edit-menu)
	   (send edit-menu append-separator)
	   (send edit-menu append-item "Insert Text Box"
		 (edit-menu:do wx:const-edit-insert-text-box))
	   (send edit-menu append-item "Insert Graphic Box"
		 (edit-menu:do wx:const-edit-insert-graphic-box))
	   (send edit-menu append-item "Insert Image..."
		 (edit-menu:do wx:const-edit-insert-image))
	   (send edit-menu append-item "Toggle Wrap Text"
		 (lambda ()
		   (let ([edit (active-edit)])
		     (when edit
		       (send edit set-auto-set-wrap (not (ivar edit auto-set-wrap?)))
		       (send (active-canvas) force-redraw)))))
	   (send edit-menu append-separator))])
	     
      (public
	[help-menu:about (lambda (menu evt) (mred:console:credits))]
	[help-menu:about-string (mred:application:current-app-name)]
	[help-menu:compare string-ci<?]
	[help-menu:insert-items
	 (lambda (items)
	   (for-each (lambda (x) (apply (ivar (ivar this help-menu) append-item) x))
		     items))]
	[help-menu:after-about
	 (let ([reg (regexp "<TITLE>(.*)</TITLE>")])
	   (lambda (help-menu)
	     (let* ([dir (with-handlers ([void (lambda (x) #f)]) (collection-path "doc"))])
	       (if (and dir (directory-exists? dir))
		   (let* ([dirs (directory-list dir)]
			  [find-title
			   (lambda (name)
			     (lambda (port)
			       (let loop ([l (read-line port)])
				 (if (eof-object? l)
				     name
				     (let ([match (regexp-match reg l)])
				       (if match
					   (cadr match)
					   (loop (read-line port))))))))]
			  [build-item
			   (lambda (local-dir output)
			     (let* ([f (build-path dir local-dir "index.htm")])
			       (if (file-exists? f)
				   (let ([title (call-with-input-file f (find-title local-dir))])
				     (cons 
				      (list title
					    (lambda ()
					      (let* ([f (make-object mred:hyper-frame:hyper-view-frame%
							  (string-append "file:" f))])
						(send f set-title-prefix title)
						f)))
				      output))
				   (begin (mred:debug:printf 'help-menu "couldn't find ~a" f)
					  output))))]
			  [item-pairs 
			   (mzlib:function:quicksort
			    (mzlib:function:foldl build-item null dirs)
			    (lambda (x y) (help-menu:compare (car x) (car y))))])
		     (unless (null? item-pairs)
		       (send help-menu append-separator))
		     (help-menu:insert-items item-pairs))
		   (mred:debug:printf 'help-menu "couldn't find PLTHOME/doc directory")))))])
	     
      (sequence
	(mred:debug:printf 'super-init "before simple-frame%")
	(super-init () name -1 -1 WIDTH HEIGHT
		    (+ wx:const-default-frame wx:const-sdi)
		    name)
	(mred:debug:printf 'super-init "after simple-frame%"))
	     
      (public
	[get-canvas (let ([c #f])
		      (lambda () 
			(unless c
			  (set! c (make-object (get-canvas%) panel))
			  (send c set-media (get-edit)))
			c))]
	[get-edit (let ([e #f])
		    (lambda () 
		      (unless e 
			(set! e (make-edit))
			(send (get-canvas) set-media e))
		      e))])
      (sequence
	(let ([icon (mred:icon:get-icon)])
	  (when (send icon ok?)
	    (set-icon icon)))
	(do-title)
	(let ([canvas (get-canvas)])
	  (send canvas set-focus)))))

  (define empty% (make-empty% frame%))
  (define standard-menus% (make-standard-menus% empty%))
  (define edit% (make-edit% standard-menus%))
  (define searchable% (make-searchable% edit%))
  (define info% (make-info% searchable%))
  (define info-file% (make-file% info%))
  (define pasteboard% (make-pasteboard% simple-menu%))
  (define pasteboard-info% (make-info% pasteboard%))
  (define pasteboard-info-file% (make-file% pasteboard-info%)))
  
  