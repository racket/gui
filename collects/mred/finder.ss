;;; finder.ss

;;; Authors: Matthew Flatt, Robby Findler, Paul Steckler 

  (unit/sig mred:finder^
    (import [wx : wx^]
	    [mred:constants : mred:constants^]
	    [mred:container : mred:container^]
	    [mred:preferences : mred:preferences^]
	    [mred:gui-utils : mred:gui-utils^]
	    [mred:edit : mred:edit^]
	    [mred:canvas : mred:canvas^]
	    [mzlib:string : mzlib:string^]
	    [mzlib:function : mzlib:function^]
	    [mzlib:file : mzlib:file^])
	    
    (mred:debug:printf 'invoke "mred:finder@")

    (define filter-match?
      (lambda (filter name msg)
	(let-values ([(base name dir?) (split-path name)])
	  (if (mzlib:string:regexp-match-exact? filter name)
	      #t
	      (begin
		(wx:message-box msg "Error")
		#f)))))
    
    (define last-directory #f)
    
    (define make-relative
      (lambda (s) s))
    
    (define current-find-file-directory
      (opt-lambda ([dir 'get])
	(cond
	 [(eq? dir 'get)
	  (if (not last-directory)
	      (set! last-directory (current-directory)))
	  last-directory]
	 [(and (string? dir)
	       (directory-exists? dir))
	  (set! last-directory dir)
	  #t]
	 [else #f])))

    (define get-slash
      (lambda ()
	(case wx:platform
	  [(unix) "/"]
	  [(windows) "\\"]
	  [else ":"])))

    (define build-updir
      (lambda (dir)
	(let ([components (mzlib:file:explode-path dir)]
	      [slash (get-slash)])
	  (letrec 
	      ([loop 
		(lambda (comps)
		  (cond
		   [(null? comps) ""]
		   [(equal? (car comps) slash)
		    (string-append slash (loop (cdr comps)))]
		   [(eq? (length comps) 1) ""]
		   [else (let ([rest (loop (cdr comps))])
			   (if (string=? rest "")
			       (car comps)
			       (build-path (car comps) rest)))]))])
	    (loop components)))))

    (mred:preferences:set-preference-default 'mred:show-periods-in-dirlist #f
					     (lambda (x)
					       (or (not x)
						   (eq? x #t))))

    ; the finder-dialog% class controls the user interface for dialogs

    (define finder-dialog%
      (class mred:container:dialog-box% (parent-win
					 save-mode? 
					 replace-ok? 
					 multi-mode? 
					 result-box 
					 start-dir 
					 start-name 
					 prompt 
					 file-filter 
					 file-filter-msg)

	(inherit new-line tab fit center
		 show
		 popup-menu)
	
	(private
	  [WIDTH 500]
	  [HEIGHT 400]
	  dirs 
	  current-dir
	  last-selected
	  [select-counter 0])
	
	(private

	  [set-directory ; sets directory in listbox 
	   (lambda (dir) ; dir is normalized
	     (mred:gui-utils:show-busy-cursor
	      (lambda ()
		(set! current-dir dir)
		(set! last-directory dir)
		(let-values 
		    ([(dir-list menu-list)
		      (let loop ([this-dir dir]
				 [dir-list ()]
				 [menu-list ()])
			(let-values ([(base-dir in-dir dir?) (split-path this-dir)])
			  (if (eq? wx:platform 'windows)
			      (mzlib:string:string-lowercase! in-dir))
			  (let* ([dir-list (cons this-dir dir-list)]
				 [menu-list (cons in-dir menu-list)])
			    (if base-dir
				(loop base-dir dir-list menu-list)
				; No more
				(values dir-list menu-list)))))])
		  (set! dirs (reverse dir-list))
		  (send dir-choice clear)
		  (let loop ([choices (reverse menu-list)])
		    (unless (null? choices)
		      (send dir-choice append (car choices))
		      (loop (cdr choices))))
		  (send dir-choice set-selection 0)
		  (send top-panel force-redraw))
		
		(send name-list clear)
		(send name-list set
		      (mzlib:function:quicksort
		       (let ([no-periods? (not (mred:preferences:get-preference
						'mred:show-periods-in-dirlist))])
			 (let loop ([l (directory-list dir)])
			   (if (null? l)
			       null
			       (let ([s (car l)]
				     [rest (loop (cdr l))])
				 (cond
				   [(and no-periods?
					 (<= 1 (string-length s))
					 (char=? (string-ref s 0) #\.))
				    rest]
				   [(directory-exists? (build-path dir s))
				    (cons (string-append s (get-slash))
					  rest)]
				   [(or (not file-filter)
					(mzlib:string:regexp-match-exact? file-filter s))
				    (cons s rest)]
				   [else rest])))))
		       (if (eq? wx:platform 'unix) string<? string-ci<?)))
		(send name-list set-selection-and-edit 0)
		(set! last-selected -1))))]

	  [set-edit
	   (lambda ()
	     (let* ([file (send name-list get-string-selection)]
		    [dir-and-file 
		     (if (null? file)
			 current-dir
			 (build-path current-dir file))])
	       (send* directory-edit
		      (begin-edit-sequence)
		      (erase)
		      (insert dir-and-file)
		      (end-edit-sequence))
	       (when save-mode?
		     (let ([fullname (build-path current-dir
					      (make-relative file))])
		       (send name-field set-value
			     (if (directory-exists? fullname)
				 ""
				 file))))))])

	(public

	  [do-period-in/exclusion
	   (lambda (button event)
	     (mred:preferences:set-preference 'mred:show-periods-in-dirlist 
					      (send event checked?))
	     (set-directory current-dir))]

	  [do-dir
	   (lambda (choice event)
	     (let ([which (send event get-selection)])
	       (if (< which (length dirs))
		   (set-directory (list-ref dirs which)))))]

	  [do-name
	   (lambda (text event)
	     (if (eq? (send event get-event-type)
		      wx:const-event-type-text-enter-command)
		 (do-ok)))]

	  [do-name-list
	   (lambda (_ event)
	     (if (and (eq? (send event get-event-type)
			   wx:const-event-type-listbox-command)
		      (send event is-selection?))
		 (set-edit)))]

	  [do-result-list
	   (lambda args #f)]
	  
	  [do-ok
	   (lambda args
	     (if multi-mode?
		 (let loop ([n (sub1 select-counter)][result ()])
		   (if (< n 0)
		       (begin
			 (set-box! result-box result)
			 (show #f))
		       (loop (sub1 n) 
			     (cons (send result-list get-string n)
				   result))))
		 (let ([name
			(if save-mode?
			    (send name-field get-value)
			    (send name-list get-string-selection))])
		   (cond
		     [(and save-mode? (not (string? name))) 'nothing-selected]
		     [(and save-mode? (string=? name ""))
		      (let ([file (send directory-edit get-text)])
			(if (directory-exists? file)
			    (set-directory (mzlib:file:normalize-path file))
			    (wx:message-box 
			     "You must specify a file name"
			     "Error")))]
		     [(and save-mode? 
			   file-filter 
			   (not (mzlib:string:regexp-match-exact? file-filter name)))
		      (wx:message-box file-filter-msg "Error")]
		     [else

		      ; if dir in edit box, go to that dir

		      (let ([dir-name (send directory-edit get-text)])
			(if (directory-exists? dir-name)
			    (set-directory (mzlib:file:normalize-path dir-name))

			    ; otherwise, try to return absolute path

			    (let* ([relative-name (make-relative name)]
				   [file (if relative-name
					     (build-path current-dir relative-name)
					     dir-name)])
			      (let ([dir-name-len (string-length dir-name)]
				    [rel-name-len (string-length relative-name)])
				(if (and (not save-mode?)
				         relative-name
					 (or (> rel-name-len dir-name-len)
					     (not (string=? relative-name
							    (substring dir-name
								   (- dir-name-len
								      rel-name-len)
								   dir-name-len)))))
				    (wx:message-box 
				     (string-append "File \"" 
						    dir-name
						    "\" does not exist"))
				    (if (or (not save-mode?)
					    (not (file-exists? file))
					    replace-ok?
					    (= (wx:message-box
						(string-append
						 "The file " 
						 name 
						 " already exists. "
						 "Replace it?")
						"Warning"
						wx:const-yes-no)
					       wx:const-yes))
					(begin
					  (set-box! 
					   result-box (mzlib:file:normalize-path file))
					  (show #f))))))))]))))]
	  
	  [add-one
	   (lambda (name)
	     (unless (or (directory-exists? name)
			 (> (send result-list find-string name) -1))
	       (set! select-counter (add1 select-counter))
	       (send result-list append (mzlib:file:normalize-path name))))]

	  [do-add
	   (lambda args
	     (let ([name (send name-list get-string-selection)])
	       (if (string? name)
		   (let ([name (build-path current-dir
					   (make-relative name))])
		     (add-one name)))))]

	  [do-add-all
	   (lambda args
	     (let loop ([n 0])
	       (let ([name (send name-list get-string n)])
		 (if (and (string? name)
			  (positive? (string-length name)))
		     (let ([name (build-path current-dir
					     (make-relative name))])
		       (add-one name)
		       (loop (add1 n)))))))]

	  [do-remove
	   (lambda args
	     (let loop ([n 0])
	       (if (< n select-counter)
		   (if (send result-list selected? n)
		       (begin
			 (send result-list delete n)
			 (set! select-counter (sub1 select-counter))
			 (loop n))
		       (loop (add1 n))))))]
	  
	  [do-cancel
	   (lambda args
	     (set-box! result-box #f)
	     (show #f))]
	  
	  [on-close (lambda () #f)])

	(sequence
	  (super-init parent-win
		      (if save-mode? "Put file" "Get file") 
		      #t 
		      300 
		      300 
		      WIDTH 
		      HEIGHT))
	
	(private

	  [main-panel (make-object mred:container:vertical-panel% this)]

	  [top-panel (make-object mred:container:horizontal-panel% main-panel)]

	  [_1 (make-object mred:container:message% top-panel prompt)]

	  [dir-choice (make-object mred:container:choice% top-panel do-dir '())]

	  [middle-panel (make-object mred:container:horizontal-panel% main-panel)]
	  [left-middle-panel (make-object mred:container:vertical-panel% middle-panel)]
	  [right-middle-panel (when multi-mode? 
				    (make-object mred:container:vertical-panel% middle-panel))]
	  [name-list%

	   (class-asi mred:container:list-box%

	     (inherit 
		 get-first-item
	         get-string
	         get-selection
	         get-string-selection
		 number
		 number-of-visible-items
		 set-first-item
		 set-focus
	         set-selection)

	     (public

	      [set-selection-and-edit ; set selection, update edit box

	       (lambda (pos)
		 (when (> (number) 0)
		     (let* ([first-item (get-first-item)]
			    [last-item (sub1 (+ (number-of-visible-items) 
						first-item))])
		       (if (or (< pos first-item) (> pos last-item))
			   (set-first-item pos))
		       (set-selection pos)))
		 (set-edit))]

              [pre-on-char ; set selection according to keystroke

	       (lambda (_ key) 
		 (let ([code (send key get-key-code)]
		       [num-items (number)]
		       [curr-pos (get-selection)])

		   (cond 

		    [(or (= code 10) (= code 13))     ; CR or LF
		     (do-ok)]
		     
		    ; look for letter at beginning of a filename

		    [(and (>= code 32) (<= code 127)) ; ASCII-dependent
		                                      ; but who uses EBCDIC?
		     (letrec 
			 ([loop
			   (lambda (pos)
			     (unless 
			      (>= pos num-items)
			      (let ([first-char (string-ref (get-string pos) 0)])
				(if (eq? code (char->integer first-char))
				    (set-selection-and-edit pos)
				    (loop (add1 pos))))))])
		       (loop (add1 curr-pos)))]

		    ; movement keys

		    [(and (= code wx:const-k-up) 
			  (> curr-pos 0))
		     (set-selection-and-edit (sub1 curr-pos))]

		    [(and (= code wx:const-k-down)
			  (< curr-pos (sub1 num-items)))
		     (let* ([num-vis (number-of-visible-items)] 
			    [curr-first (get-first-item)]
			    [new-curr-pos (add1 curr-pos)]
			    [new-first (if (< new-curr-pos (+ curr-first num-vis))
					   curr-first ; no scroll needed
					   (add1 curr-first))])
		     (set-first-item new-first)
		     (set-selection-and-edit new-curr-pos))]

		    [(and (= code wx:const-k-prior)
			  (> curr-pos 0))
		     (let* ([num-vis (number-of-visible-items)]
			    [new-first (- (get-first-item) num-vis)])
		       (set-first-item (max new-first 0))
		       (set-selection-and-edit (max 0 (- curr-pos num-vis))))]

		    [(and (= code wx:const-k-next)
			  (< curr-pos (sub1 num-items)))
		     (let* ([num-vis (number-of-visible-items)]
			    [new-first (+ (get-first-item) num-vis)])
		       (set-first-item (min new-first (- (number) num-vis)))
		       (set-selection-and-edit 
			(min (sub1 num-items) (+ curr-pos num-vis))))]

		    [else #f])))]

	      [on-default-action
	       (lambda ()
		 (let* ([which (send name-list get-string-selection)]
			[dir (build-path current-dir
					 (make-relative which))])
		   (if (directory-exists? dir)
		       (set-directory (mzlib:file:normalize-path dir))
		       (if save-mode?
			   (send name-field set-value which)
			   (if multi-mode?
			       (do-add)
			       (do-ok))))))]))]

	  [name-list (make-object name-list%
				  left-middle-panel do-name-list
				  () wx:const-single
				  -1 -1
				  (if multi-mode? (/ WIDTH 2) WIDTH) 300
				  () wx:const-needed-sb)]

	  [set-focus-to-name-list
	   (lambda ()
	     (send name-list set-focus))]

	  [save-panel (when save-mode? (make-object mred:container:horizontal-panel% main-panel))]

	  [directory-panel (make-object mred:container:horizontal-panel% main-panel)]

	  [directory-edit (make-object (class-asi mred:edit:media-edit%
					 (rename [super-on-local-char on-local-char])
					 (public
					   [on-local-char
					    (lambda (key)
					      (let ([cr-code 13]
						    [lf-code 10]
						    [code (send key get-key-code)])
						(if (or (= code cr-code)
							(= code lf-code))
						    (do-ok)
						    (super-on-local-char key))))])))]
						     
	  [dot-panel (when (eq? 'unix wx:platform)
			  (make-object mred:container:horizontal-panel% main-panel))]

	  [bottom-panel (make-object mred:container:horizontal-panel% main-panel)]

	  [result-list
	   (when multi-mode?
	       (make-object mred:container:list-box%
			    right-middle-panel do-result-list
			    () 
			    (if (eq? wx:window-system 'motif)
				wx:const-extended
				wx:const-multiple)
			    -1 -1
			    (/ WIDTH 2) 300
			    () wx:const-needed-sb))]
	  [add-panel 
	   (when multi-mode? 
		 (make-object mred:container:horizontal-panel% left-middle-panel))]

	  [remove-panel 
	   (when multi-mode? 
		 (make-object mred:container:horizontal-panel% right-middle-panel))]

	  [do-updir
	   (lambda () 
	     (set-directory (build-updir current-dir))
	     (set-focus-to-name-list))
	   ])
			
	(sequence

	  (when (eq? wx:platform 'unix)
	    (make-object mred:container:check-box% dot-panel
			 do-period-in/exclusion
			 "Show files and directories that begin with a dot")
	    (send dot-panel stretchable-in-y #f))

	  (send directory-panel stretchable-in-y #f)

	  (let ([canvas (make-object mred:canvas:one-line-canvas% directory-panel -1 -1 -1 20 ""
			      (+ wx:const-mcanvas-hide-h-scroll
				 wx:const-mcanvas-hide-v-scroll))])

	    (send* canvas
		   (set-media directory-edit)
		   (set-focus)
		   (user-min-height 20)))

	  (when multi-mode?
		(send add-panel stretchable-in-y #f)
		(send remove-panel stretchable-in-y #f)
		(send result-list stretchable-in-x #t))

	  (make-object mred:container:button% top-panel
		       (lambda (button evt) (do-updir))
		       "Up directory")

	  (send name-list stretchable-in-x #t)

	  (send top-panel stretchable-in-y #f)

	  (send bottom-panel stretchable-in-y #f)

	  (when save-mode?
	    (send save-panel stretchable-in-y #f)))
	
	(private

	  [name-field
	   (when save-mode?
	     (let* ([% (class-asi mred:container:text%
			 (public
			  [on-default-action
			   (lambda ()
			     (do-ok))]))]
		    [v (make-object %
				    save-panel do-name
				    "Name: " ""
				    -1 -1
				    400 -1
				    wx:const-process-enter)])
	       (send v stretchable-in-x #t)
	       (if (string? start-name)
		   (send v set-value start-name))
	       (new-line)
	       v))]

	  [add-button (when multi-mode?
			(make-object mred:container:horizontal-panel% add-panel)
			(make-object mred:container:button%
				       add-panel do-add
				       "Add"))]
	  [add-all-button (when multi-mode?
			    (begin0
			      (make-object mred:container:button%
					   add-panel do-add-all
					   "Add all")
			      (make-object mred:container:horizontal-panel% add-panel)))]
	  [remove-button (when multi-mode?
			   (make-object mred:container:horizontal-panel% remove-panel)
			   (begin0
			     (make-object mred:container:button%
					  remove-panel do-remove
					  "Remove")
			     (make-object mred:container:horizontal-panel% remove-panel)))])
	(sequence
	  (make-object mred:container:vertical-panel% bottom-panel)) 
	(private
	  [cancel-button (make-object mred:container:button%
				      bottom-panel do-cancel
				      "Cancel")]
	  [ok-button
	   (let ([w (send cancel-button get-width)])
	     (make-object mred:container:button%
			  bottom-panel do-ok
			  "OK" -1 -1 w))])
	(sequence
	  (cond
	    [(and start-dir
		  (not (null? start-dir))
		  (directory-exists? start-dir))
	     (set-directory (mzlib:file:normalize-path start-dir))]
	    [last-directory (set-directory last-directory)]
	    [else (set-directory (current-directory))])
	  
	  (send ok-button user-min-width (send cancel-button get-width))

	  (center wx:const-both)
	  
	  (show #t))))

    ; make-common takes a dialog function

    (define make-common
      (lambda (make-dialog)
	(let ([s (make-semaphore 1)]
	      [v (box #f)]
	      [d #f])                 ; d is a flag, what does it mean?
	  (lambda x
	    (semaphore-wait s)
	    (if d                
		(let ([my-d d]        ; this case isn't used currently
		      [my-v v])
		  (set! d #f)
		  (set! v #f)
		  (semaphore-post s)
		  (send my-d show #t)
		  (begin0 (unbox my-v)
			  (semaphore-wait s)
			  (set! d my-d)
			  (set! v my-v)
			  (semaphore-post s)))
		(begin
		  (semaphore-post s)
		  (let* ([my-v (box #f)]
			 [my-d (apply make-dialog my-v x)])
		    (semaphore-wait s)
		    (unless d         ; I don't understand this, since d, v not used - PAS
		      (set! d my-d)
		      (set! v my-v))
		    (begin0 (unbox my-v)
			    (semaphore-post s)))))))))

    ; the common versions of these functions have their visual
    ; interfaces under Scheme control

    (define common-put-file
      (make-common
       (opt-lambda (box 
		    [parent-win null]
		    [name ()]
		    [directory ()]
		    [replace? #f]
		    [prompt "Select file"]
		    [filter #f]
		    [filter-msg  "Invalid form"])
		   (let* ([directory (if (and (null? directory)
					      (string? name))
					 (or (mzlib:file:path-only name) null)
					 directory)]
			  [name (or (and (string? name)
					 (mzlib:file:file-name-from-path name))
				    name)])
		     (make-object finder-dialog% 
				  parent-win
				  #t 
				  replace? 
				  #f 
				  box 
				  directory 
				  name 
				  prompt 
				  filter 
				  filter-msg)))))
    
    (define common-get-file
      (make-common
       (opt-lambda
	   (box [parent-win null]
		[directory ()]
		[prompt "Select file"]
		[filter #f]
		[filter-msg "Bad name"])
	 (make-object finder-dialog% 
		      parent-win   ; parent window
                      #f           ; save-mode?
		      #f           ; replace-ok?
		      #f           ; multi-mode?
		      box          ; result-box
		      directory    ; start-dir
		      '()          ; start-name
		      prompt       ; prompt
		      filter       ; file-filter
		      filter-msg   ; file-filter-msg
		      ))))

    (define common-get-file-list
      (make-common
       (opt-lambda (box [directory ()][prompt "Select files"][filter #f]
			[filter-msg "Bad name"])
	 (make-object finder-dialog% #f #f #t box directory '() prompt 
		      filter filter-msg))))

    ; the std- versions of these functions rely on wx: for their
    ; visible interfaces

    ; the std- and common- forms both have opt-lambda's, with the same
    ; list of args.  Should the opt-lambda's be placed in the dispatching function?

    (define std-put-file
      (opt-lambda ([name ()]
		   [directory ()]
		   [replace? #f]
		   [prompt "Select file"]
		   [filter #f]
		   [filter-msg "That filename does not have the right form."]

		   ; quick-and-dirty solution ... probably should be first arg

		   [parent-win null])
	(let* ([directory (if (and (null? directory)
				   (string? name))
			      (or (mzlib:file:path-only name) null)
			      directory)]
	       [name (or (and (string? name)
			      (mzlib:file:file-name-from-path name))
			 name)]
	       [f (wx:file-selector 
		   prompt 
		   directory 
		   name
		   '()
		   (if (eq? wx:platform 'windows) "*.*" "*")
		   wx:const-save
		   parent-win)])
	  (if (or (null? f)
		  (and filter 
		       (not (filter-match? filter 
					   f
					   filter-msg))))
	      #f
	      (let* ([f (mzlib:file:normalize-path f)]
		     [dir (mzlib:file:path-only f)]
		     [name (mzlib:file:file-name-from-path f)])
		(cond
		 [(not (and (string? dir) (directory-exists? dir)))
		  (wx:message-box "Error" "That directory does not exist.")
		  #f]
		 [(or (not name) (equal? name ""))
		  (wx:message-box "Error" "Empty filename.")
		  #f]
		 [else f]))))))

    (define std-get-file
      (opt-lambda ([directory ()]
		   [prompt "Select file"]
		   [filter #f]
		   [filter-msg "That filename does not have the right form."]

		   ; quick-and-dirty solution ... probably should be 1st arg

		   [parent-win null])
	(let ([f (wx:file-selector 
		  prompt 
		  directory 
		  null 
		  null 
		  "*" 
		  0 
		  parent-win)])
	  (if (null? f)
	      #f
	      (if (or (not filter) (filter-match? filter f filter-msg))
		  (let ([f (mzlib:file:normalize-path f)])
		    (cond
		     [(directory-exists? f)
		      (wx:message-box "Error" 
				      "That is a directory name.")
		      #f]
		     [(not (file-exists? f))
		      (wx:message-box "File does not exist.")
		      #f]
		     [else f]))
		  #f)))))

    (mred:preferences:set-preference-default 'mred:file-dialogs
					     (if (eq? wx:platform 'unix)
						 'common
						 'std)
					     (lambda (x)
					       (or (eq? x 'common)
						   (eq? x 'std))))

    ; external interfaces to file functions

    (define put-file
      (lambda args
	(let ([actual-fun 
	       (case (mred:preferences:get-preference 'mred:file-dialogs)
		 [(std) std-put-file]
		 [(common) common-put-file])])
	(apply actual-fun args))))

    (define get-file
      (lambda args
	(let ([actual-fun
	       (case (mred:preferences:get-preference 'mred:file-dialogs)
		 [(std) std-get-file]
		 [(common) common-get-file])])
	  (apply actual-fun args)))))



