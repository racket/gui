
(define mred:finder@
  (unit/sig mred:finder^
    (import [mred:debug : mred:debug^]
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

    (mred:preferences:set-preference-default 'mred:show-periods-in-dirlist #f)

    (define finder-dialog%
      (class mred:container:dialog-box% (save-mode? replace-ok? multi-mode? 
					result-box start-dir 
					start-name prompt 
					file-filter file-filter-msg)
	(inherit new-line tab fit center
		 show
		 popup-menu)
	
	(private
	  [WIDTH 500]
	  [HEIGHT 400]
	  
	  dirs current-dir
	  last-selected
	  [select-counter 0])
	
	(private
	  [set-directory
	   (lambda (dir) ; dir is normalied
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
		  (send* directory-edit 
		    (begin-edit-sequence)
		    (erase)
		    (insert dir)
		    (end-edit-sequence))
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
				    (cons (string-append s
							 (case wx:platform
							   [(unix) "/"]
							   [(windows) "\\"]
							   [else ":"]))
					  rest)]
				   [(or (not file-filter)
					(mzlib:string:regexp-match-exact? file-filter s))
				    (cons s rest)]
				   [else rest])))))
		       (if (eq? wx:platform 'unix) string<? string-ci<?)))
		(set! last-selected -1))))])
	
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
	   (lambda args #f)]
	  [do-result-list
	   (lambda args #f)]
	  
	  [do-into-dir
	   (lambda args
	     (let ([name (send name-list get-string-selection)])
	       (if (string? name)
		   (let ([name (build-path current-dir
					   (make-relative name))])
		     (if (directory-exists? name)
			 (set-directory (mzlib:file:normalize-path name)))))))]
	  
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
		     [(not (string? name)) 'nothing-selected]
		     [(string=? name "")
		      (wx:message-box "You must specify a file name"
				      "Error")]
		     [(and save-mode? 
			   file-filter 
			   (not (mzlib:string:regexp-match-exact? file-filter name)))
		      (wx:message-box file-filter-msg "Error")]
		     [else
		      (let ([file (build-path current-dir
					      (make-relative name))])
			(if (directory-exists? file)
			    (if save-mode?
				(wx:message-box
				 "That is the name of a directory."
				 "Error")
				(set-directory (mzlib:file:normalize-path file)))
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
				  (set-box! result-box (mzlib:file:normalize-path file))
				  (show #f)))))]))))]
	  
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
	  (super-init () (if save-mode? "Put File" "Get File") 
		      #t 300 300 WIDTH HEIGHT))
	
	(private
	  [main-panel (make-object mred:container:vertical-panel% this)]
	  [top-panel (make-object mred:container:horizontal-panel% main-panel)]
	  [_1 (make-object mred:container:message% top-panel prompt)]
	  [dir-choice (make-object mred:container:choice% top-panel do-dir '())]

	  [middle-panel (make-object mred:container:horizontal-panel% main-panel)]
	  [left-middle-panel (make-object mred:container:vertical-panel% middle-panel)]
	  [right-middle-panel (when multi-mode? (make-object mred:container:vertical-panel% middle-panel))]
	  [name-list%
	   (class-asi mred:container:list-box%
	     (public
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
				  (if multi-mode? (* 1/2 WIDTH) WIDTH) 300
				  () wx:const-needed-sb)]
	  [save-panel (when save-mode? (make-object mred:container:horizontal-panel% main-panel))]
	  [directory-panel (make-object mred:container:horizontal-panel% main-panel)]
	  [directory-edit (make-object mred:edit:edit%)]
	  [period-panel (when (eq? 'unix wx:platform)
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
			    (* 1/2 WIDTH) 300
			    () wx:const-needed-sb))]
	  [add-panel (when multi-mode? (make-object mred:container:horizontal-panel% left-middle-panel))]
	  [remove-panel (when multi-mode? (make-object mred:container:horizontal-panel% right-middle-panel))])
	(sequence
	  (when (eq? wx:platform 'unix)
	    (make-object mred:container:check-box% period-panel
			 do-period-in/exclusion
			 "Show files and directories that begin with a period"))

	  (send* directory-panel 
	    (stretchable-in-y #f)
	    (border 0)
	    (spacing 0))
	  (let ([canvas (make-object mred:canvas:one-line-canvas% directory-panel -1 -1 -1 20 ""
			      (+ wx:const-mcanvas-hide-h-scroll
				 wx:const-mcanvas-hide-v-scroll))])
	    (send* canvas
	      (set-media directory-edit)
	      (user-min-height 20)))
	  (make-object mred:container:button% directory-panel 
		       (lambda (button evt)
			 (let ([t (send directory-edit get-text)])
			   (if (directory-exists? t)
			       (set-directory (mzlib:file:normalize-path t))
			       (wx:bell))))
		       "Go")

	  (send main-panel spacing 1)
	  (when multi-mode?
	    (send add-panel stretchable-in-y #f)
	    (send remove-panel stretchable-in-y #f)
	    (send result-list stretchable-in-x #t))
	  (send period-panel stretchable-in-y #f)
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
	  [into-dir-button
	   (when save-mode?
	     (make-object mred:container:button%
			  save-panel do-into-dir "Open Directory"))]
	  [add-button (when multi-mode?
			(make-object mred:container:horizontal-panel% add-panel)
			(make-object mred:container:button%
				       add-panel do-add
				       "Add"))]
	  [add-all-button (when multi-mode?
			    (begin0
			      (make-object mred:container:button%
					   add-panel do-add-all
					   "Add All")
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

    (define common-put-file
      (opt-lambda ([name ()][directory ()][replace? #f]
			    [prompt "Select File"][filter #f]
			    [filter-msg  "That name does not have the right form"])
	(let* ([directory (if (and (null? directory)
				   (string? name))
			      (or (mzlib:file:path-only name) null)
			      directory)]
	       [name (or (and (string? name)
			      (mzlib:file:file-name-from-path name))
			 name)]
	       [v (box #f)])
	  (make-object finder-dialog% #t replace? #f v 
		       directory name prompt filter filter-msg)
	  (unbox v))))

    (define common-get-file
      (opt-lambda ([directory ()][prompt "Select File"][filter #f]
				 [filter-msg "Bad name"])
	(let ([v (box #f)])
	  (make-object finder-dialog% #f #f #f v directory '() prompt 
		       filter filter-msg)
	  (unbox v))))

    (define common-get-file-list
      (opt-lambda ([directory ()][prompt "Select Files"][filter #f]
				 [filter-msg "Bad name"])
	(let ([v (box ())])
	  (make-object finder-dialog% #f #f #t v directory '() prompt 
		       filter filter-msg)
	  (unbox v))))

    (define std-put-file
      (opt-lambda ([name ()][directory ()][replace? #f][prompt "Select File"]
			    [filter #f]
			    [filter-msg 
			     "That filename does not have the right form."])
	(let* ([directory (if (and (null? directory)
				   (string? name))
			      (or (mzlib:file:path-only name) null)
			      directory)]
	       [name (or (and (string? name)
			      (mzlib:file:file-name-from-path name))
			 name)]
	       [f (wx:file-selector prompt directory name
				    '()
				    (if (eq? wx:platform 'windows)
					"*.*"
					"*")
				    wx:const-save)])
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
      (opt-lambda ([directory ()][prompt "Select File"][filter #f]
				 [filter-msg 
				  "That filename does not have the right form."])
	(let ([f (wx:file-selector prompt directory)])
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
		      (wx:message-box "That file does not exist.")
		      #f]
		     [else f]))
		  #f)))))

    (mred:preferences:set-preference-default 'mred:file-dialogs
					     (if (eq? wx:platform 'unix)
						 'common
						 'std))
    (define put-file
      (lambda args
	(apply (case (mred:preferences:get-preference 'mred:file-dialogs)
		 [(std) std-put-file]
		 [(common) common-put-file])
	       args)))
    (define get-file
      (lambda args
	(apply (case (mred:preferences:get-preference 'mred:file-dialogs)
		 [(std) std-get-file]
		 [(common) common-get-file])
	       args)))))
