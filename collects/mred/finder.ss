(define-sigfunctor (mred:finder@ mred:finder^)
  (import mred:debug^ mzlib:string^ mzlib:function^ mzlib:file^)

  (define filter-match?
    (lambda (filter name msg)
      (let-values ([(base name dir?) (split-path name)])
		  (if (mzlib:string^:regexp-match-exact? filter name)
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

  (define finder-dialog%
    (class wx:dialog-box% (save-mode? replace-ok? multi-mode? 
				      result-box start-dir 
				      start-name prompt 
				      file-filter file-filter-msg)
     (inherit
      new-line tab fit center
      show
      popup-menu)
     
     (private
      [WIDTH 500]
      [HEIGHT 500]

      dirs current-dir
      last-selected
      
      [select-counter 0])
     
     (private
      [set-directory
       (lambda (dir) ; dir is normalied
	 (set! current-dir dir)
	 (set! last-directory dir)
	 (let-values 
	  ([(dir-list menu-list)
	    (let loop ([this-dir dir]
		       [dir-list ()]
		       [menu-list ()])
	      (let-values ([(base-dir in-dir dir?) (split-path this-dir)])
			  (if (eq? wx:platform 'windows)
			      (mzlib:string^:string-lowercase! in-dir))
			  (let* ([dir-list (cons this-dir dir-list)]
				 [menu-list (cons in-dir menu-list)])
			    (if base-dir
				(loop base-dir dir-list menu-list)
				; No more
				(values dir-list menu-list)))))])
	  (set! dirs dir-list)
	  
	  (send dir-choice clear)
	  (let loop ([choices menu-list])
	    (unless (null? choices)
		    (send dir-choice append (car choices))
		    (loop (cdr choices))))
	  (send dir-choice set-selection (sub1 (length dirs)))
	  (send dir-choice set-size -1 -1 -1 -1))
	 
	 (send name-list clear)
	 (send name-list set
	       (mzlib:function^:quicksort
		(let loop ([l (directory-list dir)])
		  (if (null? l)
		      '()
		      (let ([s (car l)]
			    [rest (loop (cdr l))])
			(if (directory-exists? (build-path dir s))
			    (cons
			     (string-append s
					    (case wx:platform
					      (unix "/")
					      (windows "\\")
					      (macintosh ":")))
			     rest)
			    (if (or (not file-filter)
				    (mzlib:string^:regexp-match-exact? file-filter s))
				(cons s rest)
				rest)))))
		(if (eq? wx:platform 'unix) string<? string-ci<?)))
	 (set! last-selected -1))])
     
     (public
      [do-dir
       (lambda (choice event)
	 (let ([which (send event get-selection)])
	   (if (< which (length dirs))
	       (set-directory (list-ref dirs which)))))]
      
      [do-goto
       (opt-lambda (button event [default ""])
		   (let ([orig-dir (wx:get-text-from-user
				    "Directory" "Go to Directory"
				    default)])
		     (if (string? orig-dir)
			 (let ([dir (mzlib:file^:normalize-path orig-dir current-dir)])
			   (if (directory-exists? dir)
			       (set-directory dir)
			       (begin
				 (wx:message-box 
				  (string-append "Bad directory: " dir)
				  "Error")
				 (do-goto button event orig-dir)))))))]
      
      [on-default-action
       (lambda (which)
	 (if (eq? which name-list)
	     (let* ([which (send name-list get-string-selection)]
		    [dir (build-path current-dir
				     (make-relative which))])
	       (if (directory-exists? dir)
		   (set-directory (mzlib:file^:normalize-path dir))
		   (if save-mode?
		       (send name-field set-value which)
		       (if multi-mode?
			   (do-add)
			   (do-ok)))))
	     (if (eq? which name-field)
		 (do-ok))))]
      
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
		     (set-directory (mzlib:file^:normalize-path name)))))))]
      
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
		      (not (mzlib:string^:regexp-match-exact? file-filter name)))
		 (wx:message-box file-filter-msg "Error")]
		[else
		 (let ([file (build-path current-dir
					 (make-relative name))])
		   (if (directory-exists? file)
		       (if save-mode?
			   (wx:message-box
			    "That is the name of a directory."
			    "Error")
			   (set-directory (mzlib:file^:normalize-path file)))
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
			     (set-box! result-box (mzlib:file^:normalize-path file))
			     (show #f)))))]))))]
      
      [add-one
       (lambda (name)
	 (unless (or (directory-exists? name)
		     (> (send result-list find-string name) -1))
		 (set! select-counter (add1 select-counter))
		 (send result-list append (mzlib:file^:normalize-path name))))]
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
		   #t 300 300 WIDTH HEIGHT)
       
       (make-object wx:message% this prompt)
       
       (new-line))

     (private
      [dir-choice (make-object wx:choice%
			       this do-dir '() -1 -1 -1 -1
			       '("XXXXXXXXXXXXXXXXXXXXXXXXXXX"))]
      
      [name-list (begin
		   (new-line)
		   (make-object wx:list-box%
				this do-name-list
				() wx:const-single
				-1 -1
				(if multi-mode? (* 1/2 WIDTH) WIDTH) 300
				() wx:const-needed-sb))]
      
      [result-list
       (if multi-mode?
	   (make-object wx:list-box%
			this do-result-list
			() 
			(if (eq? wx:window-system 'motif)
			    wx:const-extended
			    wx:const-multiple)
			-1 -1
			(* 1/2 WIDTH) 300
			() wx:const-needed-sb))])
     (sequence
       (new-line))
     
     (private
      [name-field
       (if save-mode?
	   (let ([v (make-object wx:text%
				 this do-name
				 "Name: " ""
				 -1 -1
				 400 -1
				 wx:const-process-enter)])
	     (if (string? start-name)
		 (send v set-value start-name))
	     (new-line)
	     v))]
      [into-dir-button
       (if save-mode?
	   (make-object wx:button%
			this do-into-dir "Open Directory"))]
      [goto-button (make-object wx:button%
				this do-goto "Go to Directory...")]
      [add-button (if multi-mode?
		      (make-object wx:button%
				   this do-add
				   "Add"))]
      [add-all-button (if multi-mode?
			  (make-object wx:button%
				       this do-add-all
				       "Add All"))]
      [remove-button (if multi-mode?
			 (make-object wx:button%
				      this do-remove
				      "Remove"))])
     (sequence
       (if multi-mode?
	   (tab 40)
	   (tab 100)))
     (private
      [cancel-button (make-object wx:button%
				  this do-cancel
				  "Cancel")]
      [ok-button
       (let ([w (send cancel-button get-width)])
	 (make-object wx:button%
		      this do-ok
		      "OK" -1 -1 w))])
     (sequence
       (fit)
       
       (cond
	[(and start-dir
	      (not (null? start-dir))
	      (directory-exists? start-dir))
	 (set-directory (mzlib:file^:normalize-path start-dir))]
	[last-directory (set-directory last-directory)]
	[else (set-directory (current-directory))])
       
       (center wx:const-both)
       
       (show #t))))

  (define common-put-file
    (opt-lambda ([name ()][directory ()][replace? #f]
			  [prompt "Select File"][filter #f]
			  [filter-msg  "That name does not have the right form"])
		(let* ([directory (if (and (null? directory)
					   (string? name))
				      (or (mzlib:file^:path-only name) null)
				      directory)]
		       [name (or (and (string? name)
				      (mzlib:file^:file-name-from-path name))
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
				      (or (mzlib:file^:path-only name) null)
				      directory)]
		       [name (or (and (string? name)
				      (mzlib:file^:file-name-from-path name))
				 name)]
		       [f (wx:file-selector prompt directory name
					    '()
					    (if (eq? wx:platform 'windows)
						"*.*"
						"*")
					    wx:const-save)])
		  (if (or (null? f) (and filter (not (filter-match? filter 
								    f
								    filter-msg))))
		      #f
		      (let* ([f (mzlib:file^:normalize-path f)]
			     [dir (mzlib:file^:path-only f)]
			     [name (mzlib:file^:file-name-from-path f)])
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
			  (let ([f (mzlib:file^:normalize-path f)])
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

  ; By default, use platform-specific get/put
  (define put-file std-put-file)
  (define get-file std-get-file))
