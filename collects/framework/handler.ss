; File Formats and Modes

  (unit/sig framework:handler^
    (import [gui-utils : framework:gui-utils^]
	    [finder : framework:finder^]
	    [group : framework:group^]
	    [hyper:frame : framework:hyper:frame^]
	    [edit : framework:edit^]
	    [preferences : framework:preferences^]
	    [mzlib:file : mzlib:file^]
	    [mred:editor-frame : mred:editor-frame^])
	    
    (define-struct handler (name extension handler))

    (define format-handlers '())

    (define make-insert-handler
      (letrec ([string-list?
		(lambda (l)
		  (cond
		   [(null? l) #t]
		   [(not (pair? l)) #f]
		   [else
		    (and (string? (car l))
			 (string-list? (cdr l)))]))])
	(lambda (who name extension handler)
	  (cond
	   [(not (string? name))
	    (error who "name was not a string")]
	   [(and (not (procedure? extension))
		 (not (string? extension))
		 (not (string-list? extension)))
	    (error who
		   "extension was not a string, list of strings, or a predicate")]
	   [(not (procedure? handler))
	    (error who "handler was not a function")]
	   [else (make-handler name
			       extension
			       handler)]))))
    
    (define insert-format-handler
      (lambda args
	(set! format-handlers 
	      (cons (apply make-insert-handler 'insert-format-handler args)
		    format-handlers))))

    (define find-handler
      (lambda (name handlers)
	(let/ec exit
	  (let ([extension (if (string? name)
			       (or (mzlib:file:filename-extension name)
				   "")
			       "")])
	    (for-each
	     (lambda (handler)
	       (let ([ext (handler-extension handler)])
		 (when (or (and (procedure? ext)
				(ext name))
			   (and (string? ext)
				(string=? ext extension))
			   (and (pair? ext)
				(ormap (lambda (ext) 
					 (string=? ext extension))
				       ext)))
		   (exit (handler-handler handler)))))
	     handlers)
	    #f))))
    
    (define find-format-handler
      (lambda (name)
	(find-handler name format-handlers)))

    ; Finding format & mode handlers by name
    (define find-named-handler
      (lambda (name handlers)
	(let loop ([l handlers])
	  (cond
	   [(null? l) #f]
	   [(string-ci=? (handler-name (car l)) name)
	    (handler-handler (car l))]
	   [else (loop (cdr l))]))))
    
    (define find-named-format-handler
      (lambda (name)
	(find-named-handler name format-handlers)))

    (define edit-file-consult-group (make-parameter #t))

    ; Open a file for editing
    (define edit-file
      (opt-lambda (filename
		   [make-default
		    (lambda (filename)
		      (make-object mred:editor-frame:editor-frame%
				   filename #t))]
		   [consult-group? (edit-file-consult-group)])
	(gui-utils:show-busy-cursor
	 (lambda ()
	   (if filename
	       (let ([already-open (and consult-group?
					(send mred:group:the-frame-group
					      locate-file
					      filename))])
		 (if already-open
		     (begin
		       (send already-open show #t)
		       already-open)
		     (let ([handler
			    (if (string? filename)
				(find-format-handler filename)
				#f)])
		       (if handler
			   (handler filename)
			   (make-default filename)))))
	       (make-default filename))))))
    
    (define get-url-from-user
      (lambda ()
	(let* ([frame (make-object dialog-box% (get-top-level-focus-window) "Choose URL")]
	       [main (make-object vertical-panel% frame)]
	       [one-line (make-object editor-canvas% main)]
	       [_ (send one-line set-line-count 1)]
	       [valid? #f]
	       [ok-callback (lambda x (set! valid? #t) (send frame show #f))]
	       [answer (make-object edit:return% ok-callback)]
	       [bottom (make-object horizontal-panel% main)]
	       [space (make-object horizontal-panel% bottom)]
	       [bookmarks (preferences:get 'framework:bookmarks)]
	       [bk-choice
		(make-object choice% bottom
			     (lambda (box evt)
			       (let ([which (send evt get-command-int)])
				 (when (<= 0 which)
				   (send* answer
				     (begin-edit-sequence)
				     (erase)
				     (insert (list-ref bookmarks which))
				     (end-edit-sequence)))))
			     "Bookmarks" -1 -1 -1 -1 bookmarks)]
	       [browse (make-object button%
				    bottom
				    (lambda x
				      (let ([ans (finder:get-file)])
					(when ans
					  (send* answer
					    (begin-edit-sequence)
					    (erase)
					    (insert "file:")
					    (insert ans)
					    (end-edit-sequence)))))
				    "Browse...")]
	       [cancel (make-object button% bottom
				    (lambda x 
				      (send frame show #f))
				    "Cancel")]
	       [ok (make-object button% bottom
				ok-callback
				"Ok")])
	  (let ([w (max (send ok get-width)
			(send cancel get-width)
			(send browse get-width))])
	    (send ok user-min-width w)
	    (send cancel user-min-width w)
	    (send browse user-min-width w))
	  (unless (null? bookmarks)
	    (send answer insert (car bookmarks))
	    (send answer set-position 0 -1))
	  (send one-line set-focus)
	  (send one-line set-media answer)
	  (send frame set-size -1 -1 20 20)
	  (send frame center 'both)
	  (send frame show #t)
	  (and valid? 
	       (send answer get-text)))))

    (define open-url
      (opt-lambda ([input-url #f])
	(let ([url (or input-url (get-url-from-user))])
	  (and url
	      (make-object hyper:frame:hyper-view-frame% url)))))
    
    ; Query the user for a file and then edit it

    (define *open-directory* ; object to remember last directory
      (make-object 
       (class null ()
	      (private 
	       [the-dir #f])
	      (public
	       [get (lambda () the-dir)]
	       [set-from-file!
		(lambda (file) 
		  (set! the-dir (mzlib:file:path-only file)))]
	       [set-to-default
		(lambda ()
		  (set! the-dir (current-directory)))])
	      (sequence
		(set-to-default)))))

    (define open-file
      (lambda ()
	(let ([file 
	       (parameterize ([finder:dialog-parent-parameter
			       (get-top-level-focus-window)])
		 (finder:get-file
		  (send *open-directory* get)))])
	  (when file
		(send *open-directory*
		      set-from-file! file))
	  (and file
	       (edit-file file))))))
