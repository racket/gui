(module handler mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   "../gui-utils-sig.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "file.ss"))

  (provide handler@)

  (define handler@
    (unit/sig framework:handler^
      (import mred^
	      [gui-utils : framework:gui-utils^]
	      [finder : framework:finder^]
	      [group : framework:group^]
	      [text : framework:text^]
	      [preferences : framework:preferences^]
	      [frame : framework:frame^])
      
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
				 (or (filename-extension name)
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

					; Open a file for editing
      (define edit-file
	(case-lambda
	 [(filename) (edit-file
		      filename
		      (lambda ()
			(let ([frame (make-object frame:text-info-file% filename)])
			  (send frame show #t)
			  frame)))]
	 [(filename make-default)
	  (gui-utils:show-busy-cursor
	   (lambda ()
	     (if filename
		 (let ([already-open (send (group:get-the-frame-group)
					   locate-file
					   filename)])
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
			     (make-default)))))
		 (make-default))))]))
      
					; Query the user for a file and then edit it

      (define *open-directory* ; object to remember last directory
	(make-object 
	    (class object% ()
	      (private 
		[the-dir #f])
	      (public
		[get (lambda () the-dir)]
		[set-from-file!
		 (lambda (file) 
		   (set! the-dir (path-only file)))]
		[set-to-default
		 (lambda ()
		   (set! the-dir (current-directory)))])
	      (sequence
		(set-to-default)
		(super-init)))))

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
		 (edit-file file))))))))
