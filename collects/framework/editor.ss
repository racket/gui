(unit/sig framework:editor^
  (import mred-interfaces^
	  [autosave : framework:autosave^]
	  [finder : framework:finder^]
	  [path-utils : framework:path-utils^]
	  [keymap : framework:keymap^]
	  [icon : framework:icon^]
	  [preferences : framework:preferences^]
	  [gui-utils : framework:gui-utils^])
  
  (define basic<%>
    (interface (editor<%>)
      editing-this-file?
      local-edit-sequence?
      run-after-edit-sequence
      get-text-snip      
      get-pasteboard-snip
      default-auto-wrap?))

  (define make-basic%
    (mixin (editor<%>) (basic<%>) args
      (inherit modified? get-filename save-file canvases
	       refresh-delayed? 
	       get-frame get-keymap
	       get-max-width get-admin set-filename)
      (rename [super-set-modified set-modified]
	      [super-on-save-file on-save-file]
	      [super-on-focus on-focus]
	      [super-load-file load-file]
	      [super-lock lock])
      
      (public [editing-this-file? #f])

      (override
	[load-file
	 (opt-lambda ([filename #f] 
		      [the-format 'guess]
		      [show-dialog? #t])
	   (let ([filename (or filename
			       (parameterize ([finder:dialog-parent-parameter
					       (or (get-frame)
						   #f)])
				 (finder:get-file)))])
	     (and filename
		  (if (file-exists? filename)
		      (let ([res (super-load-file filename the-format #f)])
			(when (and (not res)
				   show-dialog?)
			  (message-box
			   "Error Loading File"
			   (format "Error loading file ~a" filename))
			  res))
		      (set-filename filename)))))])

      (private
	[edit-sequence-queue null]
	[edit-sequence-ht (make-hash-table)])

      (public
	[local-edit-sequence? #f]
	[run-after-edit-sequence
	 (rec run-after-edit-sequence
	      (case-lambda 
	       [(t) (run-after-edit-sequence t #f)]
	       [(t sym)
		(unless (and (procedure? t)
			     (= 0 (arity t)))
		  (error 'media-buffer::run-after-edit-sequence
			 "expected procedure of arity zero, got: ~s~n" t))
		(unless (or (symbol? sym) (not sym))
		  (error 'media-buffer::run-after-edit-sequence
			 "expected second argument to be a symbol, got: ~s~n"
			 sym))
		(if (refresh-delayed?)
		    (cond
		      [(symbol? sym)
		       (hash-table-put! edit-sequence-ht sym t)]
		      [else (set! edit-sequence-queue
				  (cons t edit-sequence-queue))])
		    (t))
		(void)]))]
	[extend-edit-sequence-queue
	 (lambda (l ht)
	   (hash-table-for-each ht (lambda (k t)
				     (hash-table-put! 
				      edit-sequence-ht
				      k t)))
	   (set! edit-sequence-queue (append l edit-sequence-queue)))])
      (rename
	[super-after-edit-sequence after-edit-sequence]
	[super-on-edit-sequence on-edit-sequence])
      (override
	[on-edit-sequence
	 (lambda ()
	   (super-on-edit-sequence)
	   (set! local-edit-sequence? #t))]
	[after-edit-sequence
	 (lambda ()
	   (set! local-edit-sequence? #f)
	   (super-after-edit-sequence)
	   (let ([queue edit-sequence-queue]
		 [ht edit-sequence-ht]
		 [find-enclosing-edit
		  (lambda (edit)
		    (let ([admin (send edit get-admin)])
		      (cond
			[(is-a? admin editor-snip-editor-admin%)
			 (send (send (send admin get-snip) get-admin) get-media)]
			;; assume that any non-media-snip 
			;; administrator doesn't have embedded edits.
			[else #f])))])
	     (set! edit-sequence-queue null)
	     (set! edit-sequence-ht (make-hash-table))
	     (let loop ([edit (find-enclosing-edit this)])
	       (cond
		 [(and edit (not (ivar edit local-edit-sequence?)))
		  (loop (find-enclosing-edit edit))]
		 [edit (send edit extend-edit-sequence-queue queue ht)]
		 [else
		  (hash-table-for-each ht (lambda (k t) (t)))
		  (for-each (lambda (t) (t)) queue)]))))])
      (public
	[locked? #f])
      (override
	[lock 
	 (lambda (x)
	   (set! locked? x)
	   (super-lock x))])
      
      (public
	[get-text-snip (lambda () (make-object editor-snip% (make-object text%)))]
	[get-pasteboard-snip (lambda () (make-object editor-snip% (make-object pasteboard%)))])
      (override
	[on-new-box
	 (lambda (type)
	   (cond
	     [(eq? type 'text) (get-text-snip)]
	     [else (get-pasteboard-snip)]))])


      (public
	[get-file (lambda (d) 
		    (let ([v (parameterize ([finder:dialog-parent-parameter
					     (or (get-frame)
						 null)])
			       (finder:get-file d))])
		      (if v
			  v
			  null)))]
	[put-file (lambda (d f) (let ([v (parameterize ([finder:dialog-parent-parameter
							 (or (get-frame)
							     null)])
					   (finder:put-file f d))])
				  (if v
				      v
				      null)))])
      
      (public
	[default-auto-wrap? #t])
      (inherit auto-wrap)
      (sequence
	(apply super-init args)
	(auto-wrap default-auto-wrap?))))
  

  (define file<%> (interface (basic<%>)))
  (define make-file%
    (mixin (basic<%>) (file<%>) args
      (inherit get-keymap find-snip  
	       get-filename lock get-style-list 
	       modified? change-style set-modified 
	       get-frame)
      (rename [super-after-save-file after-save-file]
	      [super-after-load-file after-load-file])
      
      (override [editing-this-file? #t])
      (private
	[check-lock
	 (lambda ()
	   (let* ([filename (get-filename)]
		  [lock? (and (not (null? filename))
			      (file-exists? filename)
			      (not (member
				    'write
				    (file-or-directory-permissions
				     filename))))])
	     (lock lock?)))])
      (override
	[after-save-file
	 (lambda (success)
	   (when success
	     (check-lock))
	   (super-after-save-file success))]
	
	[after-load-file
	 (lambda (sucessful?)
	   (when sucessful?
	     (check-lock))
	   (super-after-load-file sucessful?))])
      (sequence
	(apply super-init args)
	(let ([keymap (get-keymap)])
	  (keymap:set-keymap-error-handler keymap)
	  (keymap:set-keymap-implied-shifts keymap)
	  (send keymap chain-to-keymap keymap:file #f)))))
  
  (define backup-autosave<%>
    (interface (basic<%>)
      backup?
      autosave?
      do-autosave
      remove-autosave))

  ; wx: when should autosave files be removed?
  ;     also, what about checking the autosave files when a file is
  ;     opened?
  (define make-backup-autosave%
    (mixin (basic<%>) (backup-autosave<%>) args
      (inherit modified? get-filename save-file)
      (rename [super-on-save-file on-save-file]
	      [super-on-change on-change]
	      [super-do-close do-close]
	      [super-set-modified set-modified])
      (private
	[freshen-backup? #t]
	[auto-saved-name #f]
	[auto-save-out-of-date? #t]
	[auto-save-error? #f])
      (public
	[auto-save? #t]
	[backup? #t])
      (override
	[on-save-file
	 (lambda (name format)
	   (set! auto-save-error? #f)
	   (and (super-on-save-file name format)
		(begin
		  (when (and backup?
			     (not (eq? format 'copy))
			     (file-exists? name))
		    (let ([back-name (path-utils:generate-backup-name name)])
		      (when freshen-backup?
			(set! freshen-backup? #f)
			(when (file-exists? back-name)
			  (delete-file back-name)))
		      (with-handlers ([(lambda (x) #t) void])
			(copy-file name back-name))))
		  #t)))]
	[do-close
	 (lambda ()
	   (super-do-close)
	   (remove-autosave)
	   (set! auto-save? #f))]
	[on-change
	 (lambda ()
	   (super-on-change)
	   (set! auto-save-out-of-date? #t))]
	[set-modified
	 (lambda (modified?)
	   (when auto-saved-name
	     (if modified?
		 (set! auto-save-out-of-date? #t)
		 (begin
		   (delete-file auto-saved-name)
		   (set! auto-saved-name #f))))
	   (super-set-modified modified?))])
      (public
	[autosave? #t]
	[do-autosave
	 (lambda ()
	   (when (and auto-save?
		      (not auto-save-error?)
		      (modified?)
		      (or (not auto-saved-name)
			  auto-save-out-of-date?))
	     (let* ([orig-name (get-filename)]
		    [auto-name (path-utils:generate-autosave-name orig-name)]
		    [success (save-file auto-name 'copy)])
	       (if success
		   (begin
		     (when auto-saved-name
		       (delete-file auto-saved-name))
		     (set! auto-saved-name auto-name)
		     (set! auto-save-out-of-date? #f))
		   (begin
		     (message-box 
		      "Warning"
		      (format "Error autosaving ~s.~n~a~n~a"
			      (if (null? orig-name) "Untitled" orig-name)
			      "Autosaving is turned off"
			      "until the file is saved."))
		     (set! auto-save-error? #t))))))]
	[remove-autosave
	 (lambda ()
	   (when auto-saved-name
	     (delete-file auto-saved-name)
	     (set! auto-saved-name #f)))])
      (sequence
	(apply super-init args)
	(autosave:register this))))

  (define info<%> (interface (basic<%>)))
  (define make-info%
    (mixin (basic<%>) (info<%>) args
      (inherit get-frame run-after-edit-sequence)
      (rename [super-lock lock])
      (override
	[lock
	 (lambda (x)
	   (super-lock x)
	   (run-after-edit-sequence
	    (rec send-frame-update-lock-icon
		 (lambda ()
		   (let ([frame (get-frame)])
		     (when frame
		       (send frame lock-status-changed)))))
	    'framework:update-lock-icon))])
      (sequence (apply super-init args))))

  (define make-clever-file-format%
    (mixin (editor<%>) (editor<%>) args
      (inherit get-file-format set-file-format find-snip)
      (rename [super-on-save-file on-save-file]
	      [super-after-save-file after-save-file])
      
      (private [restore-file-format void])
      
      (override
	[after-save-file
	 (lambda (success)
	   (restore-file-format)
	   (super-after-save-file success))]
	[on-save-file
	 (let ([has-non-text-snips 
		(lambda ()
		  (let loop ([s (find-snip 0 'after)])
		    (cond
		      [(null? s) #f]
		      [(is-a? s text-snip%)
		       (loop (send s next))]
		      [else #t])))])
	   (lambda (name format)
	     (when (and (or (eq? format 'same)
			    (eq? format 'copy))
			(not (eq? (get-file-format) 
				  'std)))
	       (cond
		 [(eq? format 'copy)
		  (set! restore-file-format 
			(let ([f (get-file-format)])
			  (lambda ()
			    (set! restore-file-format void)
			    (set-file-format f))))
		  (set-file-format 'std)]
		 [(and (has-non-text-snips)
		       (or (not (preferences:get 'framework:verify-change-format))
			   (gui-utils:get-choice "Save this file as plain text?" "No" "Yes")))
		  (set-file-format 'std)]
		 [else (void)]))
	     (or (super-on-save-file name format)
		 (begin 
		   (restore-file-format)
		   #f))))])
      (sequence (apply super-init args)))))