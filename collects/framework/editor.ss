(dunit/sig framework:editor^
  (import mred-interfaces^
	  [autosave : framework:autosave^]
	  [finder : framework:finder^]
	  [path-utils : framework:path-utils^]
	  [keymap : framework:keymap^]
	  [icon : framework:icon^]
	  [preferences : framework:preferences^]
	  [text : framework:text^]
	  [pasteboard : framework:pasteboard^])
  
  (define basic<%>
    (interface (editor<%>)
      editing-this-file?
      local-edit-sequence?
      run-after-edit-sequence
      default-auto-wrap?
      get-top-level-window
      locked?
      on-close))

  (define basic-mixin
    (mixin (editor<%>) (basic<%>) args
      (inherit get-filename save-file
	       refresh-delayed? 
	       get-canvas
	       get-keymap
	       get-max-width get-admin set-filename)
      (rename [super-set-modified set-modified]
	      [super-on-focus on-focus]
	      [super-lock lock])
      
      (public
	[on-close void]
	[get-top-level-window
	 (lambda ()
	   (let ([c (get-canvas)])
	     (and c
		  (send c get-top-level-window))))])

      (public [editing-this-file? (lambda () #f)])

      (private
	[edit-sequence-queue null]
	[edit-sequence-ht (make-hash-table)])

      (private
	[in-local-edit-sequence? #f])
      (public
	[local-edit-sequence? (lambda () in-local-edit-sequence?)]
	[run-after-edit-sequence
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
	   (void)])]
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
	   (set! in-local-edit-sequence? #t))]
	[after-edit-sequence
	 (lambda ()
	   (set! in-local-edit-sequence? #f)
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
		 [(and edit (not (send edit local-edit-sequence?)))
		  (loop (find-enclosing-edit edit))]
		 [edit (send edit extend-edit-sequence-queue queue ht)]
		 [else
		  (hash-table-for-each ht (lambda (k t) (t)))
		  (for-each (lambda (t) (t)) queue)]))))])
      (private
	[is-locked? #f])
      (public
	[locked? (lambda () is-locked?)])
      (override
	[lock
	 (lambda (x)
	   (set! is-locked? x)
	   (super-lock x))]
	[on-new-box
	 (lambda (type)
	   (cond
	     [(eq? type 'text) (make-object editor-snip% (make-object text:basic%))]
	     [else (make-object editor-snip% (make-object pasteboard:basic%))]))])


      (override
	[get-file (lambda (d) 
		    (parameterize ([finder:dialog-parent-parameter
				    (get-top-level-window)])
		      (finder:get-file d)))]
	[put-file (lambda (d f) (parameterize ([finder:dialog-parent-parameter
						(get-top-level-window)])
				  (finder:put-file f d)))])
      
      (public
	[default-auto-wrap? (lambda () #t)])
      (inherit auto-wrap)
      (sequence
	(apply super-init args)
	(auto-wrap (default-auto-wrap?)))))
  

  (define file<%> (interface (basic<%>)))
  (define file-mixin
    (mixin (basic<%>) (file<%>) args
      (inherit get-keymap  
	       get-filename lock get-style-list 
	       is-modified? change-style set-modified 
	       get-top-level-window)
      (rename [super-after-save-file after-save-file]
	      [super-after-load-file after-load-file])
      
      (override [editing-this-file? (lambda () #t)])
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

  ; wx: what about checking the autosave files when a file is
  ;     opened?
  (define backup-autosave-mixin
    (mixin (basic<%>) (backup-autosave<%>) args
      (inherit is-modified? get-filename save-file)
      (rename [super-on-save-file on-save-file]
	      [super-on-change on-change]
	      [super-on-close on-close]
	      [super-set-modified set-modified])
      (private
	[freshen-backup? #t]
	[auto-saved-name #f]
	[auto-save-out-of-date? #t]
	[auto-save-error? #f])
      (public
	[backup? (lambda () #t)])
      (override
	[on-save-file
	 (lambda (name format)
	   (set! auto-save-error? #f)
	   (and (super-on-save-file name format)
		(begin
		  (when (and (backup?)
			     freshen-backup?
			     (not (eq? format 'copy))
			     (file-exists? name))
		    (let ([back-name (path-utils:generate-backup-name name)])
		      (set! freshen-backup? #f)
		      (when (file-exists? back-name)
			(delete-file back-name))
		      (with-handlers ([(lambda (x) #t) void])
			(copy-file name back-name))))
		  #t)))]
	[on-close
	 (lambda ()
	   (super-on-close)
	   (remove-autosave)
	   (set! autosave? (lambda () #f)))]
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
	[autosave? (lambda () #t)]
	[do-autosave
	 (lambda ()
	   (when (and (autosave?)
		      (not auto-save-error?)
		      (is-modified?)
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
  (define info-mixin
    (mixin (basic<%>) (info<%>) args
      (inherit get-top-level-window run-after-edit-sequence)
      (rename [super-lock lock])
      (override
	[lock
	 (lambda (x)
	   (super-lock x)
	   (run-after-edit-sequence
	    (rec send-frame-update-lock-icon
		 (lambda ()
		   (let ([frame (get-top-level-window)])
		     (when frame
		       (send frame lock-status-changed)))))
	    'framework:update-lock-icon))])
      (sequence (apply super-init args)))))