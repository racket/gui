(module editor mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   "../gui-utils-sig.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "file.ss"))

  (provide editor@)

  (define editor@
    (unit/sig framework:editor^
      (import [mred : mred^]
	      [autosave : framework:autosave^]
	      [finder : framework:finder^]
	      [path-utils : framework:path-utils^]
	      [keymap : framework:keymap^]
	      [icon : framework:icon^]
	      [preferences : framework:preferences^]
	      [text : framework:text^]
	      [pasteboard : framework:pasteboard^]
	      [frame : framework:frame^]
	      [gui-utils : framework:gui-utils^])
      
      (rename [-keymap<%> keymap<%>])

      (define basic<%>
	(interface (editor<%>)
	  has-focus?
	  editing-this-file?
	  local-edit-sequence?
	  run-after-edit-sequence
	  get-top-level-window
	  on-close
	  save-file-out-of-date?))

      (define basic-mixin
	(mixin (editor<%>) (basic<%>) args
	  (inherit get-filename save-file
		   refresh-delayed? 
		   get-canvas
		   get-max-width get-admin)

	  (rename [super-can-save-file? can-save-file?])
	  (override
	   [can-save-file?
	    (lambda (filename format)
	      (and (if (equal? filename (get-filename))
		       (if (save-file-out-of-date?)
			   (gui-utils:get-choice 
			    "The file has beeen modified since it was last saved. Overwrite the modifications?"
			    "Overwrite"
			    "Cancel"
			    "Warning"
			    #f
			    (get-top-level-focus-window))
			   #t)
		       #t)
		   (super-can-save-file? filename format)))])
	  
	  (rename [super-after-save-file after-save-file]
		  [super-after-load-file after-load-file])
	  (private
	    [last-saved-file-time #f])
	  (override
	   [after-save-file
	    (lambda (sucess?)
	      (when sucess?
		(let ([filename (get-filename)])
		  (set! last-saved-file-time
			(and filename
			     (file-exists? filename)
			     (file-or-directory-modify-seconds filename)))))
	      (super-after-save-file sucess?))]
	   [after-load-file
	    (lambda (sucess?)
	      (when sucess?
		(let ([filename (get-filename)])
		  (set! last-saved-file-time
			(and filename
			     (file-exists? filename)
			     (file-or-directory-modify-seconds filename)))))
	      (super-after-load-file sucess?))])
	  (public
	    [save-file-out-of-date?
	     (lambda ()
	       (and 
		last-saved-file-time
		(let ([fn (get-filename)])
		  (and fn
		       (file-exists? fn)
		       (let ([ms (file-or-directory-modify-seconds fn)])
			 (< last-saved-file-time ms))))))])

	  (private 
	    [has-focus #f])
	  (rename [super-on-focus on-focus])
	  (override
	   [on-focus
	    (lambda (x)
	      (set! has-focus x))])
	  (public
	    [has-focus?
	     (lambda ()
	       has-focus)])
	  
	  (rename [super-begin-edit-sequence begin-edit-sequence]
		  [super-end-edit-sequence end-edit-sequence])
	  (private
	    [edit-sequence-count 0])
	  (override
	   [begin-edit-sequence
	    (case-lambda
	     [() (begin-edit-sequence #t)]
	     [(undoable?)
	      (set! edit-sequence-count (+ edit-sequence-count 1))
	      (super-begin-edit-sequence undoable?)])]
	   [end-edit-sequence
	    (lambda ()
	      (set! edit-sequence-count (- edit-sequence-count 1))
	      (when (< edit-sequence-count 0)
		(error 'end-edit-sequence "extra end-edit-sequence"))
	      (super-end-edit-sequence))])

	  (public
	    [on-close void]
	    [get-top-level-window
	     (lambda ()
	       (let loop ([text this])
		 (let ([editor-admin (send text get-admin)])
		   (cond
		    [(is-a? editor-admin editor-snip-editor-admin<%>)
		     (let* ([snip (send editor-admin get-snip)]
			    [snip-admin (send snip get-admin)])
		       (loop (send snip-admin get-editor)))]
		    [(send text get-canvas) => (lambda (canvas)
						 (send canvas get-top-level-window))]
		    [else
		     #f]))))])

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
		   (if in-local-edit-sequence?
		       (cond
			[(symbol? sym)
			 (hash-table-put! edit-sequence-ht sym t)]
			[else (set! edit-sequence-queue
				    (cons t edit-sequence-queue))])
		       (let ([snip-admin (get-admin)])
			 (cond
			  [(not snip-admin)
			   (t)] ;; refresh-delayed? is always #t when there is no admin.
			  [(is-a? snip-admin editor-snip-editor-admin<%>)
			   (send (send (send (send snip-admin get-snip) get-admin) get-editor)
				 run-after-edit-sequence t sym)]
			  [else
			   (message-box "run-after-edit-sequence error"
					(format "refresh-delayed? is #t but snip admin, ~s, is not an editor-snip-editor-admin<%>"
						snip-admin))
			   '(t)])))
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
			  [(is-a? admin editor-snip-editor-admin<%>)
			   (send (send (send admin get-snip) get-admin) get-editor)]
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
	  
	  (override
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
	  
	  
	  (sequence
	    (apply super-init args))))

      
      (define -keymap<%> (interface (basic<%>) get-keymaps))
      (define keymap-mixin
	(mixin (basic<%>) (-keymap<%>) args
	  (public
	    [get-keymaps
	     (lambda ()
	       (list (keymap:get-global)))])
	  (inherit set-keymap)
	  (sequence
	    (apply super-init args)
	    (let ([keymap (make-object keymap:aug-keymap%)])
	      (set-keymap keymap)
	      (for-each (lambda (k) (send keymap chain-to-keymap k #f))
			(get-keymaps))))))

      (define autowrap<%> (interface (basic<%>)))
      (define autowrap-mixin
	(mixin (basic<%>) (autowrap<%>) args
	  
	  (rename [super-on-close on-close])
	  (override
	   [on-close
	    (lambda ()
	      (remove-callback)
	      (super-on-close))])
	  
	  (inherit auto-wrap)
	  (sequence
	    (apply super-init args)
	    (auto-wrap 
	     (preferences:get
	      'framework:auto-set-wrap?)))
	  (private
	    [remove-callback
	     (preferences:add-callback
	      'framework:auto-set-wrap?
	      (let ([autowrap-mixin-pref-callback
		     (lambda (p v)
		       (auto-wrap v))])
		autowrap-mixin-pref-callback))])))
      
      (define file<%> (interface (-keymap<%>)))
      (define file-mixin
	(mixin (-keymap<%>) (file<%>) args
	  (inherit get-filename lock get-style-list 
		   is-modified? change-style set-modified 
		   get-top-level-window)
	  (rename [super-after-save-file after-save-file]
		  [super-after-load-file after-load-file]
		  [super-get-keymaps get-keymaps]
		  [super-set-filename set-filename])
	  
	  (override
	   [editing-this-file? (lambda () #t)])

	  (inherit get-canvases)
	  (private
	    [check-lock
	     (lambda ()
	       (let* ([filename (get-filename)]
		      [lock? (and filename
				  (file-exists? filename)
				  (not (member
					'write
					(file-or-directory-permissions
					 filename))))])
		 (lock lock?)))]
	    [update-filename
	     (lambda (name)
	       (let ([filename (if name
				   (file-name-from-path (normalize-path name))
				   "")])
		 (for-each (lambda (canvas)
			     (let ([tlw (send canvas get-top-level-window)])
			       (when (is-a? tlw frame:editor<%>)
				 (send tlw set-label filename))))
			   (get-canvases))))])
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
	      (super-after-load-file sucessful?))]

	   [set-filename
	    (case-lambda
	     [(name) (set-filename name #f)]
	     [(name temp?)
	      (super-set-filename name temp?)
	      (unless temp?
		(update-filename name))])]

	   [get-keymaps
	    (lambda ()
	      (cons (keymap:get-file) (super-get-keymaps)))])
	  (sequence
	    (apply super-init args))))
      
      (define backup-autosave<%>
	(interface (basic<%>)
	  backup?
	  autosave?
	  do-autosave
	  remove-autosave))

					; what about checking the autosave files when a file is opened?
      (define backup-autosave-mixin
	(mixin (basic<%>) (backup-autosave<%>) args
	  (inherit is-modified? get-filename save-file)
	  (rename [super-on-save-file on-save-file]
		  [super-on-change on-change]
		  [super-on-close on-close]
		  [super-set-modified set-modified])
	  (private
	    [auto-saved-name #f]
	    [auto-save-out-of-date? #t]
	    [auto-save-error? #f]
	    [file-old?
	     (lambda (filename)
	       (if (and filename
			(file-exists? filename))
		   (let ([modified-seconds (file-or-directory-modify-seconds filename)]
			 [old-seconds (- (current-seconds) (* 7 24 60 60))])
		     (< modified-seconds old-seconds))
		   #t))])
	  (public
	    [backup? (lambda () #t)])
	  (override
	   [on-save-file
	    (lambda (name format)
	      (super-on-save-file name format)
	      (set! auto-save-error? #f)
	      (when (and (backup?)
			 (not (eq? format 'copy))
			 (file-exists? name))
		(let ([back-name (path-utils:generate-backup-name name)])
		  (when (or (not (file-exists? back-name))
			    (file-old? back-name))
		    (when (file-exists? back-name)
		      (delete-file back-name))
		    (with-handlers ([(lambda (x) #t) void])
		      (copy-file name back-name))))))]
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
		    (remove-autosave)))
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
			[old-auto-name auto-saved-name]
			[auto-name (path-utils:generate-autosave-name orig-name)]
			[success (save-file auto-name 'copy)])
		   (if success
		       (begin
			 (when old-auto-name
			   (delete-file old-auto-name))
			 (set! auto-saved-name auto-name)
			 (set! auto-save-out-of-date? #f))
		       (begin
			 (message-box 
			  "Warning"
			  (format "Error autosaving ~s.~n~a~n~a"
				  (or orig-name "Untitled")
				  "Autosaving is turned off"
				  "until the file is saved."))
			 (set! auto-save-error? #t))))))]
	    [remove-autosave
	     (lambda ()
	       (when auto-saved-name
		 (when (file-exists? auto-saved-name)
		   (delete-file auto-saved-name))
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
			(when (is-a? frame frame:info<%>)
			  (send frame lock-status-changed)))))
	       'framework:update-lock-icon))])
	  (sequence (apply super-init args)))))))
