
(module editor mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   (lib "class100.ss")
           (lib "string-constant.ss" "string-constants")
	   "sig.ss"
	   "../gui-utils.ss"
	   "../macro.ss"
           (lib "etc.ss")
	   (lib "mred-sig.ss" "mred")
	   (lib "file.ss"))

  (provide editor@)

  (define editor@
    (unit/sig framework:editor^
      (import mred^
	      [autosave : framework:autosave^]
	      [finder : framework:finder^]
	      [path-utils : framework:path-utils^]
	      [keymap : framework:keymap^]
	      [icon : framework:icon^]
	      [preferences : framework:preferences^]
	      [text : framework:text^]
	      [pasteboard : framework:pasteboard^]
	      [frame : framework:frame^]
              [handler : framework:handler^])

      (rename [-keymap<%> keymap<%>])
      
      ;; renaming, for editor-mixin where get-file is shadowed by a method.
      (define mred:get-file get-file) 

      (define basic<%>
	(interface (editor<%>)
	  has-focus?
	  editing-this-file?
	  local-edit-sequence?
	  run-after-edit-sequence
	  get-top-level-window
	  on-close
	  save-file-out-of-date?
          save-file/gui-error))

      (define basic-mixin
	(mixin (editor<%>) (basic<%>)
	  
	  (inherit get-filename save-file)
	  (define/public save-file/gui-error
	    (opt-lambda ([input-filename #f]
			 [fmt 'same]
			 [show-errors? #t])
	      (let ([filename (if (or (not input-filename)
				      (equal? input-filename ""))
				  (let ([internal-filename (get-filename)])
				    (if (or (not internal-filename)
					    (equal? internal-filename ""))
					(mred:get-file)
					internal-filename))
				  input-filename)])
		(if filename
		    (let ([result (save-file filename fmt #f)])
		      (unless result
			(when show-errors?
			  (message-box
			   (string-constant error-saving)
			   (format (string-constant error-saving-file/name)
				   filename))))
		      result)
		    #f))))
	  
	  (inherit refresh-delayed? 
		   get-canvas
		   get-max-width get-admin)
	  
	  (rename [super-can-save-file? can-save-file?])
	  (override can-save-file?)
	  [define can-save-file?
	    (lambda (filename format)
	      (and (if (equal? filename (get-filename))
		       (if (save-file-out-of-date?)
			   (gui-utils:get-choice 
			    (string-constant file-has-been-modified)
			    (string-constant overwrite-file-button-label)
			    (string-constant cancel)
			    (string-constant warning)
			    #f
			    (get-top-level-focus-window))
			   #t)
		       #t)
		   (super-can-save-file? filename format)))]
	  
	  (rename [super-after-save-file after-save-file]
		  [super-after-load-file after-load-file])
	  [define last-saved-file-time #f]
	  
	  [define/override after-save-file
	    (lambda (sucess?)
	      
	      ;; update recently opened file names
	      (let* ([temp-b (box #f)]
		     [filename (get-filename temp-b)])
		(unless (unbox temp-b)
		  (when filename
		    (handler:add-to-recent filename))))
	      
	      ;; update last-saved-file-time
	      (when sucess?
		(let ([filename (get-filename)])
		  (set! last-saved-file-time
			(and filename
			     (file-exists? filename)
			     (file-or-directory-modify-seconds filename)))))
	      
	      (super-after-save-file sucess?))]
	  
	  [define/override after-load-file
	    (lambda (sucess?)
	      (when sucess?
		(let ([filename (get-filename)])
		  (set! last-saved-file-time
			(and filename
			     (file-exists? filename)
			     (file-or-directory-modify-seconds filename)))))
	      (super-after-load-file sucess?))]
	  (public save-file-out-of-date?)
	  [define save-file-out-of-date?
	    (lambda ()
	      (and 
	       last-saved-file-time
	       (let ([fn (get-filename)])
		 (and fn
		      (file-exists? fn)
		      (let ([ms (file-or-directory-modify-seconds fn)])
			(< last-saved-file-time ms))))))]
	  
	  [define has-focus #f]
	  (rename [super-on-focus on-focus])
	  (override on-focus)
	  [define on-focus
	    (lambda (x)
	      (set! has-focus x))]
	  (public has-focus?)
	  [define has-focus?
	    (lambda ()
	      has-focus)]
	  
	  (public on-close get-top-level-window)
	  [define on-close (lambda () (void))]
	  [define get-top-level-window
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
		     #f]))))]
	  
	  (public editing-this-file?)
	  [define editing-this-file? (lambda () #f)]
	  
	  [define edit-sequence-queue null]
	  [define edit-sequence-ht (make-hash-table)]
	  [define in-local-edit-sequence? #f]
	  (public local-edit-sequence? run-after-edit-sequence extend-edit-sequence-queue)
	  [define local-edit-sequence? (lambda () in-local-edit-sequence?)]
	  [define run-after-edit-sequence
	    (case-lambda 
	     [(t) (run-after-edit-sequence t #f)]
	     [(t sym)
	      (unless (and (procedure? t)
			   (= 0 (procedure-arity t)))
		(error 'editor:basic::run-after-edit-sequence
		       "expected procedure of arity zero, got: ~s~n" t))
	      (unless (or (symbol? sym) (not sym))
		(error 'editor:basic::run-after-edit-sequence
		       "expected second argument to be a symbol or #f, got: ~s~n"
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
	  [define extend-edit-sequence-queue
	    (lambda (l ht)
	      (hash-table-for-each ht (lambda (k t)
					(hash-table-put! 
					 edit-sequence-ht
					 k t)))
	      (set! edit-sequence-queue (append l edit-sequence-queue)))]
	  (rename
	   [super-after-edit-sequence after-edit-sequence]
	   [super-on-edit-sequence on-edit-sequence])
	  (override on-edit-sequence after-edit-sequence)
	  [define on-edit-sequence
	    (lambda ()
	      (super-on-edit-sequence)
	      (set! in-local-edit-sequence? #t))]
	  [define after-edit-sequence
	    (lambda ()
	      (set! in-local-edit-sequence? #f)
	      (super-after-edit-sequence)
	      (let ([queue edit-sequence-queue]
		    [ht edit-sequence-ht]
		    [find-enclosing-editor
		     (lambda (editor)
		       (let ([admin (send editor get-admin)])
			 (cond
			   [(is-a? admin editor-snip-editor-admin<%>)
			    (send (send (send admin get-snip) get-admin) get-editor)]
			   [else #f])))])
		(set! edit-sequence-queue null)
		(set! edit-sequence-ht (make-hash-table))
		(let loop ([editor (find-enclosing-editor this)])
		  (cond
		    [(and editor 
                          (is-a? editor basic<%>)
                          (not (send editor local-edit-sequence?)))
		     (loop (find-enclosing-editor editor))]
		    [(and editor
                          (is-a? editor basic<%>))
                     (send editor extend-edit-sequence-queue queue ht)]
		    [else
		     (hash-table-for-each ht (lambda (k t) (t)))
		     (for-each (lambda (t) (t)) queue)]))))]
	  
	  (override on-new-box)
	  [define on-new-box
	    (lambda (type)
	      (cond
		[(eq? type 'text) (make-object editor-snip% (make-object text:basic%))]
		[else (make-object editor-snip% (make-object pasteboard:basic%))]))]
	  
	  
	  (override get-file put-file)
	  [define get-file (lambda (d) 
			     (parameterize ([finder:dialog-parent-parameter
					     (get-top-level-window)])
			       (finder:get-file d)))]
	  [define put-file (lambda (d f) (parameterize ([finder:dialog-parent-parameter
							 (get-top-level-window)])
					   (finder:put-file f d)))]
	  
	  
	  (super-instantiate ())))

      (define -keymap<%> (interface (basic<%>) get-keymaps))
      (define keymap-mixin
	(mixin (basic<%>) (-keymap<%>)
	  (public get-keymaps)
          [define get-keymaps
            (lambda ()
              (list (keymap:get-global)))]
	  (inherit set-keymap)

          (super-instantiate ())
          (let ([keymap (make-object keymap:aug-keymap%)])
            (set-keymap keymap)
            (for-each (lambda (k) (send keymap chain-to-keymap k #f))
                      (get-keymaps)))))

      (define autowrap<%> (interface (basic<%>)))
      (define autowrap-mixin
	(mixin (basic<%>) (autowrap<%>)
	  (inherit auto-wrap)
          (super-instantiate ())
          (auto-wrap 
           (preferences:get
            'framework:auto-set-wrap?))))
      
      (define file<%> (interface (-keymap<%>)))
      (define file-mixin
	(mixin (-keymap<%>) (file<%>)
	  (inherit get-filename lock get-style-list 
		   is-modified? change-style set-modified 
		   get-top-level-window)
	  (rename [super-after-save-file after-save-file]
		  [super-after-load-file after-load-file]
		  [super-get-keymaps get-keymaps]
		  [super-set-filename set-filename])
	  
	  (override editing-this-file?)
          [define editing-this-file? (lambda () #t)]

	  (inherit get-canvases)
          [define check-lock
            (lambda ()
              (let* ([filename (get-filename)]
                     [lock? (and filename
                                 (file-exists? filename)
                                 (not (member
                                       'write
                                       (file-or-directory-permissions
                                        filename))))])
                (lock lock?)))]
          [define update-filename
           (lambda (name)
             (let ([filename (if name
                                 (file-name-from-path (normalize-path name))
                                 (gui-utils:next-untitled-name))])
               (for-each (lambda (canvas)
                           (let ([tlw (send canvas get-top-level-window)])
                             (when (and (is-a? tlw frame:editor<%>)
                                        (eq? this (send tlw get-editor)))
                               (send tlw set-label filename))))
                         (get-canvases))))]
	  (override after-save-file after-load-file set-filename get-keymaps)
          [define after-save-file
	    (lambda (success)
	      (when success
		(check-lock))
	      (super-after-save-file success))]
	   
          [define after-load-file
           (lambda (sucessful?)
             (when sucessful?
               (check-lock))
             (super-after-load-file sucessful?))]

          [define set-filename
	    (case-lambda
	     [(name) (set-filename name #f)]
	     [(name temp?)
	      (super-set-filename name temp?)
	      (unless temp?
		(update-filename name))])]

          [define get-keymaps
	    (lambda ()
	      (cons (keymap:get-file) (super-get-keymaps)))]
          (super-instantiate ())))
      
      (define backup-autosave<%>
	(interface (basic<%>)
	  backup?
	  autosave?
	  do-autosave
	  remove-autosave))

      ; what about checking the autosave files when a file is opened?
      (define backup-autosave-mixin
	(mixin (basic<%>) (backup-autosave<%> autosave:autosavable<%>)
	  (inherit is-modified? get-filename save-file)
	  (rename [super-on-save-file on-save-file]
		  [super-on-change on-change]
		  [super-on-close on-close]
		  [super-set-modified set-modified])
          [define auto-saved-name #f]
          [define auto-save-out-of-date? #t]
          [define auto-save-error? #f]
          [define file-old?
            (lambda (filename)
              (if (and filename
                       (file-exists? filename))
                  (let ([modified-seconds (file-or-directory-modify-seconds filename)]
                        [old-seconds (- (current-seconds) (* 7 24 60 60))])
                    (< modified-seconds old-seconds))
                  #t))]
	  (public backup?)
          [define backup? (lambda () (preferences:get 'framework:backup-files?))]
	  (override on-save-file on-close on-change set-modified)
          [define on-save-file
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
          [define on-close
           (lambda ()
             (super-on-close)
             (remove-autosave)
             (set! do-autosave? #f))]
          [define on-change
	    (lambda ()
	      (super-on-change)
	      (set! auto-save-out-of-date? #t))]
          [define set-modified
	    (lambda (modified?)
	      (when auto-saved-name
		(if modified?
		    (set! auto-save-out-of-date? #t)
		    (remove-autosave)))
	      (super-set-modified modified?))]
          [define do-autosave? #t]
	  (public autosave? do-autosave remove-autosave)
          [define autosave? (lambda () do-autosave?)]
          [define do-autosave
            (lambda ()
              (when (and (autosave?)
                         (not auto-save-error?)
                         (is-modified?)
                         (or (not auto-saved-name)
                             auto-save-out-of-date?))
                (let* ([orig-name (get-filename)]
                       [old-auto-name auto-saved-name]
                       [auto-name (path-utils:generate-autosave-name orig-name)]
                       [success (save-file auto-name 'copy #f)])
                  (if success
                      (begin
                        (when old-auto-name
                          (delete-file old-auto-name))
                        (set! auto-saved-name auto-name)
                        (set! auto-save-out-of-date? #f))
                      (begin
                        (message-box 
                         (string-constant warning)
                         (string-append
                          (format (string-constant error-autosaving)
                                  (or orig-name (string-constant untitled)))
                          "\n"
                          (string-constant autosaving-turned-off)))
                        (set! auto-save-error? #t))))))]
          [define remove-autosave
            (lambda ()
              (when auto-saved-name
                (when (file-exists? auto-saved-name)
                  (delete-file auto-saved-name))
                (set! auto-saved-name #f)))]
	  (super-instantiate ())
          (autosave:register this)))

      (define info<%> (interface (basic<%>)))
      (define info-mixin
	(mixin (basic<%>) (info<%>)
	  (inherit get-top-level-window run-after-edit-sequence)
	  (rename [super-lock lock])
	  (override lock)
          [define lock
	    (lambda (x)
	      (super-lock x)
	      (run-after-edit-sequence
	       (rec send-frame-update-lock-icon
                 (lambda ()
                   (let ([frame (get-top-level-window)])
                     (when (is-a? frame frame:info<%>)
                       (send frame lock-status-changed)))))
	       'framework:update-lock-icon))]
          (super-instantiate ()))))))
