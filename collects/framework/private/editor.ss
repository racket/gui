
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
	   (lib "file.ss")
           (lib "list.ss"))

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
          save-file/gui-error
          load-file/gui-error))

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
					(put-file #f #f)
					internal-filename))
				  input-filename)])
                (with-handlers ([not-break-exn?
                                 (lambda (exn)
                                   (message-box
                                    (string-constant error-saving)
                                    (string-append
                                     (format (string-constant error-saving-file/name) 
                                             filename)
                                     "\n\n"
				     (format-error-message exn))
                                    #f
                                    '(stop ok))
                                   #f)])
                  (when filename
                    (save-file filename fmt #f))
                  #t))))
          
          (inherit load-file)
          (define/public load-file/gui-error
            (opt-lambda ([input-filename #f]
                         [fmt 'guess]
                         [show-errors? #t])
	      (let ([filename (if (or (not input-filename)
                                      (equal? input-filename ""))
                                  (let ([internal-filename (get-filename)])
                                    (if (or (not internal-filename)
                                            (equal? internal-filename ""))
                                        (get-file #f)
                                        internal-filename))
                                  input-filename)])
                (with-handlers ([not-break-exn?
                                 (lambda (exn)
                                   (message-box 
                                    (string-constant error-loading)
				    (string-append
				     (format (string-constant error-loading-file/name)
					     filename)
				     "\n\n"
				     (format-error-message exn))
                                    #f
                                    '(stop ok))
                                   #f)])
                  (load-file input-filename fmt show-errors?)
                  #t))))
	  
	  (define/private (format-error-message exn)
	    (let ([sp (open-output-string)])
	      (parameterize ([current-output-port sp])
		((error-display-handler)
		 (if (exn? exn)
		     (format "~a" (exn-message exn))
		     (format "uncaught exn: ~s" exn))
		 exn))
	      (get-output-string sp)))

	  (inherit refresh-delayed? 
		   get-canvas
		   get-max-width get-admin)
	  
	  (rename [super-can-save-file? can-save-file?])
	  (define/override (can-save-file? filename format)
            (and (if (equal? filename (get-filename))
                     (if (save-file-out-of-date?)
                         (gui-utils:get-choice 
                          (string-constant file-has-been-modified)
                          (string-constant overwrite-file-button-label)
                          (string-constant cancel)
                          (string-constant warning)
                          #f
                          (get-top-level-window))
                         #t)
                     #t)
                 (super-can-save-file? filename format)))
	  
	  (rename [super-after-save-file after-save-file]
		  [super-after-load-file after-load-file])
	  (define last-saved-file-time #f)
	  
	  (define/override (after-save-file success?)
            ;; update recently opened file names
            (let* ([temp-b (box #f)]
                   [filename (get-filename temp-b)])
              (unless (unbox temp-b)
                (when filename
                  (handler:add-to-recent filename))))
            
            ;; update last-saved-file-time
            (when success?
              (let ([filename (get-filename)])
                (set! last-saved-file-time
                      (and filename
                           (file-exists? filename)
                           (file-or-directory-modify-seconds filename)))))
            
            (super-after-save-file success?))
	  
	  (define/override (after-load-file success?)
            (when success?
              (let ([filename (get-filename)])
                (set! last-saved-file-time
                      (and filename
                           (file-exists? filename)
                           (file-or-directory-modify-seconds filename)))))
            (super-after-load-file success?))
	  (define/public (save-file-out-of-date?)
            (and last-saved-file-time
                 (let ([fn (get-filename)])
                   (and fn
                        (file-exists? fn)
                        (let ([ms (file-or-directory-modify-seconds fn)])
                          (< last-saved-file-time ms))))))
	  
	  (define has-focus #f)
	  (rename [super-on-focus on-focus])
	  (define/override (on-focus x)
            (set! has-focus x)
            (super-on-focus x))
	  (define/public (has-focus?) has-focus)
	  
	  (define/public (on-close) (void))
	  (define/public (get-top-level-window)
            (let loop ([text this])
              (let ([editor-admin (send text get-admin)])
                (cond
                  [(is-a? editor-admin editor-snip-editor-admin<%>)
                   (let* ([snip (send editor-admin get-snip)]
                          [snip-admin (send snip get-admin)])
                     (loop (send snip-admin get-editor)))]
                  [(send text get-canvas) 
                   => 
                   (lambda (canvas)
                     (send canvas get-top-level-window))]
                  [else #f]))))
	  
	  [define/public editing-this-file? (lambda () #f)]
	  
	  [define edit-sequence-queue null]
	  [define edit-sequence-ht (make-hash-table)]
	  [define in-local-edit-sequence? #f]
	  [define/public local-edit-sequence? (lambda () in-local-edit-sequence?)]
	  [define/public run-after-edit-sequence
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
			   '(message-box "run-after-edit-sequence error"
                                         (format "refresh-delayed? is #t but snip admin, ~s, is not an editor-snip-editor-admin<%>"
                                                 snip-admin))
			   '(t)
                           (void)])))
		  (t))
	      (void)])]
	  [define/public extend-edit-sequence-queue
	    (lambda (l ht)
	      (hash-table-for-each ht (lambda (k t)
					(hash-table-put! 
					 edit-sequence-ht
					 k t)))
	      (set! edit-sequence-queue (append l edit-sequence-queue)))]
	  (rename
	   [super-after-edit-sequence after-edit-sequence]
	   [super-on-edit-sequence on-edit-sequence])
	  [define/override on-edit-sequence
	    (lambda ()
	      (super-on-edit-sequence)
	      (set! in-local-edit-sequence? #t))]
	  [define/override after-edit-sequence
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
	  
	  [define/override on-new-box
	    (lambda (type)
	      (cond
		[(eq? type 'text) (make-object editor-snip% (make-object text:basic%))]
		[else (make-object editor-snip% (make-object pasteboard:basic%))]))]
	  
	  
	  (define/override (get-file d)
            (parameterize ([finder:dialog-parent-parameter
                            (get-top-level-window)])
              (finder:get-file d)))
	  (define/override (put-file d f)
            (parameterize ([finder:dialog-parent-parameter
                            (get-top-level-window)])
              (finder:put-file f d)))
	  
	  
	  (super-instantiate ())))

      (define standard-style-list (new style-list%))
      (define (get-standard-style-list) standard-style-list)
      
      (let ([delta (make-object style-delta% 'change-normal)])
        (send delta set-delta 'change-family 'modern)
        (let ([style (send standard-style-list find-named-style "Standard")])
          (if style
              (send style set-delta delta)
              (send standard-style-list new-named-style "Standard"
                    (send standard-style-list find-or-create-style
                          (send standard-style-list find-named-style "Basic")
                          delta)))))
      
      (define (set-font-size size)
        (update-standard-style
         (lambda (scheme-delta)
           (send scheme-delta set-size-mult 0)
           (send scheme-delta set-size-add size))))
      
      (define (set-font-name name)
        (update-standard-style
         (lambda (scheme-delta)
           (send scheme-delta set-delta-face name)
           (send scheme-delta set-family 'modern))))
      
      (define (set-font-smoothing sym)
        (update-standard-style
         (lambda (scheme-delta)
           (send scheme-delta set-smoothing-on sym))))
      
      (define (update-standard-style cng-delta)
        (let* ([scheme-standard (send standard-style-list find-named-style "Standard")]
               [scheme-delta (make-object style-delta%)])
          (send scheme-standard get-delta scheme-delta)
          (cng-delta scheme-delta)
          (send scheme-standard set-delta scheme-delta)))
      
      (define standard-style-list<%>
        (interface (editor<%>)
          ))
      
      (define standard-style-list-mixin
        (mixin (editor<%>) (standard-style-list<%>)
          (super-instantiate ())
          (inherit set-style-list set-load-overwrites-styles)
          (set-style-list standard-style-list)
	  (set-load-overwrites-styles #f)))
          
      (define (set-standard-style-list-pref-callbacks)
        (set-font-size (preferences:get 'framework:standard-style-list:font-size))
        (set-font-name (preferences:get 'framework:standard-style-list:font-name))
        (set-font-smoothing (preferences:get 'framework:standard-style-list:smoothing))
        (preferences:add-callback 'framework:standard-style-list:font-size (lambda (p v) (set-font-size v)))
        (preferences:add-callback 'framework:standard-style-list:font-name (lambda (p v) (set-font-name v)))
        (preferences:add-callback 'framework:standard-style-list:smoothing (lambda (p v) (set-font-smoothing v)))
        
        (unless (member (preferences:get 'framework:standard-style-list:font-name) (get-face-list 'mono))
          (preferences:set 'framework:standard-style-list:font-name (get-family-builtin-face 'modern))))
      
      ;; set-standard-style-list-delta : string (is-a?/c style-delta<%>) -> void
      (define (set-standard-style-list-delta name delta)
        (let* ([style-list (get-standard-style-list)]
               [style (send style-list find-named-style name)])
          (if style
              (send style set-delta delta)
              (send style-list new-named-style name
                    (send style-list find-or-create-style
                          (send style-list find-named-style "Standard")
                          delta)))
          (void)))
      
      (define -keymap<%> (interface (basic<%>) get-keymaps))
      (define keymap-mixin
	(mixin (basic<%>) (-keymap<%>)
	  [define/public get-keymaps
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
	  
          (define/override (editing-this-file?) #t)

	  (inherit get-canvases)
          (define/private (check-lock)
            (let* ([filename (get-filename)]
                   [lock? (and filename
                               (file-exists? filename)
                               (not (member
                                     'write
                                     (file-or-directory-permissions
                                      filename))))])
              (lock lock?)))
          (define/private (update-filename name)
            (let ([filename (if name
                                (file-name-from-path (normalize-path name))
                                (gui-utils:next-untitled-name))])
              (for-each (lambda (canvas)
                          (let ([tlw (send canvas get-top-level-window)])
                            (when (and (is-a? tlw frame:editor<%>)
                                       (eq? this (send tlw get-editor)))
                              (send tlw set-label filename))))
                        (get-canvases))))
	  (define/override (after-save-file success)
            (when success
              (check-lock))
            (super-after-save-file success))
	   
          (define/override (after-load-file sucessful?)
            (when sucessful?
              (check-lock))
            (super-after-load-file sucessful?))

          (define/override set-filename
	    (case-lambda
	     [(name) (set-filename name #f)]
	     [(name temp?)
	      (super-set-filename name temp?)
	      (unless temp?
		(update-filename name))]))

          (define/override (get-keymaps)
            (cons (keymap:get-file) (super-get-keymaps)))
          (super-instantiate ())))
      
      (define backup-autosave<%>
	(interface (basic<%>)
	  backup?
	  autosave?
	  do-autosave
	  remove-autosave))

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
          (define/private (file-old? filename)
            (if (and filename
                     (file-exists? filename))
                (let ([modified-seconds (file-or-directory-modify-seconds filename)]
                      [old-seconds (- (current-seconds) (* 7 24 60 60))])
                  (< modified-seconds old-seconds))
                #t))
          (define/public (backup?) (preferences:get 'framework:backup-files?))
          (define/override (on-save-file name format)
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
                    (copy-file name back-name))))))
          (define/override (on-close)
            (super-on-close)
            (remove-autosave)
            (set! do-autosave? #f))
          (define/override (on-change)
            (super-on-change)
            (set! auto-save-out-of-date? #t))
          (define/override (set-modified modified?)
            (when auto-saved-name
              (if modified?
                  (set! auto-save-out-of-date? #t)
                  (remove-autosave)))
            (super-set-modified modified?))
          
          [define do-autosave? #t]
	  (define/public (autosave?) do-autosave?)

          (define/public (do-autosave)
            (cond
              [(and (autosave?)
                    (not auto-save-error?)
                    (is-modified?)
                    (or (not auto-saved-name)
                        auto-save-out-of-date?))
               (let* ([orig-name (get-filename)]
                      [old-auto-name auto-saved-name]
                      [auto-name (path-utils:generate-autosave-name orig-name)]
		      [orig-format (and (is-a? this text%)
					(send this get-file-format))])
		 (when (is-a? this text%)
		   (send this set-file-format 'standard))
                 (with-handlers ([not-break-exn?
                                  (lambda (exn)
                                    (show-autosave-error exn orig-name)
                                    (set! auto-save-error? #t)
				    (when (is-a? this text%)
				      (send this set-file-format orig-format))
                                    #f)])
                   (save-file auto-name 'copy #f)
		   (when (is-a? this text%)
		     (send this set-file-format orig-format))
                   (when old-auto-name
                     (delete-file old-auto-name))
                   (set! auto-saved-name auto-name)
                   (set! auto-save-out-of-date? #f)
                   auto-name))]
              [else auto-saved-name]))

	  ;; show-autosave-error : any (union #f string) -> void
	  ;; opens a message box displaying the exn and the filename
	  ;; to the user.
	  (define/private (show-autosave-error exn orig-name)
	    (message-box 
	     (string-constant warning)
	     (string-append
	      (format (string-constant error-autosaving)
		      (or orig-name (string-constant untitled)))
	      "\n"
	      (string-constant autosaving-turned-off)
	      "\n\n"
	      (if (exn? exn)
		  (format "~a" (exn-message exn))
		  (format "~s" exn)))
	     #f
	     '(caution ok)))
	  
          (define/public (remove-autosave)
            (when auto-saved-name
              (when (file-exists? auto-saved-name)
                (delete-file auto-saved-name))
              (set! auto-saved-name #f)))
	  (super-instantiate ())
          (autosave:register this)))

      (define info<%> (interface (basic<%>)))
      (define info-mixin
	(mixin (basic<%>) (info<%>)
	  (inherit get-top-level-window run-after-edit-sequence)
	  (rename [super-lock lock])
	  (define callback-running? #f)
          (define/override (lock x)
            (super-lock x)
            (run-after-edit-sequence
             (rec send-frame-update-lock-icon
               (lambda ()
                 (unless callback-running?
                   (set! callback-running? #t)
                   (queue-callback
                    (lambda ()
                      (let ([frame (get-top-level-window)])
                        (when (is-a? frame frame:info<%>)
                          (send frame lock-status-changed)))
                      (set! callback-running? #f))
                    #f))))
             'framework:update-lock-icon))
          (super-instantiate ()))))))
