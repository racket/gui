(dunit/sig framework:main^
  (import mred-interfaces^
	  [preferences : framework:preferences^]
	  [exit : framework:exit^]
	  [group : framework:group^]
	  [mzlib:function : mzlib:function^])
  
  ;; preferences

  (preferences:set-default 'framework:verify-change-format #f boolean?)
  
  (preferences:set-default 'framework:auto-set-wrap? #f boolean?)
  
  (preferences:set-default 'framework:display-line-numbers #t boolean?)
  
  (preferences:set-default 'framework:show-status-line #t boolean?)
  (preferences:set-default 'framework:line-offsets #t boolean?)
  
  (preferences:set-default
   'framework:print-output-mode
   'standard
   (lambda (x) (or (eq? x 'standard) (eq? x 'postscript))))
  
  (preferences:set-default 'framework:highlight-parens #t boolean?)
  (preferences:set-default 'framework:fixup-parens #t boolean?)
  (preferences:set-default 'framework:paren-match #t boolean?)
  (let ([hash-table (make-hash-table)])
    (for-each (lambda (x) (hash-table-put! hash-table x 'define))
	      '(define defmacro define-macro
		 define-values
		 define-signature define-syntax define-schema))
    (for-each (lambda (x) (hash-table-put! hash-table x 'begin))
	      '(cond 
		 begin begin0 delay
		 unit compound-unit compound-unit/sig
		 public private
		 inherit inherit-from
		 rename rename-from
		 share share-from
		 sequence))
    (for-each (lambda (x) (hash-table-put! hash-table x 'lambda))
	      '(lambda let let* letrec recur
		 let/cc let/ec letcc catch
		 let-syntax letrec-syntax syntax-case
		 let-signature fluid-let
		 let-struct let-macro let-values let*-values
		 case when unless match
		 let-enumerate
		 class class* class-asi class-asi*
		 define-some do opt-lambda send*
		 local catch shared
		 unit/sig
		 with-handlers with-parameterization
		 interface
		 parameterize
		 call-with-input-file with-input-from-file
		 with-input-from-port call-with-output-file
		 with-output-to-file with-output-to-port))
    (preferences:set-un/marshall
     'framework:tabify 
     (lambda (t) (hash-table-map t list))
     (lambda (l) (let ([h (make-hash-table)])
		   (for-each (lambda (x) (apply hash-table-put! h x)) l)
		   h)))
    (preferences:set-default 'framework:tabify hash-table hash-table?))
  
  
  (preferences:set-default 'framework:autosave-delay 300 number?)
  (preferences:set-default 'framework:autosaving-on? #t boolean?)
  (preferences:set-default 'framework:verify-exit #t boolean?)
  (preferences:set-default 'framework:delete-forward? 
			   (not (eq? (system-type) 'unix))
			   boolean?)
  (preferences:set-default 'framework:show-periods-in-dirlist #f boolean?)
  (preferences:set-default
   'framework:file-dialogs
   (if (eq? (system-type) 'unix)
       'common
       'std)
   (lambda (x)
     (or (eq? x 'common)
	 (eq? x 'std))))
  
  (preferences:add-panel
   "Indenting"
   (lambda (p)
     (let*-values
	 ([(get-keywords)
	   (lambda (hash-table)
	     (letrec ([all-keywords (hash-table-map hash-table list)]
		      [pick-out (lambda (wanted in out)
				  (cond
				    [(null? in) (mzlib:function:quicksort out string<=?)]
				    [else (if (eq? wanted (cadr (car in))) 
					      (pick-out wanted (cdr in) (cons (symbol->string (car (car in))) out))
					      (pick-out wanted (cdr in) out))]))])
	       (values  (pick-out 'begin all-keywords null)
			(pick-out 'define all-keywords null)
			(pick-out 'lambda all-keywords null))))]
	  [(begin-keywords define-keywords lambda-keywords)
	   (get-keywords (preferences:get 'framework:tabify))])
       (let* ([add-callback
	       (lambda (keyword-type keyword-symbol list-box)
		 (lambda (button command)
		   (let ([new-one (get-text-from-user
				   (string-append "Enter new " keyword-type "-like keyword:")
				   (string-append keyword-type " Keyword"))])
		     (when new-one
		       (let ([parsed (with-handlers ((exn:read? (lambda (x) #f)))
				       (read (open-input-string new-one)))])
			 (cond
			   [(and (symbol? parsed)
				 (hash-table-get (preferences:get 'framework:tabify)
						 parsed
						 (lambda () #f)))
			    (message-box "Error"
					 (format "\"~a\" is already a specially indented keyword" parsed))]
			   [(symbol? parsed)
			    (hash-table-put! (preferences:get 'framework:tabify)
					     parsed keyword-symbol)
			    (send list-box append (symbol->string parsed))]
			   [else (message-box "Error" (format "expected a symbol, found: ~a" new-one))]))))))]
	      [delete-callback
	       (lambda (list-box)
		 (lambda (button command)
		   (let* ([selections (send list-box get-selections)]
			  [symbols (map (lambda (x) (string->symbol (send list-box get-string x))) selections)])
		     (for-each (lambda (x) (send list-box delete x)) (reverse selections))
		     (let ([ht (preferences:get 'framework:tabify)])
		       (for-each (lambda (x) (hash-table-remove! ht x)) symbols)))))]
	      [main-panel (make-object horizontal-panel% p)]
	      [make-column
	       (lambda (string symbol keywords)
		 (let* ([vert (make-object vertical-panel% main-panel)]
			[_ (make-object message% (string-append string "-like Keywords") vert)]
			[box (make-object list-box% #f keywords vert #f 'multiple void)]
			[button-panel (make-object horizontal-panel% vert)]
			[add-button (make-object button% "Add" (add-callback string symbol box) button-panel)]
			[delete-button (make-object button% "Remove" (delete-callback box) button-panel)])
		   (send* button-panel 
		     (major-align-center)
		     (stretchable-in-y #f))
		   (send add-button user-min-width (send delete-button get-width))
		   box))]
	      [begin-list-box (make-column "Begin" 'begin begin-keywords)]
	      [define-list-box (make-column "Define" 'define define-keywords)]
	      [lambda-list-box (make-column "Lambda" 'lambda lambda-keywords)]
	      [update-list-boxes
	       (lambda (hash-table)
		 (let-values ([(begin-keywords define-keywords lambda-keywords) (get-keywords hash-table)]
			      [(reset) (lambda (list-box keywords)
					 (send list-box clear)
					 (for-each (lambda (x) (send list-box append x)) keywords))])
		   (reset begin-list-box begin-keywords)
		   (reset define-list-box define-keywords)
		   (reset lambda-list-box lambda-keywords)
		   #t))])
	 (preferences:add-callback 'framework:tabify (lambda (p v) (update-list-boxes v)))
	 main-panel))))
  
  (preferences:read)
  
  ;; groups
  
  (define at-most-one-maker
    (lambda ()
      (let ([s (make-semaphore 1)]
	    [test #f])
	(lambda (return thunk)
	  (semaphore-wait s)
	  (if test
	      (begin (semaphore-post s)
		     return)
	      (begin
		(set! test #t)
		(semaphore-post s)
		(begin0 (thunk)
			(semaphore-wait s)
			(set! test #f)
			(semaphore-post s))))))))
  
  (let ([at-most-one (at-most-one-maker)])
    (send group:the-frame-group set-empty-callbacks
	  (lambda () 
	    (at-most-one (void) 
			 (lambda () (exit:exit #t))))
	  (lambda () 
	    (at-most-one #t
			 (lambda ()
			   (exit:run-callbacks)))))
  
    (exit:insert-callback
     (lambda ()
       (at-most-one
	#t
	(lambda ()
	  (send group:the-frame-group close-all))))))
  
  ;; misc other stuff
  
  (exit:insert-callback 
   (lambda ()
     (with-handlers ([(lambda (x) #t)
		      (lambda (exn)
			(message-box
			 "Saving Prefs"
			 (format "Error saving preferences: ~a"
				 (exn-message exn))))])
       (preferences:save))))
  
  ;(wx:application-file-handler edit-file) ;; how to handle drag and drop?
  )
