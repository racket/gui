(unit/sig framework:main^
  (import mred^
	  [preferences : framework:preferences^]
	  [exit : framework:exit^]
	  [group : framework:group^])
  
  ;; preferences

  (preferences:set-default 'framework:search-using-dialog? #t boolean?)
  
  (preferences:set-default 'framework:windows-mdi #f boolean?)

  (preferences:set-default 'framework:menu-bindings #t boolean?)

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
		 public private override
		 inherit sequence))
    (for-each (lambda (x) (hash-table-put! hash-table x 'lambda))
	      '(lambda let let* letrec recur
		 let/cc let/ec letcc catch
		 let-syntax letrec-syntax syntax-case
		 let-signature fluid-let
		 let-struct let-macro let-values let*-values
		 case when unless match
		 let-enumerate
		 class class* class-asi class-asi*
                 make-object mixin
		 define-some do opt-lambda send*
		 local catch shared
		 unit/sig
		 with-handlers
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
  
  ;; groups
  
  (preferences:set-default 'framework:exit-when-no-frames #t boolean?)

  (let ([at-most-one
	 (let ([skip? #f])
	   (lambda (answer thunk)
	     (if skip?
		 answer
		 (begin
		   (set! skip? #t)
		   (begin0 (thunk)
			   (set! skip? #f))))))])

    (send (group:get-the-frame-group) set-empty-callbacks

	  ;; empty test
	  (lambda ()
	    (if (preferences:get 'framework:exit-when-no-frames)
		(at-most-one #t
			     (lambda ()
			       (exit:can-exit?)))
		#t))
	  
	  ;; empty close down
	  (lambda () 
	    (if (preferences:get 'framework:exit-when-no-frames)
		(at-most-one (void) 
			     (lambda ()
			       (exit:on-exit)
			       (exit)))
		(void))))
    
    (exit:insert-can?-callback
     (lambda ()
       (at-most-one
	#t
	(lambda ()
	  (send (group:get-the-frame-group) can-close-all?)))))    

    (exit:insert-on-callback
     (lambda ()
       (at-most-one
	#t
	(lambda ()
	  (send (group:get-the-frame-group) on-close-all))))))
  
  (exit:insert-on-callback 
   (lambda ()
     (with-handlers ([(lambda (x) (void))
		      (lambda (exn)
			(message-box
			 "Saving Prefs"
			 (format "Error saving preferences: ~a"
				 (exn-message exn))))])
       (preferences:save))))
  
  ;(wx:application-file-handler edit-file) ;; how to handle drag and drop?

  (preferences:read)
  (preferences:set 'framework:exit-when-no-frames #t)
  
  (void))
