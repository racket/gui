(module main mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "sig.ss"
	   "../gui-utils.ss"
	   "../macro.ss"
           (lib "string-constant.ss" "string-constants")
	   (lib "mred-sig.ss" "mred"))
  
  (provide main@)

  (define main@
    (unit/sig framework:main^
      (import mred^
	      [preferences : framework:preferences^]
	      [exit : framework:exit^]
	      [group : framework:group^])
      
      ;; preferences
      (preferences:set-default 'framework:recently-opened-sort-by 'age 
                               (lambda (x) (or (eq? x 'age) (eq? x 'name))))
      (preferences:set-default 'framework:recent-items-window-w 400 number?)
      (preferences:set-default 'framework:recent-items-window-h 600 number?)
      (preferences:set-default 'framework:open-here? #f boolean?)
      (preferences:set-default 'framework:show-delegate? #f boolean?)
      (preferences:set-default 'framework:recently-opened-files/pos 
                               null 
                               (lambda (x) (and (list? x) 
                                                (andmap
                                                 (lambda (x) 
                                                   (and (list? x)
                                                        (= 3 (length x))
                                                        (string? (car x))
                                                        (number? (cadr x))
                                                        (number? (caddr x))))
                                                 x))))
      (preferences:set-default 'framework:search-using-dialog? #t boolean?)
      (preferences:set-default 'framework:windows-mdi #f boolean?)
      (preferences:set-default 'framework:menu-bindings #t boolean?)
      (preferences:set-default 'framework:verify-change-format #f boolean?)
      (preferences:set-default 'framework:auto-set-wrap? #t boolean?)
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
	(for-each (lambda (x) 
		    (hash-table-put! hash-table x 'define))
		  '(define defmacro define-macro
		     define-values
                     define/public define/override define/private define/field
		     define-signature 
                     define-syntax define-syntaxes
                     define-schema))
	(for-each (lambda (x) 
		    (hash-table-put! hash-table x 'begin))
		  '(cond case-lambda
		    begin begin0 delay
		    unit compound-unit compound-unit/sig
		    public private override
		    inherit sequence))
	(for-each (lambda (x) 
		    (hash-table-put! hash-table x 'lambda))
		  '(
                    instantiate super-instantiate
                    lambda let let* letrec recur
                     letrec-values
                     with-syntax
                     module
                     match match-lambda match-lambda* match-let match-let* match-letrec
                     let/cc let/ec letcc catch
                     let-syntax letrec-syntax 
                     syntax-case syntax-case*
                     let-signature fluid-let
                     let-struct let-macro let-values let*-values
                     case when unless 
                     let-enumerate
                     class class* class-asi class-asi* class*/names
                     class100 class100* class100-asi class100-asi* class100*/names
                     rec
                     make-object mixin
                     define-some do opt-lambda send*
                     define-record
                     local catch shared
                     unit/sig unit/lang
                     with-handlers
                     interface
                     parameterize
                     call-with-input-file with-input-from-file
                     with-input-from-port call-with-output-file
                     with-output-to-file with-output-to-port))
	(preferences:set-default 'framework:tabify hash-table hash-table?)
	(preferences:set-un/marshall
	 'framework:tabify 
	 (lambda (t) (hash-table-map t list))
	 (lambda (l) (let ([h (make-hash-table)])
		       (for-each (lambda (x) (apply hash-table-put! h x)) l)
		       h))))
      
      
      (preferences:set-default 'framework:autosave-delay 300 number?)
      (preferences:set-default 'framework:autosaving-on? #t boolean?)
      (preferences:set-default 'framework:backup-files? #t boolean?)
      (preferences:set-default 'framework:verify-exit #t boolean?)
      (preferences:set-default 'framework:delete-forward? 
			       (not (eq? (system-type) 'unix))
			       boolean?)
      (preferences:set-default 'framework:show-periods-in-dirlist #f boolean?)
      (preferences:set-default
       'framework:file-dialogs
       'std
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
				   (queue-callback (lambda () (exit)))))
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
      
      (exit:insert-can?-callback 
       (lambda ()
         (or (preferences:save)
             (exit-anyway?))))
      
      (define (exit-anyway?)
        (gui-utils:get-choice
         (string-constant still-locked-exit-anyway?)
         (string-constant yes)
         (string-constant no)
         (string-constant drscheme)))
      
      (preferences:read)

      ;; reset these -- they are only for the test suite.
      ;; they do not need to be set across starting up and shutting down
      ;; the application.
      (preferences:set 'framework:file-dialogs 'std)
      (preferences:set 'framework:exit-when-no-frames #t)
      
      (void))))
