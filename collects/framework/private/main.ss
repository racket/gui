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
	      [group : framework:group^]
              [handler : framework:handler^]
              [editor : framework:editor^]
              [color-prefs : framework:color-prefs^]
              [scheme : framework:scheme^])
      
      (application-preferences-handler (lambda () (preferences:show-dialog)))
      
      (preferences:set-default 'framework:standard-style-list:font-name
                               (get-family-builtin-face 'modern)
                               string?)
      
      (preferences:set-default
       'framework:standard-style-list:font-size
       (let* ([txt (make-object text%)]
              [stl (send txt get-style-list)]
              [bcs (send stl basic-style)])
         (send bcs get-size))
       (lambda (x) (and (number? x) (exact? x) (integer? x) (positive? x))))
      
      (preferences:set-default
       'framework:standard-style-list:smoothing
       'default
       (lambda (x) 
         (memq x '(unsmoothed partly-smoothed smoothed default))))
      
      (editor:set-standard-style-list-pref-callbacks)
      
      (preferences:set-default 'framework:paren-match-color
                               (let ([gray-level
                                      ;; old gray-level 192
                                      (if (eq? (system-type) 'windows)
                                          (* 3/4 256)
                                          (- (* 7/8 256) 1))])
                                 (make-object color% gray-level gray-level gray-level))
                               (lambda (x) (is-a? x color%)))
      
      (preferences:set-un/marshall
       'framework:paren-match-color
       (lambda (c) (list (send c red) (send c green) (send c blue)))
       (lambda (l) (make-object color% (car l) (cadr l) (caddr l))))
      
      (preferences:set-default 'framework:last-directory (find-system-path 'home-dir) string?)
      (preferences:set-default 'framework:recent-max-count 
                               50 
                               (lambda (x) (and (number? x)
                                                (x . > . 0) 
                                                (integer? x))))
      (preferences:add-callback
       'framework:recent-max-count
       (lambda (p v)
         (handler:size-recently-opened-files v)))
      
      (preferences:set-default 'framework:last-url-string "" string?)
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
      (preferences:set-default 'framework:col-offsets #f boolean?)
      
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
                     match-lambda match-lambda*
                     define-syntax-set
		     define-values
                     define/public define/override define/private define/field
                     define/override-final define/public-final
                     define/contract
		     define-signature 
                     define-syntax define-syntaxes
                     define-schema define/contract))
	(for-each (lambda (x) 
		    (hash-table-put! hash-table x 'begin))
		  '(case-lambda
		    cond
		    begin begin0 delay
		    unit compound-unit compound-unit/sig
		    public private override
		    inherit sequence))
	(for-each (lambda (x) 
		    (hash-table-put! hash-table x 'lambda))
		  '(
		    cases
                       instantiate super-instantiate
                     syntax/loc quasisyntax/loc

                     
                     lambda let let* letrec recur
                     letrec-values
                     with-syntax
		     with-continuation-mark
                     module
		     match match-let match-let* match-letrec
                     let/cc let/ec letcc catch
                     let-syntax letrec-syntax fluid-let-syntax letrec-syntaxes+values
                     
                     kernel-syntax-case
                     syntax-case syntax-case* syntax-rules
                     let-signature fluid-let
                     let-struct let-macro let-values let*-values
                     case when unless 
                     let-enumerate
                     class class* class-asi class-asi* class*/names
                     class100 class100* class100-asi class100-asi* class100*/names
                     rec
                     make-object mixin
                     define-some do opt-lambda
		     send* with-method
                     define-record
                     local catch shared
                     unit/sig unit/lang
                     with-handlers
                     interface
                     parameterize
                     call-with-input-file call-with-input-file* with-input-from-file
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
      
      ;; scheme prefs
      
      (for-each (lambda (line)
                  (let ([sym (car line)]
                        [color (cadr line)])
                    (color-prefs:register-color-pref (scheme:short-sym->pref-name sym)
                                                     (scheme:short-sym->style-name sym)
                                                     color)))
                (scheme:get-color-prefs-table))
      (preferences:set-default 'framework:coloring-active #t boolean?)

      ;; groups
      
      (preferences:set-default 'framework:exit-when-no-frames #t boolean?)

      (exit:insert-can?-callback
       (lambda ()
         (send (group:get-the-frame-group) can-close-all?)))    
      
      (exit:insert-on-callback
       (lambda ()
         (send (group:get-the-frame-group) on-close-all)))
      
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
