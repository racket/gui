(let ([pred (lambda (x) (void? x))]
      [old-load-framework-automatically? (load-framework-automatically)])

  (load-framework-automatically #f)

  (test
   'macro.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "macro.ss" "framework")
      (global-defined-value 'mixin)
      (void)))
  (test
   'tests.ss
   (lambda (x) x)
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "tests.ss" "framework")
      (unit/sig? (require-library "keys.ss" "framework"))))
  (test
   'testr.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "tests.ss" "framework")
      (require-library "mred-interfaces.ss" "framework")
      (eval
       '(define-values/invoke-unit/sig
         ((unit test : framework:test^))
	 (compound-unit/sig
	   (import)
	   (link [mred : mred-interfaces^ (mred-interfaces@)]
		 [keys : framework:keys^ ((require-library "keys.ss" "framework"))]
		 [test : framework:test^ ((require-library "testr.ss" "framework") mred keys)])
	   (export (unit test)))))
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (void)))
  (test
   'test.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "test.ss" "framework")
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (void)))
  (test
   'mred-interfacess.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "mred-interfacess.ss" "framework")
      (global-defined-value 'mred-interfaces^)
      (void)))
  (test
   'mred-interfaces.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "mred-interfaces.ss" "framework")
      (global-defined-value 'mred-interfaces^)
      (global-defined-value 'mred-interfaces@)
      (void)))
  (test
   'mred-interfaces.ss/gen
   (lambda (x) x)
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "invoke.ss")
      (require-library "mred-interfaces.ss" "framework")
      (eval
       '(let ([orig-button% button%])
	  (define-values/invoke-unit/sig mred-interfaces^ mred-interfaces@)
	  (let ([first-button% button%])
	    (define-values/invoke-unit/sig mred-interfaces^ mred-interfaces@)
	    (let ([second-button% button%])
	      (and (eq? second-button% first-button%)
		   (not (eq? first-button% orig-button%)))))))))
  (test
   'frameworkr.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "frameworks.ss" "framework")
      (require-library "mred-interfaces.ss" "framework")
      (eval
       '(define-values/invoke-unit/sig
         framework^
	 (compound-unit/sig
	   (import)
	   (link [mred : mred-interfaces^ (mred-interfaces@)]
		 [core : mzlib:core^ ((require-library "corer.ss"))]
		 [framework : framework^ ((require-library "frameworkr.ss" "framework") core mred)])
	   (export (open framework)))))
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (global-defined-value 'frame:basic-mixin)
      (global-defined-value 'editor:basic-mixin)
      (global-defined-value 'exit:exit)
      (void)))
  (test
   'framework.ss
   pred
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "framework.ss" "framework")
      (global-defined-value 'test:run-one)
      (global-defined-value 'test:button-push)
      (global-defined-value 'frame:basic-mixin)
      (global-defined-value 'editor:basic-mixin)
      (global-defined-value 'exit:exit)
      (void)))
  (test
   'framework.ss/gen
   (lambda (x) x)
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (require-library "pretty.ss")
      (let* ([op ((global-defined-value 'pretty-print-print-line))]
	     [np  (lambda x (apply op x))])
	((global-defined-value 'pretty-print-print-line) np)
	(require-library "framework.ss" "framework")
	(eq? np ((global-defined-value 'pretty-print-print-line))))))
  (test
   'framework.ss/test.ss
   (lambda (x) x)
   '(parameterize ([current-namespace (make-namespace 'mred)])
      (let ([orig-button% (global-defined-value 'button%)])
	(require-library "test.ss" "framework")
	(let* ([test-button% (global-defined-value 'button%)])
	  (require-library "framework.ss" "framework")
	  (let* ([fw-button% (global-defined-value 'button%)])
	    (and (eq? fw-button% test-button%)
		 (not (eq? fw-button% orig-button%))))))))

  (load-framework-automatically old-load-framework-automatically?))

