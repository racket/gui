(test
 'testr.ss
 (lambda (x) #f)
 '(parameterize ([current-namespace (make-namespace 'mred)])
    (require-library "tests.ss" "framework")
    (invoke-open-unit/sig
     (compound-unit/sig
       (import)
       (link [mred : mred-interfaces^ (mred-interfaces@)]
	     [keys : framework:keys^ ((require-library "keys.ss" "framework"))]
	     [test : framework:test^ ((require-library "testr.ss" "framework") mred keys)])
       (export (unit test))))
    (global-defined-value 'test:run-one)
    (global-defined-value 'test:button-push)
    (void)))

(test
 'test.ss
 (lambda (x) #f)
 '(parameterize ([current-namespace (make-namespace 'mred)])
    (require-library "test.ss" "framework")
    (global-defined-value 'test:run-one)
    (global-defined-value 'test:button-push)
    (void)))

(test
 'mred-interfaces.ss
 (lambda (x)
   (printf "Called predicate: ~a~n" x)
   #f)
 '(parameterize ([current-namespace (make-namespace 'mred)])
    (require-library "mred-interfaces.ss" "framework")
    (global-defined-value 'mred-interfaces^)
    (global-defined-value 'mred-interfaces@)
    (void)))

(test
 'frameworkr.ss
 (lambda (x) #f)
 '(parameterize ([current-namespace (make-namespace 'mred)])
    (require-library "frameworks.ss" "framework")
    (invoke-open-unit/sig
     (compound-unit/sig
       (import)
       (link [mred : mred-interfaces^ (mred-interfaces@)]
	     [core : mzlib:core^ ((require-library "corer.ss"))]
	     [framework : framework^ ((require-library "frameworkr.ss" "framework") core mred)])
       (export (open framework))))
    (global-defined-value 'test:run-one)
    (global-defined-value 'test:button-push)
    (global-defined-value 'frame:basic-mixin)
    (global-defined-value 'editor:basic-mixin)
    (global-defined-value 'exit:exit)
    (void)))

(test
 'framework.ss
 (lambda (x) #f)
 '(parameterize ([current-namespace (make-namespace 'mred)])
    (require-library "framework.ss" "framework")
    (global-defined-value 'test:run-one)
    (global-defined-value 'test:button-push)
    (global-defined-value 'frame:basic-mixin)
    (global-defined-value 'editor:basic-mixin)
    (global-defined-value 'exit:exit)
    (void)))
