(module load mzscheme
  (require "test-suite-utils.ss")

  (define old-load-framework-automatically? (load-framework-automatically))

  (define (test/load file exp)
    (test
     (string->symbol file)
     void?
     `(let ([mred-name 
             ((current-module-name-resolver) '(lib "mred.ss" "mred") #f #f)]
            [orig-namespace (current-namespace)])
        (parameterize ([current-namespace (make-namespace)])
          (namespace-attach-module
           orig-namespace
           mred-name)
          (eval '(require (lib ,file "framework")))
          (with-handlers ([(lambda (x) #t)
                           (lambda (x)
                             (if (exn? x)
                                 (exn-message x)
                                 (format "~s" x)))])
            (eval ',exp)
            (void))))))

  (load-framework-automatically #f)

  (test/load "prefs-file-unit.ss" 'framework:prefs-file@)
  (test/load "prefs-file.ss" 'prefs-file:get-preferences-filename)

  (test/load "gui-utils-unit.ss" 'framework:gui-utils@)
  (test/load "gui-utils.ss" 'gui-utils:next-untitled-name)

  (test/load "test-unit.ss" 'framework:test@)
  (test/load "test.ss" 'test:run-interval)

  (test/load "macro.ss" '(mixin () () ()))

  (test/load "framework-unit.ss" '(list framework@ framework-no-prefs@ framework-small-part@))
  (test/load "framework.ss" '(list prefs-file:get-preferences-filename
				   test:button-push
				   gui-utils:next-untitled-name
				   frame:basic-mixin))

  (load-framework-automatically old-load-framework-automatically?))
