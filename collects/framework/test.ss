(require-relative-library "tests.ss")
(require-relative-library "mred-interfaces.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig ((open mred-interfaces^)
				(open framework:keys^)
				(open framework:test^))
  (compound-unit/sig
   (import)
   (link [mred : mred-interfaces^ (mred-interfaces@)]
	 [keys : framework:keys^ ((require-relative-library "keys.ss"))]
	 [test : framework:test^ ((require-relative-library "testr.ss") mred keys)])
   (export
    (unit test)
    (unit keys)
    (open mred))))

