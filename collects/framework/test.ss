(require-relative-library "tests.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig ((open mred^)
				(unit keys : framework:keys^)
				(unit test : framework:test^))
  (compound-unit/sig
   (import)
   (link [mred : mred^ (mred@)]
	 [keys : framework:keys^ ((require-relative-library "keys.ss"))]
	 [test : framework:test^ ((require-relative-library "testr.ss") mred keys)])
   (export
    (unit test)
    (unit keys)
    (open mred))))

