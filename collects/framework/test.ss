(require-relative-library "tests.ss")

(invoke-open-unit/sig
 (compound-unit/sig
  (import)
  (link [mred : mred-interfaces^ (mred-interfaces@)]
	[keys : framework:keys^ ((require-relative-library "keys.ss"))]
	[test : framework:test^ ((require-relative-library "testr.ss") mred keys)])
  (export
   (unit test)
   (unit keys))))
