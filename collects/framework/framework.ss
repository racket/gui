(read-case-sensitive #t)
(compile-allow-cond-fallthrough #t)
(compile-allow-set!-undefined #t)
(require-library "mred-interfaces.ss" "framework")
(require-library "framework.ss" "framework")
(invoke-open-unit/sig
 (compound-unit/sig
   (import)
   (link [M : mred-interfaces^ (mred-interfaces@)]
	 [C : mzlib:core^ ((require-library "corer.ss"))]
	 [F : framework^ ((require-library "frameworkr.ss" "framework") C M)])
   (export (open F))))
