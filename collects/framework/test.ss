(printf "1~n")
(read-case-sensitive #t)
(compile-allow-cond-fallthrough #t)
(compile-allow-set!-undefined #t)
(printf "2~n")
(require-library "loader.ss" "system")
(printf "3~n")
;(require-library "gen-mred-interfaces.ss" "framework")
(printf "4~n")
(require-library "mred-interfaces.ss" "framework")
(printf "5~n")
(require-library "sig.ss" "framework")
(printf "6~n")
(define framework@ (require-library "frameworkr.ss" "framework"))
(printf "7~n")
(invoke-unit/sig
 (compound-unit/sig
   (import)
   (link [M : mred-interfaces^ (mred-interfaces@)]
	 [C : mzlib:core^ ((require-library "corer.ss"))]
	 [F : framework^ (framework@ C M)])
   (export)))
(printf "8~n")
