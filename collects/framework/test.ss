(printf "1~n")
(read-case-sensitive #t)
(compile-allow-cond-fallthrough #t)
(compile-allow-set!-undefined #t)
(begin
  (require-library "launcher.ss" "launcher")
  (make-mred-launcher (list "-mvL" "test.ss" "framework")
		      (mred-program-launcher-path "Test Framework")))

(printf "2~n")
(require-library "loader.ss" "system")
(printf "3~n")
;(require-library "gen-mred-interfaces.ss" "framework")
(printf "4~n")
(require-library "mred-interfaces.ss" "framework")
(printf "5~n")
(require-library "sig.ss" "framework")
(printf "6~n")
(invoke-unit/sig
 (compound-unit/sig
   (import)
   (link [M : mred-interfaces^ (mred-interfaces@)]
	 [C : mzlib:core^ ((require-library "corer.ss"))]
	 [F : framework^ ((require-library "frameworkr.ss" "framework") C M)])
   (export)))
(printf "7~n")
