(module test mzscheme
  (require (lib "unitsig.ss")
	   "test-sig.ss"
	   "test-unit.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred"))

  (provide-signature-elements ((unit test : framework:test^)))

  (define-values/invoke-unit/sig ((unit test : framework:test^))
    (compound-unit/sig
      (import [m : mred^])
      (link [test : framework:test^ (framework:test@ m)])
      (export (unit test)))
    #f
    mred^))
