(module test mzscheme
  (require (lib "unitsig.ss")
	   "test-sig.ss"
	   "test-unit.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred"))

  (provide-signature-elements framework:test^)

  (define-values/invoke-unit/sig framework:test^
    test@
    #f
    mred^))