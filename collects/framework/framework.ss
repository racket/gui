(module framework mzscheme
  (require (lib "unitsig.ss"))

  (require "framework-unit.ss"
	   "sig.ss")

  (provide-signature-elements framework^)

  (define-values/invoke-unit/sig framework^ framework@))
