;; should import the flattened test and guiutils stuff and
;; dynamically link to that.

(module framework mzscheme
  (require (lib "unitsig.ss"))

  (require "framework-unit.ss"
	   "sig.ss")

  (provide-signature-elements framework^)

  (define-values/invoke-unit/sig framework^ framework@))
