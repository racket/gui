(module gui-utils mzscheme
  (require (lib "unitsig.ss")
	   "gui-utils-sig.ss"
	   "gui-utils-unit.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred"))

  (provide-signature-elements framework:gui-utils^)

  (define-values/invoke-unit/sig 
    framework:gui-utils^
    framework:gui-utils@
    #f
    mred^))
