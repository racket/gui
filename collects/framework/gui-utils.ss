(module gui-utils mzscheme
  (require (lib "unitsig.ss")
	   "gui-utils-sig.ss"
	   "gui-utils-unit.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "mred.ss" "mred"))

  (provide-signature-elements ((unit gui-utils : framework:gui-utils^)))

  (define-values/invoke-unit/sig 
    ((unit gui-utils : framework:gui-utils^))
    (compound-unit/sig
      (import [mred : mred^])
      (link [gui-utils : framework:gui-utils^ (framework:gui-utils@ mred)])
      (export (unit gui-utils)))
    #f
    mred^))
