(module framework mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred.ss" "mred")
	   (lib "mred-sig.ss" "mred")

	   ;(prefix test: "test.ss")
	   ;"test-sig.ss"

	   ;(prefix prefs-file: "prefs-file.ss")
	   ;"prefs-file-sig.ss"

	   ;(prefix gui-utils: "gui-utils.ss")
	   ;"gui-utils-sig.ss"

	   "framework-unit.ss"
	   "sig.ss")

  (provide-signature-elements framework^)

  (define-values/invoke-unit/sig framework^ framework@ #f mred^)

  '(define-values/invoke-unit/sig frameworkc^ framework-small-part@ #f
    (test : framework:test^)
    (prefs-file : framework:prefs-file^)
    (gui-utils : framework:gui-utils^)))
