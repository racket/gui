(module framework mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred.ss" "mred")
	   (lib "mred-sig.ss" "mred")
           
	   "test.ss"
	   "test-sig.ss"
           
	   "prefs-file.ss"
	   "prefs-file-sig.ss"
           
	   "gui-utils.ss"
	   "gui-utils-sig.ss"
           
           "framework-unit.ss"
	   "framework-sig.ss"
           
           "macro.ss")
  
  (provide-signature-elements framework^)
  (provide mixin)
   
  (define-values/invoke-unit/sig 
   frameworkc^ 
   frameworkc@ 
   #f
   mred^
   (test : framework:test^)
   (prefs-file : framework:prefs-file^)
   (gui-utils : framework:gui-utils^)))

