(module framework mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred.ss" "mred")
	   (lib "mred-sig.ss" "mred")
           
	   "test.ss"
	   "test-sig.ss"
           
	   "gui-utils.ss"
	   "gui-utils-sig.ss"
           
           "framework-unit.ss"
	   "framework-sig.ss"
           
           "macro.ss")
  
  (provide-signature-elements framework^)
  (provide (all-from "macro.ss"))
   
  (define-values/invoke-unit/sig 
   frameworkc^ 
   frameworkc@ 
   #f
   mred^
   (test : framework:test^)
   (gui-utils : framework:gui-utils^)))

