;;; mysterxu.ss

(require-library "cores.ss")
(require-library "macro.ss")

(require-library "sigs.ss" "mysterx")

(require-library "xmls.ss" "xml")

(define mysterx@
  (compound-unit/sig
   (import)
   (link [core : mzlib:core^ ((require-library "corer.ss"))]
	 [mxprims : mysterx:prims^ ((require-library "prims.ss" "mysterx"))]
         [xml : xml^ ((require-library "xmlr.ss" "xml") (core function))]
	 [mysterx : mysterx:mysterx^ 
		  ((require-library "mysterxe.ss" "mysterx") 
		   (core function) (core string)
		   mxprims xml)])
   (export
    (open mysterx))))
