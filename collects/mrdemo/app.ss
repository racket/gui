(compound-unit/sig 
   (import [I : mred:application-imports^])
   (link [mzlib : mzlib:core^ 
            ((reference-library-unit/sig "corer.ss"))]
         [mred : mred^
            ((reference-library-unit/sig "link.ss" "mred") mzlib)]
         [myapp : ()
            ((unit/sig ()
	      (import)
	      (require-library "demo.ss" "mrdemo")))])
   (export (unit mred)))

