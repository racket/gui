(require-library "mred-interfaces.ss" "framework")
(require-library "frameworks.ss" "framework")

(require-library "string.ss")
(require-library "function.ss")
(require-library "pretty.ss")
(require-library "file.ss")
(require-library "thread.ss")

(invoke-open-unit/sig
 (compound-unit/sig
   (import [core:string : mzlib:string^]
	   [core:function : mzlib:function^]
	   [core:pretty-print : mzlib:pretty-print^]
	   [core:file : mzlib:file^]
	   [core:thread : mzlib:thread^])
   (link [M : mred-interfaces^ (mred-interfaces@)]
	 [F : framework^ ((require-library "frameworkc.ss" "framework")
			  core:string
			  core:function
			  core:pretty-print
			  core:file
			  core:thread
			  C M)])
   (export (open F)))
 #f
 mzlib:string^
 mzlib:function^
 mzlib:pretty-print^
 mzlib:file^
 mzlib:thread^)
