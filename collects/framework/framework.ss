(require-relative-library "frameworks.ss")

(require-library "string.ss")
(require-library "function.ss")
(require-library "pretty.ss")
(require-library "file.ss")
(require-library "thread.ss")

(require-relative-library "test.ss")

(invoke-open-unit/sig
 (compound-unit/sig
   (import [core:string : mzlib:string^]
	   [core:function : mzlib:function^]
	   [core:pretty-print : mzlib:pretty-print^]
	   [core:file : mzlib:file^]
	   [core:thread : mzlib:thread^]
	   [framework:keys : framework:keys^]
	   [framework:test : framework:test^])
   (link [M : mred-interfaces^ (mred-interfaces@)]
	 [F : frameworkc^ ((require-relative-library "frameworkc.ss")
			   core:string
			   core:function
			   core:pretty-print
			   core:file
			   core:thread
			   M
			   framework:keys
			   framework:test)])
   (export (open F)))
 #f
 mzlib:string^
 mzlib:function^
 mzlib:pretty-print^
 mzlib:file^
 mzlib:thread^
 (keys : framework:keys^)
 (test : framework:test^))
