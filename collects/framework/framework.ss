(require-relative-library "frameworks.ss")

(require-library "string.ss")
(require-library "function.ss")
(require-library "pretty.ss")
(require-library "file.ss")
(require-library "thread.ss")

(require-relative-library "test.ss")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig frameworkc^
  (compound-unit/sig
   (import [core:string : mzlib:string^]
	   [core:function : mzlib:function^]
	   [core:pretty-print : mzlib:pretty-print^]
	   [core:file : mzlib:file^]
	   [core:thread : mzlib:thread^]
	   [framework:keys : framework:keys^]
	   [framework:test : framework:test^]
	   [m : mred^])
   (link [f : frameworkc^ ((require-relative-library "frameworkc.ss")
			   core:string
			   core:function
			   core:pretty-print
			   core:file
			   core:thread
			   m
			   framework:keys
			   framework:test)])
   (export (open f)))
  #f
  mzlib:string^
  mzlib:function^
  mzlib:pretty-print^
  mzlib:file^
  mzlib:thread^
  (keys : framework:keys^)
  (test : framework:test^)
  mred^)

