
(unless (with-handlers ([not-break-exn? (lambda (x) #f)])
	  (namespace-variable-binding 'SECTION)
	  #t)
  (load-relative "testing.ss"))
