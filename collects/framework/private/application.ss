(unit/sig framework:application^
  (import)

  (define current-app-name (make-parameter
			    "MrEd"
			    (lambda (x)
			      (unless (string? x)
				(error 'current-app-name
				       "the app name must be a string"))
			      x))))