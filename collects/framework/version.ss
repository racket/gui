  (unit/sig framework:version^
    (import [mzlib:string : mzlib:string^]
	    [mzlib:function : mzlib:function^])

    (rename [-version version])

    (define specs null)

    (define -version
      (lambda ()
	(mzlib:function:foldr
	 (lambda (entry sofar)
	   (match entry
	     [(sep num) (string-append sofar sep num)]))
	 (version)
	 specs)))

    (define add-spec
      (lambda (sep num)
	(set! specs (cons (list (mzlib:string:expr->string sep)
				(mzlib:string:expr->string num))
			  specs))))
    
    '(add-version-spec ': 5))
