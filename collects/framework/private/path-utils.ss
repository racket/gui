(module path-utils mzscheme
  (require (lib "unitsig.ss")
	   "sig"
	   (lib "mred-sig.ss" "mred"))

  (provide path-utils@)

  (define path-utils@
    (unit/sig framework:path-utils^
      (import)
      
      (define generate-autosave-name 
	(lambda (name)
	  (let-values ([(base name dir?)
			(if name
			    (split-path name)
			    (values (current-directory) "mredauto" #f))])
	    (let* ([base (if (string? base) 
			     base
			     (current-directory))]
		   [path (if (relative-path? base)
			     (build-path (current-directory) base)
			     base)]
		   [without-ext
		    (if (eq? (system-type) 'windows)
			(list->string
			 (let loop ([list (string->list name)])
			   (if (or (null? list)
				   (char=? (car list) #\.))
			       ()
			       (cons (car list)
				     (loop (cdr list))))))
			name)])
	      (let loop ([n 1])
		(let ([new-name
		       (build-path path
				   (if (eq? (system-type) 'windows)
				       (string-append without-ext
						      "."
						      (number->string n))
				       (string-append "#"
						      name
						      "#"
						      (number->string n)
						      "#")))])
		  (if (file-exists? new-name)
		      (loop (add1 n))
		      new-name)))))))
      (define generate-backup-name
	(lambda (name)
	  (if (eq? (system-type) 'windows)
	      (list->string
	       (let loop ([list (string->list name)])
		 (if (or (null? list)
			 (char=? (car list) #\.))
		     '(#\. #\b #\a #\k)
		     (cons (car list)
			   (loop (cdr list))))))
	      (string-append name "~")))))))

