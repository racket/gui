(module bitmap-constant mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "file.ss"))
  (require-for-syntax (lib "stx.ss" "syntax"))

  (provide bitmap-constant)

  (define-syntax (bitmap-constant stx)
    (syntax-case stx ()
      [(_ floc)
       (let* ([loc #'floc]
	      [file 
	       (syntax-case* loc (lib build-path) module-or-top-identifier=?
		 [_
		  (and (string? (syntax-e loc))
		       (or (relative-path? (syntax-e loc))
			   (absolute-path? (syntax-e loc))))
		  (syntax-e loc)]
		 [(build-path elem1 elem ...)
		  (apply build-path (syntax-object->datum (syntax (elem1 elem ...))))]
		 [(lib filename coll ...)
		  (let ([l (syntax-object->datum (syntax (filename coll ...)))])
		    (unless (andmap string? l)
		      (raise-syntax-error
		       #f
		       "`lib' keyword is not followed by a sequence of string datums"
		       stx
		       loc))
		    (build-path (if (null? (cdr l))
				    (collection-path "mzlib")
				    (apply collection-path (cdr l)))
				(car l)))]
		 [else
		  (raise-syntax-error
		   #f
		   "not a pathname string, `build-path' form, or `lib' form for file"
		   stx
		   loc)])]
	      [c-file
	       (if (complete-path? file)
		   file
		   (path->complete-path
		    file
		    (cond
		     ;; Src of include expression is a path?
		     [(and (string? (syntax-source loc))
			   (complete-path? (syntax-source loc)))
		      (let-values ([(base name dir?) 
				    (split-path (syntax-source loc))])
			(if dir?
			    (syntax-source loc)
			    base))]
		     ;; Load relative?
		     [(current-load-relative-directory)]
		     ;; Current directory
		     [(current-directory)]
		     [else (raise-syntax-error
			    #f
			    "can't determine a base path"
			    stx)])))])
	 (let ([content
		(with-handlers ([not-break-exn?
				 (lambda (exn)
				   (error 'bitmap-constant
					  "could not load ~e: ~a"
					  c-file
					  (if (exn? exn)
					      (exn-message exn)
					      (format "~e" exn))))])
		  (with-input-from-file c-file
		    (lambda ()
		      (read-string (file-size c-file)))))])
	   (with-syntax ([content content]
			 [c-file c-file])
	     (syntax/loc stx
	       (get-or-load-bitmap-constant content c-file)))))]))

  (define cached (make-hash-table 'equal))

  (define (get-or-load-bitmap-constant content orig)
    (hash-table-get cached content
		    (lambda ()
		      (let ([bm (let ([fn (make-temporary-file)])
				  (dynamic-wind
				      void
				      (lambda ()
					(with-output-to-file fn
					  (lambda () (display content))
					  'truncate)
					(make-object bitmap% fn 'unknown/mask))
				      (lambda ()
					(delete-file fn))))])
			(unless (send bm ok?)
			  (error 'bitmap-constant
				 "unable to parse image, originated from: ~a"
				 orig))
			(hash-table-put! cached content bm)
			bm)))))
