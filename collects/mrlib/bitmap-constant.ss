(module bitmap-constant mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "file.ss"))
  (require-for-syntax (lib "path-spec.ss" "syntax"))

  (provide bitmap-constant
           (rename bitmap-constant include-bitmap)
           
           bitmap-constant/relative-to
	   (rename bitmap-constant/relative-to include-bitmap/relative-to))
  
  (define-syntax (-bitmap-constant stx)
    (syntax-case stx ()
      [(_ orig-stx source path-spec)
       (let* ([c-file (resolve-path-spec #'path-spec #'source #'orig-stx #'build-path)]
	      [content
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
	     (get-or-load-bitmap-constant content c-file))))]))

  (define-syntax (bitmap-constant/relative-to stx)
    (syntax-case stx ()
      [(_ source path-spec) #`(-bitmap-constant #,stx source path-spec)]))

  (define-syntax (bitmap-constant stx)
    (syntax-case stx ()
      [(_ path-spec) #`(-bitmap-constant #,stx #,stx path-spec)]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Run-time support

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
