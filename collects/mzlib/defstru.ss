

(define-macro define-structure 
  (lambda rest
    `(define-const-structure ,@rest)))

(define-macro define-const-structure 
 (lambda (name-and-fields . fields-with-defaults-list)
  (let ([fields-with-defaults
	 (if (null? fields-with-defaults-list)
	     fields-with-defaults-list
	     (if (and (pair? fields-with-defaults-list)
		      (null? (cdr fields-with-defaults-list)))
		 (car fields-with-defaults-list)
		 (error 'define-const-structure "bad specification: ~s"
			(cons name-and-fields fields-with-defaults-list))))]
	[strip-id
	 (lambda (id)
	   (cond
	    [(symbol? id) id]
	    [(and (pair? id)
		  (pair? (cdr id))
		  (null? (cddr id))
		  (eq? (car id) '!)
		  (symbol? (cadr id)))
	     (cadr id)]
	    [else
	     (error 'define-const-structure "bad field name: ~a" id)]))]
	[check-id
	 (lambda (x)
	   (if (not (symbol? x))
	       (error 'define-const-structure "bad structure name: ~a" x)
	       x))])
    (if (not (list? name-and-fields))
	(error 'define-const-structure "bad structure form: ~a" name-and-fields))
    (if (not (list? fields-with-defaults))
	(error 'define-const-structure "bad defaults structure form: ~a" 
	       fields-with-defaults))
    (if (null? fields-with-defaults)
	`(define-struct ,(check-id (car name-and-fields))
	   ,(map strip-id (cdr name-and-fields)))
	(let ([gs (gensym)]
	      [maker (string->symbol
		      (string-append "make-" 
				     (symbol->string (check-id (car name-and-fields)))))]
	      [args (map strip-id (cdr name-and-fields))])
	  `(begin
	     (define-struct ,(car name-and-fields)
	       ,(append args
			(map (lambda (x)
			       (if (and (pair? x)
					(pair? (cdr x))
					(null? (cddr x)))
				   (strip-id (car x))
				   (error 'define-const-structure "bad name-value pair: ~s"
					  x)))
			     fields-with-defaults)))
	     (let ([,gs ,maker])
	       (set! ,maker
		     (lambda ,args
		       (,gs ,@args ,@(map cadr fields-with-defaults)))))))))))
