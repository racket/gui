
(define make*
  (let ([form-error
	 (lambda (s p) (error 'make* "~a: ~s" s p))]
	[line-error
	 (lambda (s p n) (error 'make* "~a: ~s for line: ~a" s p n))])
    (let ([make* (lambda (spec argv)
		   ; Check the form of spec:
		   (and (or (list? spec) (form-error "specification is not a list" spec))
			(or (pair? spec) (form-error "specification is an empty list" spec))
			(andmap
			 (lambda (line)
			   (and (or (and (list? line) (= (length line) 3))
				    (form-error "list is not a list with 3 parts" line))
				(or (string? (car line))
				    (form-error "line does not start with a string" line))
				(let ([name (car line)])
				  (or (list? (cadr line))
				      (line-error "second part of line is not a list" (cadr line) name)
				      (andmap (lambda (dep)
						(or (string? dep)
						    (form-error "dependency item is not a string" dep name)))
					      (cadr line)))
				  (or (and (procedure? (caddr line))
					   (procedure-arity-includes? (caddr line) 0))
				      (line-error "command part of line is not a thunk" (caddr line) name)))))
			 spec))
		   ; Check argv
		   (or (string? argv)
		       (and (vector? argv)
			    (andmap string? (vector->list argv)))
		       (raise-type-error 'make* "string or string vector" argv))
		   (letrec ([make-file
			     (lambda (s indent)
			       (printf "~amaking ~a~n" indent s)
			       (let ([line (assoc s spec)]
				     [date (if (directory-exists? s)
					       +inf.0
					       (file-modify-seconds s))])
				 (if line
				     (let ([deps (cadr line)])
				       (for-each (lambda (d) (make-file d (string-append " " indent))) deps)
				       (when (or (not date)
						 (ormap (lambda (dep) (and (file-exists? dep)
									   (> (file-modify-seconds dep) date))) 
							deps))
					     ((caddr line))))
				     (unless date
					     (error 'make "don't know how to make ~a" s)))))])
		     (cond
		      [(string? argv) (make-file argv "")]
		      [(equal? argv #()) (make-file (caar spec) "")]
		      [else (for-each (lambda (f) (make-file f "")) (vector->list argv))])))])
      (case-lambda
       [(spec) (make* spec #())]
       [(spec argv) (make* spec argv)]))))

(define-macro make
  (let ([make (lambda (spec argv)
		(let ([form-error (lambda (s . p) (apply syntax-error 'make s spec p))])
		  (and (or (list? spec) (form-error "illegal specification (not a sequence)"))
		       (or (pair? spec) (form-error "empty specification"))
		       (andmap
			(lambda (line)
			  (and (or (and (list? line) (>= (length line) 3))
				   (form-error "clause does not have at least 3 parts" line))
			       (let ([name (car line)])
				 (or (list? (cadr line))
				     (line-error "second part of clause is not a sequence" (cadr line))))))
			spec))
		  `(make* (list ,@(map (lambda (line)
					 `(list ,(car line)
						(list ,@(cadr line))
						(lambda ()
						  ,@(cddr line))))
				       spec))
			  ,argv)))])
    (case-lambda
     [(spec) (make spec #())]
     [(spec argv) (make spec argv)])))
