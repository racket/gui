
 (unit 
  (import) 
  (export awk match:start match:end match:substring regexp-exec)

  (define awk
    (lambda (get-next-record . rest)
      (let*-values ([(user-fields rest) (values (car rest) (cdr rest))]
		    [(counter rest) (if (and (pair? rest) (symbol? (car rest)))
					(values (car rest) (cdr rest))
					(values (gensym) rest))]
		    [(user-state-var-decls rest) (values (car rest) (cdr rest))]
		    [(continue rest) (if (and (pair? rest) (symbol? (car rest)))
					 (values (car rest) (cdr rest))
					 (values (gensym) rest))]
		    [(user-state-vars) (map car user-state-var-decls)]
		    [(local-user-state-vars) (map gensym user-state-vars)]
		    [(first) (car user-fields)]
		    [(clauses) rest]
		    [(loop) (gensym)]
		    [(remainder) (gensym)]
		    [(extras) (gensym)]
		    [(arg) (gensym)]
		    [(else-ready?) (gensym)]
		    [(orig-on?) (gensym)]
		    [(post-on-on?) (gensym)]
		    [(escape) (gensym)]
		    [(initvars) null])
         (letrec ([get-after-clauses
		   (lambda ()
		     (let loop ([l clauses][afters null])
		       (cond
			[(null? l) (if (null? afters)
				       `((values ,@user-state-vars))
				       afters)]
			[(eq? (caar l) 'after)
			 (loop (cdr l) (append afters (cdar l)))]
			[else
			 (loop (cdr l) afters)])))]
		  [wrap-state
		   (lambda (e)
		     (if (eq? (car e) '=>)
			 `(=>
			   (lambda (,arg)
			     ,@(wrap-state `((,(cadr e) ,arg)))))
			 `((call-with-values
			    (lambda () ,@e)
			    (lambda ,(append local-user-state-vars extras)
			      (set! ,else-ready? #f)
			      (set!-values ,user-state-vars 
					   (values ,@local-user-state-vars)))))))]
		  [make-range
		   (lambda (include-on? include-off? body rest)
		     (let* ([on? (gensym)])
		       (set! initvars (cons `(,on? #f) initvars))
		       (cons
			`(let ([,orig-on? ,on?])
			   (unless ,on? (set! ,on? ,(make-test (car body))))
			   (let ([,post-on-on? ,on?])
			     (when ,on? (set! ,on? (not ,(make-test (cadr body)))))
			     (when ,(if include-on?
					(if include-off?
					    post-on-on?
					    on?)
					(if include-off?
					    orig-on?
					    `(and ,orig-on? ,on?)))
				   ,@(wrap-state (cddr body)))))
			rest)))]
		  [make-test
		   (lambda (test)
		     (cond
		      [(string? test)
		       (let ([g (gensym)])
			 (set! initvars (cons `(,g (regexp ,test)) initvars))
			 `(regexp-exec ,g ,first))]
		      [(number? test)
		       `(= ,test ,counter)]
		      [else test]))]
		  [get-testing-clauses
		   (lambda ()
		     (let loop ([l clauses])
		       (if (null? l)
			   null
			   (let* ([clause (car l)]
				  [test (car clause)]
				  [body (cdr clause)]
				  [rest (loop (cdr l))])
			     (cond
			      [(or (string? test) (number? test))
			       (cons
				`(cond [,(make-test test)
					,@(wrap-state body)]
				       [else (void)])
				rest)]
			      [(eq? test 'else)
			       (cons 
				`(when ,else-ready?
				       ,@(wrap-state body))
				(cons 
				 `(set! ,else-ready? #t)
				 rest))]
			      [(eq? test 'range)
			       (make-range #f #f body rest)]
			      [(eq? test ':range)
			       (make-range #t #f body rest)]
			      [(eq? test 'range:)
			       (make-range #f #t body rest)]
			      [(eq? test ':range:)
			       (make-range #t #t body rest)]
			      [(eq? test 'after)
			       rest]
			      [(eq? test '/)
			       (let ([g (gensym)]
				     [re (car body)]
				     [vars (append (map (lambda (s)
							  (or s (gensym)))
							(caddr body))
						   (gensym))]
				     [body (cdddr body)])
				 (set! initvars (cons `(,g (regexp ,re)) initvars))
				 (cons
				  `(cond
				    [(regexp-match ,re ,first)
				     => (lambda (,arg)
					  (apply
					   (lambda ,vars ,@(wrap-state body))
					   ,arg))]
				    [else (void)])
				  rest))]
			      [else
			       (cons
				`(cond (,test ,@(wrap-state body)) (else (void)))
				rest)])))))])
	   (let ([testing-clauses (get-testing-clauses)])
	     `(let (,@user-state-var-decls ,@initvars)
		(let ,loop ([,counter 1]) 
		     (call-with-values
		      (lambda () ,get-next-record)
		      (lambda ,user-fields
			(if (eof-object? ,first)
			    (begin
			      ,@(get-after-clauses))
			    (let ([,else-ready? #t])
			      (let/ec ,escape
				(let ([,continue
				       (lambda ,(append local-user-state-vars extras)
					 (set!-values ,user-state-vars 
						      (values ,@local-user-state-vars))
					 (,escape))])
				  ,@testing-clauses))
			      (,loop (add1 ,counter)))))))))))))

  (define-struct match (s a))

  (define match:start
    (case-lambda
     [(rec) (match:start rec 0)]
     [(rec which) (car (list-ref (match-a rec) which))]))

  (define match:end
    (case-lambda
     [(rec) (match:end rec 0)]
     [(rec which) (cdr (list-ref (match-a rec) which))]))

  (define match:substring
    (case-lambda
     [(rec) (match:substring rec 0)]
     [(rec which) (let ([p (list-ref (match-a rec) which)])
		    (substring (match-s rec) (car p) (cdr p)))]))

  (define regexp-exec
    (lambda (re s)
      (let ([r (regexp-match-positions re s)])
	(if r
	    (make-match s r)
	    #f)))))