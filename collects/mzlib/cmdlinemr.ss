
(unit
  (import)
  (export command-line)

  (define command-line
    (lambda args
      (let* ([serror
	      (lambda (msg . detail)
		(apply
		 raise-syntax-error
		 'command-line
		 msg
		 (cons 'command-line args)
		 detail))]
	     [missing
	      (lambda (what)
		(serror (format "missing ~a" what)))]
	     [extract (lambda (what args . detail)
			(if (null? args)
			    (apply serror (format "missing ~a" what) detail)
			    (values (car args) (cdr args))))]
	     [listify (lambda (l)
			(let loop ([l l])
			  (cond
			   [(null? l) null]
			   [(pair? l) (cons (car l) (loop(cdr l)))]
			   [else (list l)])))]
	     [check-formals
	      (lambda (l)
		(unless (andmap symbol? (listify l))
		  (serror "bad argument specification" l)))]
	     [formal-names
	      (lambda (l)
		(let loop ([l l])
		  (cond
		   [(null? l) null]
		   [(pair? l) (cons (symbol->string (car l)) (loop(cdr l)))]
		   [else (list (let ([s (symbol->string l)])
				 (if (char=? #\* (string-ref s (sub1 (string-length s))))
				     (substring s 0 (sub1 (string-length s)))
				     s)))])))])
	(let*-values ([(name args) (extract "name string" args)]
		      [(_) (unless (string? name)
			     (serror "program name is not a string" name))]
		      [(argv args) (extract "argv vector expression" args)])
	  `(parse-command-line 
	    ,name ,argv
	    ,@(let loop ([args args][clauses null])
		(if (null? args)
		    `((list ,@clauses)
		      (lambda (accum) (void))
		      null)
		    (let ([line (car args)])
		      (if (pair? line)
			  (case (car line)
			    [(once-each once-any multi)
			     (loop (cdr args)
				   (append
				    clauses
				    (list
				     (list* 'list
					    (list 'quote (car line))
					    (let loop ([sublines (cdr line)])
					      (if (null? sublines)
						  null
						  (cons
						   (let ([line (car sublines)])
						     (let-values ([(flags rest) (extract "flag string(s)" line line)])
						       (unless (or (string? flags)
								   (and (list? flags)
									(andmap string? flags)))
							 (serror "flag specification is not a string or sequence of strings" flags))
						       (if (and (pair? rest) (eq? (car rest) '=>))
							   (let ([rest (cdr rest)])
							     (unless (and (list? rest) (= 2 (length rest)))
							       (serror "two expressions must follow a => line" line))
							     `(list ',(listify flags) ,@rest))
							   (let*-values ([(formals rest) (let loop ([a null][rest rest])
											   (cond
											    [(null? rest) (values a null)]
											    [(symbol? (car rest)) (values (append a (list (car rest))) (cdr rest))]
											    [else (values a rest)]))]
									 [(_) (check-formals formals)]
									 [(help rest) (extract "help string" rest line)]
									 [(_) (unless (string? help)
										(serror "help info is not a string" help))]
									 [(expr1 rest) (extract "handler body expressions" rest line)])
							     `(list ',(listify flags)
								    (lambda ,(cons (gensym 'flag) formals)
								      ,expr1 ,@rest)
								    '(,help ,@(formal-names formals)))))))
						   (loop (cdr sublines)))))))))]
			    [(=>)
			     (when (pair? (cdr args)) (serror "=> must be the last clause"))
			     `((list ,@clauses)
			       ,@(cdr line))]
			    [(args)
			     (when (pair? (cdr args)) (serror "args must be the last clause"))
			     (let*-values ([(formals rest) (extract "arg-handler formals" (cdr line) line)]
					   [(_) (check-formals formals)]
					   [(expr1 rest) (extract "arg-handler body expressions" rest line)])
			       `((list ,@clauses)
				 (lambda ,(cons (gensym 'accum) formals) ,expr1 ,@rest)
				 ,(cons 'list (formal-names formals))))]
			    [else (serror "not a once-each, once-any, multi, args, or => line" line)])
			  (serror "not a once-each, once-any, multi, or args line" line)))))))))))