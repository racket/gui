(compound-unit/sig
 (import)
 (link
  [referf 
   : (require-unit reference-file)
   ((unit->unit/sig (require-relative-library "referf.ss") 
		    () 
		    (require-unit reference-file)))]
  [compile 
   : mzlib:compile^
   ((unit/sig mzlib:compile^
    (import (r : (require-unit reference-file)))

   (define identity (lambda (x n) x))

   ; top-level begin-elaboration-time => begin-expansion-time
   ; nested begin-elaboration-time => begin
   ; require-XXX => usual expansion w/o string check
   (define -require-library-unit/sig (r:require-unit #f #t #f #t 'require-library-unit/sig))
   (define -require-library-unit (r:require-unit #f #t #f #f 'require-library-unit))
   (define -require-relative-library-unit/sig (r:require-unit #f #t #t #t 'require-relative-library-unit/sig))
   (define -require-relative-library-unit (r:require-unit #f #t #t #f 'require-relative-library-unit))
   (define -require-unit/sig (r:require-unit #f #f #f #t 'require-unit/sig))
   (define -require-unit (r:require-unit #f #f #f #f 'require-unit))
   (define -reference-file (r:reference-file #f #f #f))
   (define -require-library (r:reference-file #f #t #f))
   (define -require-relative-library (r:reference-file #f #t #t))
   (define make--begin-elaboration-time
     (lambda (do?)
       (lambda body
	 (when do?
	       (eval `(begin ,@body)))
	 `(#%eval (#%eval (#%quote (begin ,@body)))))))

   (define make-compile-namespace
     (lambda (flags preserve-elab? do-elab? preserve-constr? do-constr?)
       (let ([n (apply make-namespace flags)]
	     [gvs (make-global-value-list)])
	 (parameterize ([current-namespace n])
	   (for-each
	    (lambda (gvp)
	      (unless (defined? (car gvp))
		      (eval `(define ,(car gvp) (quote ,(cdr gvp))))))
	    gvs)
	   (setup-preserving-compile-namespace preserve-elab? do-elab? preserve-constr? do-constr?))
	  n)))

   (define (setup-preserving-compile-namespace preserve-elab? do-elab? preserve-constr? do-constr?)
     (when (or preserve-elab? do-elab? preserve-constr? do-constr?)
       (eval `(begin
		(require-library "refer.ss")
		(define-macro reference-file ,-reference-file)
		(define-macro require-unit/sig ,-require-unit/sig)
		(define-macro require-unit ,-require-unit)
		(define-macro require-library-unit/sig ,-require-library-unit/sig)
		(define-macro require-library-unit ,-require-library-unit)
		(define-macro require-library ,-require-library)
		(define-macro require-relative-library-unit/sig ,-require-relative-library-unit/sig)
		(define-macro require-relative-library-unit ,-require-relative-library-unit)
		(define-macro require-relative-library ,-require-relative-library)
		,@(let ([e (if preserve-elab?
			       `((define-macro begin-elaboration-time ,(make--begin-elaboration-time do-elab?)))
			       null)]
			[c (if preserve-constr?
			       `((define-macro begin-construction-time ,(make--begin-elaboration-time do-constr?)))
			       null)])
		    (append e c))))))
   
   (define compile-file
     (case-lambda 
      [(srcs dest) (compile-file srcs dest null identity)]
      [(srcs dest flags) (compile-file srcs dest flags identity)]
      [(srcs dest flags preprocessor)
       (unless (or (string? srcs) 
		   (input-port? srcs)
		   (and (list? srcs) (andmap (lambda (x) (or (string? x)
							     (input-port? x)))
					     srcs)))
	       (raise-type-error 'compile-file 
				 "string, input-port, or list of strings or input-ports" 
				 srcs))
       (unless (or (string? dest) (output-port? dest))
	       (raise-type-error 'compile-file "string or output-port" dest))
       (unless (and (list flags) 
		    (andmap (lambda (s) 
			      (member s '(ignore-macro-definitions
					  strip-macro-definitions
					  expand-load 
					  use-current-namespace
					  ignore-require-library
					  expand-require-library
					  no-warnings
					  only-expand
					  preserve-elaborations
					  also-preserve-elaborations
					  preserve-constructions
					  also-preserve-constructions)))
			    flags))
	       (raise-type-error 'compile-file "list of flag symbols" flags))
       (unless (and (procedure? preprocessor)
		    (procedure-arity-includes? preprocessor 2))
	       (raise-type-error 'compile-file "procedure (arity 2)" preprocessor))
       (let* ([do-macros? (not (member 'ignore-macro-definitions flags))]
	      [keep-macros? (not (member 'strip-macro-definitions flags))]
	      [expand-load? (member 'expand-load flags)]
	      [expand-rl? (member 'expand-require-library flags)]
	      [ignore-rl? (member 'ignore-require-library flags)]
	      [expand-only? (member 'only-expand flags)]
	      [also-keep-elab? (member 'also-preserve-elaborations flags)]
	      [keep-elab? (or also-keep-elab? (member 'preserve-elaborations flags))]
	      [also-keep-constr? (member 'also-preserve-constructions flags)]
	      [keep-constr? (or also-keep-elab? (member 'preserve-constructions flags))]
	      [namespace (if (member 'use-current-namespace flags)
			     (begin
			       (setup-preserving-compile-namespace
				keep-elab?
				also-keep-elab?
				keep-constr?
				also-keep-constr?)
			       (current-namespace))
			     (make-compile-namespace
			      (if (built-in-name 'wx:frame%) ; HACK!!!
				  '(wx)
				  null)
			      keep-elab?
			      also-keep-elab?
			      keep-constr?
			      also-keep-constr?))]
	      [required (make-hash-table)]
	      [warning
	       (lambda (s)
		 (unless (member 'no-warnings flags)
			 (fprintf (current-error-port)
				  "compile-file warning: ~a~n"
				  s)))])
	 (let ([out (if (output-port? dest)
			dest
			(open-output-file dest 'truncate/replace))])
	   (dynamic-wind
	    void
	    (lambda ()
	      (when keep-elab?
		(display "'e " out)) ; mark of an elaboration-time file
	      (write
	       `(#%if (#%not (#%string=? (#%version) ,(version)))
		    (#%error (#%quote ,(if (string? srcs)
				       (string->symbol srcs)
				       'compiled-file))
			   ,(string-append
			     "compiled for MzScheme version "
			     (version)
			     ", not ~a")
			   (#%version)))
	       out)
	      (newline out)
	      (let src-loop ([srcs srcs])
		(unless (null? srcs)
		  (let*-values ([(src next-srcs) 
				 (if (list? srcs)
				     (values (car srcs) (cdr srcs))
				     (values srcs null))]
				[(in) (if (input-port? src)
					  src
					  (open-input-file src))])
		   (parameterize ([current-load-relative-directory
				   (let ([clrp (current-load-relative-directory)])
				     (if (string? src)
					 (let-values ([(base name dir?)
						       (split-path (path->complete-path 
								    src
								    (or clrp
									(current-directory))))])
					   base)
					 clrp))])
		    (dynamic-wind
		     void
		     (lambda ()
		       (let loop ([in in])
			 (let ([s (read in)])
			   (if (not (eof-object? s))
			       (let* ([do-defmacro
				       (lambda (s)
					 (let ([m (if (pair? (cdr s))
						      (cadr s)
						      #f)])
					   (if (symbol? m)
					       (parameterize ([current-namespace namespace])
							     (eval s)
							     #f)
					       (begin
					     (warning 
					      (format
					       "define-macro expression is ill-formed: ~s"
					       s))
					     #f))))]
				      [do-load
				       (lambda (s cd? rel?)
					 (let ([name (if (pair? (cdr s))
							 (cadr s)
							 #f)])
					   (if (and (string? name)
						    (null? (cddr s)))
					       (let*-values ([(name) (if (and rel?
									    (relative-path? name)
									    (current-load-relative-directory))
								       (build-path (current-load-relative-directory) name)
								       name)]
							     [(base nameonly dir?) (split-path name)]
							     [(cd?) (and cd? 
									 (string? base))]
							     [(orig-dir) (and cd? 
									      (current-directory))])
							    (if cd? 
								(current-directory 
								 base))
							    (let ([in (open-input-file 
								       (if cd? 
									   nameonly 
									   name))])
							      (dynamic-wind
							       void
							       (lambda () 
								 (parameterize ([current-load-relative-directory 
										 (if (string? base)
										     base
										     (current-load-relative-directory))])
								    (loop in))
								 #t)
							       (lambda ()
								 (close-input-port in)
								 (if cd? 
								     (current-directory 
								      orig-dir)))))
							    #t)
					       (begin
						 (warning 
						  (format
						   "load expression is ill-formed or ~a: ~s"
						   "contains an expression for file name"
						   s))
						 #f))))]
				      [find-library
				       (lambda (collection name)
					 (let ([all-paths (current-library-collection-paths)])
					   (let loop ([paths all-paths])
					     (if (null? paths)
						 (error 'compile-file "require-library: collection not found: ~s (in any of ~s)" 
							collection all-paths)
						 (let ([dir (build-path (car paths) collection)])
						   (if (directory-exists? dir)
						       (build-path dir name)
						       (loop (cdr paths))))))))]
				      [do-rl
				       (lambda (s)
					 (if (and (pair? (cdr s))
						  (string? (cadr s))
						  (or (null? (cddr s))
						      (and (pair? (cddr s))
							   (string? (caddr s))
							   (null? (cdddr s)))))
					     (let ([name (cadr s)]
						   [collection (if (null? (cddr s))
								   "mzlib"
								   (caddr s))])
					       (if expand-rl?
						   (let* ([key (string->symbol (string-append collection (string #\null) name))])
						     (if (hash-table-get required key (lambda () #f))
							 #t
							 (let ([fullname (find-library collection name)])
							   (hash-table-put! required key #t)
							   (do-load s #f #t)
							   #t)))
						   (parameterize ([current-namespace namespace])
								 (eval `(require-library/proc ,name
											      ,collection))
								 #f)))
					     (begin
					       (warning 
						(format
						 "require-library expression is ill-formed or ~a: ~s"
						 "contains an expression for library/collection name"
						 s))
					       #f)))]
				      [s (let ([p (preprocessor s namespace)])
					   (parameterize ([current-namespace namespace])
					      (expand-defmacro p)))]
				      [v-c (if expand-only?
					       s
					       (if (void? s)
						   (compile '(#%void))
						   (parameterize ([current-namespace namespace])
						      (compile s))))]
				      [v (if (pair? s)
					     (let ([t (car s)])
					       (case t
						 [(define-macro 
						    define-id-macro 
						    define-expansion-time
						    #%define-macro
						    #%define-id-macro 
						    #%define-expansion-time)
						  (and do-macros? (or (do-defmacro s) (not keep-macros?)))]
						 [(load #%load)
						  (and expand-load? (do-load s #f #f))]
						 [(load/cd #%load/cd)
						  (and expand-load? (do-load s #t #f))]
						 [(load-relative #%load-relative)
						  (and expand-load? (do-load s #f #t))]
						 [(require-library/proc #%require-library/proc)
						  (and (not ignore-rl?) (do-rl s))]
						 [else #f]))
					     #f)])
				 (unless v
				    (write v-c out))
				 (newline out)
				 (loop in))))))
		     (lambda ()
		       (if (input-port? src)
			   (void)
			   (close-input-port in)))))
		    (src-loop next-srcs)))))
	    (lambda ()
	      (if (output-port? dest)
		  (void)
		  (close-output-port out))))))])))
    referf)])
 (export (open compile)))
