(module debug mzscheme
  (provide debug-printf debug-when)

  ;; all of the steps in the tcp connection
  (define tcp? #f)

  ;; administrative messages about preferences files and
  ;; command line flags
  (define admin? #f)

  ;; tests that passed and those that failed
  (define schedule? #t)

  ;; of the sexpression transactions between mz and mred
  (define messages? #t)

  (define-syntax debug-printf
    (lambda (stx)
      (syntax-case stx ()
	[(_ flag rest ...)
	 (syntax (debug-when flag (printf rest ...)))])))

    (define-syntax debug-when
      (lambda (stx)
	(syntax-case stx (tcp admin schedule messages)
	  [(_ tcp rest ...)
	   (syntax
	    (when tcp?
	      rest ...))]
	  [(_ admin rest ...)
	   (syntax
	    (when admin?
	      rest ...))]
	  [(_ schedule rest ...)
	   (syntax
	    (when schedule?
	      rest ...))]
	  [(_ messages rest ...)
	   (syntax
	    (when messages?
	      rest ...))]
	  [(_ unk rest ...)
	   (raise-syntax-error 'debug-when "unknown flag" stx (syntax unk))]))))
