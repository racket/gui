(unit/sig framework:exit^
  (import [preferences : framework:preferences^]
	  [gui-utils : framework:gui-utils^])
  (rename (-exit exit))
  
  (define can?-callbacks '())
  (define on-callbacks '())
  
  (define insert-can?-callback
    (lambda (cb)
      (set! can?-callbacks (cons cb can?-callbacks))
      (lambda ()
	(set! can?-callbacks
	      (let loop ([cb-list can?-callbacks])
		(cond
		  [(null? cb-list) ()]
		  [(eq? cb (car cb-list)) (cdr cb-list)]
		  [else (cons (car cb-list) (loop (cdr cb-list)))]))))))

  (define insert-on-callback
    (lambda (cb)
      (set! on-callbacks (cons cb on-callbacks))
      (lambda ()
	(set! on-callbacks
	      (let loop ([cb-list on-callbacks])
		(cond
		 [(null? cb-list) ()]
		 [(eq? cb (car cb-list)) (cdr cb-list)]
		 [else (cons (car cb-list) (loop (cdr cb-list)))]))))))
  
  (define exiting? #f)

  (define (can-exit?) (andmap (lambda (cb) (cb)) can?-callbacks))
  (define (on-exit) (for-each (lambda (cb) (cb)) on-callbacks))

  (define -exit
    (opt-lambda ([just-ran-callbacks? #f])
      (unless exiting?
	(dynamic-wind
	 (lambda () (set! exiting? #t))
	 (lambda ()
	   (if (and (can-exit?)
		    (let*-values ([(w capw)
				   (if (eq? (system-type) 'windows)
				       (values "exit" "Exit")
				       (values "quit" "Quit"))]
				  [(message)
				   (string-append "Are you sure you want to "
						  w
						  "?")])
		      (printf "showing dialog~n")
		      (if (preferences:get 'framework:verify-exit)
			  (if (gui-utils:get-choice message capw "Cancel")
			      #f
			      #t)
			  #t)))
	       (begin
		 (on-exit)
		 (printf "~a~n" '(exit)))
	       #f))
	 (lambda () (set! exiting? #f)))))))