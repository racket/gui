(unit/sig framework:exit^
  (import [preferences : framework:preferences^]
	  [gui-utils : framework:gui-utils^])
  (rename (-exit exit))
  
  (define callbacks '())
  
  (define insert-callback
    (lambda (cb)
      (set! callbacks (cons cb callbacks))
      (lambda ()
	(set! callbacks
	      (let loop ([cb-list callbacks])
		(cond
		  [(null? cb-list) ()]
		  [(eq? cb (car cb-list)) (cdr cb-list)]
		  [else (cons (car cb-list) (loop (cdr cb-list)))]))))))
  
  (define exiting? #f)

  (define run-callbacks
    (lambda ()
      (let loop ([cb-list callbacks])
	    (cond
	      [(null? cb-list) #t]
	      [(not ((car cb-list))) #f]
	      [else (loop (cdr cb-list))]))))
  
  (define -exit
    (opt-lambda ([just-ran-callbacks? #f])
      (unless exiting?
	(dynamic-wind
	 (lambda () (set! exiting? #t))
	 (lambda ()
	   (if (and (let*-values ([(w capW)
				   (if (eq? (system-type) 'windows)
				       (values "exit" "Exit")
				       (values "quit" "Quit"))]
				  [(message)
				   (string-append "Are you sure you want to "
						  w
						  "?")])
		      (if (preferences:get 'framework:verify-exit)
			  (if (gui-utils:get-choice message capW "Cancel")
			      #t
			      #f)
			  #t))
		    (or just-ran-callbacks?
			(run-callbacks)))
	       (exit)
	       #f))
	 (lambda () (set! exiting? #f)))))))