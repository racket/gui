(unit/sig mred:exit^
  (import [wx : wx^]
	  [mred:constants : mred:constants^]
	  [mred:preferences : mred:preferences^]
	  [mred:gui-utils : mred:gui-utils^])
  (rename (-exit exit))
  
  (mred:debug:printf 'invoke "mred:exit@")
  
  (define exit-callbacks '())
  
  (define insert-exit-callback
    (lambda (f)
      (set! exit-callbacks (cons f exit-callbacks))
      f))
  
  (define remove-exit-callback
    (lambda (cb)
      (set! exit-callbacks
	    (let loop ([cb-list exit-callbacks])
	      (cond
		[(null? cb-list) ()]
		[(eq? cb (car cb-list)) (cdr cb-list)]
		[else (cons (car cb-list) (loop (cdr cb-list)))])))))
  
  (define exiting? #f)

  (define run-exit-callbacks
    (lambda ()
      (let loop ([cb-list exit-callbacks])
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
				   (if (eq? wx:platform 'windows)
				       (values "exit" "Exit")
				       (values "quit" "Quit"))]
				  [(message)
				   (string-append "Are you sure you want to "
						  w
						  "?")])
		      (if (mred:preferences:get-preference 'mred:verify-exit)
			  (if (mred:gui-utils:get-choice message capW "Cancel")
			      #t
			      #f)
			  #t))
		    (or just-ran-callbacks?
			(run-exit-callbacks)))
	       (exit)
	       #f))
	 (lambda () (set! exiting? #f)))))))