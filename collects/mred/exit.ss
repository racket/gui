
  (unit/sig mred:exit^
    (import [mred:constants : mred:constants^]
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

    (define run-exit-callbacks
      (lambda ()
	(set! exit-callbacks
	      (let loop ([cb-list exit-callbacks])
		(cond
		 [(null? cb-list) ()]
		 [(not ((car cb-list))) cb-list]
		 [else (loop (cdr cb-list))])))))

    (define -exit
      (let ([exiting? #f])
	(lambda ()
	  (unless exiting?
	    (dynamic-wind
	     (lambda () (set! exiting? #t))
	     (lambda ()
	       (let/ec k
		 (when (and (mred:preferences:get-preference 'mred:verify-exit)
			    (not (let ([w (if (eq? wx:platform 'macintosh)
					      "quit"
					      "exit")]
				       [capW (if (eq? wx:platform 'macintosh)
						 "Quit"
						 "Exit")])
				   (mred:gui-utils:get-choice
				    (string-append "Are you sure you want to " w "?")
				    capW "Cancel"))))
		   (k #f))
		 (run-exit-callbacks)
		 (if (null? exit-callbacks)
		     (exit)
		     #f)))
	     (lambda () (set! exiting? #f))))))))
		   

