(unit/sig framework:exit^
  (import mred^
          [preferences : framework:preferences^]
	  [gui-utils : framework:gui-utils^])
  (rename (-exit exit))
  
  (define frame-exiting (make-parameter #f))

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

  (define (can-exit?) (and (user-oks-exit)
			   (andmap (lambda (cb) (cb)) can?-callbacks)))
  (define (on-exit) (for-each (lambda (cb) (cb)) on-callbacks))

  (define (user-oks-exit)
    (if (preferences:get 'framework:verify-exit)
	(let*-values ([(w capw)
		       (if (eq? (system-type) 'windows)
			   (values "exit" "Exit")
			   (values "quit" "Quit"))]
		      [(message)
		       (string-append "Are you sure you want to "
				      w
				      "?")]
		      [(user-says) (gui-utils:get-choice message capw "Cancel" "Warning" #f
							 (frame-exiting))])
	  user-says)
	#t))

  (define -exit
    (opt-lambda ()
      (unless exiting?
	(set! exiting? #t)
	(when (can-exit?)
	  (on-exit)
	  (queue-callback (lambda () (exit))))
	(set! exiting? #f)))))