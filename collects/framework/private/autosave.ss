(module autosave mzscheme
  (require (lib "unitsig.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide autosave@)

  (define autosave@
    (unit/sig framework:autosave^
      (import [mred : mred^]
	      [exit : framework:exit^]
	      [preferences : framework:preferences^])
      
      (define objects null)

      (define autosave-timer%
	(class timer% ()
	  (inherit start)
	  (override
	   [notify
	    (lambda ()
	      (when (preferences:get 'framework:autosaving-on?)
		(set! objects
		      (let loop ([list objects])
			(if (null? list)
			    null
			    (let ([object (weak-box-value (car list))])
			      (if object
				  (begin
				    (send object do-autosave)
				    (cons (car list) (loop (cdr list))))
				  (loop (cdr list))))))))
	      (let ([seconds (preferences:get 'framework:autosave-delay)])
		(start (* 1000 seconds) #t)))])
	  (sequence
	    (super-init)
	    (let ([seconds (preferences:get 'framework:autosave-delay)])
	      (start (* 1000 seconds) #t)))))

      (define timer #f)

      (define register
	(lambda (b)
	  (unless timer
	    (set! timer (make-object autosave-timer%)))
	  (set! objects
		(let loop ([objects objects])
		  (cond
		   [(null? objects) (list (make-weak-box b))]
		   [else (let ([weak-box (car objects)])
			   (if (weak-box-value weak-box)
			       (cons weak-box (loop (cdr objects)))
			       (loop (cdr objects))))]))))))))


