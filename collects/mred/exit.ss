(define mred:exit@
  (unit/sig mred:exit^
    (import [mred:debug : mred:debug^])
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

    (define -exit
      (lambda ()
	(set! exit-callbacks
	      (let loop ([cb-list exit-callbacks])
		(cond
		 [(null? cb-list) ()]
		 [(not ((car cb-list))) cb-list]
		 [else (loop (cdr cb-list))])))
	(if (null? exit-callbacks)
	    (begin (when mred:debug:exit?
		     (exit))
		   #t)
	    #f)))))

