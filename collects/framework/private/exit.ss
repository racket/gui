(module exit mzscheme
  (require (lib "unitsig.ss")
           (lib "string-constant.ss" "string-constants")
	   (lib "class.ss")
	   "sig.ss"
	   "../gui-utils.ss"
	   (lib "mred-sig.ss" "mred")
	   (lib "file.ss")
           (lib "etc.ss"))

  (provide exit@)

  (define exit@
    (unit/sig framework:exit^
      (import mred^
	      [preferences : framework:preferences^])
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

      (define can-exit?
        (opt-lambda ([skip-user-query? #f])
          (and (or skip-user-query?
                   (user-oks-exit))
               (andmap (lambda (cb) (cb)) can?-callbacks))))
      (define (on-exit) (for-each (lambda (cb) (cb)) on-callbacks))

      (define (user-oks-exit)
	(if (preferences:get 'framework:verify-exit)
	    (gui-utils:get-choice
	     (if (eq? (system-type) 'windows)
		 (string-constant are-you-sure-exit)
		 (string-constant are-you-sure-quit))
	     (if (eq? (system-type) 'windows)
		 (string-constant exit)
		 (string-constant quit))
	     (string-constant cancel)
	     (string-constant warning)
	     #f
	     (frame-exiting))
	    #t))

      (define -exit
        (opt-lambda ([skip-user-query? #f])
          (unless exiting?
            (set! exiting? #t)
            (when (can-exit? skip-user-query?)
              (on-exit)
              (queue-callback (lambda () (exit))))
            (set! exiting? #f)))))))
