
(when (not (defined? 'test))
  (load-relative "testing.ss"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Parameterization Tests                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-hello thunk)
  (parameterize ([debug-info-handler (lambda () 'hello)])
    (with-handlers ([void (lambda (x) (exn-debug-info x))])
      (thunk))))

(test 'hello
      'debug-info-handler
      (get-hello (lambda () (make-object frame% #f))))

(test 'hello
      'debug-info-handler
      (get-hello (lambda () (let ([f (make-object frame% #f)])
			      (send f set-status-text 'bad-val)))))

;; Killing an eventspace
(define c (make-custodian))
(define e (parameterize ([current-custodian c]) (make-eventspace)))
(parameterize ([current-eventspace e]) (send (make-object frame% "x" #f 50 50) show #t))
(custodian-shutdown-all c)
(define (try-use-es t)
  (test
   'error
   'shutdown-eventspace
   (with-handlers ([(lambda (x)
		      (and (exn:misc? x)
			   (regexp-match "shut down" (exn-message x))))
		    (lambda (x)
		      (printf "got expected error: ~a~n" (exn-message x))
		      'error)])
     (parameterize ([current-eventspace e]) 
       (t)))))
(try-use-es (lambda () (make-object frame% "x" #f 50 50)))
(try-use-es (lambda () (make-object dialog% "x" #f 50 50)))
(try-use-es (lambda () (make-object timer%)))
(try-use-es (lambda () (queue-callback void)))

(report-errs)
