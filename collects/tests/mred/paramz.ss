
(load-relative "loadtest.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Yield Tests                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s (make-semaphore))
(define v 4)
(queue-callback (lambda () (set! v 5)))
(yield)
(test v 'yield-run 5)
(queue-callback (lambda () (set! v 6)))

(semaphore-post s)
(yield s)
(test v 'yield-wait 5)
(yield)
(test v 'yield-run 6)

(queue-callback (lambda () (set! v 7) (semaphore-post s)))
(yield s)
(test v 'yield-run-post 7)

(queue-callback (lambda () 
		  (set! v 8)
		  (semaphore-post s)
		  (queue-callback
		   (lambda () (set! v 9)))))
(yield s)
(test v 'yield-wait-post 8)
(yield)
(test v 'yield-run 9)

(define d (make-object dialog% "hello"))
(thread (lambda () 
	  (sleep 1)
	  (queue-callback (lambda () (set! v 11)))
	  (send d show #f)))
(queue-callback (lambda () (set! v 10)))
(send d show #t)
(test v 'dialog-wait 10)
(yield)
(test v 'dialog-run 11)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Parameterization Tests                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Killing an eventspace
(define c (make-custodian))
(define e (parameterize ([current-custodian c]) (make-eventspace)))
(parameterize ([current-eventspace e]) (send (make-object frame% "x" #f 50 50) show #t))
(test #f 'shutdown? (eventspace-shutdown? e))
(custodian-shutdown-all c)
(test #t 'shutdown? (eventspace-shutdown? e))
(define (try-use-es t)
  (test
   'error
   'shutdown-eventspace
   (with-handlers ([(lambda (x)
		      (and (exn:misc? x)
			   (regexp-match "shutdown" (exn-message x))))
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
