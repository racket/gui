(test 'exit/no-prompt
      (lambda (x)
	(and (eq? x 'passed)
	     (not (mred-running?))))
      (lambda ()
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred '(preferences:set 'framework:verify-exit #f))
	  (send-sexp-to-mred '(exit:exit))
	  'failed)))

(test 'exit/prompt
      (lambda (x) (and (eq? x 'passed)
		       (not (mred-running?))))
      (lambda ()
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred '(begin (preferences:set 'framework:verify-exit #t)
				     (test:run-one (lambda () (exit:exit)))))
	  (wait-for-frame "Warning")
	  (send-sexp-to-mred '(test:button-push "Quit"))
	  'failed)))

(test 'exit/prompt/no-twice
      (lambda (x) (and (eq? x 'passed)
		       (not (mred-running?))))
      (let ([exit/push-button
	     (lambda (button)
	       (send-sexp-to-mred '(begin (preferences:set 'framework:verify-exit #t)
					  (test:run-one (lambda () (exit:exit)))))
	       (wait-for-frame "Warning")
	       (send-sexp-to-mred `(test:button-push ,button)))])
	(lambda ()
	  (exit/push-button "Cancel")
	  (exit/push-button "Cancel")
	  (with-handlers ([eof-result? (lambda (x) 'passed)])
	    (exit/push-button "Quit")
	    'failed))))

(define tmp-file (build-path (find-system-path 'temp-dir) "framework-exit-test-suite"))
(test 'exit-callback-called
      (lambda (x)
	(begin0 (and (file-exists? tmp-file) (not (mred-running?)))
		(when (file-exists? tmp-file) (delete-file tmp-file))))

      (lambda ()
	(when (file-exists? tmp-file) (delete-file tmp-file))
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred
	   `(begin
	      (preferences:set 'framework:verify-exit #f)
	      (exit:insert-callback (lambda () (call-with-output-file ,tmp-file void) #t))
	      (exit:exit))))))

(test 'exit-callback-removed
      (lambda (x) (and (eq? x 'passed) (not (mred-running?))))
      (lambda ()
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred
	   `(begin
	      (preferences:set 'framework:verify-exit #f)
	      ((exit:insert-callback (lambda () (error 'called-exit-callback))))
	      (exit:exit))))))

(test 'exit-callback-stops-exit
      (lambda (x) (eq? x 'passed))
      (lambda ()
	(begin0
	 (send-sexp-to-mred
	  `(begin
	     (preferences:set 'framework:verify-exit #f)
	     (let ([rm-callback (exit:insert-callback (lambda () #f))])
	       (exit:exit)
	       (rm-callback))))
	 (with-handlers ([eof-result? (lambda (x) 'passed)])
	   (send-sexp-to-mred
	    `(exit:exit))))))
