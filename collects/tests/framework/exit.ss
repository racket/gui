(test 'exit:exit
      (lambda (x) (not (and (eq? x 'passed)
			    (not (mred-running?)))))
      (lambda ()
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred '(preferences:set 'framework:verify-exit #f))
	  (send-sexp-to-mred '(exit:exit))
	  'failed)))

(test 'exit:exit
      (lambda (x) (not (and (eq? x 'passed)
			    (not (mred-running?)))))
      (lambda ()
	(with-handlers ([eof-result? (lambda (x) 'passed)])
	  (send-sexp-to-mred '(preferences:set 'framework:verify-exit #t))
	  (send-sexp-to-mred '(exit:exit))
	  'failed)))
