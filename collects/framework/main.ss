(unit/sig ()
  (import [preferences : framework:preferences^]
	  [exit : framework:exit^])
  
  ;; preferences

  (preferences:set-default 'mred:autosave-delay 300 number?)
  (preferences:set-default 'mred:autosaving-on? #t 
					   (lambda (x)
					     (or (not x)
						 (eq? x #t))))
  (preferences:set-default 'mred:verify-exit #t
			   (lambda (x)
			     (or (not x)
				 (eq? x #t))))
  

  (preferences:set-default 'mred:delete-forward? 
			   (not (eq? (system-type) 'unix))
			   (lambda (x)
			     (or (not x)
				 (eq? x #t))))
  
  (preferences:read)


  ;; misc other stuff

  (exit:insert-callback 
   (lambda ()
     (with-handlers ([(lambda (x) #t)
		      (lambda (exn)
			(mred:gui-utils:message-box
			 (format "Error saving preferences: ~a"
				 (exn-message exn))
			 "Saving Prefs"))])
       (save-user-preferences))))

  (wx:application-file-handler edit-file))
