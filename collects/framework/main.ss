(unit/sig ()
  (import [preferences : framework:preferences^]
	  [exit : framework:exit^])
  
  ;; preferences

  (preferences:set-default 'framework:autosave-delay 300 number?)
  (preferences:set-default 'framework:autosaving-on? #t 
					   (lambda (x)
					     (or (not x)
						 (eq? x #t))))
  (preferences:set-default 'framework:verify-exit #t
			   (lambda (x)
			     (or (not x)
				 (eq? x #t))))
  (preferences:set-default 'framework:delete-forward? 
			   (not (eq? (system-type) 'unix))
			   (lambda (x)
			     (or (not x)
				 (eq? x #t))))
  (preferences:set 'framework:show-periods-in-dirlist #f
		   (lambda (x)
		     (or (not x)
			 (eq? x #t))))
  (preferences:set 'framework:file-dialogs
		   (if (eq? wx:platform 'unix)
		       'common
		       'std)
		   (lambda (x)
		     (or (eq? x 'common)
			 (eq? x 'std))))
  
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
