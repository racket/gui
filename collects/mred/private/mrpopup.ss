(module mrpopup mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
	   (lib "list.ss")
	   (prefix wx: "kernel.ss")
	   "lock.ss"
	   "const.ss"
	   "helper.ss"
	   "check.ss"
	   "wx.ss"
	   "wxmenu.ss"
	   "mrmenuintf.ss")

  (provide popup-menu%)

  (define popup-menu%
    (class100* mred% (menu-item-container<%> internal-menu<%>) ([title #f][popdown-callback void][demand-callback void])
      (private-field 
       [callback demand-callback])
      (public
	[get-popup-target
	 (lambda ()
	   (send wx get-popup-grabber))]
	[get-items (entry-point (lambda () (send wx get-items)))]
	[on-demand (lambda ()
		     (callback this)
		     (for-each
		      (lambda (i) 
			(when (is-a? i labelled-menu-item<%>)
			  (send i on-demand)))
		      (send wx get-items)))]
	[set-min-width (lambda (n)
			 (check-range-integer '(method popup-menu% set-min-width) n)
			 (send wx set-width n))])
      (private-field
       [wx #f])
      (sequence
	(check-label-string/false '(constructor popup-menu) title)
	(check-callback '(constructor popup-menu) popdown-callback)
	(check-callback1 '(constructor popup-menu) demand-callback)
	(as-entry 
	 (lambda ()
	   (set! wx (make-object wx-menu% this title
				 (lambda (mwx e)
				   (let ([go
					  (lambda ()
					    (let ([wx (wx:id-to-menu-item (send e get-menu-id))])
					      (when wx
						(send (wx->mred wx) command (make-object wx:control-event% 'menu)))
					      (dynamic-wind
						  void
						  (lambda ()
						    (popdown-callback this (make-object wx:control-event% 
											(if wx 
											    'menu-popdown
											    'menu-popdown-none))))
						  (lambda () (send mwx popup-release)))))])
				     (if (eq? 'windows (system-type))
					 (wx:queue-callback go wx:middle-queue-key)
					 (go))))))
	   (super-init wx)))))))
