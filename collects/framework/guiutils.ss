(unit/sig framework:gui-utils^
  (import mred-interfaces^)

  (define next-untitled-name
    (let ([n 1])
      (lambda ()
	(begin0
	 (cond
	  [(= n 1) "Untitled"]
	  [else (format "Untitled ~a" n)])
	 (set! n (+ n 1))))))

  (define cursor-delay
    (let ([x 0.25])
      (case-lambda
       [() x]
       [(v) (set! x v) x])))

  (define show-busy-cursor
    (lambda (thunk)
      (local-busy-cursor #f thunk)))

  (define delay-action
    (lambda (delay-time open close)
      (let ([semaphore (make-semaphore 1)]
	    [open? #f]
	    [skip-it? #f])
	(thread 
	 (lambda ()
	   (sleep delay-time)
	   (semaphore-wait semaphore)
	   (unless skip-it?
	     (set! open? #t)
	     (open))
	   (semaphore-post semaphore)))
	(lambda ()
	  (semaphore-wait semaphore)
	  (set! skip-it? #t)
	  (when open?
	    (close))
	  (semaphore-post semaphore)))))

  (define local-busy-cursor
    (let ([watch (make-object cursor% 'watch)])
      (opt-lambda (win thunk [delay (cursor-delay)])
	(let* ([old-cursor #f]
	       [cursor-off void])
	  (dynamic-wind
	   (lambda ()
	     (set! cursor-off
		   (delay-action
		    delay
		    (lambda ()
		      (if win
			  (set! old-cursor (send win set-cursor watch))
			  (begin-busy-cursor)))
		    (lambda ()
		      (if win
			  (send win set-cursor old-cursor)
			  (end-busy-cursor))))))
	   (lambda () (thunk))
	   (lambda () (cursor-off)))))))

  (define unsaved-warning
    (opt-lambda (filename action [can-save-now? #f])
      (let* ([result (void)]
	     [unsaved-dialog%
	      (class dialog% ()
		(inherit show center)
		(private
		  [on-dont-save
		   (lambda args
		     (set! result 'continue)
		     (show #f))]
		  [on-save-now
		   (lambda rags
		     (set! result 'save)
		     (show #f))]
		  [on-cancel
		   (lambda args
		     (set! result 'cancel)
		     (show #f))])
		(sequence
		  (super-init "Warning")
		  (let* ([panel (make-object vertical-panel% this)]
			 [msg
			  (make-object message% 
			    (string-append "The file \""
					   filename
					   "\" is not saved.")
			    panel)]
			 [button-panel
			  (make-object horizontal-panel% panel)])
		    (make-object button% 
		      (string-append action " Anyway")
		      button-panel
		      on-dont-save)
		    (let ([now (make-object button% 
				 "Save"
				 button-panel
				 on-save-now)]
			  [cancel (make-object button%
				    "Cancel"
				    button-panel
				    on-cancel)])
		      (if (not can-save-now?)
			  (begin (send cancel focus)
				 (send now show #f))
			  (send now focus))))
		  
		  (center 'both)
		  
		  (show #t)))])
	(make-object unsaved-dialog%)
	result)))
  
  (define get-choice
    (opt-lambda (message true-choice false-choice [title "Warning"])
      (letrec ([result #f]
	       [dialog (make-object dialog% title)]
	       [on-true
		(lambda args
		  (set! result #t)
		  (send dialog show #f))]
	       [on-false
		(lambda rags
		  (set! result #f)
		  (send dialog show #f))]
	       [vp (make-object vertical-panel% dialog)]
	       [hp (make-object horizontal-panel% dialog)])

	(let loop ([m message])
	  (let ([match (regexp-match (format "^([^~n]*)~n(.*)")
				     m)])
	    (if match
		(begin (make-object message% (cadr match) vp)
		       (loop (caddr match)))
		(make-object message% m vp))))

	(send vp set-alignment 'left 'center)
	(send hp set-alignment 'right 'center)
	(send (make-object button% true-choice hp on-true '(border)) focus)
	(make-object button% false-choice hp on-false)
	(send dialog center 'both)
	(send dialog show #t)
	result)))

  (define read-snips/chars-from-buffer
    (opt-lambda (edit [start 0] [end (send edit last-position)])
      (let ([pos start]
	    [box (box 0)])
	(lambda ()
	  (let* ([snip (send edit find-snip pos 'after-or-none box)]
		 [ans
		  (cond
		    [(<= end pos) eof]
		    [(not snip) eof]
		    [(is-a? snip original:string-snip%)
		     (let ([t (send snip get-text (- pos (unbox box)) 1)])
		       (unless (= (string-length t) 1)
			 (error 'read-snips/chars-from-buffer
				"unexpected string, t: ~s; pos: ~a box: ~a"
				t pos box))
		       (string-ref t 0))]
		    [else snip])])
	    (set! pos (add1 pos))
	    ans)))))

  (define open-input-buffer
    (lambda (buffer)
      (let ([pos 0])
	(make-input-port
	 (lambda ()
	   (let ([c (send buffer get-character pos)])
	     (if (char=? c #\null)
		 eof
		 (begin
		   (set! pos (add1 pos))
		   c))))
	 (lambda ()
	   #t)
	 (lambda ()
	   (void)))))))
