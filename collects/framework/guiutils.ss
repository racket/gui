(unit/sig framework:gui-utils^
  (import mred^)

  (define clickback-delta (make-object style-delta% 'change-underline #t))
  (send clickback-delta set-delta-foreground "BLUE")
  (define (get-clickback-delta) clickback-delta)
  (define clicked-clickback-delta (make-object style-delta%))
  (send clicked-clickback-delta set-delta-background "BLACK")
  (define (get-clicked-clickback-delta) clicked-clickback-delta)

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
			  (begin (set! old-cursor (send win get-cursor))
                                 (send win set-cursor watch))
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
				 on-save-now
				 (if can-save-now?
				     '(border)
				     '()))]
			  [cancel (make-object button%
				    "Cancel"
				    button-panel
				    on-cancel
				    (if can-save-now?
					'()
					'(border)))])
		      (if can-save-now?
			  (send now focus)
			  (begin (send cancel focus)
				 (send now show #f)))))
		  
		  (center 'both)
		  
		  (show #t)))])
	(make-object unsaved-dialog%)
	result)))
  
  (define get-choice
    (case-lambda 
     [(message true-choice false-choice)
      (get-choice message true-choice false-choice "Warning")]
     [(message true-choice false-choice title)
      (get-choice message true-choice false-choice title 'disallow-close)]
     [(message true-choice false-choice title default-result)
      (letrec ([result default-result]
	       [dialog (make-object 
                        (class dialog% ()
                          (rename [super-on-close on-close]
                                  [super-can-close? can-close?])
                          (override
                            [can-close?
                             (lambda ()
                               (cond
                                 [(eq? default-result 'disallow-close)
                                  (bell)
                                  (message-box title
                                               (format "Please choose either \"~a\" or \"~a\""
                                                       true-choice false-choice))
                                  #f]
                                 [else
                                  (super-can-close?)]))]
                            [on-close
                             (lambda ()
                               (set! result default-result)
                               (super-on-close))])
                          (sequence
                            (super-init title))))]
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
	result)]))

  (define text-snip<%> (interface () get-string))
  (define read-snips/chars-from-text
    (letrec ([get-snips
	      (lambda (text start end)
		(let* ([pos-box (box 0)]
		       [snip (send text find-snip start 'after-or-none pos-box)])
		  (cond
		   [(not snip) null]
		   [(> (+ (unbox pos-box) (send snip get-count)) end) null]
		   [else (cons snip (get-snips text (+ start (send snip get-count)) end))])))])
      (case-lambda
       [(text) (read-snips/chars-from-text text 0)]
       [(text start) (read-snips/chars-from-text text start (send text last-position))]
       [(text start end)
	(define pos-box (box 0))
	(define (get-next)
	  (send text split-snip start)
	  (send text split-snip end)

	  ;; must get all of the snips out of the buffer before reading -- they may change.
	  (let loop ([snips (get-snips text start end)])

	    (cond
	     [(null? snips)
	      (set! get-next (lambda () eof))
	      eof]
	     [(or (is-a? (car snips) string-snip%)
                  (is-a? (car snips) text-snip<%>))
	      (let ([str (if (is-a? (car snips) text-snip<%>)
                             (send (car snips) get-string)
                             (send (car snips) get-text 0 (send (car snips) get-count)))])
		(let string-loop ([n 0])
		  (cond
		   [(< n (string-length str))
		    (set! get-next (lambda () (string-loop (+ n 1))))
		    (string-ref str n)]
		   [else
		    (loop (cdr snips))])))]
	     [else
	      (set! get-next (lambda () (loop (cdr snips))))
	      (car snips)])))
	(let ([read-snips/chars-from-text-thunk
	       (lambda ()
		 (get-next))])
	  read-snips/chars-from-text-thunk)])))
  
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
