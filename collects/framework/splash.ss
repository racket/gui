; filename : splash-image-path
; title : title of window

(lambda (filename title width-default depth-default)
  (let/ec k
    (let*-values
	([(no-splash) (lambda () (k void void void))]
	 [(splash-get-resource)
	  (lambda (name default)
	    (let ([b (box 0)])
	      (if (get-resource "mred" name b #f)
		  (unbox b)
		  default)))]
	 [(set-resource)
	  (lambda (name value)
	    (write-resource "mred" name value (find-path 'setup-file)))]
	 [(_)
	  (begin
	    (unless filename
	      (no-splash))
	    (unless (file-exists? filename)
	      (fprintf (current-error-port) "WARNING: bitmap path ~s not found~n" filename)
	      (no-splash)))]

	 [(splash-width-resource) (format "~a-splash-max-width" title)]
	 [(splash-depth-resource) (format "~a-splash-max-depth" title)]
	 [(splash-max-width) (splash-get-resource splash-width-resource width-default)]
	 [(splash-max-depth) (splash-get-resource splash-depth-resource depth-default)]
	 
	 [(splash-sofar-depth) 0]
	 [(splash-current-width) 0]

	 [(splitup-path)
	  (lambda (f)
	    (let*-values ([(absf) (if (relative-path? f)
				      (build-path (current-directory) f)
				      f)]
			  [(base name _1) (split-path absf)])

	      (if base
		  (let-values ([(base2 name2 _2) (split-path base)])
		    (if base2
			(let-values ([(base3 name3 _2) (split-path base2)])
			  (build-path name3 name2 name))
			(build-path name2 name)))
		  name)))]

	 [(quit-on-close?) #t]
	 [(splash-frame%)
	  (class frame% (title)
	    (override
	      [on-close
	       (lambda ()
		 (when quit-on-close?
		   (exit)))])
	    (sequence (super-init title)))]
	 [(frame) (parameterize ([current-eventspace (make-eventspace)])
		    (make-object splash-frame% title))]
	 [(bitmap-flag)
	  (let ([len (string-length filename)])
	    (if (<= len 4)
		'guess
		(let ([suffix (substring filename (- len 4) len)])
		  (cond
		   [(string-ci=? ".xpm" suffix) 'xpm]
		   [(string-ci=? ".xbm" suffix) 'xbm]
		   [(string-ci=? ".gif" suffix) 'gif]
		   [(string-ci=? "pict" suffix) 'pict]
		   [else 'xpm]))))]
	 [(bitmap) (make-object bitmap% filename bitmap-flag)]
	 [(_) (unless (send bitmap ok?)
		(fprintf (current-error-port) "WARNING: bad bitmap ~s" filename)
		(no-splash))]
	 [(canvas%)
	  (class canvas% args
	    (inherit get-dc)
	    (override
	     [on-paint
	      (lambda ()
		(send (get-dc) draw-bitmap bitmap 0 0))])
	    (sequence
	      (apply super-init args)))]
	 [(box1) (box 0.)]
	 [(box2) (box 0.)]
	 [(c-x-offset) 0]
	 [(c-y-offset) 0]
	 
	 [(logo-canvas) (make-object canvas% frame)]
	 [(show-messages?) (let ([b (box 0)])
			     (if (get-resource "mred" "splashMessages" b)
				 (not (zero? (unbox b)))
				 #f))]
	 [(gauge) (make-object gauge% #f splash-max-width frame)]
	 [(splash-messages)
	  (and show-messages?
	       (cons (make-object message% (format "Welcome to ~a" title) frame)
		     (let loop ([n (- splash-max-depth 1)])
		       (cond
			[(zero? n) null]
			[else (cons (make-object message% "" frame)
				    (loop (sub1 n)))]))))]
	 [(_) (begin
		(send frame set-alignment 'left 'center)
		(send logo-canvas min-width (send bitmap get-width))
		(send logo-canvas min-height (send bitmap get-height))
		(send logo-canvas stretchable-width #f)
		(send logo-canvas stretchable-height #f)
		(send frame center 'both)
		(send frame show #t)
		(flush-display) (yield) (sleep)
		(flush-display) (yield) (sleep))]
	 [(change-splash-message)
	  (letrec ([change-splash-message
		    (case-lambda 
		     [(s) (change-splash-message s 0 #f)]
		     [(s depth clear-after)
		      (if splash-messages
			  (unless (null? splash-messages)
			    (if (< depth splash-max-depth)
				(begin '(printf "setting depth ~a (of ~a) to ~s~n" depth splash-max-depth s)
				       (send (list-ref splash-messages depth) set-label s)
				       (when (and clear-after
						  (< (+ depth 1) splash-max-depth))
					 (let ([next-message (list-ref splash-messages (+ depth 1))])
					   (unless (string=? "" (send next-message get-label))
					     '(printf "clearing depth ~a (of ~a)~n"
						      (+ depth 1) current-splash-max-depth)
					     (send next-message set-label ""))))
				       #t)
				#f))
			  #t)])])
	    change-splash-message)]
	 [(splash-load-handler)
	  (let ([depth 0])
	    (lambda (old-load f)
	      (let ([finalf (splitup-path f)])
		(set! splash-sofar-depth (max (+ depth 1) splash-sofar-depth))
		(set! splash-current-width (+ splash-current-width 1))
		(when (change-splash-message (format "Loading ~a..." finalf) depth #f)
		  (when (<= splash-current-width splash-max-width)
		    (send gauge set-value splash-current-width)))
		(set! depth (+ depth 1))
		(begin0
		 (old-load f)
		 (begin (set! depth (- depth 1))
			(change-splash-message (format "Loading ~a...done." finalf)
					       depth #t))))))]
	 [(_) (current-load
	       (let ([old-load (current-load)])
		 (lambda (f)
		   (splash-load-handler old-load f))))]
	 [(shutdown-splash)
	  (lambda ()
	    (set! splash-load-handler (lambda (old-load f) (old-load f)))
	    (unless (= splash-max-width splash-current-width)
	      (set-resource splash-width-resource splash-current-width))
	    (unless (= splash-max-depth splash-sofar-depth)
	      (set-resource splash-depth-resource splash-sofar-depth)))]
	 [(close-splash)
	  (lambda ()
	    (set! quit-on-close? #f)
	    (send frame show #f))])
      (values
       change-splash-message
       shutdown-splash
       close-splash))))
