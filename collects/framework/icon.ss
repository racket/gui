(dunit/sig framework:icon^
  (import mred-interfaces^)

  (define icon-path 
    (with-handlers ([void (lambda (x) (collection-path "system"))]) 
      (collection-path "icons")))
  
  (define (load-icon name type)
    (let ([p (build-path icon-path name)]
	  [bitmap #f])
      (unless (file-exists? p)
	(fprintf (current-error-port) "WARNING: couldn't find ~a~n" p))
      (lambda ()
	(if bitmap
	    bitmap
	    (begin (set! bitmap (make-object bitmap% p type))
		   bitmap)))))
  
  (define (load-bitmap/bdc name type)
    (let* ([p (build-path icon-path name)]
	   [bitmap #f]
	   [bitmap-dc #f]
	   [force
	    (lambda ()
	      (set! bitmap (make-object bitmap% p type))
	      (set! bitmap-dc (make-object bitmap-dc%))
	      (when (send bitmap ok?)
		(send bitmap-dc select-object bitmap)))])
      (unless (file-exists? p)
	(fprintf (current-error-port) "WARNING: couldn't find ~a~n" p))
      (values 
       (lambda ()
	 (or bitmap
	     (begin (force)
		    bitmap)))
       (lambda ()
	 (or bitmap-dc
	     (begin (force)
		    bitmap-dc))))))
  
  (define-values (get-anchor-bitmap get-anchor-bdc)
    (load-bitmap/bdc "anchor.gif" 'gif))
  (define-values (get-lock-bitmap get-lock-bdc)
    (load-bitmap/bdc "lock.gif" 'gif))
  (define-values (get-unlock-bitmap get-unlock-bdc)
    (load-bitmap/bdc "unlock.gif" 'gif))
  
  (define get-autowrap-bitmap (load-icon "return.xbm" 'xbm))
  (define get-paren-highlight-bitmap (load-icon "paren.xbm" 'xbm))
  
  (define get
    (let ([icon #f]
	  [p (build-path icon-path "mred.xbm")])
      (unless (file-exists? p)
	(fprintf (current-error-port) "WARNING: couldn't find ~a~n" p))
      (lambda ()
	(or icon
	    (begin
	      (set! icon (make-object bitmap% p 'xbm))
	      icon)))))
  
  (define gc-on-bitmap #f)
  (define gc-on-bdc #f)
  (define (fetch)
    (unless gc-on-bdc
      (set! gc-on-bdc (make-object bitmap-dc%))
      (set! gc-on-bitmap ((load-icon "recycle.gif" 'gif)))
      (send gc-on-bdc select-object gc-on-bitmap)))

  (define (get-gc-on-dc) (fetch) gc-on-bdc)
  (define (get-gc-width) (fetch) (if (send gc-on-bitmap ok?)
				     (send gc-on-bitmap get-width)
				     10))
  (define (get-gc-height) (fetch) (if (send gc-on-bitmap ok?)
				      (send gc-on-bitmap get-height)
				      10))
  
  (define get-gc-off-dc 
    (let ([bdc #f])
      (lambda ()
	(if bdc
	    bdc
	    (begin
	      (set! bdc (make-object bitmap-dc%))
	      (send bdc select-object
		    (make-object bitmap%
				 (get-gc-width)
				 (get-gc-height)))
	      (send bdc clear)
	      bdc))))))
