(unit/sig framework:icon^
  (import mred^)

  (define icon-path 
    (with-handlers ([void (lambda (x) (collection-path "mzlib"))]) 
      (collection-path "icons")))
  
  (define (load-icon name type)
    (letrec ([p (build-path icon-path name)]
	     [f
	      (lambda ()
		(let ([bitmap (make-object bitmap% p type)])
		  (set! f (lambda () bitmap))
		  bitmap))])
      (unless (file-exists? p)
	(fprintf (current-error-port) "WARNING: couldn't find ~a~n" p))
      (lambda ()
	(f))))
  
  (define (load-bitmap name type)
    (letrec ([p (build-path icon-path name)]
	     [f
	      (lambda ()
		(let ([bitmap (make-object bitmap% p type)])
		  (set! f (lambda () bitmap))
		  bitmap))])
      (unless (file-exists? p)
	(fprintf (current-error-port) "WARNING: couldn't find ~a~n" p))
      (lambda ()
	(f))))
  
  (define-values (get-anchor-bitmap) (load-bitmap "anchor.gif" 'gif))
  (define-values (get-lock-bitmap) (load-bitmap "lock.gif" 'gif))
  (define-values (get-unlock-bitmap) (load-bitmap "unlock.gif" 'gif))
  
  (define get-autowrap-bitmap (load-icon "return.xbm" 'xbm))
  (define get-paren-highlight-bitmap (load-icon "paren.bmp" 'bmp))
  
  (define (make-get/mask filename type)
    (let ([icon #f]
	  [p (build-path icon-path filename)])
      (unless (file-exists? p)
	(fprintf (current-error-port) "WARNING: couldn't find ~a~n" p))
      (lambda ()
	(or icon
	    (begin
	      (set! icon (make-object bitmap% p type))
	      icon)))))
  
  ;(define get (make-get/mask "plt16x16.bmp" 'bmp))
  ;(define get-mask (make-get/mask "dot16x16.xbm" 'xbm))
  
  (define gc-on-bitmap #f)

  (define (fetch)
    (unless gc-on-bitmap
      (set! gc-on-bitmap ((load-icon "recycle.gif" 'gif)))))

  (define (get-gc-on-bitmap) (fetch) gc-on-bitmap)
  
  (define get-gc-off-bitmap
    (let ([bitmap #f])
      (lambda ()
	(if bitmap
	    bitmap
	    (begin
	      (let ([bdc (make-object bitmap-dc%)]
		    [onb (get-gc-on-bitmap)])
		(set! bitmap (make-object bitmap%
			       (send onb get-width)
			       (send onb get-height)))
		(send bdc set-bitmap bitmap)
		(send bdc clear)
		(send bdc set-bitmap #f)
		bitmap)))))))
