(module icon mzscheme
  (require (lib "unitsig.ss")
	   (lib "class.ss")
	   "bday.ss"
           "sig.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide icon@)
  
  (define icon@
    (unit/sig framework:icon^
      (import mred^)

      (define icon-path 
	(with-handlers ([not-break-exn?
			 (lambda (x)
			   (case (system-type)
			     [(windows) "C:"] ;; just avoid quering the floppy drive
			     [else (car (filesystem-root-list))]))])
	  (collection-path "icons")))
      
      (define (load-icon name type)
	(letrec ([p (build-path icon-path name)]
		 [f
		  (lambda ()
		    (let ([bitmap (make-object bitmap% p type)])
		      (set! f (lambda () bitmap))
		      bitmap))])
	  (lambda ()
	    (f))))
      
      (define (load-bitmap name type)
	(letrec ([p (build-path icon-path name)]
		 [f
		  (lambda ()
		    (let ([bitmap (make-object bitmap% p type)])
                      (set! f (lambda () bitmap))
                      bitmap))])
	  (lambda ()
	    (f))))
      
      (define-values (get-anchor-bitmap) (load-bitmap "anchor.gif" 'gif))
      (define-values (get-lock-bitmap) (load-bitmap "lock.gif" 'gif))
      (define-values (get-unlock-bitmap) (load-bitmap "unlock.gif" 'gif))
      
      (define get-autowrap-bitmap (load-icon "return.xbm" 'xbm))
      (define get-paren-highlight-bitmap (load-icon "paren.xbm" 'xbm))
      
      (define (make-get/mask filename type)
	(let ([icon #f]
	      [p (build-path icon-path filename)])
	  (lambda ()
	    (or icon
		(begin
		  (set! icon (make-object bitmap% p type))
		  icon)))))

      (define (make-cursor name mask fallback)
        (let* ([msk-b (make-object bitmap% (build-path icon-path mask))]
	       [csr-b (make-object bitmap% (build-path icon-path name))])
	  (if (and (send msk-b ok?)
		   (send csr-b ok?))
	      (let ([csr (make-object cursor% msk-b csr-b 7 7)])
		(if (send csr ok?)
		    csr
		    (make-object cursor% fallback)))
	      (make-object cursor% fallback))))
      
      (define up/down-cursor (make-cursor "up-down-cursor.xbm" "up-down-mask.xbm" 'size-n/s))
      (define (get-up/down-cursor) up/down-cursor)
      (define left/right-cursor (make-cursor "left-right-cursor.xbm" "left-right-mask.xbm" 'size-e/w))
      (define (get-left/right-cursor) left/right-cursor)
      
      (define gc-on-bitmap #f)

      (define (fetch)
	(unless gc-on-bitmap
	  (set! gc-on-bitmap 
                (if (mrf-bday?)
                    ((load-icon "mrf.jpg" 'jpeg))
                    ((load-icon "recycle.gif" 'gif))))))
      
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
		    bitmap)))))))))
